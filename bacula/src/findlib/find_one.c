/* 
   Copyright (C) 2000-2003 Kern Sibbald and John Walker

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of
   the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public
   License along with this program; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA.

   This file is based on GNU TAR source code. Except for a few key
   ideas, it has been rewritten for Bacula.

      Kern Sibbald, MM

   Thanks to the TAR programmers.

 */

#include "bacula.h"
#include "find.h"

extern int32_t name_max;	      /* filename max length */
extern int32_t path_max;	      /* path name max length */

/*
 * Structure for keeping track of hard linked files, we   
 *   keep an entry for each hardlinked file that we save,
 *   which is the first one found. For all the other files that
 *   are linked to this one, we save only the directory
 *   entry so we can link it.
 */
struct f_link {
    struct f_link *next;
    dev_t dev;			      /* device */
    ino_t ino;			      /* inode with device is unique */
    short linkcount;
    uint32_t FileIndex; 	      /* Bacula FileIndex of this file */
    char name[1];		      /* The name */
};

static void free_dir_ff_pkt(FF_PKT *dir_ff_pkt)
{
   free(dir_ff_pkt->fname);
   free(dir_ff_pkt->link);
   free_pool_memory(dir_ff_pkt->sys_fname);
   free(dir_ff_pkt);
}

/*
 * Find a single file.			      
 * handle_file is the callback for handling the file.
 * p is the filename
 * parent_device is the device we are currently on 
 * top_level is 1 when not recursing or 0 when 
 *  decending into a directory.
 */
int
find_one_file(JCR *jcr, FF_PKT *ff_pkt, int handle_file(FF_PKT *ff, void *hpkt), 
	       void *pkt, char *fname, dev_t parent_device, int top_level)
{
   struct utimbuf restore_times;
   int rtn_stat;

   ff_pkt->fname = ff_pkt->link = fname;

   if (lstat(fname, &ff_pkt->statp) != 0) {
       /* Cannot stat file */
       ff_pkt->type = FT_NOSTAT;
       ff_pkt->ff_errno = errno;
       return handle_file(ff_pkt, pkt);
   }

   Dmsg1(300, "File ----: %s\n", fname);

   /* Save current times of this directory in case we need to
    * reset them because the user doesn't want them changed.
    */
   restore_times.actime = ff_pkt->statp.st_atime;
   restore_times.modtime = ff_pkt->statp.st_mtime;


   /* 
    * If this is an Incremental backup, see if file was modified
    * since our last "save_time", presumably the last Full save
    * or Incremental.
    */
   if (ff_pkt->incremental && !S_ISDIR(ff_pkt->statp.st_mode)) {
      Dmsg1(300, "Non-directory incremental: %s\n", ff_pkt->fname);
      /* Not a directory */
      if (ff_pkt->statp.st_mtime < ff_pkt->save_time
	  && (ff_pkt->mtime_only || 
	      ff_pkt->statp.st_ctime < ff_pkt->save_time)) {
	 /* Incremental option, file not changed */
	 ff_pkt->type = FT_NOCHG;
	 return handle_file(ff_pkt, pkt);
      }
   }

/* ***FIXME*** implement this */
#if xxxxxxx
   /* See if we are trying to dump the archive.  */
   if (ar_dev && ff_pkt->statp.st_dev == ar_dev && ff_pkt->statp.st_ino == ar_ino) {
       ff_pkt->type = FT_ISARCH;
       return handle_file(ff_pkt, pkt);
   }
#endif
   ff_pkt->LinkFI = 0;
   /* 
    * Handle hard linked files
    *
    * Maintain a list of hard linked files already backed up. This
    *  allows us to ensure that the data of each file gets backed 
    *  up only once.
    */
   if (ff_pkt->statp.st_nlink > 1
       && (S_ISREG(ff_pkt->statp.st_mode)
	   || S_ISCHR(ff_pkt->statp.st_mode)
	   || S_ISBLK(ff_pkt->statp.st_mode)
	   || S_ISFIFO(ff_pkt->statp.st_mode)
	   || S_ISSOCK(ff_pkt->statp.st_mode))) {

       struct f_link *lp;

      /* Search link list of hard linked files */
      for (lp = ff_pkt->linklist; lp; lp = lp->next)
	 if (lp->ino == ff_pkt->statp.st_ino && lp->dev == ff_pkt->statp.st_dev) {
             /* If we have already backed up the hard linked file don't do it again */
	     if (strcmp(lp->name, fname) == 0) {
                Jmsg1(jcr, M_WARNING, 0, _("Attempt to backup hard linked file %s twice ignored.\n"),
		   fname);
		return 1;	      /* ignore */
	     }
	     ff_pkt->link = lp->name;
	     ff_pkt->type = FT_LNKSAVED;       /* Handle link, file already saved */
	     ff_pkt->LinkFI = lp->FileIndex;
	     return handle_file(ff_pkt, pkt);
	 }

      /* File not previously dumped. Chain it into our list. */
      lp = (struct f_link *)bmalloc(sizeof(struct f_link) + strlen(fname) +1);
      lp->ino = ff_pkt->statp.st_ino;
      lp->dev = ff_pkt->statp.st_dev;
      strcpy(lp->name, fname);
      lp->next = ff_pkt->linklist;
      ff_pkt->linklist = lp;
      ff_pkt->linked = lp;	      /* mark saved link */
   } else {
      ff_pkt->linked = NULL;
   }

   /* This is not a link to a previously dumped file, so dump it.  */
   if (S_ISREG(ff_pkt->statp.st_mode)) {
      off_t sizeleft;

      sizeleft = ff_pkt->statp.st_size;

      /* Don't bother opening empty, world readable files.  Also do not open
	 files when archive is meant for /dev/null.  */
      if (ff_pkt->null_output_device || (sizeleft == 0
	      && MODE_RALL == (MODE_RALL & ff_pkt->statp.st_mode))) {
	 ff_pkt->type = FT_REGE;
      } else {
	 ff_pkt->type = FT_REG;
      }
      rtn_stat = handle_file(ff_pkt, pkt);
      if (ff_pkt->linked) {
	 ff_pkt->linked->FileIndex = ff_pkt->FileIndex;
      }
      return rtn_stat;


   } else if (S_ISLNK(ff_pkt->statp.st_mode)) {  /* soft link */
      int size;
      char *buffer = (char *)alloca(path_max + name_max + 102);

      size = readlink(fname, buffer, path_max + name_max + 101);
      if (size < 0) {
	 /* Could not follow link */				 
	 ff_pkt->type = FT_NOFOLLOW;
	 ff_pkt->ff_errno = errno;
	 rtn_stat = handle_file(ff_pkt, pkt);
	 if (ff_pkt->linked) {
	    ff_pkt->linked->FileIndex = ff_pkt->FileIndex;
	 }
	 return rtn_stat;
      }
      buffer[size] = 0;
      ff_pkt->link = buffer;	      /* point to link */
      ff_pkt->type = FT_LNK;	      /* got a real link */
      rtn_stat = handle_file(ff_pkt, pkt);
      if (ff_pkt->linked) {
	 ff_pkt->linked->FileIndex = ff_pkt->FileIndex;
      }
      return rtn_stat;

   } else if (S_ISDIR(ff_pkt->statp.st_mode)) {
      DIR *directory;
      struct dirent *entry, *result;
      char *link;
      int link_len;
      int len;	 
      int status;
      dev_t our_device = ff_pkt->statp.st_dev;

      /*  
       * If we are using Win32 (non-portable) backup API, don't check
       *  access as everything is more complicated, and
       *  in principle, we should be able to access everything.
       */
      if (!have_win32_api() || (ff_pkt->flags & FO_PORTABLE)) {
	 if (access(fname, R_OK) == -1 && geteuid() != 0) {
	    /* Could not access() directory */
	    ff_pkt->type = FT_NOACCESS;
	    ff_pkt->ff_errno = errno;
	    rtn_stat = handle_file(ff_pkt, pkt);
	    if (ff_pkt->linked) {
	       ff_pkt->linked->FileIndex = ff_pkt->FileIndex;
	    }
	    return rtn_stat;
	 }
      }

      /* Build a canonical directory name with a trailing slash in link var */
      len = strlen(fname);
      link_len = len + 200;
      link = (char *)bmalloc(link_len + 2);
      bstrncpy(link, fname, link_len);
      /* Strip all trailing slashes */
      while (len >= 1 && link[len - 1] == '/')
	len--;
      link[len++] = '/';             /* add back one */
      link[len] = 0;

      ff_pkt->link = link;
      if (ff_pkt->incremental &&
	  (ff_pkt->statp.st_mtime < ff_pkt->save_time &&
	   ff_pkt->statp.st_ctime < ff_pkt->save_time)) {
	 /* Incremental option, directory entry not changed */
	 ff_pkt->type = FT_DIRNOCHG;
      } else {
	 ff_pkt->type = FT_DIR;
      }

      /*
       * Create a temporary ff packet for this directory
       *   entry, and defer handling the directory until
       *   we have recursed into it.  This saves the
       *   directory after all files have been processed, and
       *   during the restore, the directory permissions will
       *   be reset after all the files have been restored.
       */
      Dmsg1(300, "Create temp ff packet for dir: %s\n", ff_pkt->fname);
      FF_PKT *dir_ff_pkt = (FF_PKT *)bmalloc(sizeof(FF_PKT));
      memcpy(dir_ff_pkt, ff_pkt, sizeof(FF_PKT));
      dir_ff_pkt->fname = bstrdup(ff_pkt->fname);
      dir_ff_pkt->link = bstrdup(ff_pkt->link);
      dir_ff_pkt->sys_fname = get_pool_memory(PM_FNAME);
      dir_ff_pkt->included_files_list = NULL;
      dir_ff_pkt->excluded_files_list = NULL;
      dir_ff_pkt->excluded_paths_list = NULL;
      dir_ff_pkt->linklist = NULL;
	
      ff_pkt->link = ff_pkt->fname;     /* reset "link" */

      /* 
       * Do not decend into subdirectories (recurse) if the
       * user has turned it off for this directory.
       */
      if (ff_pkt->flags & FO_NO_RECURSION) {
	 /* No recursion into this directory */
	 ff_pkt->type = FT_NORECURSE;
	 rtn_stat = handle_file(ff_pkt, pkt);
	 if (ff_pkt->linked) {
	    ff_pkt->linked->FileIndex = ff_pkt->FileIndex;
	 }
	 free(link);
	 free_dir_ff_pkt(dir_ff_pkt);
	 return rtn_stat;
      }

      /* 
       * See if we are crossing file systems, and
       * avoid doing so if the user only wants to dump one file system.
       */
      if (!top_level && !(ff_pkt->flags & FO_MULTIFS) &&
	   parent_device != ff_pkt->statp.st_dev) {
	 /* returning here means we do not handle this directory */
	 ff_pkt->type = FT_NOFSCHG;
	 rtn_stat = handle_file(ff_pkt, pkt);
	 if (ff_pkt->linked) {
	    ff_pkt->linked->FileIndex = ff_pkt->FileIndex;
	 }
	 free(link);
	 free_dir_ff_pkt(dir_ff_pkt);
	 return rtn_stat;
      }
      /* 
       * Decend into or "recurse" into the directory to read
       *   all the files in it.
       */
      errno = 0;
      if ((directory = opendir(fname)) == NULL) {
	 ff_pkt->type = FT_NOOPEN;
	 ff_pkt->ff_errno = errno;
	 rtn_stat = handle_file(ff_pkt, pkt);
	 if (ff_pkt->linked) {
	    ff_pkt->linked->FileIndex = ff_pkt->FileIndex;
	 }
	 free(link);
	 free_dir_ff_pkt(dir_ff_pkt);
	 return rtn_stat;
      }

      /*
       * Process all files in this directory entry (recursing).
       *    This would possibly run faster if we chdir to the directory
       *    before traversing it.
       */
      rtn_stat = 1;
      entry = (struct dirent *)malloc(sizeof(struct dirent) + name_max + 100);
      for ( ; !job_canceled(jcr); ) {
	 char *p, *q;
	 int i;

	 status  = readdir_r(directory, entry, &result);
	 if (status != 0 || result == NULL) {
	    break;
	 }
	 ASSERT(name_max+1 > (int)sizeof(struct dirent) + (int)NAMELEN(entry));
	 p = entry->d_name;
         /* Skip `.', `..', and excluded file names.  */
         if (p[0] == '\0' || (p[0] == '.' && (p[1] == '\0' ||
             (p[1] == '.' && p[2] == '\0')))) {
	    continue;
	 }

	 if ((int)NAMELEN(entry) + len >= link_len) {
	     link_len = len + NAMELEN(entry) + 1;
	     link = (char *)brealloc(link, link_len + 1);
	 }
	 q = link + len;
	 for (i=0; i < (int)NAMELEN(entry); i++) {
	    *q++ = *p++;
	 }
	 *q = 0;
	 if (!file_is_excluded(ff_pkt, link)) {
	    rtn_stat = find_one_file(jcr, ff_pkt, handle_file, pkt, link, our_device, 0);
	    if (ff_pkt->linked) {
	       ff_pkt->linked->FileIndex = ff_pkt->FileIndex;
	    }
	 }
      }
      closedir(directory);
      free(link);
      free(entry);

      /*
       * Now that we have recursed through all the files in the
       *  directory, we "save" the directory so that after all
       *  the files are restored, this entry will serve to reset
       *  the directory modes and dates.  Temp directory values
       *  were used without this record.
       */
      handle_file(dir_ff_pkt, pkt);	  /* handle directory entry */
      if (ff_pkt->linked) {
	 ff_pkt->linked->FileIndex = dir_ff_pkt->FileIndex;
      }
      free_dir_ff_pkt(dir_ff_pkt);

      if (ff_pkt->atime_preserve) {
	 utime(fname, &restore_times);
      }
      return rtn_stat;
   } /* end check for directory */

   /*
    * If it is explicitly mentioned (i.e. top_level) and is
    *  a block device, we do a raw backup of it or if it is
    *  a fifo, we simply read it.
    */
#ifdef HAVE_FREEBSD_OS
   /*
    * On FreeBSD, all block devices are character devices, so
    *	to be able to read a raw disk, we need the check for
    *	a character device.
    * crw-r-----  1 root  operator  - 116, 0x00040002 Jun  9 19:32 /dev/ad0s3
    * crw-r-----  1 root  operator  - 116, 0x00040002 Jun  9 19:32 /dev/rad0s3
    */
   if (top_level && (S_ISBLK(ff_pkt->statp.st_mode) || S_ISCHR(ff_pkt->statp.st_mode))) {
#else
   if (top_level && S_ISBLK(ff_pkt->statp.st_mode)) {
#endif
      ff_pkt->type = FT_RAW;	      /* raw partition */
   } else if (top_level && S_ISFIFO(ff_pkt->statp.st_mode) &&
	      ff_pkt->flags & FO_READFIFO) {
      ff_pkt->type = FT_FIFO;
   } else {
      /* The only remaining types are special (character, ...) files */
      ff_pkt->type = FT_SPEC;
   }
   rtn_stat = handle_file(ff_pkt, pkt);
   if (ff_pkt->linked) {
      ff_pkt->linked->FileIndex = ff_pkt->FileIndex;
   }
   return rtn_stat;
}

int term_find_one(FF_PKT *ff)
{
   struct f_link *lp, *lc;
   int count = 0;
  
   /* Free up list of hard linked files */
   for (lp = ff->linklist; lp;) {
      lc = lp;
      lp = lp->next;
      if (lc) {
	 free(lc);
	 count++;
      }
   }
   return count;
}
