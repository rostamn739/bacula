/*
 *
 *   Bacula Director -- Bootstrap Record routines.
 *
 *	BSR (bootstrap record) handling routines split from 
 *	  ua_restore.c July MMIII
 *
 *     Kern Sibbald, July MMII
 *
 *   Version $Id$
 */

/*
   Copyright (C) 2002-2003 Kern Sibbald and John Walker

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

 */

#include "bacula.h"
#include "dird.h"

/* Forward referenced functions */
static void write_bsr(UAContext *ua, RBSR *bsr, FILE *fd);


/*
 * Create new FileIndex entry for BSR 
 */
RBSR_FINDEX *new_findex() 
{
   RBSR_FINDEX *fi = (RBSR_FINDEX *)bmalloc(sizeof(RBSR_FINDEX));
   memset(fi, 0, sizeof(RBSR_FINDEX));
   return fi;
}

/* Free all BSR FileIndex entries */
static void free_findex(RBSR_FINDEX *fi)
{
   if (fi) {
      free_findex(fi->next);
      free(fi);
   }
}

/*
 * Our data structures were not designed completely
 *  correctly, so the file indexes cover the full
 *  range regardless of volume. The FirstIndex and LastIndex
 *  passed in here are for the current volume, so when 
 *  writing out the fi, constrain them to those values.
 */
static void write_findex(UAContext *ua, RBSR_FINDEX *fi, 
	      int32_t FirstIndex, int32_t LastIndex, FILE *fd) 
{
   if (fi) {
      int32_t findex, findex2;
      findex = fi->findex < FirstIndex ? FirstIndex : fi->findex;
      findex2 = fi->findex2 > LastIndex ? LastIndex : fi->findex2;
      if (findex == findex2) {
         fprintf(fd, "FileIndex=%d\n", findex);
      } else {
         fprintf(fd, "FileIndex=%d-%d\n", findex, findex2);
      }
      write_findex(ua, fi->next, FirstIndex, LastIndex, fd);
   }
}

static bool is_volume_selected(RBSR_FINDEX *fi, 
	      int32_t FirstIndex, int32_t LastIndex) 
{
   if (fi) {
      if ((fi->findex >= FirstIndex && fi->findex <= LastIndex) ||
	  (fi->findex2 >= FirstIndex && fi->findex2 <= LastIndex)) {
	 return true;
      }
      return is_volume_selected(fi->next, FirstIndex, LastIndex);
   }
   return false;
}



static void print_findex(UAContext *ua, RBSR_FINDEX *fi)
{
   if (fi) {
      if (fi->findex == fi->findex2) {
         bsendmsg(ua, "FileIndex=%d\n", fi->findex);
      } else {
         bsendmsg(ua, "FileIndex=%d-%d\n", fi->findex, fi->findex2);
      }
      print_findex(ua, fi->next);
   }
}

/* Create a new bootstrap record */
RBSR *new_bsr()
{
   RBSR *bsr = (RBSR *)bmalloc(sizeof(RBSR));
   memset(bsr, 0, sizeof(RBSR));
   return bsr;
}

/* Free the entire BSR */
void free_bsr(RBSR *bsr)
{
   if (bsr) {
      free_findex(bsr->fi);
      free_bsr(bsr->next);
      if (bsr->VolParams) {
	 free(bsr->VolParams);
      }
      free(bsr);
   }
}

/*
 * Complete the BSR by filling in the VolumeName and
 *  VolSessionId and VolSessionTime using the JobId
 */
int complete_bsr(UAContext *ua, RBSR *bsr)
{
   if (bsr) {
      JOB_DBR jr;
      memset(&jr, 0, sizeof(jr));
      jr.JobId = bsr->JobId;
      if (!db_get_job_record(ua->jcr, ua->db, &jr)) {
         bsendmsg(ua, _("Unable to get Job record. ERR=%s\n"), db_strerror(ua->db));
	 return 0;
      }
      bsr->VolSessionId = jr.VolSessionId;
      bsr->VolSessionTime = jr.VolSessionTime;
      if ((bsr->VolCount=db_get_job_volume_parameters(ua->jcr, ua->db, bsr->JobId, 
	   &(bsr->VolParams))) == 0) {
         bsendmsg(ua, _("Unable to get Job Volume Parameters. ERR=%s\n"), db_strerror(ua->db));
	 if (bsr->VolParams) {
	    free(bsr->VolParams);
	    bsr->VolParams = NULL;
	 }
	 return 0;
      }
      return complete_bsr(ua, bsr->next);
   }
   return 1;
}

/*
 * Write the bootstrap record to file
 */
int write_bsr_file(UAContext *ua, RBSR *bsr)
{
   FILE *fd;
   POOLMEM *fname = get_pool_memory(PM_MESSAGE);
   int stat;

   Mmsg(&fname, "%s/restore.bsr", working_directory);
   fd = fopen(fname, "w+");
   if (!fd) {
      bsendmsg(ua, _("Unable to create bootstrap file %s. ERR=%s\n"), 
	 fname, strerror(errno));
      free_pool_memory(fname);
      return 0;
   }
   /* Write them to file */
   write_bsr(ua, bsr, fd);
   stat = !ferror(fd);
   fclose(fd);
   bsendmsg(ua, _("Bootstrap records written to %s\n"), fname);

   /* Tell the user what he will need to mount */
   bsendmsg(ua, "\n");
   bsendmsg(ua, _("The restore job will require the following Volumes:\n"));
   /* Create Unique list of Volumes using prompt list */
   start_prompt(ua, "");
   for (RBSR *nbsr=bsr; nbsr; nbsr=nbsr->next) {
      for (int i=0; i < nbsr->VolCount; i++) {
	 if (nbsr->VolParams[i].VolumeName[0]) {
	    add_prompt(ua, nbsr->VolParams[i].VolumeName);
	 }
      }
   }
   for (int i=0; i < ua->num_prompts; i++) {
      bsendmsg(ua, "   %s\n", ua->prompt[i]);
      free(ua->prompt[i]);
   }
   ua->num_prompts = 0;
   bsendmsg(ua, "\n");
   free_pool_memory(fname);
   return stat;
}

static void write_bsr(UAContext *ua, RBSR *bsr, FILE *fd)
{
   if (bsr) {
      for (int i=0; i < bsr->VolCount; i++) {
	 if (!is_volume_selected(bsr->fi, bsr->VolParams[i].FirstIndex,
	      bsr->VolParams[i].LastIndex)) {
	    bsr->VolParams[i].VolumeName[0] = 0;  /* zap VolumeName */
	    continue;
	 }
         fprintf(fd, "Volume=\"%s\"\n", bsr->VolParams[i].VolumeName);
         fprintf(fd, "VolSessionId=%u\n", bsr->VolSessionId);
         fprintf(fd, "VolSessionTime=%u\n", bsr->VolSessionTime);
         fprintf(fd, "VolFile=%u-%u\n", bsr->VolParams[i].StartFile, 
		 bsr->VolParams[i].EndFile);
         fprintf(fd, "VolBlock=%u-%u\n", bsr->VolParams[i].StartBlock,
		 bsr->VolParams[i].EndBlock);

//       Dmsg2(000, "bsr VolParam FI=%u LI=%u\n",
//	    bsr->VolParams[i].FirstIndex, bsr->VolParams[i].LastIndex);
	 write_findex(ua, bsr->fi, bsr->VolParams[i].FirstIndex,
	    bsr->VolParams[i].LastIndex, fd);
      }
      write_bsr(ua, bsr->next, fd);
   }
}

static void print_bsr(UAContext *ua, RBSR *bsr)
{
   if (bsr) {
      for (int i=0; i < bsr->VolCount; i++) {
         bsendmsg(ua, "Volume=\"%s\"\n", bsr->VolParams[i].VolumeName);
         bsendmsg(ua, "VolSessionId=%u\n", bsr->VolSessionId);
         bsendmsg(ua, "VolSessionTime=%u\n", bsr->VolSessionTime);
         bsendmsg(ua, "VolFile=%u-%u\n", bsr->VolParams[i].StartFile, 
		  bsr->VolParams[i].EndFile);
         bsendmsg(ua, "VolBlock=%u-%u\n", bsr->VolParams[i].StartBlock,
		  bsr->VolParams[i].EndBlock);
	 print_findex(ua, bsr->fi);
      }
      print_bsr(ua, bsr->next);
   }
}


/*
 * Add a FileIndex to the list of BootStrap records.
 *  Here we are only dealing with JobId's and the FileIndexes
 *  associated with those JobIds.
 */
void add_findex(RBSR *bsr, uint32_t JobId, int32_t findex)
{
   RBSR *nbsr;
   RBSR_FINDEX *fi, *lfi;

   if (findex == 0) {
      return;			      /* probably a dummy directory */
   }
   
   if (!bsr->fi) {		      /* if no FI add one */
      /* This is the first FileIndex item in the chain */
      bsr->fi = new_findex();
      bsr->JobId = JobId;
      bsr->fi->findex = findex;
      bsr->fi->findex2 = findex;
      return;
   }
   /* Walk down list of bsrs until we find the JobId */
   if (bsr->JobId != JobId) {
      for (nbsr=bsr->next; nbsr; nbsr=nbsr->next) {
	 if (nbsr->JobId == JobId) {
	    bsr = nbsr;
	    break;
	 }
      }

      if (!nbsr) {		      /* Must add new JobId */
	 /* Add new JobId at end of chain */
	 for (nbsr=bsr; nbsr->next; nbsr=nbsr->next) 
	    {  }
	 nbsr->next = new_bsr();
	 nbsr->next->JobId = JobId;
	 nbsr->next->fi = new_findex();
	 nbsr->next->fi->findex = findex;
	 nbsr->next->fi->findex2 = findex;
	 return;
      }
   }

   /* 
    * At this point, bsr points to bsr containing JobId,
    *  and we are sure that there is at least one fi record.
    */
   lfi = fi = bsr->fi;
   /* Check if this findex is smaller than first item */
   if (findex < fi->findex) {
      if ((findex+1) == fi->findex) {
	 fi->findex = findex;	      /* extend down */
	 return;
      }
      fi = new_findex();	      /* yes, insert before first item */
      fi->findex = findex;
      fi->findex2 = findex;
      fi->next = lfi;
      bsr->fi = fi;
      return;
   }
   /* Walk down fi chain and find where to insert insert new FileIndex */
   for ( ; fi; fi=fi->next) {
      if (findex == (fi->findex2 + 1)) {  /* extend up */
	 RBSR_FINDEX *nfi;     
	 fi->findex2 = findex;
	 if (fi->next && ((findex+1) == fi->next->findex)) { 
	    nfi = fi->next;
	    fi->findex2 = nfi->findex2;
	    fi->next = nfi->next;
	    free(nfi);
	 }
	 return;
      }
      if (findex < fi->findex) {      /* add before */
	 if ((findex+1) == fi->findex) {
	    fi->findex = findex;
	    return;
	 }
	 break;
      }
      lfi = fi;
   }
   /* Add to last place found */
   fi = new_findex();
   fi->findex = findex;
   fi->findex2 = findex;
   fi->next = lfi->next;
   lfi->next = fi;
   return;
}
