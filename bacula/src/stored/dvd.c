/*
 *
 *   dvd.c  -- Routines specific to DVD devices (and
 *             possibly other removable hard media). 
 *
 *    Nicolas Boichat, MMV
 *
 *   Version $Id$
 */
/*
   Copyright (C) 2005-2006 Kern Sibbald

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   version 2 as amended with additional clauses defined in the
   file LICENSE in the main source directory.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
   the file LICENSE for additional details.

 */

#include "bacula.h"
#include "stored.h"

/* Forward referenced functions */
static bool do_mount_dvd(DEVICE* dev, int mount, int dotimeout);
static void add_file_and_part_name(DEVICE *dev, POOL_MEM &archive_name);

/* 
 * Write the current volume/part filename to archive_name.
 */
void make_mounted_dvd_filename(DEVICE *dev, POOL_MEM &archive_name) 
{
   pm_strcpy(archive_name, dev->device->mount_point);
   add_file_and_part_name(dev, archive_name);
}

void make_spooled_dvd_filename(DEVICE *dev, POOL_MEM &archive_name)
{
   /* Use the working directory if spool directory is not defined */
   if (dev->device->spool_directory) {
      pm_strcpy(archive_name, dev->device->spool_directory);
   } else {
      pm_strcpy(archive_name, working_directory);
   }
   add_file_and_part_name(dev, archive_name);
}      

static void add_file_and_part_name(DEVICE *dev, POOL_MEM &archive_name)
{
   char partnumber[20];
   if (archive_name.c_str()[strlen(archive_name.c_str())-1] != '/') {
      pm_strcat(archive_name, "/");
   }

   pm_strcat(archive_name, dev->VolCatInfo.VolCatName);
   /* if part > 1, append .# to the filename (where # is the part number) */
   if (dev->part > 1) {
      pm_strcat(archive_name, ".");
      bsnprintf(partnumber, sizeof(partnumber), "%d", dev->part);
      pm_strcat(archive_name, partnumber);
   }
   Dmsg2(400, "Exit add_file_part_name: arch=%s, part=%d\n",
                  archive_name.c_str(), dev->part);
}  

/* Mount the device.
 * If timeout, wait until the mount command returns 0.
 * If !timeout, try to mount the device only once.
 */
bool mount_dvd(DEVICE* dev, int timeout) 
{
   Dmsg0(90, "Enter mount_dvd\n");
   if (dev->is_mounted()) {
      return true;
   } else if (dev->requires_mount()) {
      return do_mount_dvd(dev, 1, timeout);
   }       
   return true;
}

/* Unmount the device
 * If timeout, wait until the unmount command returns 0.
 * If !timeout, try to unmount the device only once.
 */
bool unmount_dvd(DEVICE *dev, int timeout) 
{
   if (!dev->is_dvd()) {
      return true;
   }
   Dmsg0(90, "Enter unmount_dvd\n");
   if (dev->is_mounted()) {
      return do_mount_dvd(dev, 0, timeout);
   }
   return true;
}

/* (Un)mount the device */
static bool do_mount_dvd(DEVICE* dev, int mount, int dotimeout) 
{
   POOL_MEM ocmd(PM_FNAME);
   POOLMEM *results;
   char *icmd;
   int status, timeout;
   
   sm_check(__FILE__, __LINE__, false);
   if (mount) {
      if (dev->is_mounted()) {
         Dmsg0(200, "======= DVD mount=1\n");
         return true;
      }
      icmd = dev->device->mount_command;
   } else {
      if (!dev->is_mounted()) {
         Dmsg0(200, "======= DVD mount=0\n");
         return true;
      }
      icmd = dev->device->unmount_command;
   }
   
   dev->edit_mount_codes(ocmd, icmd);
   
   Dmsg2(200, "do_mount_dvd: cmd=%s mounted=%d\n", ocmd.c_str(), !!dev->is_mounted());

   if (dotimeout) {
      /* Try at most 1 time to (un)mount the device. This should perhaps be configurable. */
      timeout = 1;
   } else {
      timeout = 0;
   }
   results = get_memory(2000);
   results[0] = 0;
   /* If busy retry each second */
   Dmsg1(20, "Run mount prog=%s\n", ocmd.c_str());
   while ((status = run_program_full_output(ocmd.c_str(), 
                       dev->max_open_wait/2, results)) != 0) {
      Dmsg2(20, "Mount status=%d result=%s\n", status, results);
      /* Doesn't work with internationalization (This is not a problem) */
      if (mount && fnmatch("*is already mounted on*", results, 0) == 0) {
         break;
      }
      if (!mount && fnmatch("* not mounted*", results, 0) == 0) {
         break;
      }
      if (timeout-- > 0) {
         /* Sometimes the device cannot be mounted because it is already mounted.
          * Try to unmount it, then remount it */
         if (mount) {
            Dmsg1(400, "Trying to unmount the device %s...\n", dev->print_name());
            do_mount_dvd(dev, 0, 0);
         }
         bmicrosleep(1, 0);
         continue;
      }
      if (status != 0) {
         berrno be;
         Dmsg5(40, "Device %s cannot be %smounted. stat=%d result=%s ERR=%s\n", dev->print_name(),
              (mount ? "" : "un"), status, results, be.strerror(status));
         Mmsg(dev->errmsg, _("Device %s cannot be %smounted. ERR=%s\n"), 
              dev->print_name(), (mount ? "" : "un"), be.strerror(status));
      } else {
         Dmsg4(40, "Device %s cannot be %smounted. stat=%d ERR=%s\n", dev->print_name(),
              (mount ? "" : "un"), status, results);
         Mmsg(dev->errmsg, _("Device %s cannot be %smounted. ERR=%s\n"), 
              dev->print_name(), (mount ? "" : "un"), results);
      }
      /*
       * Now, just to be sure it is not mounted, try to read the
       *  filesystem.
       */
      DIR* dp;
      struct dirent *entry, *result;
      int name_max;
      int count;
      
      name_max = pathconf(".", _PC_NAME_MAX);
      if (name_max < 1024) {
         name_max = 1024;
      }
         
      if (!(dp = opendir(dev->device->mount_point))) {
         berrno be;
         dev->dev_errno = errno;
         Dmsg3(29, "do_mount_dvd: failed to open dir %s (dev=%s), ERR=%s\n", 
               dev->device->mount_point, dev->print_name(), be.strerror());
         goto get_out;
      }
      
      entry = (struct dirent *)malloc(sizeof(struct dirent) + name_max + 1000);
      count = 0;
      while (1) {
         if ((readdir_r(dp, entry, &result) != 0) || (result == NULL)) {
            dev->dev_errno = EIO;
            Dmsg2(129, "do_mount_dvd: failed to find suitable file in dir %s (dev=%s)\n", 
                  dev->device->mount_point, dev->print_name());
            break;
         }
         if (strcmp(result->d_name, ".") && strcmp(result->d_name, "..") && 
             strcmp(result->d_name, ".keep")) {
            count++; /* result->d_name != ., .. or .keep (Gentoo-specific) */
            Dmsg1(100, "Inc count=%d\n", count);
            break;
         } else {
            Dmsg2(129, "do_mount_dvd: ignoring %s in %s\n", 
                  result->d_name, dev->device->mount_point);
         }
      }
      free(entry);
      closedir(dp);
      
      Dmsg1(29, "do_mount_dvd: got %d files in the mount point (not counting ., .. and .keep)\n", count);
      
      if (count > 0) {
         /* If we got more than ., .. and .keep */
         /*   there must be something mounted */
         if (mount) {
            Dmsg1(100, "Did Mount by count=%d\n", count);
            break;
         } else {
            /* An unmount request. We failed to unmount - report an error */
            dev->set_mounted(true);
            free_pool_memory(results);
            Dmsg0(200, "== DVD mount=1\n");
            return false;
         }
      }
get_out:
      dev->set_mounted(false);
      sm_check(__FILE__, __LINE__, false);
      free_pool_memory(results);
      Dmsg0(200, "== DVD mount=0\n");
      return false;
   }
   Dmsg0(100, "Out of mount/umount loop\n");
   
   dev->set_mounted(mount);              /* set/clear mounted flag */
   free_pool_memory(results);
   /* Do not check free space when unmounting */
   if (mount) {
      Dmsg0(100, "Calling update_free_space\n");
      if (!update_free_space_dev(dev)) {
         return false;
      }
   }
   Dmsg1(200, "== DVD mount=%d\n", mount);
   return true;
}

/* Update the free space on the device */
bool update_free_space_dev(DEVICE* dev) 
{
   POOL_MEM ocmd(PM_FNAME);
   POOLMEM* results;
   char* icmd;
   int timeout;
   uint64_t free;
   char ed1[50];
   bool ok = false;
   int status;

   if (!dev->is_dvd() || dev->is_freespace_ok()) {
      return true;
   }
   
   /* The device must be mounted in order to dvd-freespace to work */
   mount_dvd(dev, 1);
   
   sm_check(__FILE__, __LINE__, false);
   icmd = dev->device->free_space_command;
   
   if (!icmd) {
      dev->free_space = 0;
      dev->free_space_errno = 0;
      dev->clear_freespace_ok();              /* No valid freespace */
      dev->clear_media();
      Dmsg2(29, "ERROR: update_free_space_dev: free_space=%s, free_space_errno=%d (!icmd)\n", 
            edit_uint64(dev->free_space, ed1), dev->free_space_errno);
      Mmsg(dev->errmsg, _("No FreeSpace command defined.\n"));
      return false;
   }
   
   dev->edit_mount_codes(ocmd, icmd);
   
   Dmsg1(29, "update_free_space_dev: cmd=%s\n", ocmd.c_str());

   results = get_pool_memory(PM_MESSAGE);
   
   /* Try at most 3 times to get the free space on the device. This should perhaps be configurable. */
   timeout = 3;
   
   while (1) {
      berrno be;
      Dmsg1(20, "Run freespace prog=%s\n", ocmd.c_str());
      status = run_program_full_output(ocmd.c_str(), dev->max_open_wait/2, results);
      Dmsg2(20, "Freespace status=%d result=%s\n", status, results);
      if (status == 0) {
         free = str_to_int64(results);
         Dmsg1(400, "Free space program run: Freespace=%s\n", results);
         if (free >= 0) {
            dev->free_space = free;
            dev->free_space_errno = 0;
            dev->set_freespace_ok();     /* have valid freespace */
            dev->set_media();
            Mmsg(dev->errmsg, "");
            ok = true;
            break;
         }
      }
      dev->free_space = 0;
      dev->free_space_errno = EPIPE;
      dev->clear_freespace_ok();         /* no valid freespace */
      Mmsg2(dev->errmsg, _("Cannot run free space command. Results=%s ERR=%s\n"), 
            results, be.strerror(status));
      
      if (--timeout > 0) {
         Dmsg4(40, "Cannot get free space on device %s. free_space=%s, "
            "free_space_errno=%d ERR=%s\n", dev->print_name(), 
               edit_uint64(dev->free_space, ed1), dev->free_space_errno, 
               dev->errmsg);
         bmicrosleep(1, 0);
         continue;
      }

      dev->dev_errno = dev->free_space_errno;
      Dmsg4(40, "Cannot get free space on device %s. free_space=%s, "
         "free_space_errno=%d ERR=%s\n",
            dev->print_name(), edit_uint64(dev->free_space, ed1),
            dev->free_space_errno, dev->errmsg);
      break;
   }
   
   free_pool_memory(results);
   Dmsg4(29, "leave update_free_space_dev: free_space=%s freespace_ok=%d free_space_errno=%d have_media=%d\n", 
      edit_uint64(dev->free_space, ed1), !!dev->is_freespace_ok(), dev->free_space_errno, !!dev->have_media());
   sm_check(__FILE__, __LINE__, false);
   return ok;
}

/*
 * Note!!!! Part numbers now begin at 1. The part number is
 *  suppressed from the first part, which is just the Volume
 *  name. Each subsequent part is the Volumename.partnumber.
 *
 * Write a part (Vol, Vol.2, ...) from the spool to the DVD   
 * This routine does not update the part number, so normally, you
 *  should call open_next_part()
 *
 * It is also called from truncate_dvd to "blank" the medium, as
 *  well as from block.c when the DVD is full to write the last part.
 */
bool dvd_write_part(DCR *dcr)
{
   DEVICE *dev = dcr->dev;
   POOL_MEM archive_name(PM_FNAME);
   
   dev->clear_freespace_ok();             /* need to update freespace */

   /* Don't write empty part files.
    * This is only useful when growisofs does not support write beyond
    * the 4GB boundary.
    * Example :
    *   - 3.9 GB on the volume, dvd-freespace reports 0.4 GB free
    *   - Write 0.2 GB on the volume, Bacula thinks it could still
    *     append data, it creates a new empty part.
    *   - dvd-freespace reports 0 GB free, as the 4GB boundary has
    *     been crossed
    *   - Bacula thinks he must finish to write to the device, so it
    *     tries to write the last part (0-byte), but dvd-writepart fails...
    *
    *  ***FIXME****  we cannot write a blank part!!!!!!!
    * There is one exception: when recycling a volume, we write a blank part
    * file, so, then, we need to accept to write it.
    */
   if (dev->part_size == 0) {
      Dmsg2(29, "dvd_write_part: device is %s, won't write blank part %d\n", dev->print_name(), dev->part);
      /* Delete spool file */
      make_spooled_dvd_filename(dev, archive_name);
      unlink(archive_name.c_str());
      dev->set_part_spooled(false);
      Dmsg1(29, "unlink(%s)\n", archive_name.c_str());
      sm_check(__FILE__, __LINE__, false);
      return true;
   }
   
   POOL_MEM ocmd(PM_FNAME);
   POOL_MEM results(PM_MESSAGE);
   char* icmd;
   int status;
   int timeout;
   char ed1[50];
   
   sm_check(__FILE__, __LINE__, false);
   Dmsg3(29, "dvd_write_part: device is %s, part is %d, is_mounted=%d\n", dev->print_name(), dev->part, dev->is_mounted());
   icmd = dev->device->write_part_command;
   
   dev->edit_mount_codes(ocmd, icmd);
      
   /*
    * original line follows
    * timeout = dev->max_open_wait + (dev->max_part_size/(1350*1024/2));
    * I modified this for a longer timeout; pre-formatting, blanking and
    * writing can take quite a while
    */

   /* Explanation of the timeout value, when writing the first part,
    *  by Arno Lehmann :
    * 9 GB, write speed 1x: 6990 seconds (almost 2 hours...)
    * Overhead: 900 seconds (starting, initializing, finalizing,probably 
    *   reloading 15 minutes)
    * Sum: 15780.
    * A reasonable last-exit timeout would be 16000 seconds. Quite long - 
    * almost 4.5 hours, but hopefully, that timeout will only ever be needed 
    * in case of a serious emergency.
    */

   if (dev->part == 1) {
      timeout = 16000;
   } else {
      timeout = dev->max_open_wait + (dev->part_size/(1350*1024/4));
   }

   Dmsg2(20, "Write part: cmd=%s timeout=%d\n", ocmd.c_str(), timeout);
   status = run_program_full_output(ocmd.c_str(), timeout, results.c_str());
   Dmsg2(20, "Write part status=%d result=%s\n", status, results.c_str());

   dev->truncated_dvd = false;
   if (status != 0) {
      Jmsg2(dcr->jcr, M_FATAL, 0, _("Error writing part %d to the DVD: ERR=%s\n"),
         dev->part, results.c_str());
      Mmsg1(dev->errmsg, _("Error while writing current part to the DVD: %s"), 
            results.c_str());
      Dmsg1(000, "%s\n", dev->errmsg);
      dev->dev_errno = EIO;
      mark_volume_in_error(dcr);
      sm_check(__FILE__, __LINE__, false);
      return false;
   }
   Jmsg(dcr->jcr, M_INFO, 0, _("Part %d (%lld bytes) written to DVD.\n"), dev->part, dev->part_size);
   Dmsg3(400, "dvd_write_part: Part %d (%lld bytes) written to DVD\nResults: %s\n",
            dev->part, dev->part_size, results.c_str());
    
   dev->num_dvd_parts++;            /* there is now one more part on DVD */
   dev->VolCatInfo.VolCatParts = dev->num_dvd_parts;
   dcr->VolCatInfo.VolCatParts = dev->num_dvd_parts;
   Dmsg1(000, "Update num_parts=%d\n", dev->num_dvd_parts);

   /* Delete spool file */
   make_spooled_dvd_filename(dev, archive_name);
   unlink(archive_name.c_str());
   dev->set_part_spooled(false);
   Dmsg1(29, "unlink(%s)\n", archive_name.c_str());
   sm_check(__FILE__, __LINE__, false);
   
   /* growisofs umounted the device, so remount it (it will update the free space) */
   dev->clear_mounted();
   mount_dvd(dev, 1);
   Jmsg(dcr->jcr, M_INFO, 0, _("Remaining free space %s on %s\n"), 
      edit_uint64_with_commas(dev->free_space, ed1), dev->print_name());
   sm_check(__FILE__, __LINE__, false);
   return true;
}

/*
 * Open the next part file.
 *  - Close the fd
 *  - Increment part number 
 *  - Reopen the device
 */
int dvd_open_next_part(DCR *dcr)
{
   DEVICE *dev = dcr->dev;

   Dmsg6(29, "Enter: == open_next_part part=%d npart=%d dev=%s vol=%s mode=%d file_addr=%d\n", 
      dev->part, dev->num_dvd_parts, dev->print_name(),
         dev->VolCatInfo.VolCatName, dev->openmode, dev->file_addr);
   if (!dev->is_dvd()) {
      Dmsg1(000, "Device %s is not dvd!!!!\n", dev->print_name()); 
      return -1;
   }
   
   /* When appending, do not open a new part if the current is empty */
   if (dev->can_append() && (dev->part > dev->num_dvd_parts) && 
       (dev->part_size == 0)) {
      Dmsg0(29, "open_next_part exited immediately (dev->part_size == 0).\n");
      return dev->fd;
   }

   dev->close_part(dcr);               /* close current part */
   
   /*
    * If we have a spooled part open, write it to the
    *  DVD before opening the next part.
    */
   if (dev->is_part_spooled()) {
      Dmsg2(100, "Before open next write previous. part=%d num_parts=%d\n",
         dev->part, dev->num_dvd_parts);
      if (!dvd_write_part(dcr)) {
         Dmsg0(29, "Error in dvd_write part.\n");
         return -1;
      }
   }
     
   dev->part_start += dev->part_size;
   dev->part++;
   Dmsg2(29, "Inc part=%d num_dvd_parts=%d\n", dev->part, dev->num_dvd_parts);

   /* Are we working on a part past what is written in the DVD? */
   if (dev->num_dvd_parts < dev->part) {
      POOL_MEM archive_name(PM_FNAME);
      struct stat buf;
      /* 
       * First check what is on DVD.  If our part is there, we
       *   are in trouble, so bail out.
       * NB: This is however not a problem if we are writing the first part.
       * It simply means that we are over writing an existing volume...
       */
      if (dev->num_dvd_parts > 0) {
         make_mounted_dvd_filename(dev, archive_name);   /* makes dvd name */
         Dmsg1(100, "Check if part on DVD: %s\n", archive_name.c_str());
         if (stat(archive_name.c_str(), &buf) == 0) {
            /* bad news bail out */
            dev->set_part_spooled(false);
            Mmsg1(&dev->errmsg, _("Next Volume part already exists on DVD. Cannot continue: %s\n"),
               archive_name.c_str());
            return -1;
         }
      }

#ifdef neeeded
      Dmsg2(400, "num_dvd_parts=%d part=%d\n", dev->num_dvd_parts, dev->part);
      make_spooled_dvd_filename(dev, archive_name);   /* makes spool name */
      
      /* Check if the next part exists in spool directory . */
      Dmsg1(100, "Check if part on spool: %s\n", archive_name.c_str());
      if ((stat(archive_name.c_str(), &buf) == 0) || (errno != ENOENT)) {
         Dmsg1(29, "======= Part %s is in the way, deleting it...\n", archive_name.c_str());
         /* Then try to unlink it */
         if (unlink(archive_name.c_str()) < 0) {
            berrno be;
            dev->set_part_spooled(false);
            dev->dev_errno = errno;
            Mmsg2(dev->errmsg, _("open_next_part can't unlink existing part %s, ERR=%s\n"), 
                   archive_name.c_str(), be.strerror());
            return -1;
         }
      }
#endif
   }

   Dmsg2(400, "Call dev->open(vol=%s, mode=%d)\n", dcr->VolCatInfo.VolCatName, 
         dev->openmode);

   /* Open next part.  Note, this sets part_size for part opened. */
   if (dev->open(dcr, OPEN_READ_ONLY) < 0) {
      return -1;
   } 
   dev->set_labeled();                   /* all next parts are "labeled" */
   
   return dev->fd;
}

/*
 * Open the first part file.
 *  - Close the fd
 *  - Reopen the device
 */
int dvd_open_first_part(DCR *dcr, int mode)
{
   DEVICE *dev = dcr->dev;

   Dmsg5(29, "Enter: ==== open_first_part dev=%s Vol=%s mode=%d num_dvd_parts=%d append=%d\n", dev->print_name(), 
         dev->VolCatInfo.VolCatName, dev->openmode, dev->num_dvd_parts, dev->can_append());


   dev->close_part(dcr);

   Dmsg2(400, "Call dev->open(vol=%s, mode=%d)\n", dcr->VolCatInfo.VolCatName, 
         mode);
   Dmsg0(100, "Set part=1\n");
   dev->part = 1;
   dev->part_start = 0;

   if (dev->open(dcr, mode) < 0) {
      Dmsg0(400, "open dev() failed\n");
      return -1;
   }
   Dmsg2(400, "Leave open_first_part state=%s append=%d\n", dev->is_open()?"open":"not open", dev->can_append());
   
   return dev->fd;
}


/* Protected version of lseek, which opens the right part if necessary */
off_t lseek_dev(DEVICE *dev, off_t offset, int whence)
{
   DCR *dcr;
   off_t pos;
   char ed1[50], ed2[50];
   
   Dmsg5(400, "Enter lseek_dev fd=%d off=%s w=%d part=%d nparts=%d\n", dev->fd,
      edit_int64(offset, ed1), whence, dev->part, dev->num_dvd_parts);
   if (!dev->is_dvd()) { 
      Dmsg0(400, "Using sys lseek\n");
      return lseek(dev->fd, offset, whence);
   }

   dcr = (DCR *)dev->attached_dcrs->first();  /* any dcr will do */
   switch(whence) {
   case SEEK_SET:
      Dmsg2(400, "lseek_dev SEEK_SET to %s (part_start=%s)\n",
         edit_int64(offset, ed1), edit_uint64(dev->part_start, ed2));
      if ((uint64_t)offset >= dev->part_start) {
         if ((uint64_t)offset == dev->part_start || 
             (uint64_t)offset < dev->part_start+dev->part_size) {
            /* We are staying in the current part, just seek */
            if ((pos = lseek(dev->fd, offset-dev->part_start, SEEK_SET)) < 0) {
               return pos;
            } else {
               return pos + dev->part_start;
            }
         } else {
            /* Load next part, and start again */
            Dmsg0(100, "lseek open next part\n");
            if (dvd_open_next_part(dcr) < 0) {
               Dmsg0(400, "lseek_dev failed while trying to open the next part\n");
               return -1;
            }
            Dmsg2(100, "Recurse lseek after open next part=%d num_part=%d\n",
               dev->part, dev->num_dvd_parts);
            return lseek_dev(dev, offset, SEEK_SET);
         }
      } else {
         /*
          * pos < dev->part_start :
          * We need to access a previous part, 
          * so just load the first one, and seek again
          * until the right one is loaded
          */
         Dmsg0(100, "lseek open first part\n");
         if (dvd_open_first_part(dcr, dev->openmode) < 0) {
            Dmsg0(400, "lseek_dev failed while trying to open the first part\n");
            return -1;
         }
         Dmsg2(100, "Recurse lseek after open first part=%d num_part=%d\n",
               dev->part, dev->num_dvd_parts);
         return lseek_dev(dev, offset, SEEK_SET);
      }
      break;
   case SEEK_CUR:
      Dmsg1(400, "lseek_dev SEEK_CUR to %s\n", edit_int64(offset, ed1));
      if ((pos = lseek(dev->fd, (off_t)0, SEEK_CUR)) < 0) {
         Dmsg0(400, "Seek error.\n");
         return pos;                  
      }
      pos += dev->part_start;
      if (offset == 0) {
         Dmsg1(400, "lseek_dev SEEK_CUR returns %s\n", edit_uint64(pos, ed1));
         return pos;
      } else { 
         Dmsg1(400, "do lseek_dev SEEK_SET %s\n", edit_uint64(pos, ed1));
         return lseek_dev(dev, pos, SEEK_SET);
      }
      break;
   case SEEK_END:
      Dmsg1(400, "lseek_dev SEEK_END to %s\n", edit_int64(offset, ed1));
      /*
       * Bacula does not use offsets for SEEK_END
       *  Also, Bacula uses seek_end only when it wants to
       *  append to the volume, so for a dvd that means
       *  that the volume must be spooled since the DVD
       *  itself is read-only (as currently implemented).
       */
      if (offset > 0) { /* Not used by bacula */
         Dmsg1(400, "lseek_dev SEEK_END called with an invalid offset %s\n", 
            edit_uint64(offset, ed1));
         errno = EINVAL;
         return -1;
      }
      /* If we are already on a spooled part and have the
       *  right part number, simply seek
       */
      if (dev->is_part_spooled() && dev->part > dev->num_dvd_parts) {
         if ((pos = lseek(dev->fd, (off_t)0, SEEK_END)) < 0) {
            return pos;   
         } else {
            Dmsg1(400, "lseek_dev SEEK_END returns %s\n", 
                  edit_uint64(pos + dev->part_start, ed1));
            return pos + dev->part_start;
         }
      } else {
         /*
          * Load the first part, then load the next until we reach the last one.
          * This is the only way to be sure we compute the right file address.
          *
          * Save previous openmode, and open all but last part read-only 
          * (useful for DVDs) 
          */
         int modesave = dev->openmode;
         if (dvd_open_first_part(dcr, OPEN_READ_ONLY) < 0) {
            Dmsg0(400, "lseek_dev failed while trying to open the first part\n");
            return -1;
         }
         if (dev->num_dvd_parts > 0) {
            while (dev->part < dev->num_dvd_parts) {
               if (dvd_open_next_part(dcr) < 0) {
                  Dmsg0(400, "lseek_dev failed while trying to open the next part\n");
                  return -1;
               }
            }
            dev->openmode = modesave;
            if (dvd_open_next_part(dcr) < 0) {
               Dmsg0(400, "lseek_dev failed while trying to open the next part\n");
               return -1;
            }
         }
         return lseek_dev(dev, 0, SEEK_END);
      }
      break;
   default:
      Dmsg0(400, "Seek call error.\n");
      errno = EINVAL;
      return -1;
   }
}

bool dvd_close_job(DCR *dcr)
{
   DEVICE *dev = dcr->dev;
   JCR *jcr = dcr->jcr;
   bool ok = true;

   /*
    * If the device is a dvd and WritePartAfterJob
    * is set to yes, open the next part, so, in case of a device
    * that requires mount, it will be written to the device.
    */
   if (dev->is_dvd() && jcr->write_part_after_job && (dev->part_size > 0)) {
      Dmsg1(400, "Writing last part=%d write_partafter_job is set.\n",
         dev->part);
      if (dev->part < dev->num_dvd_parts+1) {
         Jmsg3(jcr, M_FATAL, 0, _("Error writing. Current part less than total number of parts (%d/%d, device=%s)\n"),
               dev->part, dev->num_dvd_parts, dev->print_name());
         dev->dev_errno = EIO;
         ok = false;
      }
      
      if (ok && !dvd_write_part(dcr)) {
         Jmsg2(jcr, M_FATAL, 0, _("Unable to write last on %s: ERR=%s\n"),
               dev->print_name(), dev->bstrerror());
         dev->dev_errno = EIO;
         ok = false;
      }
   }
   return ok;
}

bool truncate_dvd(DCR *dcr) 
{
   DEVICE* dev = dcr->dev;

   dev->close_part(dcr);

   if (!unmount_dvd(dev, 1)) {
      Dmsg0(400, "truncate_dvd: Failed to unmount DVD\n");
      return false;
   }

   /* Set num_dvd_parts to zero (on disk) */
   dev->num_dvd_parts = 0;
   dcr->VolCatInfo.VolCatParts = 0;
   dev->VolCatInfo.VolCatParts = 0;
   
   Dmsg0(400, "truncate_dvd: Opening first part (1)...\n");
   
   dev->truncating = true;
   if (dvd_open_first_part(dcr, CREATE_READ_WRITE) < 0) {
      Dmsg0(400, "truncate_dvd: Error while opening first part (1).\n");
      dev->truncating = false;
      return false;
   }

   Dmsg0(400, "truncate_dvd: Truncating...\n");

   /* If necessary, truncate it spool file. */
   if (ftruncate(dev->fd, 0) != 0) {
      berrno be;
      Mmsg2(dev->errmsg, _("Unable to truncate device %s. ERR=%s\n"), 
         dev->print_name(), be.strerror());
      dev->truncating = false;
      return false;
   }
   
   dev->close_part(dcr);
   
   Dmsg0(400, "truncate_dvd: Opening first part (2)...\n");
   
   /* 
    * Now actually truncate the DVD
    *  This is really kludgy, why not an argument or a separate
    *  subroutine?  KES
    */
   if (!dvd_write_part(dcr)) {
      Dmsg0(400, "truncate_dvd: Error while writing to DVD.\n");
      dev->truncating = false;
      return false;
   }
   dev->truncating = false;
   
   /* Set num_dvd_parts to zero (on disk) */
   dev->num_dvd_parts = 0;
   dcr->VolCatInfo.VolCatParts = 0;
   dev->VolCatInfo.VolCatParts = 0;

   /* Update catalog */
   if (!dir_update_volume_info(dcr, false)) {
      return false;
   }
   
   if (dvd_open_first_part(dcr, OPEN_READ_WRITE) < 0) {
      Dmsg0(400, "truncate_dvd: Error while opening first part (2).\n");
      return false;
   }

   return true;
}

/* Checks if we can write on a non-blank DVD: meaning that it just have been
 * truncated (there is only one zero-sized file on the DVD, with the right
 * volume name).   
 *  
 * Note!  Normally if we can mount the device, which should be the case
 *   when we get here, it is not a blank DVD.  Hence we check if
 *   there is a zero length file with the right name, in which case
 *   we allow it.
 * This seems terribly kludgie to me.  KES
 */
bool check_can_write_on_non_blank_dvd(DCR *dcr) 
{
   DEVICE* dev = dcr->dev;
   DIR* dp;
   struct dirent *entry, *result;
   int name_max;
   int count = 0;
   bool matched = true;
   struct stat filestat;
      
   name_max = pathconf(".", _PC_NAME_MAX);
   if (name_max < 1024) {
      name_max = 1024;
   }
   
   if (!(dp = opendir(dev->device->mount_point))) {
      berrno be;
      dev->dev_errno = errno;
      Dmsg3(29, "check_can_write_on_non_blank_dvd: failed to open dir %s (dev=%s), ERR=%s\n", 
            dev->device->mount_point, dev->print_name(), be.strerror());
      return false;
   }
   
   entry = (struct dirent *)malloc(sizeof(struct dirent) + name_max + 1000);
   while (1) {
      if ((readdir_r(dp, entry, &result) != 0) || (result == NULL)) {
         dev->dev_errno = EIO;
         Dmsg2(129, "check_can_write_on_non_blank_dvd: no more files in dir %s (dev=%s)\n", 
               dev->device->mount_point, dev->print_name());
         break;
      } else {
         Dmsg2(99, "check_can_write_on_non_blank_dvd: found %s (versus %s)\n", 
               result->d_name, dev->VolCatInfo.VolCatName);
         if (strcmp(result->d_name, dev->VolCatInfo.VolCatName) == 0) {
            /* Found the file, checking it is empty */
            POOL_MEM filename(PM_FNAME);
            pm_strcpy(filename, dev->device->mount_point);
            if (filename.c_str()[strlen(filename.c_str())-1] != '/') {
               pm_strcat(filename, "/");
            }
            pm_strcat(filename, dev->VolCatInfo.VolCatName);
            if (stat(filename.c_str(), &filestat) < 0) {
               berrno be;
               dev->dev_errno = errno;
               Dmsg2(29, "check_can_write_on_non_blank_dvd: cannot stat file (file=%s), ERR=%s\n", 
                  filename.c_str(), be.strerror());
               return false;
            }
            Dmsg2(99, "check_can_write_on_non_blank_dvd: size of %s is %lld\n", 
               filename.c_str(), filestat.st_size);
            matched = filestat.st_size == 0;
         }
      }
      count++;
   }
   free(entry);
   closedir(dp);
   
   if (count > 3) {
      /* There are more than 3 files (., .., and the volume file) */
      Dmsg1(29, "Cannot write on blank DVD too many files %d greater than 3\n", count);
      return false;
   }
   
   Dmsg2(29, "OK  can_write_on_non_blank_dvd: got %d files in the mount point (matched=%d)\n", count, matched);
   return matched;
}

/* 
 * Mount a DVD device, then scan to find out how many parts
 *  there are.
 */
int find_num_dvd_parts(DCR *dcr)
{
   DEVICE *dev = dcr->dev;
   int num_parts = 0;

   if (!dev->is_dvd()) {
      return 0;
   }
   
   if (dev->mount(1)) {
      DIR* dp;
      struct dirent *entry, *result;
      int name_max;
      int len = strlen(dcr->VolCatInfo.VolCatName);

      /* Now count the number of parts */
      name_max = pathconf(".", _PC_NAME_MAX);
      if (name_max < 1024) {
         name_max = 1024;
      }
         
      if (!(dp = opendir(dev->device->mount_point))) {
         berrno be;
         dev->dev_errno = errno;
         Dmsg3(29, "find_num_dvd_parts: failed to open dir %s (dev=%s), ERR=%s\n", 
               dev->device->mount_point, dev->print_name(), be.strerror());
         goto get_out;
      }
      
      entry = (struct dirent *)malloc(sizeof(struct dirent) + name_max + 1000);

      Dmsg1(100, "Looking for Vol=%s\n", dcr->VolCatInfo.VolCatName);
      for ( ;; ) {
         int flen;
         bool ignore;
         if ((readdir_r(dp, entry, &result) != 0) || (result == NULL)) {
            dev->dev_errno = EIO;
            Dmsg2(129, "find_num_dvd_parts: failed to find suitable file in dir %s (dev=%s)\n", 
                  dev->device->mount_point, dev->print_name());
            break;
         }
         flen = strlen(result->d_name);
         ignore = true;
         if (flen >= len) {
            result->d_name[len] = 0;
            if (strcmp(dcr->VolCatInfo.VolCatName, result->d_name) == 0) {
               num_parts++;
               Dmsg1(100, "find_num_dvd_parts: found part: %s\n", result->d_name);
               ignore = false;
            }
         }
         if (ignore) {
            Dmsg2(129, "find_num_dvd_parts: ignoring %s in %s\n", 
                  result->d_name, dev->device->mount_point);
         }
      }
      free(entry);
      closedir(dp);
      Dmsg1(29, "find_num_dvd_parts = %d\n", num_parts);
   }
   
get_out:
   dev->set_freespace_ok();
   if (dev->is_mounted()) {
      dev->unmount(0);
   }
   return num_parts;
}
