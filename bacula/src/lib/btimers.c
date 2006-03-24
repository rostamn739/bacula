/*
 * Process and thread timer routines, built on top of watchdogs.
 *
 *    Nic Bellamy <nic@bellamy.co.nz>, October 2004.
 *
*/
/*
   Copyright (C) 2004-2006 Kern Sibbald

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
#include "jcr.h"

/* Forward referenced functions */
static void stop_btimer(btimer_t *wid);
static btimer_t *btimer_start_common(uint32_t wait);

/* Forward referenced callback functions */
static void callback_child_timer(watchdog_t *self);
static void callback_thread_timer(watchdog_t *self);
#ifdef xxx
static void destructor_thread_timer(watchdog_t *self);
static void destructor_child_timer(watchdog_t *self);
#endif

/*
 * Start a timer on a child process of pid, kill it after wait seconds.
 *
 *  Returns: btimer_t *(pointer to btimer_t struct) on success
 *           NULL on failure
 */
btimer_t *start_child_timer(pid_t pid, uint32_t wait)
{
   btimer_t *wid;

   wid = btimer_start_common(wait);
   if (wid == NULL) {
      return NULL;
   }
   wid->type = TYPE_CHILD;
   wid->pid = pid;
   wid->killed = false;

   wid->wd->callback = callback_child_timer;
   wid->wd->one_shot = false;
   wid->wd->interval = wait;
   register_watchdog(wid->wd);

   Dmsg3(900, "Start child timer %p, pid %d for %d secs.\n", wid, pid, wait);
   return wid;
}

/*
 * Stop child timer
 */
void stop_child_timer(btimer_t *wid)
{
   if (wid == NULL) {
      Dmsg0(900, "stop_child_timer called with NULL btimer_id\n");
      return;
   }
   Dmsg2(900, "Stop child timer %p pid %d\n", wid, wid->pid);
   stop_btimer(wid);
}

#ifdef xxx
static void destructor_child_timer(watchdog_t *self)
{
   btimer_t *wid = (btimer_t *)self->data;
   free(wid->wd);
   free(wid);
}
#endif

static void callback_child_timer(watchdog_t *self)
{
   btimer_t *wid = (btimer_t *)self->data;

   if (!wid->killed) {
      /* First kill attempt; try killing it softly (kill -SONG) first */
      wid->killed = true;

      Dmsg2(050, "watchdog %p term PID %d\n", self, wid->pid);

      /* Kill -TERM the specified PID, and reschedule a -KILL for 5 seconds
       * later. (Warning: this should let dvd-writepart enough time to term
       * and kill growisofs, which takes 3 seconds, so the interval must not
       * be less than 5 seconds)
       */
      kill(wid->pid, SIGTERM);
      self->interval = 5;
   } else {
      /* This is the second call - terminate with prejudice. */
      Dmsg2(050, "watchdog %p kill PID %d\n", self, wid->pid);

      kill(wid->pid, SIGKILL);

      /* Setting one_shot to true before we leave ensures we don't get
       * rescheduled.
       */
      self->one_shot = true;
   }
}

/*
 * Start a timer on a thread. kill it after wait seconds.
 *
 *  Returns: btimer_t *(pointer to btimer_t struct) on success
 *           NULL on failure
 */
btimer_t *start_thread_timer(pthread_t tid, uint32_t wait)
{
   btimer_t *wid;
   wid = btimer_start_common(wait);
   if (wid == NULL) {
      Dmsg1(900, "start_thread_timer return NULL from common. wait=%d.\n", wait);
      return NULL;
   }
   wid->type = TYPE_PTHREAD;
   wid->tid = tid;

   wid->wd->callback = callback_thread_timer;
   wid->wd->one_shot = true;
   wid->wd->interval = wait;
   register_watchdog(wid->wd);

   Dmsg3(900, "Start thread timer %p tid %p for %d secs.\n", wid, tid, wait);

   return wid;
}

/*
 * Start a timer on a BSOCK. kill it after wait seconds.
 *
 *  Returns: btimer_t *(pointer to btimer_t struct) on success
 *           NULL on failure
 */
btimer_t *start_bsock_timer(BSOCK *bsock, uint32_t wait)
{
   btimer_t *wid;
   wid = btimer_start_common(wait);
   if (wid == NULL) {
      return NULL;
   }
   wid->type = TYPE_BSOCK;
   wid->tid = pthread_self();
   wid->bsock = bsock;

   wid->wd->callback = callback_thread_timer;
   wid->wd->one_shot = true;
   wid->wd->interval = wait;
   register_watchdog(wid->wd);

   Dmsg4(950, "Start bsock timer %p tid=%p for %d secs at %d\n", wid,
         wid->tid, wait, time(NULL));

   return wid;
}

/*
 * Stop bsock timer
 */
void stop_bsock_timer(btimer_t *wid)
{
   if (wid == NULL) {
      Dmsg0(900, "stop_bsock_timer called with NULL btimer_id\n");
      return;
   }
   Dmsg3(950, "Stop bsock timer %p tid=%p at %d.\n", wid, wid->tid, time(NULL));
   stop_btimer(wid);
}


/*
 * Stop thread timer
 */
void stop_thread_timer(btimer_t *wid)
{
   if (wid == NULL) {
      Dmsg0(900, "stop_thread_timer called with NULL btimer_id\n");
      return;
   }
   Dmsg2(900, "Stop thread timer %p tid=%p.\n", wid, wid->tid);
   stop_btimer(wid);
}

#ifdef xxx
static void destructor_thread_timer(watchdog_t *self)
{
   btimer_t *wid = (btimer_t *)self->data;
   free(wid->wd);
   free(wid);
}
#endif

static void callback_thread_timer(watchdog_t *self)
{
   btimer_t *wid = (btimer_t *)self->data;

   Dmsg4(50, "thread timer %p kill %s tid=%p at %d.\n", self,
      wid->type == TYPE_BSOCK ? "bsock" : "thread", wid->tid, time(NULL));

   if (wid->type == TYPE_BSOCK && wid->bsock) {
      wid->bsock->timed_out = true;
   }
   pthread_kill(wid->tid, TIMEOUT_SIGNAL);
}

static btimer_t *btimer_start_common(uint32_t wait)
{
   btimer_t *wid = (btimer_t *)malloc(sizeof(btimer_t));

   wid->wd = new_watchdog();
   if (wid->wd == NULL) {
      free(wid);
      return NULL;
   }
   wid->wd->data = wid;
   wid->killed = FALSE;

   return wid;
}

/*
 * Stop btimer
 */
static void stop_btimer(btimer_t *wid)
{
   if (wid == NULL) {
      Emsg0(M_ABORT, 0, _("stop_btimer called with NULL btimer_id\n"));
   }
   unregister_watchdog(wid->wd);
   free(wid->wd);
   free(wid);
}
