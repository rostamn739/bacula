/*
 *   util.c  miscellaneous utility subroutines for Bacula
 * 
 *    Kern Sibbald, MM
 */

/*
   Copyright (C) 2000, 2001, 2002 Kern Sibbald and John Walker

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
#include "findlib/find.h"

/*
 * Various Bacula Utility subroutines
 *
 */

/*
 * Edit a number with commas, the supplied buffer
 * must be at least 27 bytes long.
 */
char *edit_uint_with_commas(uint64_t val, char *buf)
{
   sprintf(buf, "%" lld, val);
   return add_commas(buf, buf);
}

char *add_commas(char *val, char *buf)
{
   int len, nc;
   char *p, *q;
   int i;

   if (val != buf) {
      strcpy(buf, val);
   }
   len = strlen(buf);
   if (len < 1) {
      len = 1;
   }
   nc = (len - 1) / 3;
   p = buf+len;
   q = p + nc;
   *q-- = *p--;
   for ( ; nc; nc--) {
      for (i=0; i < 3; i++) {
	  *q-- = *p--;
      }
      *q-- = ',';
   }   
   return buf;
}


/* Convert a string in place to lower case */
void 
lcase(char *str)
{
   while (*str) {
      if (ISUPPER(*str))
	 *str = tolower((int)(*str));
       str++;
   }
}

/* Convert spaces to non-space character. 
 * This makes scanf of fields containing spaces easier.
 */
void
bash_spaces(char *str)
{
   while (*str) {
      if (*str == ' ')
	 *str = 0x1;
      str++;
   }
}

/* Convert non-space characters (0x1) back into spaces */
void
unbash_spaces(char *str)
{
   while (*str) {
     if (*str == 0x1)
        *str = ' ';
     str++;
   }
}

/* Strip any trailing junk from the command */
void strip_trailing_junk(char *cmd)
{
   char *p;
   p = cmd + strlen(cmd) - 1;

   /* strip trailing junk from command */
   while ((p >= cmd) && (*p == '\n' || *p == '\r' || *p == ' '))
      *p-- = 0;
}

/* Strip any trailing slashes from a directory path */
void strip_trailing_slashes(char *dir)
{
   char *p;
   p = dir + strlen(dir) - 1;

   /* strip trailing slashes */
   while ((p >= dir) && (*p == '/'))
      *p-- = 0;
}

/*
 * Skip spaces
 *  Returns: 0 on failure (EOF) 	    
 *	     1 on success
 *	     new address in passed parameter 
 */
int skip_spaces(char **msg)
{
   char *p = *msg;
   if (!p) {
      return 0;
   }
   while (*p && *p == ' ') {
      p++;
   }
   *msg = p;
   return *p ? 1 : 0;
}

/*
 * Skip nonspaces
 *  Returns: 0 on failure (EOF) 	    
 *	     1 on success
 *	     new address in passed parameter 
 */
int skip_nonspaces(char **msg)
{
   char *p = *msg;

   if (!p) {
      return 0;
   }
   while (*p && *p != ' ') {
      p++;
   }
   *msg = p;
   return *p ? 1 : 0;
}

/* folded search for string - case insensitive */
int
fstrsch(char *a, char *b)   /* folded case search */
{
   register char *s1,*s2;
   register char c1, c2;

   s1=a;
   s2=b;
   while (*s1) {		      /* do it the fast way */
      if ((*s1++ | 0x20) != (*s2++ | 0x20))
	 return 0;		      /* failed */
   }
   while (*a) { 		      /* do it over the correct slow way */
      if (ISUPPER(c1 = *a)) {
	 c1 = tolower((int)c1);
      }
      if (ISUPPER(c2 = *b)) {
	 c2 = tolower((int)c2);
      }
      if (c1 != c2) {
	 return 0;
      }
      a++;
      b++;
   }
   return 1;
}


char *encode_time(time_t time, char *buf)
{
   struct tm tm;
   int n;

   if (localtime_r(&time, &tm)) {
      n = sprintf(buf, "%04d-%02d-%02d %02d:%02d:%02d",
		   tm.tm_year + 1900, tm.tm_mon + 1, tm.tm_mday,
		   tm.tm_hour, tm.tm_min, tm.tm_sec);
   }
   return buf+n;
}

/***********************************************************************
 * Encode the mode bits into a 10 character string like LS does
 ***********************************************************************/

char *encode_mode(mode_t mode, char *buf)
{
  char *cp = buf;  

  *cp++ = S_ISDIR(mode) ? 'd' : S_ISBLK(mode) ? 'b' : S_ISCHR(mode) ? 'c' :
          S_ISLNK(mode) ? 'l' : '-';
  *cp++ = mode & S_IRUSR ? 'r' : '-';
  *cp++ = mode & S_IWUSR ? 'w' : '-';
  *cp++ = (mode & S_ISUID
               ? (mode & S_IXUSR ? 's' : 'S')
               : (mode & S_IXUSR ? 'x' : '-'));
  *cp++ = mode & S_IRGRP ? 'r' : '-';
  *cp++ = mode & S_IWGRP ? 'w' : '-';
  *cp++ = (mode & S_ISGID
               ? (mode & S_IXGRP ? 's' : 'S')
               : (mode & S_IXGRP ? 'x' : '-'));
  *cp++ = mode & S_IROTH ? 'r' : '-';
  *cp++ = mode & S_IWOTH ? 'w' : '-';
  *cp++ = (mode & S_ISVTX
               ? (mode & S_IXOTH ? 't' : 'T')
               : (mode & S_IXOTH ? 'x' : '-'));
  *cp = '\0';
  return cp;
}

#ifdef WORKING
extern char *getuser(uid_t uid);
extern char *getgroup(gid_t gid);

void print_ls_output(char *fname, char *lname, int type, struct stat *statp)
{
   char buf[1000]; 
   char *p, *f;
   int n;

   p = encode_mode(statp->st_mode, buf);
   n = sprintf(p, "  %2d ", (uint32_t)statp->st_nlink);
   p += n;
   n = sprintf(p, "%-8.8s %-8.8s", getuser(statp->st_uid), getgroup(statp->st_gid));
   p += n;
   n = sprintf(p, "%8ld  ", statp->st_size);
   p += n;
   p = encode_time(statp->st_ctime, p);
   *p++ = ' ';
   *p++ = ' ';
   for (f=fname; *f; )
      *p++ = *f++;
   if (type == FT_LNK) {
      *p++ = ' ';
      *p++ = '-';
      *p++ = '>';
      *p++ = ' ';
      /* Copy link name */
      for (f=lname; *f; )
	 *p++ = *f++;
   }
   *p++ = '\n';
   *p = 0;
   fputs(buf, stdout);
}
#endif

int do_shell_expansion(char *name)
{
/*  ****FIXME***** this should work for Win32 too */
#define UNIX
#ifdef UNIX
#ifndef PATH_MAX
#define PATH_MAX 512
#endif

   int pid, wpid, stat;
   int waitstatus;
   char *shellcmd;
   void (*istat)(int), (*qstat)(int);
   int i;
   char echout[PATH_MAX + 256];
   int pfd[2];
   static char meta[] = "~\\$[]*?`'<>\"";
   int found = FALSE;
   int len;

   /* Check if any meta characters are present */
   len = strlen(meta);
   for (i = 0; i < len; i++) {
      if (strchr(name, meta[i])) {
	 found = TRUE;
	 break;
      }
   }
   stat = 0;
   if (found) {
#ifdef nt
       /* If the filename appears to be a DOS filename,
          convert all backward slashes \ to Unix path
          separators / and insert a \ infront of spaces. */
       len = strlen(name);
       if (len >= 3 && name[1] == ':' && name[2] == '\\') {
	  for (i=2; i<len; i++)
             if (name[i] == '\\')
                name[i] = '/';
       }
#else
       /* Pass string off to the shell for interpretation */
       if (pipe(pfd) == -1)
	  return 0;
       switch(pid = fork()) {
       case -1:
	  break;

       case 0:				  /* child */
	  /* look for shell */
          if ((shellcmd = getenv("SHELL")) == NULL)
             shellcmd = "/bin/sh";
	  close(1); dup(pfd[1]);	  /* attach pipes to stdin and stdout */
	  close(2); dup(pfd[1]);
	  for (i = 3; i < 32; i++)	  /* close everything else */
	     close(i);
          strcpy(echout, "echo ");        /* form echo command */
	  strcat(echout, name);
          execl(shellcmd, shellcmd, "-c", echout, NULL); /* give to shell */
          exit(127);                      /* shouldn't get here */

       default: 			  /* parent */
	  /* read output from child */
	  i = read(pfd[0], echout, sizeof echout);
	  echout[--i] = 0;		  /* set end of string */
	  /* look for first word or first line. */
	  while (--i >= 0) {
             if (echout[i] == ' ' || echout[i] == '\n')
		echout[i] = 0;		  /* keep only first one */
	  }
	  istat = signal(SIGINT, SIG_IGN);
	  qstat = signal(SIGQUIT, SIG_IGN);
	  /* wait for child to exit */
	  while ((wpid = wait(&waitstatus)) != pid && wpid != -1)
	     { ; }
	  signal(SIGINT, istat);
	  signal(SIGQUIT, qstat);
	  strcpy(name, echout);
	  stat = 1;
	  break;
       }
       close(pfd[0]);			  /* close pipe */
       close(pfd[1]);
#endif /* nt */
   }
   return stat;

#endif /* UNIX */

#if  MSC | MSDOS | __WATCOMC__

   char prefix[100], *env, *getenv();

   /* Home directory reference? */
   if (*name == '~' && (env=getenv("HOME"))) {
      strcpy(prefix, env);	      /* copy HOME directory name */
      name++;			      /* skip over ~ in name */
      strcat(prefix, name);
      name--;			      /* get back to beginning */
      strcpy(name, prefix);	      /* move back into name */
   }
   return 1;
#endif

}
