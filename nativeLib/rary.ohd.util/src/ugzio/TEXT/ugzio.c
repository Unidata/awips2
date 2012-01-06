/* Routines to read and write gzip compressed files.

   Included in this file are the functions:
      ugzopn - open a file
      ugzcls - close a file
      ugzrd  - read a line from a file
      ugzwt  - write a line to a file
      ugzrwd - rewind afile
      ugznam - get the file name
      ugzflh - flush the buffer contents to the disk

   Function ugzrd can read non-gzipped files as well gzipped files but 
   it can not read 'compressed' (i.e. '.Z') files.

*/

#include <stdio.h>
#include <zlib.h>

#define mfiles 299

/* Cross-reference between the FORTRAN units and the C file handles
   is done using internal arrays. Arrays are sized to match the 
   maximum number of unit numbers allowed in FORTRAN routines used to
   read the NWSRFS DATACARD format files.
   
   File descriptions are kept track of using the special file types
   allowed by the zlib package.
*/

gzFile   fhandle[mfiles];
int      open_state[mfiles];
char     open_file_name[128][mfiles];
extern   int errno;

/**********************************************************************/

/* ugzopn opens a file */

int ugzopn (int *unit, char *filename, char *filenamen, int *lfn,
             char *status, int *icond) {
             
int i, lfnz;
char filenamei[128], filenameo[128];   int mfilename=128;
char stat[5];

   *icond=0;
   
/* put filename into local variable and append NULL. */
   for (i = 0; i < *lfn; i++) filenamei[i]=filename[i];
   filenamei[*lfn]='\0';
   strcpy (filenameo,filenamei);
   strcpy (filenamen,filenamei);

   if (*lfn > mfilename) {
      printf ("ERROR in ugzopn: number of characters in file name %s (%i) exceeds %i.\n",
              filenameo,*lfn,mfilename);
      *icond=1;
      return 0;
      }

/* check if file is opened */
   if (open_state[*unit] != +1) {
   /* check status status is 'OLD' */
      if (strncmp(status, "O", 1) == 0) {
      /* check if the file is a 'compressed' file (i.e. '.Z' suffix) -
         even though the gzip command can handle a compressed file the 
         gzip library routines can not. */
         if (strstr(filenamei,".Z")) {
            printf ("ERROR in ugzopn: file %s is a compressed (.Z) file.\n",filenameo);
            *icond=1;
            return 0;
            }
         fhandle[*unit] = gzopen(filenamei, "rb");
         if (fhandle[*unit]) {
            open_state[*unit]=+1;
            strncpy (open_file_name[*unit], filenamei, *lfn+1);
            open_file_name[*unit][*lfn+1]='\0';
            }
            else {
            /* check if file found */
               if (strstr(filenamei,".gz")) {
               /* remove '.gz' */
                  strncpy (filenamei,filenamei,*lfn-3);
                  filenamei[*lfn-3]='\0';
                  fhandle[*unit] = gzopen(filenamei, "rb");
                  if (fhandle[*unit]) {
                     open_state[*unit]=+1;
                     strncpy (open_file_name[*unit], filenamei, *lfn-2);
                     open_file_name[*unit][*lfn+2]='\0';
                     }
                     else {
                        printf ("ERROR in ugzopn: cannot open file %s.\n",filenamei);
                        *icond=1;
                        }
                     }
                  else {
                  /* add '.gz' */
                     strcat (filenamei,".gz");
                     lfnz=*lfn+3;
                     filenamei[lfnz]='\0';
                     fhandle[*unit] = gzopen(filenamei, "rb");
                     if (fhandle[*unit]) {
                        open_state[*unit]=+1;
                        strncpy (open_file_name[*unit], filenamei, lfnz+1);
                        open_file_name[*unit][lfnz+1]='\0';
                        *lfn=lfnz;
                     }
                     else {
                        printf ("ERROR in ugzopn: cannot open file %s.\n",filenameo);
                        *icond=1;
                        }
                     }
            }
        }
        else {
        /* status is 'NEW' or 'UNKNOWN' - append '.gz' if needed */
           lfnz=*lfn+1;
           if ( ! strstr(filenamei,".gz")) {
              strcat (filenamei,".gz");
              lfnz=*lfn+3;
              filenamei[lfnz]='\0';
              }
           fhandle[*unit] = gzopen(filenamei, "wb");
           if (fhandle[*unit]) {
              open_state[*unit]=+1;
              strncpy (open_file_name[*unit] ,filenamei, lfnz);
              open_file_name[*unit][lfnz]='\0';
              *lfn=lfnz;
              } 
              else {
                 printf ("ERROR in ugzopn: cannot create file %s.\n",filenamei);
                 *icond=2;
                 }
         }
     }
     
   for (i = 0; i < *lfn; i++) filenamen[i]=filenamei[i];
   filenamen[*lfn]='\0';

   return 0;

}


/**********************************************************************/

/* ugzcls closes a file if it is open */

int ugzcls (int *unit) {

   if (open_state[*unit] == +1) {
      gzclose(fhandle[*unit]);
      open_state[*unit]=-1;
      }

   return 0;

}


/**********************************************************************/

/* ugzrd reads a line from the file (will read until number of  
   characters specified is read or a new-line character is found) */

int ugzrd (int *unit, char *string, int *nchar, int *icond) {

int i,j;

   *icond=0;

   if (open_state[*unit] == +1) {
      /*printf ("in ugzrd : unit=%d nchar=%d\n",*unit,*nchar);*/
      strncpy (string, " ", *nchar);
      if (gzgets(fhandle[*unit], string, *nchar) == Z_NULL) {
         if (gzeof(fhandle[*unit]))
            *icond=2;
            else
               *icond=1;
         }
   /* check for line-feed control character */
      for (i = 0; i < *nchar; i++) {
         if (string[i] == 10) {
            /*printf ("i=%d\n",i);*/
            strcpy (&string[i]," ");
            if (i < *nchar) {
            /* fill out record with blanks */
               for (j = i+1; j < *nchar; j++) {
                  strcpy (&string[j]," ");
                  }
               }
            break;
            }
         }
      }
      else {
         printf ("ERROR in ugzrd: cannot read from unit %i ",
                 "because it is not open.\n",
                 unit);
         *icond=1;
         }

   return 0;

}


/**********************************************************************/
 
/* ugzwt writes a line to a file */

int ugzwt (int *unit, char *string, int *nchar, int *icond) {

char fmt[5];

   *icond=0;

   if (open_state[*unit] == +1) {
      sprintf(fmt,"%%%d%s\n", *nchar, "s");
      string[*nchar+1]='\0';
      if (gzprintf(fhandle[*unit], fmt, string) == 0) {
         printf ("ERROR in ugzwt: writing to unit %i.\n",unit);
         *icond=1;
         }
      }
      else {
         printf ("ERROR in ugzwt: cannot write to unit %i ",
                 "because it is not open.\n",
                 unit);
         *icond=1;
         }

   return 0;

}


/**********************************************************************/

/* ugzrwd rewinds a file */

int ugzrwd (int *unit, int *icond) {

   *icond=0;

   if (open_state[*unit] == +1) {
      gzrewind(fhandle[*unit]);
      }
      else {
         printf ("ERROR in ugzrwd: cannot reqind unit %i ",
                 "because it is not open.\n",
                 unit);
         *icond=1;
         }
 
   return 0;
   
}


/**********************************************************************/

/* ugznam gets the file name associated with a FORTRAN unit number */

int ugznam (int *unit, char *filename, int *lenname, int *icond) {

   *icond=0;

   if (open_state[*unit] == +1) {
      *lenname=strlen(open_file_name[*unit]);
      strncpy (filename, open_file_name[*unit],*lenname+1);
      }
      else {
         printf ("ERROR in ugznam: cannot get file name for unit %i ",
                 "because it is not open.\n",
                 unit);
         *icond=1;
         }

   return 0;

}


/**********************************************************************/
 
/* ugzflh flushes a file if it is open */

int ugzflh (int *unit, int *icond) {

   *icond=0;

   if (open_state[*unit] == +1) {
      if (gzflush(fhandle[*unit],Z_SYNC_FLUSH) != Z_OK) {
         printf ("ERROR in ugzflh: flushing unit %i.\n",unit);
         *icond=1;
         }
      else {
         printf ("ERROR in ugzflh: cannot write to unit %i ",
                 "because it is not open.\n",
                 unit);
         *icond=1;
         }
      }

   return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/util/src/ugzio/RCS/ugzio.c,v $";
 static char rcs_id2[] = "$Id: ugzio.c,v 1.4 2001/06/12 19:40:25 dws Exp $";}
/*  ===================================================  */

}
