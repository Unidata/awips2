#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* EJM - RTi - 08/08/98
/* This function finds all of the temporary RES-J fs5files for a segment  */
/* and moves them to the permanent files.                */

int mvresjfilestoperm( float *p, int *iseg ){

  int i, iold, ierr, len_fs5_dir, j, len12, i_0; 
  char fromName[128], toName[128], fs5_dir[128];
  /* AV pgf90 linuxport 7/10/01 */
  extern void generateresjfilename_();
  /* AV end pgf90 linuxport 7/10/01 */
  
  char hankname[128];
  char systemcall[256];
  float hankfloat = 1;

  len12 = 12;   
  i = 0;
  i_0 = 0;
  while ( p[i] > 0 ){
    if ( (p[i] > 57.90) && (p[i] < 58.9)){
      p[i+7] = 0;
      /* AV pgf90 linuxport 7/10/01 */
      generateresjfilename_( &(p[i+7]), fromName, iseg, &ierr);
      if ( ierr > 0.1 ) {
        ierr = 1;
        return 1;
      } 

      for ( j = 0; j < 127; j++ ){
        if ( fromName[j] == ' ' )  
          fromName[j] = '\0';
      }
      
      /* This code was inserted by Hank (2004-04-29).  Rather than
         determining the name of the permanent file based on the temp
         file, I now call generateresjfilename_ forcing the file name
         returned to be the permanent one corresponding to the temporary
         one acquired in the previous call.  This was done in order to
         more readily account for the temporary file having a 
         process id in its name. */
      hankfloat=p[i+7];
      p[i+7] = 1;  /*Force it to be permanent file*/
      generateresjfilename_( &(p[i+7]), hankname, iseg, &ierr);
      p[i+7] = hankfloat;  /*Recover the original value*/
      if ( ierr > 0.1 ) {
        ierr = 1;
        return 1;
      } 

      for ( j = 0; j < 127; j++ ){
        if ( hankname[j] == ' ' )  
          hankname[j] = '\0';
      }
      
      ierr = get_apps_defaults ( "ofs_fs5files", &len12, fs5_dir,
                  &len_fs5_dir ); 
      if ( ierr > 0.1 ) {
        ierr = 1;
        return 1;
      } 

      strcpy(toName, hankname);
/*     strcpy( toName, "" );
/*      fprintf(stderr, "mvRESJFilesToPerm: 1to string %s from string %s\n", toName, fromName );
/*      strncat( toName, fs5_dir, len_fs5_dir );
/*      strcat( toName, "/" );
/*     fprintf(stderr, "mvRESJFilesToPerm: 2to string %s \n", toName );
/*      strcat( toName, &(fromName[len_fs5_dir + 6]) ); 
/*    fprintf(stderr, "mvRESJFilesToPerm: 3to string %s\n", toName );  */
/*
 *    rename cannot be called now that the temp files are placed in the /tmp
 *    directory, which is not on the same device as the ofs_fs5files directory.
 *    Hank Herr (2004-05-03)
 *     ierr = rename( fromName, toName ); 
 */    
  
      /* 
      * Build the system call.
      */
      sprintf(systemcall, "mv %s %s",
          fromName, toName);
        
/*printf("  systemc all = '%s'.\n", systemcall);*/
        
      /*
      * Do the system call, and discard the results of the call.
      */
      ierr = system(systemcall);
      if ( ierr != 0 ) 
      {
        ierr = 1;        
        return 1;
      } 

      p[i+7] = 1;
    }     
    iold = i;
    i = p[ i + 1 ] - 1;

    if ( i <= iold ) {
    ierr = 1;
    return 1;
    }

  }
  return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_lx/RCS/mvRESJFilesToPerm.c,v $";
 static char rcs_id2[] = "$Id: mvRESJFilesToPerm.c,v 1.2 2004/05/04 14:26:12 dsa Exp $";}
/*  ===================================================  */

}
           
