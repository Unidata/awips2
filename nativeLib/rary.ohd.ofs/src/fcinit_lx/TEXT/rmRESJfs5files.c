#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* EJM - RTi - 08/08/98
/* This function finds all of the RES-J fs5files for a segment 	*/
/* and removes them from the processed database.  		*/
/* If pold[i+7] = 0 then it removes the temporary resj files,  	*/
/* otherwise it removes the permanent files    			*/


int rmresjfs5files( float *pold, int *iseg ){

  int i, iold, ierr, len_fs5_dir, j, len12, temp; 
  char fileName[128];
/* AV pgf90 linuxport 7/10/01 */
  extern void generateresjfilename_();
/* AV end pgf90 linuxport 7/10/01 */
  len12 = 12;   
  i = 0;

  while ( pold[i] > 0 ){
    if ( (pold[i] > 57.90) && (pold[i] < 58.9)){
      temp = pold[i + 7];
      pold[i + 7] = 1;

      /* AV pgf90 linuxport 7/10/01 */
      generateresjfilename_( &(pold[ 7+ i ]), fileName, iseg, &ierr);
      if ( ierr > 0.1 ) {
        ierr = 1;
	return 1;
      }

      pold[7 + 1] = temp;
      for ( j = 0; j < 127; j++ ){
        if ( fileName[j] == ' ' )  
          fileName[j] = '\0';
      }

      ierr = remove ( fileName );

      if ( ierr < -0.1 ) {
        ierr = 1;
        return 1;
      }


    }
    iold = i;
    i = pold[ i + 1 ] - 1;

    if ( i <= iold ) {
    ierr = 1;
    return 1;
    }


  }
  return 0;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_lx/RCS/rmRESJfs5files.c,v $";
 static char rcs_id2[] = "$Id: rmRESJfs5files.c,v 1.1 2001/09/18 15:22:12 dws Exp $";}
/*  ===================================================  */

}
           
