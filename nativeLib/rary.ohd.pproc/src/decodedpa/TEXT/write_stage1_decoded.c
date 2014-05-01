#include <stdio.h>
#include <math.h>
#include "GeneralUtil.h"
#include "GetOS.h"
#include "Swap4Bytes.h"
#include "decodedpa.h"


       void write_stage1_decoded(short int idat[][NUM_DPA_COLS],
                                 char name[256],
                                 short int *min,
                                 short int *incr,
                                 short int *levout,
                                 float *xmax,
                                 int *ier)

{

/*
   This subroutine writes the array containing the decoded precip values
     to a file.  This is done if the maximum precip value is > 0.0 else
     no file is written.

   the file is written using a FORTRAN unformatted write

   the units of the precip values in the file = dBA.

   IDAT() = data level from 0 - (numlevel - 1)
   IDAT() = 0 -- precip = 0.0

   precip value outside of radar = -99.
   precip value equal to 0.0 = -98.

   calling subroutines: decodeDPA

*/

       int i,j;
       float precip[NUM_DPA_COLS][NUM_DPA_ROWS], xmin, xinc;
       FILE *fp;
       OperSys os ;
       size_t       num_dpa_cols = NUM_DPA_COLS ;
 
   /*----------------------------*/
   /* Get  the operating system. */
   /*----------------------------*/

   os = GetOS ( ) ;

   if ( os == OS_ERROR )
   {
      printf ( "In \"write_stage1_decoded\":\n"
                "The call to \"GetOS\" failed.  The type of the\n"
                "operating system could not be determined.\n"
                "Aborting the attempt to read the stage1\n"
                "decoded file \"%s\".\n" , name ) ;
      *ier = -1;
      return ;
   }

   xmin = *min/10.;
   xinc = *incr/1000.;
   *xmax = -99.;
   *ier = 0;
 
   for(i=0; i<NUM_DPA_COLS; i++)
   {
      for(j=0; j<NUM_DPA_ROWS; j++)
      {
         if(idat[i][j] == *levout) 
            precip[i][j] = -99.0;
         else if(idat[i][j] == 0) 
            precip[i][j] = -98.0;
         else
            precip[i][j] = xmin + (idat[i][j] - 1)*xinc;

         if(precip[i][j] > *xmax) *xmax = precip[i][j];
      }
   }

/*--------------------*/
/*  change xmax to mm */
/*--------------------*/

   if(*xmax == -99. || *xmax == -98.)
      *xmax=0.0;
   else
      *xmax = pow ( 10 , *xmax / 10. ) ;
 
   /*----------------------------------------------*/
   /*  if there is > 0.0 precip, then create file  */
   /*----------------------------------------------*/

   if(*xmax > 0.0)
   {
      if ((fp = fopen(name, "wb")) == NULL)
      {
         printf("error opening file containing decoded file\n");
         *ier = -1;
         return;
      }

      /*-------------------------*/
      /*  write records to file  */
      /*-------------------------*/

      for ( i = 0 ; i < NUM_DPA_ROWS ; ++ i )
      {
         if ( os == OS_UNIX )
         {
            Swap4Bytes_ ( ( int * ) precip [ i ] , & num_dpa_cols ) ;
         }

         fwrite(precip[i], sizeof(float), NUM_DPA_COLS, fp);

      }

      fclose(fp);
   }
}
