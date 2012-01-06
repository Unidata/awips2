
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
/*
c   int *validFlag : return value 
c   validFlag      = 1 (outlet)
c                  = 2 (inflow)
c                  = 0 DHM input deck's label error
c   ob8.3 - handle snow-17 input
c           check for label "USE_RAIN_PLUS_MELT"
c           set validFlag = 3
*/
void pin64_is_input_valid(char inputStr[], int* validFlag)
{

   char *tmpchar = NULL;
   *validFlag = 0; 
   tmpchar = strstr(inputStr,"OUTLET:");
   if (tmpchar != NULL){
       *validFlag = 1;
       
   } 
   
   tmpchar = strstr(inputStr,"INFLOW:");
   if (tmpchar != NULL){
       *validFlag = 2;
       
   }
   tmpchar = strstr(inputStr,"USE_RAIN_PLUS_MELT:");
   if (tmpchar != NULL){
       *validFlag = 3;
       
   }
    

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}


