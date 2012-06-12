/***************************************************************/
/*  SHARP-95                                                   */
/*  Advanced Interactive Sounding Analysis Program             */
/*                                                             */
/*  John A. Hart                                               */
/*  National Severe Storms Forecast Center                     */
/*  Kansas City, Missouri                                      */
/*                                                             */
/*  L. Hinson/AWC     11/02  corrected 'return type of Main not*/
/*                           int error                         */
/***************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#define MAINMOD
#include <sharp95.h>
#include "proto_dg.h"

	/*NP*/
	int main( int argc, char *argv[], char *envp[] )
        {

        int iret;
        int mode=1;
        
        nobanner = 1;
        if((argc > 1) &&(!strcmp(argv[1], "-v")))
           {
           nobanner = 0;

	   printf( "\n\n\n-------------------------------------------------\n");
	   printf(       "--------------------SHARP v3.0 ------------------\n");
	   printf(       "------------ John Hart & Jim Whistler -----------\n");
	   printf(       "- National Centers for Environmental Prediction -\n"); 
	   printf(       "--------- SPC/AWC Kansas City, Missouri ---------\n");
	   printf(       "-------------------------------------------------\n");
	   printf(       "----------- Last Modified: 26 Aug 1996 ----------\n");
	   printf(       "-------------------------------------------------\n");
           }
        gd_init ( &iret );
        gg_init (&mode, &iret);
        dg_intl(&iret);

	make_screen(argc, argv);
    
	return 0;
	}
