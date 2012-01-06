/************************************************************************
 *  SHARP-95                                                   		*
 *  Advanced Interactive Sounding Analysis Program             		*
 *                                                             		*
 *  John A. Hart                                               		*
 *  National Severe Storms Forecast Center                     		*
 *  Kansas City, Missouri                                      		*
 *                                                             		*
 * T. Lee/SAIC		10/02	Set gemhds_env to MODEL	       		*
 * K. Brill/HPC		 4/03	Invoke dg_intl for DG library  		*
 * R. Tian/SAIC		 1/04	Added nuflg to dg_intl call    		*
 * R. Tian/SAIC		 2/04	Removed nuflg from dg_intl call		*
 * T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
 ***********************************************************************/
#include "gui.h"
#define MAINMOD
#include "sharp95.h"
#include "proto_dg.h"

int main( void )
        {
        char *envvar;
	int ii, iret;
	int mode=1;

	sndgp = NULL;
	for ( ii = 0; ii < MAX_PIXMAP;  ii++)
	   sndgs[ii] = NULL;

        /* get environmental variables */
        envvar = getenv("GEMDATA\0");
        if(envvar != NULL)
          {
          gemdata_env = (char *)malloc(strlen(envvar)+1);
          strcpy(gemdata_env,envvar);
          }
        else
           gemdata_env = NULL;

        envvar = getenv("MODEL\0");
        if(envvar != NULL)
          {
          gemhds_env = (char *)malloc(strlen(envvar)+1);
          strcpy(gemhds_env,envvar);
          }
        else
           gemhds_env = NULL;
        

	/* initialize gempak variables */
	in_bdta ( &iret );
	gd_init ( &iret );
	gg_init  ( &mode, &iret );
	dg_intl ( &iret );

	printf("loading clo tables.....\n");
	clo_init ( &iret );
	printf("finished reading tables!\n");
	
	make_screen();

	return(0);
}
