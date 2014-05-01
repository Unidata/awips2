
#include <Component.h>
#include <ResJSys.h>

//int __main( int argc, char **argv )

extern "C" {
int __main( int argc, char **argv )
{        
        printf("Program: ifp_nwsrfs (Version:  ob8.3 10/24/07)\n");
        return 1;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ifp/src/ifp_nwsrfs/RCS/ifp_nwsrfs_main.cxx,v $";
 static char rcs_id2[] = "$Id: ifp_nwsrfs_main.cxx,v 1.62 2007/03/20 17:29:05 dsa Exp $";}
/*  ===================================================  */

}
}
int Component::_is_co_date = 0;
double Component::_lfactor = 1.0;
double Component::_ffactor = 1.0;
double Component::_vfactor = 1.0;
double Component::_cfactor = 1.0;
TSDate*  ResJSys::_co_dates = NULL;



