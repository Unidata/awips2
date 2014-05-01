
#include <Component.h>
#include <ResJSys.h>

// int main( int argc, char **argv )   // replaced with the following 2 lines for linux
extern "C" {
extern int main_( int argc, char **argv )
{
	return 1;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_lx/RCS/cxx_main_lx.cxx,v $";
 static char rcs_id2[] = "$Id: cxx_main_lx.cxx,v 1.1 2001/09/18 18:08:40 dws Exp $";}
/*  ===================================================  */

}
}
int Component::_is_co_date = 0;
double Component::_lfactor = 1.0;
double Component::_ffactor = 1.0;
double Component::_vfactor = 1.0;
double Component::_cfactor = 1.0;
TSDate*  ResJSys::_co_dates = NULL;
