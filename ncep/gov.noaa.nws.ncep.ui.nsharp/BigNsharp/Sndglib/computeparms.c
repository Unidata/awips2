#include <stdlib.h>
#include "sndglib.h"

/*************************************************************
 *                                                           *
 * Utility routines which all the user to compute all of     *
 * the thermodynamic or wind (or both) parameters for a      *
 * sounding in one swoop rather than having to do a million  *
 * calls individually.                                       *
 *                                                           *
 *                                                           *
 * Mike Kay                                                  *
 * Storm Prediction Center                                   *
 * mkay@spc.noaa.gov                                         *
 * 01/07/00                                                  *
 *************************************************************/

#ifdef BLAMMO

void computeallparms(void)
{
	/* Compute thermo paramters */
	computethermoparms();

	/* Compute wind parameters */
	computewindparms();
}

void computethermoparms(void)
{
	float parm, parm2, parm3, parm4;
	Parcel pcl;

	k_index(&parm);
	t_totals(&parm, &ct, &vt);
	parcel(-1, -1, pres, temp, dwpt, &pcl);
	wb_lvl(0.0, &parm);
	max_temp(&parm, -1);
	delta_t(&parm); /* 700 to 500 */
	parm = bulk_rich(pcl, &parm2);
	cnvtv_temp(&parm, -1);
	sweat_index(&parm);
}

void computewindparms(void)
{
	float parm, parm2, parm3, parm4;

	mean_wind(-1, -1, &parm, &parm2, &parm3, &parm4);
	wind_shear(-1, -1, &parm, &parm2, &parm3, &parm4);
	bunkers_storm_motion(&parm, &parm2, &parm3, &parm4);
	sr_wind(-1, -1, stdir, stspd, &parm, &parm2, &parm3, &parm4);
	helicity(-1, -1, stdir, stspd, &parm, &parm2);
}

#endif
