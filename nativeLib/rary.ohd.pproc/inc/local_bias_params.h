/*******************************************************************************
* FILENAME:				local_bias_params.h
*
* DESCRIPTION:			This file contains local bias parameters.
*
* ORIGINAL AUTHOR:		Guoxian Zhou
* CREATION DATE:		January 25, 2005
* ORGANIZATION:			HSEB / OHD
* MACHINE:				HP-UX / Dell-Redhat Linux
* MODIFICATION HISTORY:
*   DATE         PROGRAMMER        DESCRIPTION/REASON
*
********************************************************************************
*/

#ifndef LOCAL_BIAS_PARAMS_H
#define LOCAL_BIAS_PARAMS_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "mpe_constants.h"

/*--------------------------------*/
/*  definition of variables       */
/*--------------------------------*/

typedef struct _local_bias_params
{
	/**
	  *	unit number for the state variable file
	  **/
	short int unit; 

	/**
	  *	save sv files ending at hours div. by imh
	  **/
	short int mult_hour;
	
	/**
	  *	number of hours to look back
	  **/
	short int nhr_look_back;
	
	/**
	  *	type_of_est=1 for areal estimation (default)
	  * type_of_est=2 for bin-specific estimation
	  **/
	short int type_of_est;
	  
	/**
	  *	radius (in km) within which updating is performed
	  **/
	float radius;

	/**
	  * maximum number of neighbors used in updating state variables 
	  * (default for rho0=0 is 500: if rho0 is positive, nborx must be set
	  * to a much smaller number in order to solve linear systems many
	  * times over in a reasonable amount of time)
	  **/
	int nborx;

	/**
	  *	lag-0+ spatial correlation coefficient (default is 0)
	  **/
	float rho0;

	/**
	  *	spatial correlation scale (in km)
	  * if rho0=0, it does not matter what cor_range is set to.
	  **/
	float cor_range;

	/**
	  *	cutoff Fisher information content
	  * number of positive pairs to choose "best" bias
	  * equivalent to npair_bias_select field in RWBiasStat table
	  **/
	float sel_npr_lb_rad;
	float sel_npr_lb_sat;

	/**
	  *	update state variables once every imult bins along either 
	  *	direction
	  * (default is 4)
	  **/
	int mult;

	/**
	  *	maximum number of neighbors to be used in filling holes 
	  * (default is 10)
	  **/
	int nbory;

	/**
	  *	interp=1 to interpolate via spiral search
	  * interp=2 to interpolate via double heap-sorting
	  **/
	int interp;
	
	/* The number of array indices found. */
	int ndat;

	/**
	  * hrap_x  - array containing array indices along the x-axis
	  * hrap_y  - array containing array indices along the y-axis
	  **/
	short idis [ NIND ];
        short jdis [ NIND ];

	/**
	  *	if interp=2, specify the maximum distance (in km) to the nearest 
	  * neighbor within which interpolation is to be performed
	  * to fill the fringes.
	  **/
	float dist_cut;

} local_bias_params;

typedef struct _local_bias_values {
   float si;   /* The number of gage radar pairs. */
   float xg;   /* The mean gage value. */
   float xr;   /* The mean radar value. */
}  local_bias_values;

typedef struct _local_bias_values_record {
   int col;
   int row;
   int datetime;
   local_bias_values local_bias_val [ NUM_MEMORY_SPANS ];
} local_bias_values_record;

/*-----------------------------*/
/*  function prototypes        */
/*-----------------------------*/

#endif /* #ifndef LOCAL_BIAS_PARAMS */
