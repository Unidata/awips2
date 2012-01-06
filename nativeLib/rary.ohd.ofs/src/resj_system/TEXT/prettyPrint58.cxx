//------------------------------------------------------------------------------
// prettyPrint58.cc : C++ function called by the FORTRAN wrapper to ResJ.
//			It will ouput the control file with some comments to
// 			the iodebug unit number.
//------------------------------------------------------------------------------
// Copyright:   See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 02 Jun 1998  Daniel Weiler, Riverside Technology, inc 	Initial version.
// 26 Oct 1998	DKW, RTi		Exported the banners to fwrite one
//					line at a time.
// 14 Jun 2001	James R. VanShaar, RTi	Added NOTE about initial conditions and
//					carryover values being as in the input
//					file only, and not reflectant of any 
//					transferred values.
// 29 Jun 2001	JRV, RTi	Improved error handling
// 14 Dec 2001  JRV, RTi        Removed file_len from function parameter set
//------------------------------------------------------------------------------
// Variables:   I/O     Description
//
//
//------------------------------------------------------------------------------

#include "ResJSys.h"
#include <stdio.h>
#include <stdlib.h>

void prettyprint58( char* fname, int* iodebug, int* ipr, int* ierr )
{
	char routine[]="prettyPrint58", string[MAXC], banner[2000], error[256],
		**list = NULL, ctl_file[MAXC], tmp_str[70];
	FILE* fp = (FILE*)NULL;
	int i, length = 0, nlist = 0;
	ResJSys resj;
	ResJSys::setIPR_IODEBUG (*ipr, *iodebug);

	if( fname == NULL ) {
		*ierr = 0;
		sprintf( error, "ResJ fs5file is not specified." );
		PrintError( routine, error );
		// length = strlen( error );
		// ResJ_fwrite( &length, error, ipr );
		return;
	}

        // Otherwise, use the length of the file name
	// to determine the actual file name. We have to do this explicitly
	// to cleanly handle the Fortran to C++ char* interface, which is
	// not a pretty one.
        for( i = 0; i < strlen( fname ); i++ ) {
		if( fname[i] == ' ' ) {
			break;
		}
		length++;
	}
	strncpy( ctl_file, fname, length );

	if( ctl_file == NULL ) {
		*ierr = 0;
		sprintf( error, "Cannot execute ResJ - fs5file non-existent." );
		length = strlen( error );
		PrintError( routine, error );
		// ResJ_fwrite( &length, error, ipr );
	}
	else {
		ctl_file[ length ] = '\0';
	}

	fp = fopen( ctl_file, "r" );
	if( fp == NULL ) {
		*ierr = 1;
		sprintf( error,"Troubles opening ResJ fs5file %s.", ctl_file );
		PrintError( routine, error );
		// length = strlen( error );
		// ResJ_fwrite( &length, error, ipr );
		return;
	}

	while( fgets( string, 256, fp ) != NULL ) {
		list = BreakStringList( string, " \t\n", DELIM_SKIP_BLANKS,
			&nlist );
		if( list == NULL || nlist == 0 ) {
			list = FreeStringList( list );
			continue;
		}
		UnpadString( string, " \t\n", PAD_BACK ); 
		if( !strcasecmp( list[0], "TIMESERIES" ) ) {
			sprintf( banner,
	"#####################################################################"
	"# Time Series specification from TIMESERIES to ENDTIMESERIES         "
	"#                                                                    "
	"# The time step fo the simulation must be specified in units of hours"
	"# in the TIMESTEP line.                                              " 
	"#                                                                    "
	"# Any Time Series to be used in ResJ must be specified by their      "
	"# in this section. The identifier must be prepended by either an     "
	"# INPUT or an OUTPUT designating its usage.                          "
	"#####################################################################"
			);
			length = 69;
			for( i = 0; i < 10; i++ ) {
				strncpy( tmp_str, &banner[i*69], 69 );
				ResJ_fwrite( &length, tmp_str , ipr ); 
			}
		}
		if( !strcasecmp( list[0], "TOPOLOGY" ) ) {
			sprintf( banner,
	"#####################################################################"
	"# Topology specification from TOPOLOGY to ENDTOPOLOGY                "
	"#                                                                    "
	"# Any Topology specifications to be used in ResJ must be specified   "
	"# in this section.                                                   "
	"#####################################################################"
			);
			length = 69;
			for( i = 0; i < 6; i++ ) {
				strncpy( tmp_str, &banner[i*69], 69 );
				ResJ_fwrite( &length, tmp_str , ipr ); 
			}
		}
		if( !strcasecmp( list[0], "PARAMETERS" ) ) {
			sprintf( banner,
	"#####################################################################"
	"# All Component and Method specification from PARAMETERS to          "
	"# ENDPARAMETERS                                                      "
	"#                                                                    "
	"# This section has many subsections denoting the specification of    "
	"# individual Components (Reservoirs, Reaches, Nodes) and individual  "
	"# Methods (SetRelease, LagK, etc.). The first specification must be  "
	"# the Component states, including initial values and input Time      "
	"# Series for that component.                                         "
	"# Component specification is characterized by, for a Reservoir, for  "
	"# example, RESERVOIR and ENDRESERVOIR. Method specification          "
	"# is similarly denoted by, for LagK for example, LAGK and            "
	"# ENDLAGK.                                                           "
	"# Every piece of data that the system needs to solve itself must be  "
	"# specified in the PARAMETERS section.                               "
	"# ***NOTE*** The initial conditions and carryover values herein may  "
	"#            not represent true values, as they are simply copies of "
	"#            those values input in the parameter file.  No transfer  "
	"#            of values will be reflected here.                       "
	"#####################################################################"
			);
			length = 69;
			for( i = 0; i < 20; i++ ) {
				strncpy( tmp_str, &banner[i*69], 69 );
				ResJ_fwrite( &length, tmp_str , ipr ); 
			}
		}
		if( !strcasecmp( list[0], "RULES" ) ) {
			sprintf( banner,
	"#####################################################################"
	"# System operating Rules information from RULES to ENDRULES          "
	"#                                                                    "
	"# Any operating rules for the network must be specified in this      "
	"# section.                                                           "
	"#####################################################################"
			);
			length = 69;
			for( i = 0; i < 6; i++ ) {
				strncpy( tmp_str, &banner[i*69], 69 );
				ResJ_fwrite( &length, tmp_str , ipr ); 
			}
		}

		// Always write out the string just read in from the control file.
		length = strlen( string );
		ResJ_fwrite( &length, string, ipr );
		list = FreeStringList( list );
	}

	fclose( fp );

	return;

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_system/RCS/prettyPrint58.cxx,v $";
 static char rcs_id2[] = "$Id: prettyPrint58.cxx,v 1.5 2006/10/26 15:37:43 hsu Exp $";}
/*  ===================================================  */

}
