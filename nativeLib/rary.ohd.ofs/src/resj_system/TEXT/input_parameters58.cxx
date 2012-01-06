//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Copyright:   See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 29 May 1998  Daniel Weiler, Riverside Technology, inc 	Initial version.
// 16 Oct 1998	DKW, RTi	Added some more carryover data.
// 08 May 2001  James R. VanShaar, RTi	Allowed for Debug information to be 
//					printed if errors in input occurred.
// 08 May 2001  JRV, RTi	Improved error, warning, status handling.
// 14 Dec 2001  JRV, RTi        Removed file_len from function parameter set
//
//------------------------------------------------------------------------------
// Variables:   I/O     Description
//
//
//------------------------------------------------------------------------------

#include "ResJSys.h"
#include <stdio.h>
#include <stdlib.h>


void input_parameters58( char* fname, float* PO, float* CO, 
	int* iusec, int* ipr, int* iodebug, int* ibug, int* ierr )
{
	char *po_list = NULL, *co_list = NULL, error[256], ctl_file[MAXC], 
		routine[]="input_parameters58";
	int i, po_size = 0, co_size = 0, length = 0, size = 0, t_step = 0;

	ResJSys resj;
	ResJSys::setIPR_IODEBUG (*ipr, *iodebug);

	// set the destinations for the debug (and status) and warning messages 
	// to the appropriate levels...
	ResJSys :: setDebugSaveLevel( *ibug );
	// The use of setWarningSaveLevel is currently required to reach the
	// 	SetWarningLevel(...) function
	ResJSys :: setWarningSaveLevel( 1 + *ibug );
		// value of 2 will cause functions printing the warning to 
		// appear

	if( fname == NULL || fname[0] == '\0') {
		*ierr = 1;
		sprintf( error, "Resj fs5file not specified" );
		length = strlen( error );
		PrintError( routine, error );
		//ResJ_fwrite( &length, error, ipr ); 
		return;
	}

        // Otherwise, use the length of the file name
	// to determine the actual file name. We have to do this explicitly
	// to cleanly handle the Fortran to C++ char* interface, which is
	// not a pretty one.
        for( i = 0; i < strlen( fname ); i++ ) {
		if( fname[i] == ' ') {
			break;
		}
		length++;
	}

	strcpy( ctl_file, fname );
	if( ctl_file == NULL ) {
		*ierr = 1;
		sprintf( error, "Cannot execute ResJ - fs5file non-existent." );
		length = strlen( error );
		PrintError( routine, error );
		//ResJ_fwrite( &length, error, ipr );
		return;
	}
	else {
		ctl_file[ length ] = '\0';
	}

	PrintStatus( 1, routine,
	"Using \"%s\" for input to ResJSys.", ctl_file);


	*ierr = resj.run( ctl_file ); 
	if( *ierr > 0 ) {
		// NOTE: The following will need to be modified as the errors
		// will likely be handled on a "as occurs" basis, rather than
		// storing them and printing them all at the end. JRV-5/9/01

		sprintf( error, "Faulty syntax in %s.", ctl_file );
		length = strlen( error );
		ResJ_ccwrite( &length, error, ipr ); 
		//ResJ_fwrite( &length, error, ipr ); 

		// Write the debug information, if appropriate.
		if( *ibug > 0 ) {
			char *resj_string = NULL;
			resj_string = ResJSys :: getDebugString( &size );   
			if( resj_string != NULL ) {
				ResJ_ccwrite( &size, resj_string, iodebug );
				//ResJ_fwrite( &size, resj_string , iodebug );
			}
		}

		// Write the error and status messages always.
		char *resj_string = NULL;
		resj_string = ResJSys :: getWarningString( &size );   
		if( resj_string != NULL ) {
			ResJ_ccwrite( &size, resj_string, ipr );
			//ResJ_fwrite( &size, resj_string, ipr );
		}
		return;
	}

	// Get the char** PO and CO lists from the resj object. Notice that
	// arguments to input_parameters58 for PO and CO are float*. This
	// means we are going to have to loop thru the lists and do memcpy.
	po_list = resj.getPOString( &po_size, &t_step );
	co_list = resj.getCOString( &co_size );
	if( po_list == NULL || po_size == 0 || co_list == NULL ||
		co_size == 0 ) {
		*ierr = 1;
		sprintf( error, "Troubles getting carryover or time series "
			"info from %s.", ctl_file );
		length = strlen( error );
		PrintError( routine, error );
		//ResJ_fwrite( &length, error, ipr ); 
		return;
	}

	// Now memcpy the po_list string into the PO float*
	length = strlen( po_list );
	if( memcpy( &PO[4], po_list, length ) == NULL ) {
		*ierr = 1;
		sprintf( error, "Troubles allocating memory for PO array." );
		length = strlen( error );
		PrintError( routine, error );
		//ResJ_fwrite( &length, error, ipr ); 
		return;
	}
	// Set the number of time series in the PO array. This info 
	// can be determined by using the length of the po_list.
	PO[1] = length/(4*5*sizeof(char));
	PO[2] = t_step;

	// Now memcpy the co_list string into the CO float*
	length = strlen( co_list );
	if( memcpy( CO, co_list, length ) == NULL ) {
		*ierr = 1;
		sprintf( error, "Troubles allocating memory for CO array." );
		length = strlen( error );
		PrintError( routine, error );
		//ResJ_fwrite( &length, error, ipr ); 
		return;
	}

	// set iusec to co_size
	*iusec = strlen( co_list )/4; 
	
	// Destination for messages generated during the the Res-J run...
	// for now assume that debug and warning messages go to the same 
	// file.
	// EJM's routine here
	if( *ibug > 0 ) {
		char *resj_string = NULL;
		resj_string = ResJSys :: getDebugString( &size );   
		if( resj_string != NULL ) {
			ResJ_ccwrite( &size, resj_string, iodebug );
			//ResJ_fwrite( &size, resj_string , iodebug );
		}
	}
	 
	// Write the error and status messages always.
	char *resj_string = NULL;
	resj_string = ResJSys :: getWarningString( &size );   
	if( resj_string != NULL ) {
		ResJ_ccwrite( &size, resj_string, ipr );
		//ResJ_fwrite( &size, resj_string, ipr );
	}

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */

	return;


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_system/RCS/input_parameters58.cxx,v $";
 static char rcs_id2[] = "$Id: input_parameters58.cxx,v 1.6 2006/10/26 15:37:39 hsu Exp $";}
/*  ===================================================  */

}
