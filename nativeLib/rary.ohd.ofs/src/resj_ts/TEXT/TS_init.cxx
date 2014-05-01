//------------------------------------------------------------------------------
// TS::init - initialize data members to initial values.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 11 Nov 1996	Matthew J. Rutherford,	Initial version.
//		Riverside Technology,
//		inc.
// 05 Feb 1997	Steven A. Malers, RTi	Iniitalize _dirty.
// 12 Mar 1997	MJR, RTi		Initialize mult intervals to 1 instead
//					of zero to help dividing by zero whe
//					trying to find data position.
// 23 Sep 1997	SAM, RTi		Initialize the global static flags.
// 08 Jan 1998	SAM, RTi		Update to agree with Java.
// 16 Apr 1998  Daniel Weiler, RTi	Initialized _is_instantaneous to 0.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS :: init( void )
{
	_version	= (char*)NULL;
	setVersion( "" );

	_input_name	= (char*)NULL;

	//_input_stream	= (ifstream)NULL;

	_input_stream_status = 0;

	_is_instantaneous = 0;

	//TSIdent and TSDate are initialized when they are constructed

	_sequence_number = 0;

	_data_type	= 0;

	_data_type	= (char*)NULL;
        setDataType( "" );

	_data_interval_base = 0;

	_data_interval_mult = 1;

	_data_interval_base_original = 1;

	_data_interval_mult_original = 0;

	_description	= (char*)NULL;
	setDescription( "" );

	_comments	= (char**)NULL;

	_genesis	= (char**)NULL;

	_data_units	= (char*)NULL;
	setDataUnits( "" );

	_data_units_original = (char*)NULL;
	setDataUnitsOriginal( "" );

	setMissing( -999.0 );

	setMaxValue( 0.0 );

	setMinValue( 0.0 );

	_dirty = 1;	// We need to recompute limits when we get the chance

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_init.cxx,v $";
 static char rcs_id2[] = "$Id: TS_init.cxx,v 1.3 2003/05/22 19:30:29 gzhou Exp $";}
/*  ===================================================  */

}

// Global static data...

const int TS :: DATES_ABSOLUTE		= 1;
const int TS :: DATES_RELATIVE		= 2;
const int TS :: DATES_UNIT		= 3;

const int TS :: FORMAT_MATRIX			= 1;
const int TS :: FORMAT_SPREADSHEET		= 2;

const int TS :: FORMAT_MATRIX_PRINT_NOTES	= 0x1;
const int TS :: FORMAT_MATRIX_PRINT_HEADER	= 0x2;
					// 0x4, 0x8, 0x10, 0x20... 0x800
					// are reserved for future use

const int TS :: FORMAT_SPREADSHEET_TAB_DELIM	= 0x1000;
const int TS :: FORMAT_SPREADSHEET_COMMA_DELIM	= 0x2000;
const int TS :: FORMAT_SPREADSHEET_PIPE_DELIM	= 0x4000;

const int TS :: FORMAT_MATRIX_PRINT_STAT_TOT	= 0x1000;
const int TS :: FORMAT_MATRIX_PRINT_STAT_AT	= 0x2000;
const int TS :: FORMAT_MATRIX_PRINT_STAT_NUM	= 0x4000;
const int TS :: FORMAT_MATRIX_PRINT_STAT_MIN	= 0x8000;
const int TS :: FORMAT_MATRIX_PRINT_STAT_MAX	= 0x10000;
const int TS :: FORMAT_MATRIX_PRINT_STAT_MEAN	= 0x20000;
const int TS :: FORMAT_MATRIX_PRINT_STAT_MED	= 0x40000;
const int TS :: FORMAT_MATRIX_PRINT_STAT_SDEV	= 0x80000;

const int TS :: FORMAT_MATRIX_PRINT_ALL_STATS	=
		TS :: FORMAT_MATRIX_PRINT_STAT_TOT 	|
		TS :: FORMAT_MATRIX_PRINT_STAT_AT	|
		TS :: FORMAT_MATRIX_PRINT_STAT_NUM	|
		TS :: FORMAT_MATRIX_PRINT_STAT_MIN	|
		TS :: FORMAT_MATRIX_PRINT_STAT_MAX	|
		TS :: FORMAT_MATRIX_PRINT_STAT_MEAN	|
		TS :: FORMAT_MATRIX_PRINT_STAT_MED	|
		TS :: FORMAT_MATRIX_PRINT_STAT_SDEV;

/*  Modified by guoxian Zhou 05/22/2003
const double TS :: INFINITY		= 100000000.0;
*/
const double TS :: u_INFINITY		= 100000000.0;

const int TS :: INTERVAL_SECOND 	= 1;
const int TS :: INTERVAL_MINUTE		= 60;
const int TS :: INTERVAL_HOUR		= 3600;
const int TS :: INTERVAL_DAY		= 86400;
const int TS :: INTERVAL_WEEK		= 604800;
const int TS :: INTERVAL_MONTH		= 2678400;
const int TS :: INTERVAL_YEAR		= 31536000;
const int TS :: INTERVAL_IRREGULAR	= 999;

const int TS :: ISTREAM_STATUS_NONE	= 1;
const int TS :: ISTREAM_STATUS_OPEN	= 2;
const int TS :: ISTREAM_STATUS_CLOSED	= 3;
