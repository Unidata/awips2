//------------------------------------------------------------------------------
// TSUtil.getDataLimits - get the data limits for the time series
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This used to be calcMaxMinValues.
//------------------------------------------------------------------------------
// History:
// 
// 06 Jan 1998	Steven A. Malers,	Split out of calcMaxMinValues.
//		Riverside Technology,
//		inc.
// 09 Jan 1998	SAM, RTi		Move from the individual time series
//					classes to the TSUtil library.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//------------------------------------------------------------------------------

#include "resj/TS.h"
#include "IrregularTS.h"
#include "TSUtil.h"
#include "TSLimits.h"
#include "TSDateIterator.h"

TSLimits TSUtil :: getDataLimits ( TS *ts, TSDate& start, TSDate& end )
{	char	routine[]="TSUtil.getDataLimits";
	double	max=0.0, min=0.0, value=0.0;
	int	base=0, found=0, mult=0;
	TSDate	max_date(TSDate::DATE_STRICT), min_date(TSDate::DATE_STRICT),
		non_missing_data_date1(TSDate::DATE_STRICT),
		non_missing_data_date2(TSDate::DATE_STRICT),
		t( TSDate::DATE_FAST ), ts_date1(TSDate::DATE_STRICT),
		ts_date2(TSDate::DATE_STRICT);

	if ( !ts ) {
		PrintWarning ( 2, routine, "NULL time series" );
		TSLimits tslimits0;
		return tslimits0;
	}

	// Make sure that the time series has current limits...

	ts->refresh ();

	max	= 1.0;
	min	= 0.0;

	// Get the variables that are used often in this function.

	base = ts->getDataIntervalBase();
	mult = ts->getDataIntervalMult();
	ts_date1 = ts->getDate1();
	ts_date2 = ts->getDate2();

	//Loop through the dates and get max and min data values;

	TSDate date(TSDate::DATE_STRICT);
	if ( base == TS::INTERVAL_IRREGULAR ) {
		TSData		*ptr=NULL;
/*
		IrregularTS	*ts = ts0;

		//Loop through the dates and get max and min data values;

		for( ptr = _ts_data_head; ptr != NULL; ptr = ptr->getNext() ){
*/
		TSDateIterator tsdi(ts,ts_date1,ts_date2);
		for (	;
			!tsdi.isIterationComplete();
			tsdi.advanceDate() ){
/*
			date = ptr->getDate();
*/
			date = tsdi.getCurrentDate();
	
			if ( date < ts_date1 ) {
				// Still looking for data...
				continue;
			}
			else if ( date > ts_date2 ) {
				// No need to continue processing...
				break;
			}

/*
			value = ptr->getData();
*/
			value = ts->getDataValue(date);
		
			if( ts->isDataMissing( value ) ) {
				//The value is missing
                        	continue;
			}

			if ( found == 0 ){
				// We set the limits to the first value found...
				max			= value;
				max_date		= date;
				min			= value;
				min_date		= date;
				non_missing_data_date1	= date;
				non_missing_data_date2	= date;
				found = 1;
				continue;
			}

			non_missing_data_date2 = date;
			if( value > max ) {
                		max		= value;
				max_date	= date;
			}

			if( value < min ) {
				min		= value;
				min_date	= date;
                	}
        	}

	}
	else {	// A regular TS... easier to iterate...
		for( t=start; t<=end; t.addInterval( base, mult )){
			if ( t < ts_date1 ) {
				// TS does not have data so no need to
				// process...
				continue;
			}
			else if ( t > ts_date2 ) {
				// TS does not have data so no need to
				// process...
				break;
			}

			value = ts->getDataValue( t );
		
			if( ts->isDataMissing( value ) ) {
				//The value is missing
                        	continue;
			}

			if( found == 0 ){
				// We set the limits to the first value found...
				max			= value;
				max_date		= t;
				min			= value;
				min_date		= t;
				non_missing_data_date1	= t;
				non_missing_data_date2	= t;
				found = 1;
			}

			non_missing_data_date2 = t;

			if( value > max ) {
                		max = value;
				max_date = t;
			}

			if( value < min ) {
                		min = value;
				min_date = t;
                	}
        	}
	}

	if( found == 0 ){
		PrintWarning( 1, routine,
		"Problems Finding Limits, whole POR missing!" );
		TSLimits limits0;
		return limits0;
	}

	PrintDebug( 10, routine, "Found limits to be: %f (%s)-> %f (%s)",
		min, (char *)min_date, max, (char *)max_date );
	PrintDebug( 10, routine,
		"Found non-missing data dates to be: %s -> %s",
		(char *)non_missing_data_date1, (char *)non_missing_data_date2);

	TSLimits limits;
	limits.setMaxValue( max, max_date );
	limits.setMinValue( min, min_date );
	limits.setNonMissingDataDate1 ( non_missing_data_date1 );
	limits.setNonMissingDataDate2 ( non_missing_data_date2 );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TSUtil_getDataLimits.cxx,v $";
 static char rcs_id2[] = "$Id: TSUtil_getDataLimits.cxx,v 1.3 2000/05/19 13:35:20 dws Exp $";}
/*  ===================================================  */

	return limits;

}
