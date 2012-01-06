//------------------------------------------------------------------------------
// DistributedTS - constructors
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 18 Sep 1997	Matthew J. Rutherford, RTi	Created initial versions.
// 18 May 2001	James R. VanShaar, RTi	Adjusted getDataValue to use Julian
//					Hour instead of Julian day in the
//					interpolation scheme
// 18 May 2001	JRV, Rti	Corrected some issues in determining relative
//				years in the relative timeseries in getDataValue
// 02 July 2001	JRV, RTi	Adjusted warning level within
//				IrregularTS::freeDataSpace( void )
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------
// DistributedTS constructor
//
// Notes:	(1)	Need to spend time on this
//

#include "DistributedTS.h"

DistributedTS::DistributedTS ( void ) : IrregularTS()
{	char	routine[]="DistributedTS::DistributedTS";

	PrintDebug( 50, routine, "Constructing" );

	init();
}
// ----------------------------------------------------------------------------
// ~Distributed destructor - construct from file
//
// Notes:	(1)	Need to spend time on this
// ----------------------------------------------------------------------------
// History:
//
// 05 Mar 1998	Daniel Weiler, RTi	Created initial version.
// ----------------------------------------------------------------------------

DistributedTS::~DistributedTS ( void )
{
}
//------------------------------------------------------------------------------
// DistributedTS::operator - Operators.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 05 May 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

void DistributedTS::operator= ( const DistributedTS& ts )
{
	char	routine[]="DistributedTS::operator=";
	TSData	*tsd=NULL;
        TSDate  temp;

	freeDataSpace();
	init();

	_n_dist		= ts._n_dist;

	_is_relative	= ts._is_relative;

	for( tsd=(TSDistData*)ts._ts_data_head; tsd!=NULL; tsd=tsd->getNext() ){
		TSDistData	*dd = (TSDistData*)tsd;
                temp = dd->getDate();
		setDataValue( 	temp, dd->getData(),
				dd->getDistArray(), dd->getNDist() );
	}
}
//------------------------------------------------------------------------------
// DistributedTS::getDataDate - get the date relative to a starting date
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This function finds the next/previous date for a
//			irregular interval time series.  This can be somewhat
//			inefficient if get a long string of data but many
//			irregular time series do not have a lot of data and
//			the performance hit will not be noticeable.
//------------------------------------------------------------------------------
// History:
//
// 04 May 1998	Daniel Weiler,			First version.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------


TSDate DistributedTS::getDataDate ( TSDate &date0, int increment )
{	char	routine[] = "DistributedTS::getDataDate";
	int	found=0;
	TSDate	date;
	TSData	*ptr=NULL;

	date = date0;

	if( _is_relative ) {
		date.setYear( 0 );
	}

	return( IrregularTS::getDataDate( date, increment ) );
}

//------------------------------------------------------------------------------
// DistributedTS::getDataValue - returns a data value for a date
//		assuming the presence of a distribution applied to the ts.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes: 	if a distributed, irregular TS looks like (DD/MM):
//		02/01 00:00	1.2
//		03/01 00:00	2.3
//		04/01 00:00	0.7
//		04/01 12:00	0.9
//		05/01 00:00	0.0
//		etc. If the incoming date is 03/15, the returned value will
//		be 2.3 or 1.5, depending on the flag which designates that the
//		distibution is either a block or is to be interpolated.
//------------------------------------------------------------------------------
// History:
//
// 18 Feb 1998	Daniel Weiler, RTi	Created initial version.
//------------------------------------------------------------------------------
// Variable	I/O	Description
//
// column	L	column (y) index of data.
// date		I	date to retrieve data for.
// routine	L	name of routine.
// row		L	row (x) index of data.
// t		L	date used for counter.
//------------------------------------------------------------------------------

double DistributedTS::getDataValue( TSDate &cur_date, int flag )
{
	char	routine[] = "DistributedTS::getDistDataValue";
//	int	i=0, jul_day_min = 0, jul_day_max = 0, jul_day_find = 0;
	int	i=0, jul_hour_min = 0, jul_hour_max = 0, jul_hour_find = 0;
	int	prev_year, next_year;
	TSDate	date;
	TSData	*prev_ptr=NULL, *next_ptr = NULL;
	double	value = -999.0, value_next = 0.0;

	// Check the date coming in and manipulate it if this is a
	// relative time series...
	date = cur_date;
	if( _is_relative ) {
		date.setYear( 0 );
	}


	// If the date is before the first date in the time series, then
	// return the value for the last date in the timeseries. Doing
	// this effects a psuedo-circular time series that implies no
	// date is out of range.
	if( date < _date1 ) {
		// prev_ptr needs to be set to the last date in the
		// timeseries and the prev_year will be one less than
		// the cur_date year.
		prev_ptr = _ts_data_tail;
		prev_year = cur_date.getYear() - 1;
	}
	else if( date > _date2 ) {
		// prev_ptr needs to be set to the last date in the
		// timeseries and the prev_year will be equal to the
		// cur_date year.
		prev_ptr = _ts_data_tail;
		prev_year = cur_date.getYear();
	}
	else {
		for( i=0, prev_ptr=_ts_data_head; prev_ptr != NULL;
			prev_ptr = prev_ptr->getNext(), i++ ) {
			// get the data value from the closest previous
			// timestep that is in the TS, unless it is
			// equal to the current prev_ptr.  prev_year will be
			// equal to the cur_date year.
			if( date == prev_ptr->getDate() ) {
				prev_year = cur_date.getYear();
				break;
			}
			else if( date < prev_ptr->getDate() ){
				prev_year = cur_date.getYear();
				prev_ptr = prev_ptr->getPrevious();
				break;
			}
		}
	}

	// We need to check that prev_ptr isn't NULL.
	if( prev_ptr == NULL ){
		return( -999.0 );
	}

	// If we get here, this means that prev_ptr represents the TSData that
	// is the closest previous data relative to the incoming TSDate.

	PrintDebug( 30, routine,
		"%f for %d/%d/%d from _data (%d)", prev_ptr->getData(),
		date.getMonth(), date.getDay(), date.getYear(), i );

	// We have the TSDistData prev_ptr, now we need to check the flag and
	// determine if the returned value is to be a product of linear
	// interpolation between the previous and next distributed values or a
	// normal block distribution.
	for( i = 0; i < _n_dist; i++ ) {
		if( date.getHour() <= ( 24 / _n_dist ) * ( i + 1 ) ) {
			value = ((TSDistData*)prev_ptr)->getData() *
				((TSDistData*)prev_ptr)->getDistribution( i );
			break;
		}
	}

	// If the flag is NORMAL, then we don't do anything to value, else...
	if( flag == NORMAL ) {
		return( value );
	}
	else if( flag == INTERPOLATE ) {
		next_ptr = prev_ptr->getNext();
		if( next_ptr == NULL ) {
			next_ptr = _ts_data_head;
			next_year = prev_year + 1;
		}
		else {
			next_year = prev_year;
		}
		// get the value at the next Distributed date...
		value_next = ( ( TSDistData* )( next_ptr ) )->getData() *
			( ( TSDistData* )( next_ptr ) )->getDistribution( i );

		// Get Julian Hour for each of the dates
		// previous date (min)
		if ( GetJulianHour1900FromDate( prev_ptr->getDate().getMonth(),
                        prev_ptr->getDate().getDay(), prev_year,
			prev_ptr->getDate().getHour(), &jul_hour_min ) ) {
			PrintWarning( 2, routine, "Troubles interpolating "
				"Distributed time series, returning NORMAL "
				"value.");
			return( value );
		}
		// current date (find)
		if ( GetJulianHour1900FromDate( date.getMonth(), date.getDay(),
			cur_date.getYear(), cur_date.getHour(),
			&jul_hour_find ) ) {
			PrintWarning( 2, routine, "Troubles interpolating "
				"Distributed time series, returning NORMAL "
				"value.");
			return( value );
		}
		// next date (max)
		if ( GetJulianHour1900FromDate( next_ptr->getDate().getMonth(),
                        next_ptr->getDate().getDay(), next_year,
			next_ptr->getDate().getHour(), &jul_hour_max ) ) {
			PrintWarning( 2, routine, "Troubles interpolating "
				"Distributed time series, returning NORMAL "
				"value.");
			return( value );
		}

		// We can interpolate for the value now...
		double int_value = -999.0;
		int_value = Interp( (float)jul_hour_find, (float)jul_hour_min,
			(float)jul_hour_max, value, value_next );
/****
		if( GetJulianDay1900FromDate( prev_ptr->getDate().getMonth(),
			prev_ptr->getDate().getDay(), cur_date.getYear(),
			&jul_day_min ) ||
			GetJulianDay1900FromDate(next_ptr->getDate().getMonth(),
			next_ptr->getDate().getDay(), next_year,
			&jul_day_max ) ||
			GetJulianDay1900FromDate(date.getMonth(), date.getDay(),
			cur_date.getYear(), &jul_day_find ) ) {
			PrintWarning( 2, routine, "Troubles interpolating"
			" Distributed time series, returning NORMAL value.");
			return( value );
		}
		// We can interpolate for the value now...
		double int_value = -999.0;
		int_value = Interp( (float)jul_day_find, (float)jul_day_min,
			(float)jul_day_max, value, value_next );
****/

		return( int_value );
	}
}
//------------------------------------------------------------------------------
// DistributedTS::getNextPrevDates - returns two dates that
//				represent the previous and the next data
//				for a date.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes: 	if a distributed, irregular TS looks like (DD/MM):
//		02/01 00:00	1.2
//		03/01 00:00	2.3
//		04/01 00:00	0.7
//		04/01 12:00	0.9
//		05/01 00:00	0.0
//		etc. If the incoming date is 03/15, the returned values will
//		be prev = 03/01 and next = 04/01.
//------------------------------------------------------------------------------
// History:
//
// 18 Feb 1998	Daniel Weiler, RTi	Created initial version.
//------------------------------------------------------------------------------
// Variable	I/O	Description
//
// column	L	column (y) index of data.
// date		I	date to retrieve data for.
// routine	L	name of routine.
// row		L	row (x) index of data.
// t		L	date used for counter.
//------------------------------------------------------------------------------

int DistributedTS::getPrevNextDates( TSDate &cur_date, TSDate* prev,
					TSDate* next )
{
	char	routine[] = "DistributedTS::getPrevNextDates";
	int	i=0, jul_day_min = 0, jul_day_max = 0, jul_day_find = 0;
	TSDate	date;
	TSData	*next_ptr = NULL;
	TSDistData* ptr=NULL;

	// Check the date coming in and manipulate it if this is a
	// relative time series...
	date = cur_date;
	if( _is_relative ) {
		date.setYear( 0 );
	}

	if( ( date <= _date1 ) || ( date >= _date2 ) ) {
		ptr = (TSDistData*)_ts_data_tail;
		*prev = ptr->getDate();
		ptr = (TSDistData*)_ts_data_head;
		*next = ptr->getDate();
		return( STATUS_SUCCESS );
	}

	//
	// Start at the head, for simplicity
	//
	for( i=0, ptr=(TSDistData*)_ts_data_head; ptr != NULL;
	ptr = (TSDistData*)(ptr->getNext() ), i++ ){
		// get the data value from the closest previous
		// timestep that is in the TS, unless it is equal
		// to the current ptr
		if( date == ptr->getDate() ) {
			*prev = date;
			*next = date;
			break;
		}
		else if( date < ptr->getDate() ) {
			if( ptr->getPrevious() == NULL ) {
				// This will be null on the first time step
				// in a TS
				*prev = ptr->getDate();
			}
			else {
				*prev = ptr->getPrevious()->getDate();
			}
			*next = ptr->getDate();
			break;
		}
	}

	// If we get here, this means that ptr represents the TSData that is the
	// closest previous data relative to the incoming TSDate.

	return( STATUS_SUCCESS );
}
//------------------------------------------------------------------------------
// DistributedTS::getNextPrevValue - returns two data values that
//				represent the previous and the next data
//				for a date.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes: 	if a distributed, irregular TS looks like (DD/MM):
//		02/01 00:00	1.2
//		03/01 00:00	2.3
//		04/01 00:00	0.7
//		04/01 12:00	0.9
//		05/01 00:00	0.0
//		etc. If the incoming date is 03/15, the returned values will
//		be prev = 2.3 and next = 0.7.
//------------------------------------------------------------------------------
// History:
//
// 18 Feb 1998	Daniel Weiler, RTi	Created initial version.
//------------------------------------------------------------------------------
// Variable	I/O	Description
//
// column	L	column (y) index of data.
// date		I	date to retrieve data for.
// routine	L	name of routine.
// row		L	row (x) index of data.
// t		L	date used for counter.
//------------------------------------------------------------------------------

int DistributedTS::getPrevNextValues( TSDate &cur_date, double* prev,
					double* next )
{
	char	routine[] = "DistributedTS::getPrevNextValues";
	int	i=0, jul_day_min = 0, jul_day_max = 0, jul_day_find = 0;
	TSDate	date;
	TSData	*next_ptr = NULL;
	TSDistData* ptr=NULL;
	double	value = -999.0, value_next = 0.0;


	// Initialize these pointers to MISSING
	*prev = -999.0;
	*next = -999.0;

	// Check the date coming in and manipulate it if this is a
	// relative time series...
	date = cur_date;
	if( _is_relative ) {
		date.setYear( 0 );
	}

	if( ( date < _date1 ) || ( date > _date2 ) ) {
		ptr = (TSDistData*)_ts_data_head;
		*prev = ptr->getData() * ptr->getDistribution( 0 );
		ptr = (TSDistData*)_ts_data_tail;
		int index = ptr->getNDist();
		*next = ptr->getData() * ptr->getDistribution( index-1 );
		return( STATUS_SUCCESS );
	}

	//
	// Start at the head, for simplicity
	//
	for( i=0, ptr=(TSDistData*)_ts_data_head; ptr != NULL;
	ptr = (TSDistData*)(ptr->getNext() ), i++ ){
		// get the data value from the closest previous
		// timestep that is in the TS, unless it is equal
		// to the current ptr
		if( date <= ptr->getDate() ) {
			if( ptr->getPrevious() == NULL ) {
				// This will be null on the first time step
				// in a TS
				*prev = ptr->getData() *
					ptr->getDistribution( 0 );
			}
			else {
				*prev = ptr->getPrevious()->getData() *
					ptr->getDistribution( 0 );
			}
			*next = ptr->getData() *
				ptr->getDistribution( 0 );
			break;
		}
	}

	// If we get here, this means that ptr represents the TSData that is the
	// closest previous data relative to the incoming TSDate.

	return( STATUS_SUCCESS );
}
//------------------------------------------------------------------------------
// DistributedTS::init
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 18 Sep 1997	Matthew J. Rutherford, RTi	created member function
//
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

int DistributedTS::init( void )
{
	_n_dist = 0;

	_is_relative = 0;

	return( STATUS_SUCCESS );
}
//------------------------------------------------------------------------------
// DistributedTS::isDateInTS - returns a flag signifying if cur_date is
//				an entry in this DistributedTS
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes: 	if a distributed, irregular TS looks like (DD/MM):
//		02/01 00:00	1.2
//		03/01 00:00	2.3
//		04/01 00:00	0.7
//		04/01 12:00	0.9
//		05/01 00:00	0.0
//		etc. If the incoming date is 03/15, the returned flag is 0
//------------------------------------------------------------------------------
// History:
//
// 25 Aug 1998	Daniel Weiler, RTi	Created initial version.
//------------------------------------------------------------------------------
// Variable	I/O	Description
//
// column	L	column (y) index of data.
// date		I	date to retrieve data for.
// routine	L	name of routine.
// row		L	row (x) index of data.
// t		L	date used for counter.
//------------------------------------------------------------------------------

int DistributedTS::isDateInTS( TSDate &cur_date )
{
	char	routine[] = "DistributedTS::getPrevNextDates";
	int	i=0, jul_day_min = 0, jul_day_max = 0, jul_day_find = 0;
	TSDate	date;
	TSData	*next_ptr = NULL;
	TSDistData* ptr=NULL;

	// Check the date coming in and manipulate it if this is a
	// relative time series...
	date = cur_date;
	if( _is_relative ) {
		date.setYear( 0 );
	}

	if( ( date < _date1 ) || ( date > _date2 ) ) {
		return( 0 );
	}

	//
	// Start at the head, for simplicity
	//
	for( i=0, ptr=(TSDistData*)_ts_data_head; ptr != NULL;
	ptr = (TSDistData*)(ptr->getNext() ), i++ ){
		// get the data value from the closest previous
		// timestep that is in the TS, unless it is equal
		// to the current ptr
		if( date == ptr->getDate() ) {
			return( 1 );
		}
	}

	// If we get here, this means that the cur_date didn't find a match
	// with the dates in the time series.
	return( 0 );
}
//------------------------------------------------------------------------------
// DistributedTS::setDataValue - sets a data value for a date.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//------------------------------------------------------------------------------
// History:
//
// 18 Sep 1997	Matthew J. Rutherford, RTi	Created initial version.
// 05 Mar 1998  Daniel Weiler, RTi	Redefined from IrregularTS. This
//					version uses TSDistData instead of
//					TSData.
// 20 Mar 2006	James R. VanShaar, RTi	Revised debug message that referenced
//					getNext() when date is last, resulting
//					in segmentation fault.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

int DistributedTS::setDataValue( TSDate &date, double value, float* dist,
	int n_dist)
{
	char	routine[]="DistributedTS::setDataValue";
	int	found, i;
	TSData	*next=NULL, *ptr=NULL;
	TSDistData *tsdata=NULL;

	// Set this data member first off
	_n_dist = n_dist;

	if( !_ts_data_head ){
		tsdata = new TSDistData();

		if( !tsdata ){
			PrintWarning( 1, routine,
			"Unable to allocate memory for TSData object." );
			return( STATUS_FAILURE );
		}

		tsdata->setValues( date, value, _data_units, "" );

		//
		// We need to set the head of the list.
		//
		_ts_data_head	= tsdata;
		_ts_data_tail	= tsdata;

		_date1 		= tsdata->getDate();
		_date2 		= tsdata->getDate();

		PrintDebug( 10, routine,
		"Initiated the Linked list with: \"%s\".", (char*)tsdata );

		// Construct the distribution array on the TSDistData object...
		if( tsdata->setDistribution( dist, n_dist ) ) {
			PrintWarning( 1, routine, "Troubles setting distribution." );
			return( STATUS_FAILURE );
		}
		return( STATUS_SUCCESS );
	}
        //
        // We need to determine if the date is closer to the
        // tail of the list, or the head.
        //
        double  date_double     = date.toDouble();
        double  date1_double    = _date1.toDouble();
        double  date2_double    = _date2.toDouble();

	found = 0;
        if(     fabs( date_double - date1_double ) <
                fabs( date_double - date2_double ) ){

		for( 	i=0, ptr = _ts_data_head;
			ptr != NULL; ptr = ptr->getNext(), i++ ){
			if( ptr->getDate() == date ){
				PrintDebug( 30, routine,
				"Setting %f for %s at %d",
				value, (char*)date, i );

				// Set the dirty flag so that we know to
				// recompute the limits if desired...

				_dirty = 1;
				found  = 1;

				ptr->setData( value );
				break;
			}
		}
	}
	else {
		for( 	i=_ndata, ptr = _ts_data_tail;
			ptr != NULL; ptr = ptr->getPrevious(), i-- ){
			if( ptr->getDate() == date ){
				PrintDebug( 30, routine,
				"Setting %f for %s at %d",
				value, (char*)date, i );

				// Set the dirty flag so that we know to
				// recompute the limits if desired...

				_dirty = 1;
				found  = 1;

				ptr->setData( value );
				break;
			}
		}
	}

	if( found ){
		PrintDebug( 10, routine,
		"Updated data value at: %s to %f.", (char*)date, value );
		return( STATUS_SUCCESS );
	}
	//
	// If we got here, then we didn't find the date, we need to
	// insert the TSData object created above..
	//
	tsdata = new TSDistData();

	if( !tsdata ){
		PrintWarning( 1, routine,
		"Unable to allocate memory for TSData object." );
		return( STATUS_FAILURE );
	}

	tsdata->setValues( date, value, _data_units, "" );

	found=0;
        if(     fabs( date_double - date1_double ) <
                fabs( date_double - date2_double ) ){

		for( 	i=0, ptr = _ts_data_head;
			ptr != NULL; ptr = ptr->getNext(), i++ ){
			if( ptr->getDate() < date && !ptr->getNext() ){
				//
				// We are adding an element to the end of the
				// list.
				//
				PrintDebug( 10, routine,
				"Inserting %s after %s.",
				(char*)tsdata->getDate(),
				(char*)ptr->getDate() );

				ptr->setNext( tsdata );
				tsdata->setPrevious( ptr );

				_date2 = tsdata->getDate();
				_ts_data_tail = tsdata;

				_dirty	= 1;
				_ndata++;
				found = 1;
				break;
			}
			else if(ptr->getDate() > date &&
				!ptr->getPrevious() ){
				//
				// We have to add an element to the beginning
				// of the list.
				//
				PrintDebug( 10, routine,
				"Inserting %s before %s.",
				(char*)tsdata->getDate(),
				(char*)ptr->getDate() );

				tsdata->setNext( ptr );
				ptr->setPrevious( tsdata );
				_ts_data_head = tsdata;

				_date1 = tsdata->getDate();

				 _dirty	= 1;
				 _ndata++;
				 found = 1;
				 break;
			}
			else if(ptr->getDate() < date &&
				ptr->getNext()->getDate() > date ){
				//
				// We have to insert one between this element
				// and the next one.
				//
				PrintDebug( 10, routine,
				"Inserting %s between %s and %s.",
				(char*)tsdata->getDate(), (char*)ptr->getDate(),
				(char*)ptr->getNext()->getDate() );

				next = ptr->getNext();

				tsdata->setNext( next );
				tsdata->setPrevious( ptr );

				ptr->setNext( tsdata );
				next->setPrevious( tsdata );

				_dirty	= 1;
				_ndata++;
				found = 1;
				break;
			}
			else {
				//
				// Date is less than both this and the next one.
				//
				PrintDebug( 10, routine,
			"%s is less than %s and %s, continuing search...",
				(char*)tsdata->getDate(), (char*)ptr->getDate(),
				(char*)ptr->getNext()->getDate() );
			}
		}
	}
	else {
		for( 	i=_ndata, ptr = _ts_data_tail;
			ptr != NULL; ptr = ptr->getPrevious(), i++ ){
			if( ptr->getDate() < date && !ptr->getNext() ){
				//
				// We are adding an element to the end of the
				// list.
				//
				PrintDebug( 10, routine,
				"Inserting %s after %s.",
				(char*)tsdata->getDate(),
				(char*)ptr->getDate() );

				ptr->setNext( tsdata );
				tsdata->setPrevious( ptr );

				_date2 = tsdata->getDate();
				_ts_data_tail = tsdata;

				_dirty	= 1;
				_ndata++;
				found = 1;
				break;
			}
			else if(ptr->getDate() > date &&
				!ptr->getPrevious() ){
				//
				// We have to add an element to the beginning
				// of the list.
				//
				PrintDebug( 10, routine,
				"Inserting %s before %s.",
				(char*)tsdata->getDate(),
				(char*)ptr->getDate() );

				tsdata->setNext( ptr );
				ptr->setPrevious( tsdata );
				_ts_data_head = tsdata;

				_date1 = tsdata->getDate();

				 _dirty	= 1;
				 _ndata++;
				 found = 1;
				 break;
			}
			else if(ptr->getDate() < date &&
				ptr->getNext()->getDate() > date ){
				//
				// We have to insert one between this element
				// and the next one.
				//
				PrintDebug( 10, routine,
				"Inserting %s between %s and %s.",
				(char*)tsdata->getDate(), (char*)ptr->getDate(),
				(char*)ptr->getNext()->getDate() );

				next = ptr->getNext();

				tsdata->setNext( next );
				tsdata->setPrevious( ptr );

				ptr->setNext( tsdata );
				next->setPrevious( tsdata );

				_dirty	= 1;
				_ndata++;
				found = 1;
				break;
			}
			else {
				//
				// Date is less than both this and the next one.
				//
				PrintDebug( 10, routine,
			"%s is less than %s, continuing search...",
				(char*)tsdata->getDate(), (char*)ptr->getDate() );
			}
		}
	}

	// Construct the distribution array on the TSDistData object...
	if( tsdata->setDistribution( dist, n_dist ) ) {
		PrintWarning( 1, routine, "Troubles setting distribution." );
		return( STATUS_FAILURE );
	}

	if( !found ){
		PrintWarning( 1, routine,
		"With DistributedTS we should always insert the data!" );
		delete tsdata;
		return( STATUS_FAILURE );
	}
	return( STATUS_SUCCESS );
}
//------------------------------------------------------------------------------
// DistributedTS::setRelativeFlag
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 09 Apr 1997	Daniel Weiler, RTi	created member function
//
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

void DistributedTS::setRelativeFlag( int flag )
{
	_is_relative = flag;

	return;
}
//------------------------------------------------------------------------------
// HourTS - constructors
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
//		inc
// 13 Mar 1997	Steven A. Malers, RTi	Bump up the debug messages.
// 05 May 1998	MJR,		Put in the guts of the copy constructor.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "HourTS.h"

// HourTS constructor
//
// Notes:	(1)	Need to spend time on this
//

HourTS::HourTS ( void ) : TS()
{	char	routine[]="HourTS::HourTS";

	PrintDebug( 50, routine, "Constructing" );

	init();
}

// HourTS constructor - construct from file
//
// Notes:	(1)	Need to spend time on this
//

HourTS::HourTS ( char *input ) : TS()
{	char	routine[] = "HourTS::HourTS(char*)";

	PrintWarning( 1, routine, "Constructing from file (%s) not enabled",
	input );
	init();
}

// HourTS copy constructor
//
// Notes:	(1)	Need to spend time on this
//

HourTS::HourTS ( const HourTS& ts ) : TS( ts )
{
	char	routine[] = "HourTS::HourTS(copy)";

	init();

	// We should be able to call allocate data space and loop through
	// the dates...
	if( allocateDataSpace() ){
		PrintWarning( 1, routine,
		"Troubles allocating data space in the copy constructor." );
	}
	else {
		int		row=0, col=0;
		TSDate		t( _date1, TSDate::DATE_FAST );

		for( ; t<=_date2; t.addHour( _data_interval_mult ) ){
			if( getDataPosition( t, &row, &col, NULL ) ){
				PrintWarning( 1, routine,
				"Troubles getting data position for %s.",
				t.toString() );
				continue;
			}
			_data[row][col] = ts._data[row][col];
		}
	}
}
// ~HourTS destructor - construct from file
//
// Notes:	(1)	Need to spend time on this
// ----------------------------------------------------------------------------
// History:
//
// 06 Feb 1997	Steven A. Malers, RTi	Change deleteDataSpace to
//					freeDataSpace.
// ----------------------------------------------------------------------------

#include "HourTS.h"

HourTS::~HourTS ( void )
{
	freeDataSpace();
}

// HourTS overload =
//
// Notes:	(1)	Need to spend time on this
// 01 Apr 2004	James R. VanShaar, RTi	Modified to be more complete by
// combining functionality from espts code and the copy constructor above.
//

#include "HourTS.h"

HourTS& HourTS::operator= ( const HourTS& ts )
{
	char	routine[]="HourlyTS::operator=";
	int 	i=0, j=0;

	if( &ts	== this ){
		//
		// Self declaration.
		//
		PrintDebug( 10, routine,
		"Self Declaration in \"%s\".", routine );
		return( *this );
	}
	//
	// Invoke the base class operator=.
	//
	TS :: operator= ( ts );
	//
	// Now move on to the members of HourTS...
	//
	// Free the data space in case the data has been allocated already.
	//
	if( freeDataSpace() ){
		PrintWarning( 1, routine,
		"Troubles deleting data space." );
		return( *this );
	}

	if( ts._data ){
		if( allocateDataSpace() ){
			PrintWarning( 1, routine,
			"Troubles allocating data space." );
			return( *this );
		}

		int		row=0, col=0;
		TSDate		t( _date1, TSDate::DATE_FAST );

		for( ; t<=_date2; t.addHour( _data_interval_mult ) ){
			if( getDataPosition( t, &row, &col, NULL ) ){
				PrintWarning( 1, routine,
				"Troubles getting data position for %s.",
				t.toString() );
				continue;
			}
			_data[row][col] = ts._data[row][col];
		}
	}

	return *this;
}
//------------------------------------------------------------------------------
// HourTS.allocateDataSpace - sets up two dimensionsal array for data;
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 11 Nov 1996	Matthew J. Rutherford,	Created member function
//		RTi
// 14 Nov 1996	MJR, RTi		Added 1 to nmonths to make array proper
//					size.
// 06 Feb 1997	Steven A. Malers, RTi	Do not use the original period to
//					allocate the memory.  Also allocate
//					memory based exactly on the needed
//					amount.
// 11 Jun 1997	MJR, RTi		Fixed bug where the number of
//					days in the first month was being
//					used to allocate memory for the rest
//					of the months.
// 23 Sep 1997	SAM, RTi		Update to use new library flags.
// 09 Jan 1998	SAM, RTi		Update to agree with Java.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "HourTS.h"

int HourTS::allocateDataSpace( void )
{	char	routine[]="HourTS.allocateDataSpace";
	int	i, ndays_in_month, nmonths=0, nvals;
	TSDate	date ( TSDate::DATE_FAST );

	//TSDate date1 = _data_limits.getDate1();
	//TSDate date2 = _data_limits.getDate2();
	TSDate date1 = _date1;
	TSDate date2 = _date2;
	nmonths = date2.getAbsMonth() - date1.getAbsMonth() + 1;

	if( !nmonths ){
		PrintWarning( 1, routine,
		"TS has 0 months POR, maybe Dates haven't been set yet" );
		return( STATUS_FAILURE );
	}

	if( _data ) {
		PrintWarning( 1, routine,
		"Memory already allocated for data space.  Re-allocating." );
		// Free the old memory...
		freeDataSpace();
	}

	_data = new double* [nmonths];

	if( !_data ) {
		PrintWarning( 1, routine,
		"Trouble allocating memory for %d double*'s", nmonths );
		return( STATUS_FAILURE );
	}

	// Set the counter date to be on the first day of the first month and
	// year.  The 1st is used because every month has a day 1.
	date.setDay( 1 );
	date.setMonth( date1.getMonth() );
	date.setYear( date1.getYear() );

	for (	i = 0; i < nmonths;
		i++, date.addInterval( TS::INTERVAL_MONTH, 1 ) ){
		ndays_in_month =	NumDaysInMonth ( date.getMonth(),
					date.getYear() );
		if ( _data_interval_mult <= 24 ) {
			// Easy to handle 1-24 hour data...
			nvals = ndays_in_month*(24/_data_interval_mult);
		}
		else {	// Do not know how to handle > 24-hour data...
			PrintWarning ( 1, routine,
			"Only know how to handle 1-24 hour data, not %d-hour",
			_data_interval_mult );
		}
		_data[i] = new double [ nvals ];

		if( !_data[i] ) {
			PrintWarning( 1, routine,
			"Unable to allocate memory for %d doubles at row %d",
			nvals, i );
			return( STATUS_FAILURE );
		}

		// Now fill the entire month with the missing data value...

		for ( int j = 0; j < nvals; j++ ) {
			_data[i][j] = _missing;
		}
	}

	PrintDebug( 10, routine, "Successfully allocated %d months of memory",
	nmonths );

	return 0;
}
//------------------------------------------------------------------------------
// HourTS::formatOutput - format for simple output
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	These output formats apply to all hourly time series.
//------------------------------------------------------------------------------
// History:
//
// 30 Sep 1997	Steven A. Malers, RTi	First cut based on legacy TSPrint*
//					functions.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "HourTS.h"

char ** HourTS::formatOutput( int format, unsigned long int flags )
{	char  	routine[] = "HourTS::formatOutput(int,long)";

	// Need to pull in TSPrint* code here...

	if ( format == TS::FORMAT_SPREADSHEET ) {
		// Spreadsheet
	}
	else if ( format == TS::FORMAT_MATRIX ) {
	}

	// Put in so that we do not get compiler warnings...

	if ( format ) {
		; // nothing
	}
	if ( flags ) {
		; // nothing
	}

	PrintWarning( 1, routine, "Routine is not enabled" );

	// Would normally return a string list of formatted output...
	return (char **)NULL;
}

int HourTS::formatOutput ( ostream& fp, int format, unsigned long int flags )
{	char	**formatted_output,
		routine[] = "HourTS::formatOutput(ostream&,int,long)";

	// First get the formatted output...

	formatted_output = formatOutput ( format, flags );

	// Put in so that we do not get compiler warnings...

	if ( fp ) {
		; // nothing
	}

	PrintWarning( 1, routine, "Routine is not enabled" );
	return STATUS_FAILURE;
}

int HourTS::formatOutput ( char *fname, int format, unsigned long int flags )
{	char		routine[] = "HourTS::formatOutput(char*,int,long)";
	ofstream	stream( fname );
	int		status;

	// First open the output file...

	if( !stream ){
		PrintWarning( 1, routine,
		"Unable to open file \"%s\"", fname );
		return( STATUS_FAILURE );
	}

	status = formatOutput ( stream, format, flags );

	stream.close();

	PrintWarning( 1, routine, "Routine is not enabled" );
	return status;
}
//------------------------------------------------------------------------------
// HourTS::freeDataSpace - free the two-dimensionsal array for data
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 11 Nov 1996	Matthew J. Rutherford,	Created member function
//		RTi
// 14 Nov 1996	MJR, RTi		Added 1 to nmonths to make array proper
//					size.
// 06 Feb 1997	SAM, RTi		Change the name of the routine from
//					delete... to free...
// 22 APr 1997	MJR, RTi		Added a check for _data being allocated.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//------------------------------------------------------------------------------

#include "HourTS.h"

int HourTS::freeDataSpace( void )
{
	char	routine[]="HourTS::freeDataSpace";
	int	i, nmonths=0, nvals;

	if( !_data ){
		PrintStatus( 10, routine,
	"Data Array hasn't been allocated yet, no need to freeDataSpace." );
		return( STATUS_SUCCESS );
	}

	nmonths = 	_date2.getAbsMonth() - _date1.getAbsMonth() + 1;

	for( i=0; i< nmonths; i++ ){
		if( _data[i] ){
			delete [] _data[i];
		}
	}

	delete [] _data;

	_data = (double**)NULL;

	return( STATUS_SUCCESS );
}
//------------------------------------------------------------------------------
// HourTS.getDataPoint - returns a data point for a date.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)Hour data is stored in a two-dimensional array:
//		     |----------------> invervals
//		     |
//		    \|/
//		   month
//
//------------------------------------------------------------------------------
// History:
//
// 06 Jan 1998	Steven J. Malers,	Created member function.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variable	I/O	Description
//
// column	L	column (y) index of data.
// date		I	date to retrieve data for.
// routine	L	name of routine.
// row		L	row (x) index of data.
// t		L	date used for counter.
//------------------------------------------------------------------------------

#include "HourTS.h"
#include "TSData.h"

TSData HourTS::getDataPoint ( TSDate &date )
{	char	routine[] = "HourTS.getDataPoint";
	int	column=0, row=0;

	// Initialize data to most of what we need...

	TSData data_point;
	data_point.setDate ( date );
	data_point.setUnits ( _data_units );
	data_point.setDataFlag ( "" );	// Until we know better.

	//Check the date coming in

	if(	(date < _date1) || (date > _date2) ) {

		PrintWarning( 1, routine,
		"%d/%d/%d 1245 not within POR (%d/%d/%d - %d/%d/%d)",
		date.getMonth(), date.getDay(), date.getYear(),
		_date1.getMonth(), _date1.getDay(), _date1.getYear(),
		_date2.getMonth(), _date2.getDay(), _date2.getYear() );
		data_point.setData ( _missing );
		return data_point;
	}

	if ( getDataPosition(date, &row, &column, NULL) ) {
		PrintWarning( 1, routine,
		"%d/%d/%d:%d Unable to get data position",
		date.getMonth(), date.getDay(), date.getYear(),
		date.getHour() );
		data_point.setData ( _missing );
		return data_point;
	}

	PrintDebug( 30, routine,
	"%f for %d/%d/%d from _data[%d][%d]", _data[row][column],
	date.getMonth(), date.getDay(), date.getYear(), row, column );

	// Initialize a TSData and return it.

	data_point.setData ( _data[row][column] );
	return data_point;
}
//------------------------------------------------------------------------------
// HourTS::getDataPointer - returns pointer to a data value for a date
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)Hour data is stored in a two-dimensional array:
//		     |----------------> invervals
//		     |
//		    \|/
//		   month
//		(2)The data space is always allocated based on the original
//		dates, so they are used to find rows and columns
//
//------------------------------------------------------------------------------
// History:
//
// 08 Nov 1996	Matthew J. Rutherford,	Created member function.
//		Riverside Technology,
//		inc.
// 06 Feb 1997	Steven A. Malers, RTi	Update to call getDataPosition.
// 06 Jun 1997	SAM, RTi		Update to have 3rd argument to
//					getDataPosition (unused here).
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
// column	L	column (y) index of data.
// date		I	date to retrieve data for.
// routine	L	name of routine.
// row		L	row (x) index of data.
// t		L	date used for counter.
//------------------------------------------------------------------------------

#include "HourTS.h"

double *HourTS::getDataPointer( TSDate &date )
{	char	routine[] = "HourTS.getDataPointer";
	int	column=0, row=0;

	//Check the date coming in

	if(	(date < _date1) || (date > _date2) ) {
		PrintWarning( 1, routine,
		"%d/%d/%d 1313 not within POR (%d/%d/%d - %d/%d/%d)",
		date.getMonth(), date.getDay(), date.getYear(),
		_date1.getMonth(), _date1.getDay(), _date1.getYear(),
		_date2.getMonth(), _date2.getDay(), _date2.getYear() );

		return( (double*)NULL );
	}

	if ( getDataPosition(date, &row, &column, NULL) ) {
		PrintWarning( 1, routine,
		"%d/%d/%d:%d Unable to get data position",
		date.getMonth(), date.getDay(), date.getYear(),
		date.getHour() );
		return (double *)NULL;
	}

	PrintDebug( 10, routine,
	"Returning data for %d/%d/%d from _data[%d][%d]",
	date.getMonth(), date.getDay(), date.getYear(), row, column );

	return( &(_data[row][column]) );
}
//------------------------------------------------------------------------------
// HourTS.getDataPosition - returns the position of a data value for a date.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)Hour data is stored in a two-dimensional array:
//		     |----------------> invervals
//		     |
//		    \|/
//		   month
//
//------------------------------------------------------------------------------
// History:
//
// 05 Feb 1997	Steven A. Malers, RTi	Created member function.
// 13 Mar 1997	SAM, RTi		Put in check to make sure time zones
//					line up but do not add shift yet.
// 06 Jun 1997	SAM, RTi		Add third dimension to support minute
//					data.  Unused here.
//------------------------------------------------------------------------------
// Variable	I/O	Description
//
// column	L	Column (y) index of data.
// date		I	Date to retrieve data for.
// routine	L	Name of routine.
// row		L	Row (x) index of data.
// t		L	Date used for counter.
// tz, tz1	L	Time zones for requested date and time series.
// tzshift	L	Time zone shift - do not know how this will be handled
//			yet.
// unused	L	Unused dimension.
//------------------------------------------------------------------------------

#include "HourTS.h"

int HourTS::getDataPosition ( TSDate &date, int *row, int *column,
				int *unused )
{	char	routine[] = "HourTS.getDataPosition", tz[8], tz1[8];
	int	tzshift;

	// This is where we would calculate the shift between the requested
	// time zone and the time zone that we have stored.  For now, just check
	// the time zones against each other and print a warning if not
	// compatible.  When there is time, use the  routines to calculate the
	// shift.

	strcpy ( tz, date.getTimeZoneAbbr() );
	strcpy ( tz1, _date1.getTimeZoneAbbr() );
	if ( !tz[0] || !strcmp(tz,tz1) ) {
		// We are OK doing a straight retrieval
		tzshift = 0;
	}
	else {	PrintWarning ( 10, routine,
		"Do not know how to shift time zones yet (\"%s\" to \"%s\"",
		tz1, tz );
		tzshift = 0;
	}

	// Check the date coming in

	*row	= 0;
	*column	= 0;
	if(	(date < _date1) || (date > _date2) ) {

		PrintWarning( 1, routine,
		"%d/%d/%d 1400 not within POR (%d/%d/%d - %d/%d/%d)",
		date.getMonth(), date.getDay(), date.getYear(),
		getDate1().getMonth(), getDate1().getDay(),
		getDate1().getYear(), getDate2().getMonth(),
		getDate2().getDay(), getDate2().getYear() );
		return 1;
	}

	//calculate the row position of the data

	*row = date.getAbsMonth() - _date1.getAbsMonth();

        // Calculate the column position of the data. We know that Hourly data
        // is stored in a 2 dimensional array with the column being the hourly
	// data by interval.

        *column = ((date.getDay()-1)*24 + date.getHour())/_data_interval_mult;

	return 0;
}
//------------------------------------------------------------------------------
// HourTS::getDataValue - returns a data value for a date.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)Hour data is stored in a two-dimensional array:
//		     |----------------> invervals
//		     |
//		    \|/
//		   month
//
//------------------------------------------------------------------------------
// History:
//
// 08 Nov 1996	Matthew J. Rutherford,	Created member function.
//		Riverside Technology,
//		inc.
// 31 Jan 1997	MJR, RTI		Changed logic based on
//					TSGetHourlyDataPosition.
// 05 Feb 1997	Steven A. Malers, RTi	Update to use protected data members
//					in TS.
// 06 Jun 1997	SAM, RTi		Update to have 3rd argument to
//					getDataPosition (unused here).
// 09 Jan 1998	SAM, RTi		Do not hard-code -999 for missing data.
//------------------------------------------------------------------------------
// Variable	I/O	Description
//
// column	L	column (y) index of data.
// date		I	date to retrieve data for.
// routine	L	name of routine.
// row		L	row (x) index of data.
// t		L	date used for counter.
//------------------------------------------------------------------------------

#include "HourTS.h"

double HourTS::getDataValue( TSDate &date )
{	char	routine[] = "HourTS.getDataValue";
	int	column=0, row=0;

	//Check the date coming in

	if(	(date < _date1) || (date > _date2) ) {

		PrintWarning( 1, routine,
		"%d/%d/%d 1465 not within POR (%d/%d/%d - %d/%d/%d)",
		date.getMonth(), date.getDay(), date.getYear(),
		_date1.getMonth(), _date1.getDay(), _date1.getYear(),
		_date2.getMonth(), _date2.getDay(), _date2.getYear() );
		return( _missing );
	}

	if ( getDataPosition(date, &row, &column, NULL) ) {
		PrintWarning( 1, routine,
		"%d/%d/%d:%d Unable to get data position",
		date.getMonth(), date.getDay(), date.getYear(),
		date.getHour() );
		return _missing;
	}

	PrintDebug( 30, routine,
	"%f for %d/%d/%d from _data[%d][%d]", _data[row][column],
	date.getMonth(), date.getDay(), date.getYear(), row, column );

	return( _data[row][column] );
}
//------------------------------------------------------------------------------
// HourTS::init
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 15 Nov 1996	Matthew J. Rutherford,	Created member function.
//		RTi
// 30 Sep 1997	Steven A. Malers, RTi	Initialize the interval.
// 06 May 1998	MJR	Got rid of the initialization of the mult interval.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "HourTS.h"

int HourTS::init( void )
{
	_data 				= (double**)NULL;
	_data_interval_base		= TS::INTERVAL_HOUR;
	_data_interval_base_original	= TS::INTERVAL_HOUR;

	return( STATUS_SUCCESS );
}
//------------------------------------------------------------------------------
// HourTS::printSample
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)This is a virtual function at this level and should be
//		redefined.
//
//------------------------------------------------------------------------------
// History:
//
// 12 Nov 1996	Matthew J. Rutherford, RTi	Created function
//
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "HourTS.h"

int HourTS::printSample( ostream& out )
{
	char	routine[]="HourTS::printSample";

	PrintWarning( 1, routine,
	"This is a virtual function, should be defined lower down" );

	return( STATUS_SUCCESS );
}
//------------------------------------------------------------------------------
// HourTS::readPersistent
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)This is a virtual function at this level and should be
//		redefined.
//
//------------------------------------------------------------------------------
// History:
//
// 12 Nov 1996	Matthew J. Rutherford, RTi	Created function
//
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "HourTS.h"

int HourTS::readPersistent( istream& in )
{
	char	routine[]="HourTS::readPersistent(istream&)";

	PrintWarning( 1, routine,
	"This is a virtual function, should be defined lower down" );

	return( STATUS_SUCCESS );
}

int HourTS::readPersistent( char *fname )
{
	char	routine[]="HourTS::readPersistent(char*)";

	PrintWarning( 1, routine,
	"This is a virtual function, should be defined lower down" );

	return( STATUS_SUCCESS );
}
//------------------------------------------------------------------------------
// HourTS::readPersistentHeader
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)This is a virtual function at this level and should be
//		redefined.
//
//------------------------------------------------------------------------------
// History:
//
// 13 Nov 1996	Matthew J. Rutherford, RTi	Created function
//
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "HourTS.h"

int HourTS::readPersistentHeader( istream& in )
{
	char	routine[]="HourTS::readPersistentHeader(istream&)";

	PrintWarning( 1, routine,
	"This is a virtual function, should be defined lower down" );

	return( STATUS_SUCCESS );
}

int HourTS::readPersistentHeader( char *fname )
{
	char	routine[]="HourTS::readPersistentHeader(char*)";

	PrintWarning( 1, routine,
	"This is a virtual function, should be defined lower down" );

	return( STATUS_SUCCESS );
}
//------------------------------------------------------------------------------
// HourTS.refresh - resets the max/min values for a time series
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine should be used to reset time series data
//			because some data values have been reset.
//------------------------------------------------------------------------------
// History:
//
// 11 Nov 1996	Matthew J. Rutherford,	Created function.
//		Riverside Technology,
//		inc.
// 05 Feb 1997	Steven A. Malers, RTi	Update to check the "dirty" flag.
// 27 May 1997	SAM, RTi		Update to set the non-missing data
//					dates.
// 16 Jun 1997	MJR, RTi		Added overload.
// 23 Sep 1997	SAM, RTi		Use new library flags.
// 06 Jan 1998	SAM, RTi		Move calcMaxMinValues to refresh.  Also
//					split out some code as getDataLimits.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "HourTS.h"
#include "TSLimits.h"
#include "TSUtil.h"

int HourTS::refresh( void )
{	char		routine[]="HourTS.refresh";
	TSLimits	limits;

	// If the data is not dirty, then we do not have to recompute the
	// limits...

	if ( !_dirty ) {
		PrintDebug ( 10, routine,
		"Time series is not dirty.  Not recomputing limits" );
		return 0;
	}

	// Else we need to refresh...

	PrintDebug( 10, routine, "Time Series is dirty. Recomputing limits" );

	limits = TSUtil::getDataLimits ( this, _date1, _date2 );
	if ( limits.areLimitsFound() ) {
		// Now reset the limits for the time series...
		setDataLimits ( limits );
	}

	_dirty = 0;

	return 0;
}
//------------------------------------------------------------------------------
// HourTS.setDataValue - sets a data value for a date.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)The memory is allocated based on the original dates
//		so these are used for lookup
//
//------------------------------------------------------------------------------
// History:
//
// 12 Nov 1996	Matthew J. Rutherford,	First version.
//		Riverside Technology,
//		inc.
// 05 Feb 1997	Steven A. Malers, RTi	Use protected TS data and set _dirty.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "HourTS.h"

int HourTS::setDataValue( TSDate &date, double value )
{	char	routine[]="HourTS.setDataValue";
	int	column, row;

	if( 	(date < _date1) || (date > _date2) ) {
		PrintWarning( 1, routine,
		"Date %d/%d/%d %02d:%02d:%02d.%02d is outside bounds",
		date.getMonth(), date.getDay(), date.getYear(), date.getHour(),
		date.getMinute(), date.getSecond(), date.getHSecond() );

		return( STATUS_FAILURE );
	}

	row	= date.getAbsMonth() - _date1.getAbsMonth();
	column	= ((date.getDay() - 1)*24 + date.getHour())/_data_interval_mult;

	PrintDebug( 30, routine,
	"Setting %f for %d/%d/%d %02d at %d,%d", value,
	date.getMonth(), date.getDay(), date.getYear(), date.getHour(),
	row, column );

	// Set the dirty flag so that we know to recompute the limits if
	// desired...

	_dirty = 1;

	_data[row][column] = value;

	return 0;
}
//------------------------------------------------------------------------------
// writePersistent - write an NWS DATACARD format file
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This is used to write a time series to a DATACARD
//			format file.
//
//------------------------------------------------------------------------------
// History:
//
// 08 Nov 1996	Matthew J. Rutherford, Riverside Technology, inc.
//					Broke out of NWSHourTS.cc
//
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "HourTS.h"

int HourTS::writePersistent ( ostream& out )
{
	char	routine[]="HourTS::writePersistent(ostream&)";

	PrintWarning( 1, routine,
	"This function is virtual, redefine in lower classes" );

	return( STATUS_SUCCESS );
}

int HourTS::writePersistent ( char *fname )
{
	char	routine[]="HourTS::writePersistent(char*)";

	PrintWarning( 1, routine,
	"This function is virtual, redefine in lower classes" );

	return( STATUS_SUCCESS );
}
//------------------------------------------------------------------------------
// IrregularTS - constructors
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 18 Sep 1997	Matthew J. Rutherford, RTi	Created initial versions.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "IrregularTS.h"

// IrregularTS constructor
//
// Notes:	(1)	Need to spend time on this
//

IrregularTS::IrregularTS ( void )
{	char	routine[]="IrregularTS::IrregularTS";

	PrintDebug( 50, routine, "Constructing" );

	init();
}

// IrregularTS constructor - construct from file
//
// Notes:	(1)	Need to spend time on this
//

IrregularTS::IrregularTS ( char *input )
{	char	routine[] = "IrregularTS::IrregularTS(char*)";

	PrintWarning( 1, routine, "Constructing from file (%s) not enabled",
	input );
	init();
}

// IrregularTS copy constructor
//
// Notes:	(1)	Need to spend time on this
//

IrregularTS::IrregularTS ( const IrregularTS& ts ) : TS( ts )
{
	char	routine[] = "IrregularTS::IrregularTS(copy)";
	TSData	*tsd=NULL;
	TSDate  temp;

	init();

	// Loop through the incoming data and add each one.
	for( tsd=ts._ts_data_head; tsd!=NULL; tsd=tsd->getNext() ){
        	temp = tsd->getDate();
		setDataValue( temp, tsd->getData() );
	}
}
// ----------------------------------------------------------------------------
// ~IrregularTS destructor - construct from file
//
// Notes:	(1)	Need to spend time on this
// ----------------------------------------------------------------------------
// History:
//
// 18 Sep 1997	Matthew J. Rutherford, RTi	Created initial version.
// ----------------------------------------------------------------------------

#include "IrregularTS.h"

IrregularTS::~IrregularTS ( void )
{
	freeDataSpace();
}

// IrregularTS overload =
//
// Notes:	(1)	Need to spend time on this
//

#include "IrregularTS.h"

IrregularTS& IrregularTS::operator= ( const IrregularTS& ts )
{
	return *this;
}
//------------------------------------------------------------------------------
// IrregularTS::allocateDataSpace - sets up two dimensionsal array for data;
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 18 Sep 1997	Matthew J. Rutherford, RTi	Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "IrregularTS.h"

int IrregularTS::allocateDataSpace( void )
{
	char	routine[]="IrregularTS::allocateDataSpace";

	PrintWarning( 2, routine,
	"Memory allocation is dynamic for IrregularTS." );

	return( STATUS_SUCCESS );
}
//------------------------------------------------------------------------------
// IrregularTS.enforceDataGaps - insert missing data to enforce data gaps
//------------------------------------------------------------------------------
// Notes:	(1)	This routine should be called where needed to force
//			insertion of missing data records.  This may be
//			necessary, for example, when we want to plot the time
//			series and want to enforce gaps in the data to cause
//			line breaks.
//------------------------------------------------------------------------------
// History:
//
// ?		Daniel K. Weiler, RTi	First version.
// 09 Jan 1998	Steven A. Malers, RTi	Update for port to C++.
//------------------------------------------------------------------------------

#include "IrregularTS.h"
#include "TSDateIterator.h"
#include "TSUtil.h"

int IrregularTS::enforceDataGaps (	int interval_base, int interval_mult,
					TSDate date1, TSDate date2 )
{	char	routine[] = "IrregularTS.enforceDataGaps";

	// Loop through the data using the given dates.  If the dates are not
	// specified, use the time series start and end dates.  Do this by
	// filling out the TS.getValidPeriod function.

	int		dl = 20;
	double		data_value = 0.0;
	TSLimits	dates = TSUtil::getValidPeriod ( this, date1, date2 );
	TSDate		start(TSDate::DATE_STRICT);
	TSDate		end (TSDate::DATE_STRICT);
	TSDate 		t (TSDate::DATE_STRICT),
			prevDate (TSDate::DATE_STRICT);

	start = dates.getDate1 ();
	start = dates.getDate2 ();

	// Now loop through the period.  If between any known data values the
	// time gap is longer than the interval indicated above, insert ONE
	// missing data value in the gap at some date in the middle of the
	// gap.

	prevDate = start;
	TSDateIterator tsdi ( this, start, end );
	int missingcount = 0, datacount = 0;
	for (	;
		!tsdi.isIterationComplete();
		tsdi.advanceDate(), ++datacount ){

		t = tsdi.getCurrentDate();
		data_value = getDataValue(t);

		PrintDebug ( 2, routine,
		"Processing date %s in period %s to %s",
		(char *)t, (char *)start, (char *)end );

		if( t.notEquals(start) ) {
			prevDate.addInterval( interval_base, interval_mult );
			if( prevDate.lessThan(t) && !isDataMissing(data_value)){
				// We have a gap that is not bounded by missing
				// data already so insert a missing data
				// point...
				setDataValue( prevDate, _missing );
				++missingcount;
				PrintDebug(5, routine,
				" Set MISSING value at: %s", (char *)prevDate);
			}
			prevDate = t;
		}

	}

	PrintDebug ( dl, routine,
	"Set %d missing values after checking %d values",
	datacount, missingcount );
	return 0;
}
//------------------------------------------------------------------------------
// IrregularTS::formatOutput - format for simple output
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	These output formats apply to all irregular time series.
//------------------------------------------------------------------------------
// History:
//
// 30 Sep 1997	Matthew J. Rutherford, RTi	First cut based on legacy TSPrint*
//					functions.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "IrregularTS.h"

char ** IrregularTS::formatOutput( int format, unsigned long int flags )
{	char  	routine[] = "IrregularTS::formatOutput(int,long)";

	// Need to pull in TSPrint* code here...

	if ( format == TS::FORMAT_SPREADSHEET ) {
		// Spreadsheet
	}
	else if ( format == TS::FORMAT_MATRIX ) {
	}

	// Put in so that we do not get compiler warnings...

	if ( format ) {
		; // nothing
	}
	if ( flags ) {
		; // nothing
	}

	PrintWarning( 1, routine, "Routine is not enabled" );

	// Would normally return a string list of formatted output...
	return (char **)NULL;
}

int IrregularTS::formatOutput ( ostream& fp, int format, unsigned long int flags )
{	char	**formatted_output,
		routine[] = "IrregularTS::formatOutput(ostream&,int,long)";

	// First get the formatted output...

	formatted_output = formatOutput ( format, flags );

	// Put in so that we do not get compiler warnings...

	if ( fp ) {
		; // nothing
	}

	PrintWarning( 1, routine, "Routine is not enabled" );
	return STATUS_FAILURE;
}

int IrregularTS::formatOutput ( char *fname, int format, unsigned long int flags )
{	char		routine[] = "IrregularTS::formatOutput(char*,int,long)";
	ofstream	stream( fname );
	int		status;

	// First open the output file...

	if( !stream ){
		PrintWarning( 1, routine,
		"Unable to open file \"%s\"", fname );
		return( STATUS_FAILURE );
	}

	status = formatOutput ( stream, format, flags );

	stream.close();

	PrintWarning( 1, routine, "Routine is not enabled" );
	return status;
}
//------------------------------------------------------------------------------
// IrregularTS::freeDataSpace - free the two-dimensionsal array for data
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 18 Sep 1997	Matthew J. Rutherford, RTi	Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//------------------------------------------------------------------------------

#include "IrregularTS.h"

int IrregularTS::freeDataSpace( void )
{
	char	routine[]="IrregularTS::freeDataSpace";
	TSData	*ptr=NULL, *tmp=NULL;

	if( !_ts_data_head ){
		//PrintWarning( 1, routine, "Data Array is not allocated, no "
		PrintWarning( 2, routine, "Data Array is not allocated, no "
			"need to freeDataSpace." );
		return( STATUS_SUCCESS );
	}

	ptr = _ts_data_head;
	while( ptr ){
		tmp = ptr;

		ptr = ptr->getNext();

		delete tmp;
	}

	_ts_data_head = NULL;

	return( STATUS_SUCCESS );
}
//------------------------------------------------------------------------------
// IrregularTS.getData - returns data array
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This is a dummy routine to match the Java.  This routine
//			should not be used in C++.  Use the TSDateIterator to
//			iterate.
//------------------------------------------------------------------------------
// History:
//
// 22 Jan 1998	Steven A. Malers, RTi	Created initial version of function.
//------------------------------------------------------------------------------
// Variable	I/O	Description
//
//------------------------------------------------------------------------------

#include "IrregularTS.h"

TSData* IrregularTS::getData( void )
{	char	routine[] = "IrregularTS.getData";
	return _ts_data_head;
}
//------------------------------------------------------------------------------
// IrregularTS::getDataDate - get the date relative to a starting date
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This function finds the next/previous date for a
//			irregular interval time series.  This can be somewhat
//			inefficient if get a long string of data but many
//			irregular time series do not have a lot of data and
//			the performance hit will not be noticeable.
//------------------------------------------------------------------------------
// History:
//
// 30 Sep 1997	Steven A. Malers,	First version.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "IrregularTS.h"

TSDate IrregularTS::getDataDate ( TSDate &date0, int increment )
{	char	routine[] = "IrregularTS::getDataDate";
	int	found=0;
	TSDate	t;
	TSData	*ptr=NULL;

	if( increment != 1 ){
		PrintWarning( 1, routine,
		"Support for non-one increment %d is not yet included",
		increment );
	}

	// For now, allow the date to go past the end of the time series...

	// Matt - put in the intelligence to find the starting date and then
	// position relative to that.  If the starting date is not an exact
	// match, find the nearest one.  For CRDSS the starting date will
	// normally start off with the first value stored in the time series.

	found = 0;
	for( ptr = _ts_data_head; ptr != NULL; ptr = ptr->getNext() ){
		if( ptr->getDate() == date0 ){
			if( ptr->getNext() == NULL ){
				// We have been given the last date and
				// need to increment somehow.
				PrintWarning( 2, routine,
		"Given the last date (%s), will return a date one hour later.",
				(char*)date0 );
				t = date0;
				t.addHour( 1 );
			}
			else {
				t = ptr->getNext()->getDate();
				PrintDebug( 10, routine,
				"\"%s\" is next date after \"%s\".",
				(char*)t, (char*)date0 );
			}
			found = 1;
			break;
		}

		if( 	ptr->getDate() < date0 &&
			ptr->getNext() != NULL &&
			ptr->getNext()->getDate() > date0 ){
			// We have a date that isn't explicitly included,
			// use the nearest.
			PrintDebug( 10, routine,
"Date (%s) is between this date (%s) and the next date (%s), using closest",
			(char*)date0, (char*)ptr->getDate(),
			(char*)ptr->getNext()->getDate() );

			double date0_double = date0.toDouble();
			double date_double  = ptr->getDate().toDouble();
			double next_double  =
				ptr->getNext()->getDate().toDouble();

			if( 	fabs( date0_double - date_double ) <=
				fabs( date0_double - next_double ) ){
				PrintDebug( 10, routine,
				"%s is closer to %s than %s",
				(char*)date0, (char*)ptr->getDate(),
				(char*)ptr->getNext()->getDate() );

				t = ptr->getNext()->getDate();
			}
			else {
				if( ptr->getNext()->getNext() != NULL ){
					PrintDebug( 10, routine,
					"%s is closer to %s than %s",
					(char*)date0,
					(char*)ptr->getNext()->getDate(),
					(char*)ptr->getDate() );
					t =
					ptr->getNext()->getNext()->getDate();
				}
				else {
					PrintDebug( 10, routine,
"%s is closer to %s than %s but there is a NULL, incrementing %s by 1 hour.",
					(char*)date0,
					(char*)ptr->getNext()->getDate(),
					(char*)ptr->getDate(),
//cfan					ptr->getNext()->getDate() );
					(char*)ptr->getNext()->getDate() );  //cfan

					t = ptr->getNext()->getDate();
					t.addHour( 1 );
				}
			}
			found = 1;
			break;
		}
	}

	if( found == 0 ) {
		// Return one our past the last date...
		// Need to get this in!
		// t = date;
		t.addHour( 1 );
		PrintDebug( 10, routine,
		"Can't find the date after %s, returning the last date + 1 hour: %s.",
		(char*)date0, (char*)t );
	}

	return t;
}
//------------------------------------------------------------------------------
// IrregularTS.getDataPoint - returns a data point for a date.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//------------------------------------------------------------------------------
// History:
//
// 06 Jan 1998	Steven A. Malers, RTi	Created initial version of function.
//------------------------------------------------------------------------------
// Variable	I/O	Description
//
// date		I	Date to retrieve data for.
// data_point	L	Data point to return.
// ptr		L	Pointer to data point.
// routine	L	Name of routine.
// t		L	Date used for counter.
//------------------------------------------------------------------------------

#include "IrregularTS.h"
#include "TSData.h"

TSData IrregularTS::getDataPoint( TSDate &date )
{	char	routine[] = "IrregularTS::getDataPoint";
	int	i = 0;
	TSData	*ptr=NULL;

	TSData data_point;
	data_point.setDate ( date );
	data_point.setUnits ( _data_units );
	data_point.setDataFlag ( "" );	// Until we know better.

	// Check the date coming in

	if(	(date < _date1) || (date > _date2) ) {

		PrintWarning( 1, routine,
		"%d/%d/%d not within POR (%d/%d/%d - %d/%d/%d)",
		date.getMonth(), date.getDay(), date.getYear(),
		_date1.getMonth(), _date1.getDay(), _date1.getYear(),
		_date2.getMonth(), _date2.getDay(), _date2.getYear() );
		data_point.setData ( _missing );
		return data_point;
	}

	//
	// We need to determine if the date is closer to the
	// tail of the list, or the head.
	//
	double	date_double 	= date.toDouble();
	double	date1_double 	= _date1.toDouble();
	double	date2_double	= _date2.toDouble();

	if( 	fabs( date_double - date1_double ) <
		fabs( date_double - date2_double ) ){
		for( 	i=0, ptr=_ts_data_head;
			ptr != NULL; ptr = ptr->getNext(), i++ ){
			if( ptr->getDate() == date ){
				break;
			}
		}
	}
	else {	for(	i=_ndata, ptr=_ts_data_tail;
			ptr != NULL; ptr = ptr->getPrevious(), i-- ){
			if( ptr->getDate() == date ){
				break;
			}
		}
	}
	PrintDebug( 30, routine,
	"%f for %d/%d/%d from _data (%d)", ptr->getData(),
	date.getMonth(), date.getDay(), date.getYear(), i );

	// Return the data point...

	data_point.setData ( ptr->getData() );
	data_point.setDataFlag ( ptr->getDataFlag() );

	return data_point;
}
//------------------------------------------------------------------------------
// IrregularTS::getDataPointer - returns pointer to a data value for a date
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//------------------------------------------------------------------------------
// History:
//
// 18 Sep 1997	Matthew J. Rutherford, RTi	Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
// date		I	date to retrieve data for.
// routine	L	name of routine.
// row		L	row (x) index of data.
// t		L	date used for counter.
//------------------------------------------------------------------------------

#include "IrregularTS.h"

double *IrregularTS::getDataPointer( TSDate &date )
{	char	routine[] = "IrregularTS::getDataPointer";

	PrintWarning( 1, routine,
	"The \"%s\" routine is not implemented.", routine );
	return( NULL );
}
//------------------------------------------------------------------------------
// IrregularTS::getDataPosition - virtual place-holder.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//------------------------------------------------------------------------------
// History:
//
// 18 Sep 1997	Matthew J. Rutherford, RTi	Created initial version of
//						function.
//------------------------------------------------------------------------------
// Variable	I/O	Description
//
// column	L	Column (y) index of data.
// date		I	Date to retrieve data for.
// routine	L	Name of routine.
// row		L	Row (x) index of data.
// t		L	Date used for counter.
// tz, tz1	L	Time zones for requested date and time series.
// tzshift	L	Time zone shift - do not know how this will be handled
//			yet.
// unused	L	Unused dimension.
//------------------------------------------------------------------------------

#include "IrregularTS.h"

int IrregularTS::getDataPosition ( TSDate &date, int *unused0, int *unused1,
				int *unused2 )
{	char	routine[] = "IrregularTS::getDataPosition";

	PrintWarning( 2, routine,
	"\"%s\" is not used with IrregularTS.", routine );

	return( STATUS_SUCCESS );
}
//------------------------------------------------------------------------------
// IrregularTS::getDataValue - returns a data value for a date.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//------------------------------------------------------------------------------
// History:
//
// 18 Sep 1997	Matthew J. Rutherford, RTi	Created initial version of
//						function.
//------------------------------------------------------------------------------
// Variable	I/O	Description
//
// column	L	column (y) index of data.
// date		I	date to retrieve data for.
// routine	L	name of routine.
// row		L	row (x) index of data.
// t		L	date used for counter.
//------------------------------------------------------------------------------

#include "IrregularTS.h"

double IrregularTS::getDataValue( TSDate &date )
{
	char	routine[] = "IrregularTS::getDataValue";
	int	i=0;
	TSData	*ptr=NULL;

	//Check the date coming in

	if(	(date < _date1) || (date > _date2) ) {

		PrintWarning( 1, routine,
		"%d/%d/%d not within POR (%d/%d/%d - %d/%d/%d)",
		date.getMonth(), date.getDay(), date.getYear(),
		_date1.getMonth(), _date1.getDay(), _date1.getYear(),
		_date2.getMonth(), _date2.getDay(), _date2.getYear() );
		return( -999.0 );
	}

	//
	// We need to determine if the date is closer to the
	// tail of the list, or the head.
	//
	double	date_double 	= date.toDouble();
	double	date1_double 	= _date1.toDouble();
	double	date2_double	= _date2.toDouble();

	if( 	fabs( date_double - date1_double ) <
		fabs( date_double - date2_double ) ){
		for( 	i=0, ptr=_ts_data_head;
			ptr != NULL; ptr = ptr->getNext(), i++ ){
			if( ptr->getDate() == date ){
				break;
			}
		}
	}
	else {
		for(	i=_ndata, ptr=_ts_data_tail;
			ptr != NULL; ptr = ptr->getPrevious(), i-- ){
			if( ptr->getDate() == date ){
				break;
			}
		}
	}
	PrintDebug( 30, routine,
	"%f for %d/%d/%d from _data (%d)", ptr->getData(),
	date.getMonth(), date.getDay(), date.getYear(), i );

	return( ptr->getData() );
}
//------------------------------------------------------------------------------
// IrregularTS::init
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 18 Sep 1997	Matthew J. Rutherford, RTi	created member function
//
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "IrregularTS.h"

int IrregularTS::init( void )
{
        _data_interval_base             = TS::INTERVAL_IRREGULAR;
        _data_interval_mult             = 1;
        _data_interval_base_original    = TS::INTERVAL_IRREGULAR;
        _data_interval_mult_original    = 1;


	_ndata				= 0;
	_ts_data_head 			= NULL;
	_ts_data_tail 			= NULL;

	return( STATUS_SUCCESS );
}
//------------------------------------------------------------------------------
// IrregularTS::printSample
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)This is a virtual function at this level and should be
//		redefined.
//
//------------------------------------------------------------------------------
// History:
//
// 18 Sep 1997	Matthew J. Rutherford, RTi	Created function
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "IrregularTS.h"

int IrregularTS::printSample( ostream& out )
{
	char	routine[]="IrregularTS::printSample";

	PrintWarning( 1, routine,
	"This is a virtual function, should be defined lower down" );

	return( STATUS_SUCCESS );
}
//------------------------------------------------------------------------------
// IrregularTS::readPersistent
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)This is a virtual function at this level and should be
//		redefined.
//
//------------------------------------------------------------------------------
// History:
//
// 18 Sep 1997	Matthew J. Rutherford, RTi	Created function
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "IrregularTS.h"

int IrregularTS::readPersistent( istream& in )
{
	char	routine[]="IrregularTS::readPersistent(istream&)";

	PrintWarning( 1, routine,
	"This is a virtual function, should be defined lower down" );

	return( STATUS_SUCCESS );
}

int IrregularTS::readPersistent( char *fname )
{
	char	routine[]="IrregularTS::readPersistent(char*)";

	PrintWarning( 1, routine,
	"This is a virtual function, should be defined lower down" );

	return( STATUS_SUCCESS );
}
//------------------------------------------------------------------------------
// IrregularTS::readPersistentHeader
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)This is a virtual function at this level and should be
//		redefined.
//
//------------------------------------------------------------------------------
// History:
//
// 18 Sep 1997	Matthew J. Rutherford, RTi	Created function
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "IrregularTS.h"

int IrregularTS::readPersistentHeader( istream& in )
{
	char	routine[]="IrregularTS::readPersistentHeader(istream&)";

	PrintWarning( 1, routine,
	"This is a virtual function, should be defined lower down" );

	return( STATUS_SUCCESS );
}

int IrregularTS::readPersistentHeader( char *fname )
{
	char	routine[]="IrregularTS::readPersistentHeader(char*)";

	PrintWarning( 1, routine,
	"This is a virtual function, should be defined lower down" );

	return( STATUS_SUCCESS );
}
//------------------------------------------------------------------------------
// IrregularTS:refresh - resets the max/min values and other values
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine used to be calcMaxMinValues but it has been
//			expanded to be more general so that other refresh
//			functions can be done here.  See also getDataLimits.
//------------------------------------------------------------------------------
// History:
//
// 18 Sep 1997	Matthew J. Rutherford,	Created initial version.
//		RTi
// 05 Jan 1998	Steven A. Malers, RTi	Update to agree with Java.
//					Upon review.  Decided to split into
//					refresh() (the old void version) and
//					getDataLimtis (the old TSDate version).
//					Save the revision, move the file, and
//					then extract.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//------------------------------------------------------------------------------

#include "IrregularTS.h"
#include "TSLimits.h"
#include "TSUtil.h"

int IrregularTS::refresh ( void )
{	char		routine[]="IrregularTS.refresh";
	TSLimits	limits;

	// If the data is not dirty, then we do not have to refresh other
	// information...

	if ( !_dirty ) {
		PrintDebug ( 10, routine,
		"Time series is not dirty.  Not refreshing" );
		return 0;
	}

	// Else we need to refresh...

	PrintDebug( 10, routine,
	"Time Series is dirty. Recomputing limits" );

	// All we do at this time is recompute the limits...

	// Doing one step causes problems with references...
	TSDate date1 = _data_limits.getDate1();
	TSDate date2 = _data_limits.getDate2();
	limits = TSUtil::getDataLimits ( this, date1, date2 );
	if ( limits.areLimitsFound() ) {
		// Now reset the limits for the time series...
		setDataLimits ( limits );
	}

	_dirty = 0;

	return 0;
}
//------------------------------------------------------------------------------
// IrregularTS::setDataValue - sets a data value for a date.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//------------------------------------------------------------------------------
// History:
//
// 18 Sep 1997	Matthew J. Rutherford, RTi	Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "IrregularTS.h"

int IrregularTS::setDataValue( TSDate &date, double value )
{
	char	routine[]="IrregularTS::setDataValue";
	int	found, i;
	TSData	*next=NULL, *ptr=NULL, *tsdata=NULL;

	if( !_ts_data_head ){
		tsdata = new TSData();

		if( !tsdata ){
			PrintWarning( 1, routine,
			"Unable to allocate memory for TSData object." );
			return( STATUS_FAILURE );
		}

		tsdata->setValues( date, value, _data_units, "" );

		//
		// We need to set the head of the list.
		//
		_ts_data_head	= tsdata;
		_ts_data_tail	= tsdata;

		_date1 		= tsdata->getDate();
		_date2 		= tsdata->getDate();

		PrintDebug( 10, routine,
		"Initiated the Linked list with: \"%s\".", (char*)tsdata );

		return( STATUS_SUCCESS );
	}
        //
        // We need to determine if the date is closer to the
        // tail of the list, or the head.
        //
        double  date_double     = date.toDouble();
        double  date1_double    = _date1.toDouble();
        double  date2_double    = _date2.toDouble();

	found = 0;
        if(     fabs( date_double - date1_double ) <
                fabs( date_double - date2_double ) ){

		for( 	i=0, ptr = _ts_data_head;
			ptr != NULL; ptr = ptr->getNext(), i++ ){
			if( ptr->getDate() == date ){
				PrintDebug( 30, routine,
				"Setting %f for %s at %d",
				value, (char*)date, i );

				// Set the dirty flag so that we know to
				// recompute the limits if desired...

				_dirty = 1;
				found  = 1;

				ptr->setData( value );
				break;
			}
		}
	}
	else {
		for( 	i=_ndata, ptr = _ts_data_tail;
			ptr != NULL; ptr = ptr->getPrevious(), i-- ){
			if( ptr->getDate() == date ){
				PrintDebug( 30, routine,
				"Setting %f for %s at %d",
				value, (char*)date, i );

				// Set the dirty flag so that we know to
				// recompute the limits if desired...

				_dirty = 1;
				found  = 1;

				ptr->setData( value );
				break;
			}
		}
	}

	if( found ){
		PrintDebug( 10, routine,
		"Updated data value at: %s to %f.", (char*)date, value );
		return( STATUS_SUCCESS );
	}
	//
	// If we got here, then we didn't find the date, we need to
	// insert the TSData object created above..
	//
	tsdata = new TSData();

	if( !tsdata ){
		PrintWarning( 1, routine,
		"Unable to allocate memory for TSData object." );
		return( STATUS_FAILURE );
	}

	tsdata->setValues( date, value, _data_units, "" );

	found=0;
        if(     fabs( date_double - date1_double ) <
                fabs( date_double - date2_double ) ){

		for( 	i=0, ptr = _ts_data_head;
			ptr != NULL; ptr = ptr->getNext(), i++ ){
			if( ptr->getDate() < date && !ptr->getNext() ){
				//
				// We are adding an element to the end of the
				// list.
				//
				PrintDebug( 10, routine,
				"Inserting %s after %s.",
				(char*)tsdata->getDate(),
				(char*)ptr->getDate() );

				ptr->setNext( tsdata );
				tsdata->setPrevious( ptr );

				_date2 = tsdata->getDate();
				_ts_data_tail = tsdata;

				_dirty	= 1;
				_ndata++;
				found = 1;
				break;
			}
			else if(ptr->getDate() > date &&
				!ptr->getPrevious() ){
				//
				// We have to add an element to the beginning
				// of the list.
				//
				PrintDebug( 10, routine,
				"Inserting %s before %s.",
				(char*)tsdata->getDate(),
				(char*)ptr->getDate() );

				tsdata->setNext( ptr );
				ptr->setPrevious( tsdata );
				_ts_data_head = tsdata;

				_date1 = tsdata->getDate();

				 _dirty	= 1;
				 _ndata++;
				 found = 1;
				 break;
			}
			else if(ptr->getDate() < date &&
				ptr->getNext()->getDate() > date ){
				//
				// We have to insert one between this element
				// and the next one.
				//
				PrintDebug( 10, routine,
				"Inserting %s between %s and %s.",
				(char*)tsdata->getDate(), (char*)ptr->getDate(),
				(char*)ptr->getNext()->getDate() );

				next = ptr->getNext();

				tsdata->setNext( next );
				tsdata->setPrevious( ptr );

				ptr->setNext( tsdata );
				next->setPrevious( tsdata );

				_dirty	= 1;
				_ndata++;
				found = 1;
				break;
			}
			else {
				//
				// Date is less than both this and the next one.
				//
				PrintDebug( 10, routine,
			"%s is less than %s and %s, continuing search...",
				(char*)tsdata->getDate(), (char*)ptr->getDate(),
				(char*)ptr->getNext()->getDate() );
			}
		}
	}
	else {
		for( 	i=_ndata, ptr = _ts_data_tail;
			ptr != NULL; ptr = ptr->getPrevious(), i++ ){
			if( ptr->getDate() < date && !ptr->getNext() ){
				//
				// We are adding an element to the end of the
				// list.
				//
				PrintDebug( 10, routine,
				"Inserting %s after %s.",
				(char*)tsdata->getDate(),
				(char*)ptr->getDate() );

				ptr->setNext( tsdata );
				tsdata->setPrevious( ptr );

				_date2 = tsdata->getDate();
				_ts_data_tail = tsdata;

				_dirty	= 1;
				_ndata++;
				found = 1;
				break;
			}
			else if(ptr->getDate() > date &&
				!ptr->getPrevious() ){
				//
				// We have to add an element to the beginning
				// of the list.
				//
				PrintDebug( 10, routine,
				"Inserting %s before %s.",
				(char*)tsdata->getDate(),
				(char*)ptr->getDate() );

				tsdata->setNext( ptr );
				ptr->setPrevious( tsdata );
				_ts_data_head = tsdata;

				_date1 = tsdata->getDate();

				 _dirty	= 1;
				 _ndata++;
				 found = 1;
				 break;
			}
			else if(ptr->getDate() < date &&
				ptr->getNext()->getDate() > date ){
				//
				// We have to insert one between this element
				// and the next one.
				//
				PrintDebug( 10, routine,
				"Inserting %s between %s and %s.",
				(char*)tsdata->getDate(), (char*)ptr->getDate(),
				(char*)ptr->getNext()->getDate() );

				next = ptr->getNext();

				tsdata->setNext( next );
				tsdata->setPrevious( ptr );

				ptr->setNext( tsdata );
				next->setPrevious( tsdata );

				_dirty	= 1;
				_ndata++;
				found = 1;
				break;
			}
			else {
				//
				// Date is less than both this and the next one.
				//
				PrintDebug( 10, routine,
			"%s is less than %s and %s, continuing search...",
				(char*)tsdata->getDate(), (char*)ptr->getDate(),
				(char*)ptr->getNext()->getDate() );
			}
		}
	}

	if( !found ){
		PrintWarning( 1, routine,
		"With IrregularTS we should always insert the data!" );
		delete tsdata;
		return( STATUS_FAILURE );
	}
	return( STATUS_SUCCESS );
}
//------------------------------------------------------------------------------
// writePersistent - write an NWS DATACARD format file
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This is used to write a time series to a DATACARD
//			format file.
//
//------------------------------------------------------------------------------
// History:
//
// 18 Sep 1997	Matthew J. Rutherford, Riverside Technology, inc.
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "IrregularTS.h"

int IrregularTS::writePersistent ( ostream& out )
{
	char	routine[]="IrregularTS::writePersistent(ostream&)";

	PrintWarning( 1, routine,
	"This function is virtual, redefine in lower classes" );

	return( STATUS_SUCCESS );
}

int IrregularTS::writePersistent ( char *fname )
{
	char	routine[]="IrregularTS::writePersistent(char*)";

	PrintWarning( 1, routine,
	"This function is virtual, redefine in lower classes" );

	return( STATUS_SUCCESS );
}
//----------------------------------------------------------------------------
// TSData Constructors
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------

#include "TSData.h"

//----------------------------------------------------------------------------
// TSData Default Constructor
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------
// Notes:
//
//----------------------------------------------------------------------------
// History:
//
// 22 Sep 1997	Matthew J Rutherford, Riverside Technology, inc.
//					Initial version.
//----------------------------------------------------------------------------
// Variables:		I/O	Description
//
//
//----------------------------------------------------------------------------

TSData::TSData( )
{
	initialize();
}

//----------------------------------------------------------------------------
// TSData Copy Constructor
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------
// Notes:
//
//----------------------------------------------------------------------------
// History:
//
// 22 Sep 1997	MJR, RTi	Initial version.
// 05 Jan 1998	SAM, RTi	Change _quality_flag to _data_flag.
//----------------------------------------------------------------------------
// Variables:	I/O	Description
//
//----------------------------------------------------------------------------

TSData::TSData ( const TSData& t )
{	char	routine[] = "TSData::CopyConstructor";

	initialize();

	setDataFlag( t._data_flag );
	setUnits( t._units );
	_data		= t._data;
	_date	 	= t._date;
	_next		= t._next;
	_previous	= t._previous;
}
//----------------------------------------------------------------------------
// TSData Destructor
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------
// Notes:
//
//----------------------------------------------------------------------------
// History:
//
// 22 Sep 1997	Matthew J. Rutherford,	Initial version.
//		Riverside Technology,
//		inc.
// 05 Jan 1998	Steven A. Malers, RTi	Change _quality_flag to _data_flag.
//----------------------------------------------------------------------------
// Variables:	I/O	Description
//
// routine	L	name of function
//----------------------------------------------------------------------------

#include "TSData.h"

TSData::~TSData ( )
{
	if( _data_flag ){
		delete [] _data_flag;
	}

	if( _units ){
		delete [] _units;
	}
}
//----------------------------------------------------------------------------
// TSData Operators
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------

#include "TSData.h"

//----------------------------------------------------------------------------
// TSData overload operator
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------
// Notes:
//
//----------------------------------------------------------------------------
// History:
//
// 22 Sep 1997	Matthew J. Rutherford,	Copied from ESP TSData stuff.
//		Riverside Technology,
//		inc.
// 05 Jan 1998	Steven A. Malers, RTi	Update to use toString for (char*)
//					cast.
//----------------------------------------------------------------------------
// Variables:	I/O	Description
//
// t		I	incoming TSData
//----------------------------------------------------------------------------

TSData& TSData::operator= ( const TSData& t )
{
        if ( &t == this) {
                // self declaration
        }
        else {	setDataFlag( t._data_flag );
		setUnits( t._units );
		_data		= t._data;
		_date		= t._date;
		_next		= t._next;
		_previous	= t._previous;
        }
	return *this;
}

TSData::operator char *( void )
{
	// Just call toString...

	return toString ();
}
//----------------------------------------------------------------------------
// TSData.getData - return data value
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------
// Notes:
//
//----------------------------------------------------------------------------
// History:
//
// 05 Jan 1998	Steven A. Malers, RTi	Break out of SetGet code.
//----------------------------------------------------------------------------

#include "TSData.h"

double TSData::getData()
{
	return( _data );
}
//----------------------------------------------------------------------------
// TSData.getDataFlag - return the data flag
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------
// Notes:
//
//----------------------------------------------------------------------------
// History:
//
// 05 Jan 1998	Steven A. Malers, RTi	Break out of SetGet code.
//----------------------------------------------------------------------------

#include "TSData.h"

char* TSData::getDataFlag()
{
	return( _data_flag );
}
//----------------------------------------------------------------------------
// TSData.getDate - return the date
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------
// Notes:
//
//----------------------------------------------------------------------------
// History:
//
// 05 Jan 1998	Steven A. Malers, RTi	Break out of SetGet code.
//----------------------------------------------------------------------------

#include "TSData.h"

TSDate TSData::getDate()
{
	return( _date );
}
//----------------------------------------------------------------------------
// TSData.getNext - get the next TSData in the list
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------
// Notes:
//
//----------------------------------------------------------------------------
// History:
//
// 05 Jan 1998	Steven A. Malers, RTi	Break out of SetGet code.
//----------------------------------------------------------------------------

#include "TSData.h"

TSData* TSData::getNext()
{
	return( _next );
}
//----------------------------------------------------------------------------
// TSData.setPrevious - set the previous TSData pointer
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------
// Notes:
//
//----------------------------------------------------------------------------
// History:
//
// 05 Jan 1998	Steven A. Malers, RTi	Split out of SetGet code.
//----------------------------------------------------------------------------

#include "TSData.h"

TSData* TSData::getPrevious()
{
	return( _previous );
}
//----------------------------------------------------------------------------
// TSData.getUnits - return the units
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------
// Notes:
//
//----------------------------------------------------------------------------
// History:
//
// 05 Jan 1998	Steven A. Malers, RTi	Break out of SetGet file.
//----------------------------------------------------------------------------

#include "TSData.h"

char* TSData::getUnits()
{
	return( _units );
}
//-----------------------------------------------------------------------------
// TSData::init - initialized all the data members to appropriate values
//-----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//-----------------------------------------------------------------------------
// Notes:
//
//-----------------------------------------------------------------------------
// History:
//
// 22 Sep 1997	Matthew J. Rutherford,	Created initial version.
//		Riverside Technology,
//		inc.
// 05 Jan 1998	Steven A. Malers, RTi	Change _quality_flag to _data_flag.
//-----------------------------------------------------------------------------
// Variables:	I/O	Description
//
// routine	L	name of routine.
//-----------------------------------------------------------------------------

#include "TSData.h"

int TSData::initialize ( )
{	char	routine[] = "TSData::initialize";

	_data_flag = NULL;
	setDataFlag( "" );

	_units = NULL;
	setUnits( "" );

	_data	= 0.0;
	_next		= NULL;
	_previous	= NULL;
	return 0;
}
//----------------------------------------------------------------------------
// TSData.setData - set data value
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------
// Notes:
//
//----------------------------------------------------------------------------
// History:
//
// 05 Jan 1998	Steven A. Malers, RTi	Break out of SetGet code.
//----------------------------------------------------------------------------

#include "TSData.h"

int TSData::setData( double d )
{
	_data = d;
	return 0;
}
//----------------------------------------------------------------------------
// TSData.setDataFlag - set the data flag
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------
// Notes:
//
//----------------------------------------------------------------------------
// History:
//
// 05 Jan 1998	Steven A. Malers, RTi	Break out of SetGet code.  Change from
//					_quality_flag to _data_flag.
//----------------------------------------------------------------------------

#include "TSData.h"

int TSData::setDataFlag( char *flag )
{	char routine[]="TSData::setDataFlag";

	if( flag == NULL ){
		PrintWarning( 1, routine,
		"Attempting to set a NULL data flag." );
		return 1;
	}

	if( _data_flag ){
		delete [] _data_flag;
		_data_flag = NULL;
	}

	_data_flag = new char [ strlen( flag ) + 1 ];

	if( !_data_flag ){
		PrintWarning( 1, routine,
		"Unable to allocate memory for %d char.",
		strlen( flag ) + 1 );
		return 1;
	}

	strcpy( _data_flag, flag );

	return 0;
}
//----------------------------------------------------------------------------
// TSData.setDate - set the date
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------
// Notes:
//
//----------------------------------------------------------------------------
// History:
//
// 05 Jan 1998	Steven A. Malers, RTi	Break out of SetGet code.
//----------------------------------------------------------------------------

#include "TSData.h"

int TSData::setDate( TSDate& d )
{
	_date = d;
	return 0;
}
//----------------------------------------------------------------------------
// TSData.setNext - set the next TSData pointer
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------
// Notes:
//
//----------------------------------------------------------------------------
// History:
//
// 05 Jan 1998	Steven A. Malers, RTi	Split out of SetGet code.
//----------------------------------------------------------------------------

#include "TSData.h"

int TSData::setNext( TSData* d )
{
	_next	= d;
	return( STATUS_SUCCESS );
}
//----------------------------------------------------------------------------
// TSData.setPrevious - set previous TSDate pointer
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------
// Notes:
//
//----------------------------------------------------------------------------
// History:
//
// 05 Jan 1998	Steven A. Malers, RTi	Break out of SetGet code.
//----------------------------------------------------------------------------

#include "TSData.h"

int TSData::setPrevious( TSData* d )
{
	_previous = d;
	return 0;
}
//----------------------------------------------------------------------------
// TSData.setUnits - set the units
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------
// Notes:
//
//----------------------------------------------------------------------------
// History:
//
// 05 Jan 1998	Steven A. Malers, RTi	Break out of SetGet code.
//----------------------------------------------------------------------------

#include "TSData.h"

int TSData::setUnits( char *units )
{	char routine[]="TSData::setUnits";

	if( units == NULL ){
		PrintWarning( 1, routine,
		"Attempting to set a NULL units string." );
		return 1;
	}

	if( _units ){
		delete [] _units;
		_units = NULL;
	}

	_units = new char [ strlen( units ) + 1 ];

	if( !_units ){
		PrintWarning( 1, routine,
		"Unable to allocate memory for %d char.",
		strlen( units ) + 1 );
		return 1;
	}

	strcpy( _units, units );

	return 0;
}
//----------------------------------------------------------------------------
// TSData.setValues
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------
// Notes:
//
//----------------------------------------------------------------------------
// History:
//
// 05 Jan 1998	Steven A. Malers, RTi	Break out of SetGet code.
//----------------------------------------------------------------------------

#include "TSData.h"

int TSData::setValues( TSDate& date, double d, char* units, char* flag )
{	setDate( date );
	setData( d );

	if( units != NULL ){
		setUnits( units );
	}

	if( flag != NULL ){
		setDataFlag( flag );
	}

	return 0;
}
//------------------------------------------------------------------------------
// TSData.toString - convert to a string representation
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This function converts the data point to a readable
//			string representation.
//------------------------------------------------------------------------------
// History:
//
// 05 Jan 1998	Steven A. Malers, RTi	Document and clean up code a little.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//----------------------------------------------------------------------------

#include "TSData.h"

char TSDataString[256];  //cfan

char * TSData::toString ( void )
{
//cfan	char tmp_string[256];

//cfan	sprintf ( tmp_string,
	sprintf ( TSDataString,  //cfan
	"TSData: Date: \"%s\" Value: %g Units: \"%s\" Flag: \"%s\"",
	(char *)_date, _data, _units, _data_flag );

//cfan	return( tmp_string );
	return( TSDataString );  //cfan
}
// ----------------------------------------------------------------------------
// TSDateIterator - Constructors
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------

#include "TSDateIterator.h"

// ----------------------------------------------------------------------------
// TSDateIterator - default constructor
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:	(1)	Set the period for the iterator to be the full limits
//			of the time series data.
// ----------------------------------------------------------------------------
// History:
//
// 30 Sep 1997	Steven A. Malers	Initial version.
//		Riverside Technology,
//		inc.
// 05 Jan 1998	SAM, RTi		Update to agree with Java version.
// ----------------------------------------------------------------------------
// Variable	I/O	Description
//
// ts		I	Time series that will be iterated on.
// ----------------------------------------------------------------------------
TSDateIterator::TSDateIterator ( TS *ts )
{	char routine[] = "TSDateIterator(TS*)";
	init();

	_ts		= ts;
	if ( ts ) {
		_date1		= ts->getNonMissingDataDate1 ();
		_date2		= ts->getNonMissingDataDate2 ();

		// If DataDate1 has not been set yet, then we will have to
		// just use the start and end dates.

		if( !_date1 || !_date2 ){
			_date1 	= ts->getDate1();
			_date2	= ts->getDate2 ();
		}
		if ( !_date1 ) {
			PrintWarning ( 2, routine,
			"Unable to set start date in constructor" );
		}
		if ( !_date2 ) {
			PrintWarning ( 2, routine,
			"Unable to set end date in constructor" );
		}
	}
	else {	PrintWarning ( 2, routine, "NULL time series for iterator" );
	}
	_current_date	= _date1;
}

// ----------------------------------------------------------------------------
// TSDateIterator - constructor with period of record specified
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:	(1)	Set the period for the iterator to be the full limits
//			of the time series data.
// ----------------------------------------------------------------------------
// History:
//
// 30 Sep 1997	Steven A. Malers	Initial version.
//		Riverside Technology,
//		inc.
// ----------------------------------------------------------------------------
// Variable	I/O	Description
//
// date*	I	Dates to start and end iteration.
// ts		I	Time series that will be iterated on.
// ----------------------------------------------------------------------------
TSDateIterator::TSDateIterator ( TS *ts, TSDate date1, TSDate date2 )
{	char routine[] = "TSDateIterator(TS*,TSDate,TSDate)";
	init();

	if ( !ts ) {
		PrintWarning ( 2, routine, "Null time series for iterator" );
	}
	else {	_ts = ts;

		if( date1 ) {
			_date1 = date1;
		}
		else {	_date1 = ts->getNonMissingDataDate1();
			if( !_date1 ) {
				_date1 = ts->getDate1();
			}
		}

		if( date2 ) {
			_date2 = date2;
		}
		else {	_date2 = ts->getNonMissingDataDate2();
			if( !_date2 ) {
				_date2 = ts->getDate2();
			}
		}
		if ( !_date1 ) {
			PrintWarning ( 2, routine,
			"Unable to set start date in constructor" );
		}
		if ( !_date2 ) {
			PrintWarning ( 2, routine,
			"Unable to set end date in constructor" );
		}
	}

	_current_date	= _date1;
}

// ----------------------------------------------------------------------------
// TSDateIterator - copy Constructor
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:
// ----------------------------------------------------------------------------
// History:
//
// 30 Sep 1997	Steven A. Malers	Initial version.
//		Riverside Technology,
//		inc.
// ----------------------------------------------------------------------------
TSDateIterator::TSDateIterator ( const TSDateIterator& i )
{	char	routine[] = "TSDateIterator::CopyConstructor";

	init();

	_ts 			= i._ts;
	_date1 			= i._date1;
	_date2 			= i._date2;
	_current_date		= i._current_date;
	_last_date_encountered	= i._last_date_encountered;
}
//------------------------------------------------------------------------------
// TSDateIterator destructor
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This is the destructor for the TSDateIterator class.
//			There is really nothing to do.
//------------------------------------------------------------------------------
// History:
//
// 30 Sep 1997	Steven A. Malers,	Initial version.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "TSDateIterator.h"

TSDateIterator::~TSDateIterator( void )
{
}
//------------------------------------------------------------------------------
// TSDateIterator - overloaded operators
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 30 Sep 1997	Steven A. Malers	Created member function.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "TSDateIterator.h"

// nothing at this time
// ----------------------------------------------------------------------------
// TSDateIterator::advanceDate
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// History:
//
// 30 Sep 1997	Steven A. Malers	Initial version.
//		Riverside Technology,
//		inc.
// 05 Jan 1998	SAM, RTi		Update to agree with Java.
// ----------------------------------------------------------------------------

#include "TSDateIterator.h"

int TSDateIterator::advanceDate ( void )
{	int dl = 30;
	char routine[] = "TSDateIterator.advanceDate";

	if ( !_last_date_encountered ) {
		// We only want to advance the date if we have not already
		// gone past the end...
		_current_date = _ts->getDataDate ( _current_date, 1 );
		if ( !_current_date ) {
			// We are at the end or have exceeded the
			// limits of the data...
			_last_date_encountered = 1;
		}
		else if ( !_date2 ) {
			// We are at the end or have exceeded the
			// limits of the data...
			_last_date_encountered = 1;
		}
		else if ( _current_date.greaterThan(_date2) ) {
			// We are at the end or have exceeded the
			// limits of the data...
			_last_date_encountered = 1;
			PrintDebug ( dl, routine,
			"Have passed end data: %s", (char *)_date2 );
		}
	}
	// Do we need to return anything else?
	return 0;
}
//------------------------------------------------------------------------------ // TSDateIterator::getCurrentDate - return the current date
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine returns the current date.
//------------------------------------------------------------------------------
// History:
//
// 30 Sep 1997	Steven A. Malers,	Initial version.
//		Riverside Technology,
//		inc.
// 05 Jan 1997	SAM, RTi		Update to agree with Java.
//------------------------------------------------------------------------------
// Variable	I/O	Description
//
//------------------------------------------------------------------------------

#include "TSDateIterator.h"

TSDate TSDateIterator::getCurrentDate ( void )
{	char	routine[] = "TSDateIterator::getCurrentDate";

	/*
	if( !_current_date ){
		PrintWarning( 2, routine,
		"Current date is NULL, returning date." );
		return( *(new TSDate()) );
	}
	else {	return _current_date;
	}
	*/
	return( _current_date );
}
//------------------------------------------------------------------------------
// TSDateIterator::init - initialize private data members
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This function should be called from constructors, etc.,
//			when data members need to be initialied.
//------------------------------------------------------------------------------
// History:
//
// 30 Sep 1997	Steven A. Malers,	Initial version.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variable	I/O	Description
//
// error_count	L	Counter for errors during initialization.
// routine	L	Name of this routine.
// _*		C	All other variables are TSDateIterator class variables.
//------------------------------------------------------------------------------

#include "TSDateIterator.h"

int TSDateIterator::init ( void )
{	char	routine[] = "TSDateIterator::init";

	_ts = (TS *)NULL;
	_last_date_encountered = 0;
	// Dates are already initialized upon construction.
	return 0;
}
// ----------------------------------------------------------------------------
// TSDateIterator::isIterationComplete
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// History:
//
// 30 Sep 1997	Steven A. Malers	Initial version.
//		Riverside Technology,
//		inc.
// ----------------------------------------------------------------------------

#include "TSDateIterator.h"

int TSDateIterator::isIterationComplete ()
{
	return _last_date_encountered;
}
//------------------------------------------------------------------------------
// TSDateIterator::setBeginDate - set the starting date for the iterator
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This function should be called only if the begin date of
//			the iterator is different from the complete period of
//			record of the time series that is being iterated.
//------------------------------------------------------------------------------
// History:
//
// 30 Sep 1997	Steven A. Malers,	Initial version.
//		Riverside Technology,
//		inc.
// 05 Jan 1998	SAM, RTi		Add check for null date.
//------------------------------------------------------------------------------
// Variable	I/O	Description
//
//------------------------------------------------------------------------------

#include "TSDateIterator.h"

int TSDateIterator::setBeginDate ( TSDate begin_date )
{	char	routine[] = "TSDateIterator::setBeginDate";

	if ( begin_date ) {
		_date1 = begin_date;
	}
	return 0;
}
//------------------------------------------------------------------------------
// TSDateIterator::setEndDate - set the ending date for the iterator
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This function should be called only if the end date of
//			the iterator is different from the complete period of
//			record of the time series that is being iterated.
//------------------------------------------------------------------------------
// History:
//
// 30 Sep 1997	Steven A. Malers,	Initial version.
//		Riverside Technology,
//		inc.
// 05 Jan 1998	SAM, RTi		Add check for null date.
//------------------------------------------------------------------------------
// Variable	I/O	Description
//
//------------------------------------------------------------------------------

#include "TSDateIterator.h"

int TSDateIterator::setEndDate ( TSDate end_date )
{	char	routine[] = "TSDateIterator::setEndDate";

	if ( end_date ) {
		_date2 = end_date;
	}
	return 0;
}
// ----------------------------------------------------------------------------
// TSDate Default Constructor
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:
//
// ----------------------------------------------------------------------------
// History:
//
// 07 Nov 1996	Matthew J Rutherford, Riverside Technology, inc.
//							Broke out of TSDate.cc
//
// ----------------------------------------------------------------------------
// Variables:		I/O	Description
//
//
// ----------------------------------------------------------------------------

#include "resj/TSDate.h"
#include "resj/TSTimeZone.h"

TSDate::TSDate ( void )
{
	init ();
	_behavior_flag = DATE_STRICT;
}

// We probably will never need to initialize a TSDate based on the number of
// seconds from a datum so we use the int overload to pass in flags.
TSDate::TSDate ( unsigned int flag )
{
	if ( flag & DATE_ZERO ) {
		init0();
	}
	else {	init();
	}

	_behavior_flag 	= flag;
}

// ----------------------------------------------------------------------------
// TSDate Copy Constructor
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:
//
// ----------------------------------------------------------------------------
// History:
//
// 07 Nov 1996	MJR, RTi	Broke out of TSDate.cc, and copied from
//				ESP TSDate stuff.
//
// ----------------------------------------------------------------------------
// Variables:	I/O	Description
//
// t		I	incoming date
// ----------------------------------------------------------------------------

TSDate::TSDate ( const TSDate& t )
{
	char	routine[] = "TSDate::CopyConstructor";

	init();

	_hsecond 	= t._hsecond;
	_second		= t._second;
	_minute		= t._minute;
	_hour 		= t._hour;
	_day 		= t._day;
	_month		= t._month;
	_year		= t._year;
	_isleap		= t._isleap;
	_weekday	= t._weekday;
	_yearday	= t._yearday;
	_abs_month	= t._abs_month;
	_itz		= t._itz;
	_dsflag 	= t._dsflag;
	_behavior_flag	= t._behavior_flag;

	// Call setTimeZone, allocation for the string is handled there

	setTimeZone( t._tz );
}

// Convert time zones...

TSDate::TSDate ( const TSDate &t, char *tz )
{
	char	routine[] = "TSDate::TSDate(TSDate&,char*)";

	init();

	// First copy...

	_hsecond 	= t._hsecond;
	_second		= t._second;
	_minute		= t._minute;
	_hour 		= t._hour;
	_day 		= t._day;
	_month		= t._month;
	_year		= t._year;
	_isleap		= t._isleap;
	_weekday	= t._weekday;
	_yearday	= t._yearday;
	_abs_month	= t._abs_month;
	_itz		= t._itz;
	_dsflag 	= t._dsflag;
	_behavior_flag	= t._behavior_flag;

	//Call setTimeZone, allocation for the string is handled there

	setTimeZone( t._tz );

	// Now compute the time zone offset...

	int offset = TSTimeZone::calculateOffsetHours ( t._tz, tz );
	addHour ( offset );
	setTimeZone( tz );
}

// Construct using another TSDate and a flag...

TSDate::TSDate ( const TSDate &t, unsigned int flag )
{
	char	routine[] = "TSDate::TSDate(TSDate,int)";

	if ( flag & DATE_ZERO ) {
	 	init0();
	}
	else {	init();
	}

	_hsecond 	= t._hsecond;
	_second		= t._second;
	_minute		= t._minute;
	_hour 		= t._hour;
	_day 		= t._day;
	_month		= t._month;
	_year		= t._year;
	_isleap		= t._isleap;
	_weekday	= t._weekday;
	_yearday	= t._yearday;
	_abs_month	= t._abs_month;
	_itz		= t._itz;
	_dsflag 	= t._dsflag;
	_behavior_flag	= flag;

	//Call setTimeZone, allocation for the string is handled there

	setTimeZone( t._tz );
}
// ----------------------------------------------------------------------------
// TSDate Destructor
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:
//
// ----------------------------------------------------------------------------
// History:
//
// 06 Jan 1998	Steven A. Malers,	Initial version.  Port from Java.
//		Riverside Technology,
//		inc.
// ----------------------------------------------------------------------------
// Variables:	I/O	Description
//
// routine	L	name of function
// ----------------------------------------------------------------------------

#include "resj/TSDate.h"

TSDate::~TSDate ( void )
{
	if( _tz != NULL ) {
		delete [] _tz;
	}
}
//----------------------------------------------------------------------------
// TSDate Operators
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------
// Notes:	(1)	For Java, we do not have overloaded operators, but
//			for the C++ versions we have now implemented Java-like
//			functions so that it is easier to keep the C++ and
//			Java versions compatible.
//----------------------------------------------------------------------------

#include "resj/TSDate.h"

//----------------------------------------------------------------------------
// TSDate overload = operator
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------
// Notes:
//
//----------------------------------------------------------------------------
// History:
//
// 07 Nov 1996	Matthew J. Rutherford, Riverside Technology, inc.
//					copied from ESP TSDate stuff.
//
//----------------------------------------------------------------------------
// Variables:	I/O	Description
//
// t		I	incoming TSDate
//
//----------------------------------------------------------------------------

TSDate& TSDate::operator= ( const TSDate& t )
{
        if ( &t == this) {
                // self declaration
        }
        else {
		_hsecond	= t._hsecond;
		_second		= t._second;
		_minute		= t._minute;
		_hour 		= t._hour;
		_day 		= t._day;
		_month		= t._month;
		_year		= t._year;
		_isleap		= t._isleap;
		_weekday	= t._weekday;
		_yearday	= t._yearday;
		_abs_month	= t._abs_month;
		_itz		= t._itz;
		_dsflag 	= t._dsflag;
		_behavior_flag	= t._behavior_flag;
		setTimeZone( t._tz );
        }
	return *this;
}

int TSDate::operator= ( char* string )
{
	char		**list1=(char**)NULL, **list2=(char**)NULL,
			routine[]="TSDate::operator=(char*)", tmp[MAXC]="";
	int		nlist1=0, nlist2=0, value=0;

	//
	// First we need to break the string up by spaces.
	//
	list1 = BreakStringList( string, " ", DELIM_SKIP_BLANKS, &nlist1 );

	if( !nlist1 || !list1 ){
		PrintWarning( 2, routine,
		"Troubles breaking initial string \"%s\".", string );
		return( STATUS_FAILURE );
	}

	//
	// We always assume that the first string contains the
	// month day year in either North American format(5/21/1997, ANSI
	// format (19970521), or HHECDSS format 22APR96
	//
	if( strstr( list1[0], "/" ) ){
		//
		// We are dealing with North American Format.
		//
		list2 = BreakStringList( list1[0], "/", DELIM_SKIP_BLANKS,
			&nlist2 );

		if( nlist2 != 3 ){
			PrintWarning( 1, routine,
		"Month/Day/Year string \"%s\" not in anticipated format.",
			list1[0] );
			list1 = FreeStringList( list1 );
			list2 = FreeStringList( list2 );
			return( STATUS_FAILURE );
		}
		setMonth( atoi( list2[0] ) );

		setDay( atoi( list2[1] ) );

		setYear( atoi( list2[2] ) );

		list2 = FreeStringList( list2 );
	}
	else if( strlen( list1[0] ) == 7 ){
		//
		// we are dealing with HEC DSS format.
		//
		strncpy( tmp, list1[0], 2 );
		tmp[2] = '\0';
		setDay( atoi( tmp ) );

		strncpy( tmp, &list1[0][2], 3 );
		tmp[3] = '\0';
		value = MonthFromAbbreviation( tmp );
		if( !value ){
			PrintWarning( 1, routine,
			"Month portion of HECDSS string \"%s\" is not valid.",
			list1[0] );
			list1 = FreeStringList( list1 );
			return( STATUS_FAILURE );
		}

		setMonth( value );

		strncpy( tmp, &list1[0][5], 2 );
		tmp[2] = '\0';

		if( !IsInteger( tmp ) ){
			PrintWarning( 1, routine,
			"Year portion  of string \"%s\" is not valid.",
			list1[0] );
			list1 = FreeStringList( list1 );
			return( STATUS_FAILURE );
		}
		value = atoi( tmp );

		//
		// Don't know what's going to happen at the year 2000.
		//
		value += 1900;

		setYear( value );
	}
	else {
		//
		// We are dealing with ANSI format.
		//
		if( strlen( list1[0] ) != 8 ){
			PrintWarning( 1, routine,
		"YearDayMonth string \"%s\" is not in anticipated format.",
			list1[0] );
			list1 = FreeStringList( list1 );
			return( STATUS_FAILURE );
		}
		strncpy( tmp, list1[0], 4 );
		tmp[4] = '\0';
		setYear( atoi( tmp ) );

		strncpy( tmp, &list1[0][4], 2 );
		tmp[2] = '\0';
		setMonth( atoi( tmp ) );

		strncpy( tmp, &list1[0][6], 2 );
		tmp[2] = '\0';
		setDay( atoi( tmp ) );
	}

	if( nlist1 >= 2 ){
		//
		// We have hour data.
		//
		nlist2 = 0;
		list2 = BreakStringList( list1[1], ":", DELIM_SKIP_BLANKS,
			&nlist2 );

		if( nlist2 != 3 ){
			PrintWarning( 2, routine,
			"Hour:Min:Sec string \"%s\" not in anticipated format.",
			list2[1] );
			list1 = FreeStringList( list1 );
			list2 = FreeStringList( list2 );
			return( STATUS_FAILURE );
		}

		setHour( atoi( list2[0] ) );

		setMinute( atoi( list2[1] ) );

		setSecond( atoi( list2[2] ) );

		list2 = FreeStringList( list2 );
	}
	else {
		PrintDebug( 10, routine,
		"Date \"%s\" only will set Month, Day, and year.", string );
	}

	if( nlist1 >= 3 ){
		//
		// This last string contains the TZ.
		//
		if( IsInteger( list1[2] ) ){
			PrintWarning( 1, routine,
			"TZ string \"%s\" not in anticipated format." );
			list1 = FreeStringList( list1 );
			return( STATUS_FAILURE );
		}

		setTimeZone( list1[2] );
	}
	else {
		PrintDebug( 10, routine,
		"Date \"%s\" does not contain TZ, using Zulu.",
		string );
		setTimeZone( "Z" );
	}

	PrintDebug( 10, routine,
	"Converted \"%s\" to %d/%d/%d %02d:%02d:%02d.%02d %s.",
	string, _month, _day, _year, _hour, _minute, _second, _hsecond, _tz );

	return( STATUS_SUCCESS );
}

//----------------------------------------------------------------------------
// TSDate.compareTo - similar to the strcmp function but for TSDate's
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------
// Notes:	(1)	Goes down to hundredths of seconds
//		(2)	Return -1 if this date is less than "t", 0 if equal,
//			and 1 if greater.
//----------------------------------------------------------------------------
// History:
//
// 07 Nov 1996	MJR, RTi	Copied from ESP TSDate stuff, added logic
//				to go down to hundredths of seconds.
// 23 Sep 1997	SAM, RTi	Update for Java where we cannot overload
//				operators.
//----------------------------------------------------------------------------
// Variables:	I/O	Description
//
// t		I	incoming TSDate
//----------------------------------------------------------------------------

int TSDate::compareTo ( const TSDate &t )
{
	if ( equals(t) ) {
		return 0;
	}
	else if ( lessThan(t) ) {
		return -1;
	}
	else {	return 1;
	}
}

//----------------------------------------------------------------------------
// TSDate overload == operator using the equals function
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------
// Notes:	(1)Goes down to hundredths of seconds
//		(2)Does not yet handle dates that are equivalent, but in
//		different time-zones.
//
//----------------------------------------------------------------------------
// History:
//
// 07 Nov 1996	MJR, RTi	Wrote member function
//
//----------------------------------------------------------------------------
// Variables:	I/O	Description
//
// t		I	incoming TSDate
//
//----------------------------------------------------------------------------

int TSDate::equals ( const TSDate& t )
{
	if (	(_year	== t._year)	&&
		(_month	== t._month)	&&
		(_day	== t._day)	&&
		(_hour	== t._hour)	&&
		(_minute == t._minute)	&&
		(_second == t._second)	&&
		(_hsecond== t._hsecond)	&&
		(_itz	== t._itz) ) {
		return 1;
	}
	return 0;
}

int TSDate::equals ( const TSDate& t, unsigned int flag )
{
	if ( _year != t._year ) {
		return 0;
	}
	if ( flag & YEAR ) {
		return 1;
	}
	if ( _month != t._month ) {
		return 0;
	}
	if ( flag & MONTH ) {
		return 1;
	}
	if ( _day != t._day ) {
		return 0;
	}
	if ( flag & DAY ) {
		return 1;
	}
	if ( _hour != t._hour ) {
		return 0;
	}
	if ( flag & HOUR ) {
		return 1;
	}
	if ( _minute != t._minute ) {
		return 0;
	}
	if ( flag & MINUTE ) {
		return 1;
	}
	if ( _second != t._second ) {
		return 0;
	}
	if ( flag & SECOND ) {
		return 1;
	}
	if ( _hsecond != t._hsecond ) {
		return 0;
	}
	if ( flag & HSECOND ) {
		return 1;
	}
	if ( _itz != t._itz ) {
		return 0;
	}

	// They are not equal

	return 0;
}

int TSDate::operator== ( const TSDate& t )
{
	// Just call the named function...

	return equals ( t );
}

//----------------------------------------------------------------------------
// TSDate overload >= operator using greaterThanOrEqualTo function
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------
// Notes:	(1)Goes down to hundredths of seconds
//
//----------------------------------------------------------------------------
// History:
//
// 07 Nov 1996	MJR, RTi	Copied from ESP TSDate stuff, added logic to
//				go down to hundredths of seconds.
//
//----------------------------------------------------------------------------
// Variables:	I/O	Description
//
// t		I	incoming TSDate
//
//----------------------------------------------------------------------------

int TSDate::greaterThanOrEqualTo (const TSDate& t)
{
	if ( !lessThan(t) ) {
		return 1;
	}
	else {	return 0;
	}
}

int TSDate::operator>= (const TSDate& t)
{
	// Just call the named function...

	return greaterThanOrEqualTo ( t );
}

//----------------------------------------------------------------------------
// TSDate Overload <= operator using lessThanOrEqualTo function
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------
// Notes:
//
//----------------------------------------------------------------------------
// History:
//
// 07 Nov 1996	MJR, RTi	Copied from ESP TSDate constructor, and
//				continued down to hundredths of seconds
//
//----------------------------------------------------------------------------
// Variables:	I/O	Description
//
// t		I	incoming TSDate
//
//----------------------------------------------------------------------------
int TSDate::lessThanOrEqualTo ( const TSDate& t )
{
	if ( lessThan(t) ) {
		return 1;
	}
	else if ( equals(t) ) {
		return 1;
	}
	else {	return 0;
	}
}

int TSDate::operator<= (const TSDate& t)
{
	// Call the named function...
	return lessThanOrEqualTo ( t );
}

//----------------------------------------------------------------------------
// TSDate overload < operator using the lessThan function
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------
// Notes:	(1)	Goes down to hundredths of seconds
//----------------------------------------------------------------------------
// History:
//
// 07 Nov 1996	MJR, RTi	Copied from ESP TSDate stuff, added logic
//				to go down to hundredths of seconds.
//----------------------------------------------------------------------------
// Variables:	I/O	Description
//
// t		I	incoming TSDate
//----------------------------------------------------------------------------

int TSDate::lessThan (const TSDate& t)
{
        if( _year < t._year) {
                return 1;
        }
        else {	if(_year > t._year) {
                        return 0;
                }
        }

        // otherwise years are equal so check months

        if(_month < t._month) {
               return 1;
        }
        else {	if(_month > t._month) {
                        return 0;
                }
        }

        // months must be equal so check day

        if (_day < t._day) {
                return 1;
        }
        else {	if(_day > t._day) {
                        return 0;
                }
        }

        // days are equal so check hour

         if (_hour < t._hour) {
                return 1;
         }
         else {	if(_hour > t._hour) {
                        return 0;
                }
        }

	// hours are esual so check minutes

	if( _minute < t._minute ) {
		return 1;
	}
	else {	if( _minute > t._minute ) {
			return 0;
		}
	}

	// means that minutes match - so check second

	if( _second < t._second ) {
		return 1;
	}
	else {	if( _second > t._second ) {
			return 0;
		}
	}

	// means that seconds match - so check hundredths of seconds

	if( _hsecond < t._hsecond ) {
		return 1;
	}
	else {	if( _hsecond > t._hsecond ) {
			return 0;
		}
	}

        // everthing must be equal so not less than

	return 0;
}

int  TSDate::operator< (const TSDate& t)
{
	// Just call the named function...

	return lessThan ( t );
}

//----------------------------------------------------------------------------
// TSDate overload > operator using greaterThan function
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------
// Notes:	(1)Goes down to hundredths of seconds
//
//----------------------------------------------------------------------------
// History:
//
// 07 Nov 1996	MJR, RTi	Copied from ESP TSDate stuff, added logic down
// 				to hundredths of seconds
//
//----------------------------------------------------------------------------
// Variables:		I/O	Description
//
//
//----------------------------------------------------------------------------

int TSDate::greaterThan (const TSDate& t)
{
        if( _year < t._year) {
                return 0;
        }
        else {	if(_year > t._year) {
                        return 1;
                }
        }

        // otherwise years are equal so check months

        if(_month < t._month) {
               return 0;
        }
        else {	if(_month > t._month) {
                        return 1;
                }
        }

        // months must be equal so check day

        if (_day < t._day) {
                return 0;
        }
        else {	if(_day > t._day) {
                        return 1;
                }
        }

        // days are equal so check hour

         if (_hour < t._hour) {
                return 0;
         }
         else {	if(_hour > t._hour) {
                        return 1;
                }
        }

        // means that hours match - so check minute

	if( _minute < t._hour ) {
		return 0;
	}
	else {	if( _minute > t._minute ) {
			return 1;
		}
	}

	// means that minutes match - so check second

	if( _second < t._second ) {
		return 0;
	}
	else {	if( _second > t._second ) {
			return 1;
		}
	}

	// means that seconds match - so check hundredths of seconds

	if( _hsecond < t._hsecond ) {
		return 0;
	}
	else {	if( _hsecond > t._hsecond ) {
			return 1;
		}
	}
        // means they are equal

        return 0;
}

int TSDate::operator> (const TSDate& t)
{
	// Just call the named function

	return greaterThan ( t );
}

//------------------------------------------------------------------------------
// TSDate::!= operator using the notEquals function
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 05 Jan 1998	SAM, RTi		Implemented initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

int TSDate::notEquals ( const TSDate& t )
{
	if( equals(t) ) {
		return 0;
	}
	else {	return 1;
	}
}

int TSDate::operator!= ( const TSDate& t )
{
	// Call the named function...

	return notEquals ( t );
}

TSDate::operator char *( void )
{
	// Call the named function...

	return( toString() );
}

int TSDate::operator! ()
{
	TSDate	test( DATE_ZERO );

	return( (*this) == test );
}
// ----------------------------------------------------------------------------
// TSDate::addDay - add days(s) to a time data structure
// ----------------------------------------------------------------------------
// Copyright:   See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:	(1)	Add days to time and cascade if necessary.
// ----------------------------------------------------------------------------
// History:
//
// 07 Nov 1996	Matthew J. Rutherford, Riverside Technology, inc.
//					Adapted from AddDayToTime
// 05 Dec 1996	MJR, RTi		Set day before addMonth to prevent
//					error messages from being issued.
// 05 Jan 1998	Steven A. Malers, RTi	Only call NumDaysInMonth twice (rather
//					than 3 times as before).
// ----------------------------------------------------------------------------
// Variable	I/O	Description
//
// add		I	Days to add.
// i		L	Counter for add operations.
// routine	L	Name of this routine.
// ----------------------------------------------------------------------------

#include "resj/TSDate.h"

int TSDate::addDay ( int add )
{	int	day, i;
	char	routine[] = "TSDate::addDay";

	// First figure out if we are trying to add more than one day.
	// If so, recurse (probably a faster way, but this works)...

	if ( add > 1 || add < -1 ) {
		if ( add > 0 ) {
			for ( i = 0; i < add; i++ ) {
				addDay ( 1 );
			}
		}
		else {	for ( i = add; i < 0; i++ ){
				addDay ( -1 );
			}
		}
	}
	else {	int num_days_in_month = NumDaysInMonth ( _month, _year );
		if ( add > 0 ) {
			_day += 1;
			if ( _day > num_days_in_month ) {
				// Have gone into the next month...
				_day -= num_days_in_month;
				addMonth( 1 );
			}
		}
		else {	_day -= 1;
			if ( _day < 1 ) {
				// Have gone into the previous month...
				// Temporarily set day to 1, determine the day
				// and year, and then set the day.
				_day = 1;
				addMonth( -1 );
				_day = NumDaysInMonth( _month, _year );
			}
		}

		// Reset the private data members.
		setYearDay();
	}
	return STATUS_SUCCESS;
}
//-----------------------------------------------------------------------------
// TSDate::addHSecond - add hundredths of second(s) to a time data structure
//-----------------------------------------------------------------------------
// Copyright:   See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// History:
//
// 01/25/96	Peter T. Abplanalp, RTi	Created routine.
// 06 Sep 1996  Steven A. Malers, RTi   Split out of the TD.c file.
// 07 Nov 1996	MJR, RTi		Adapted from TSDate_addSecond.cc
//-----------------------------------------------------------------------------
// Notes:
//
//-----------------------------------------------------------------------------
// Variable	I/O	Description
//
// add		I	Number of hundredths of seconds to add
// routine	L	Routine name
// secs		L	Number of seconds to add
//-----------------------------------------------------------------------------

#include "resj/TSDate.h"

int TSDate::addHSecond ( int add )
{	int	secs;
	char	routine[]="TSDate::addHSecond";

	if ( add >= 100 || add <= -100 ) {
		// Need to add/subtract seconds first
		secs = add / 100;
		addSecond( secs );
	}

	if ( add > 0 ) {
		_hsecond += add % 100;
		if ( _hsecond > 99 ) {
			// Need to add a second and subtract the same from
			// hsecond
			_hsecond -= 100;
			addSecond( 1 );
		}
	}
	else {	_hsecond += add % 100;
		if ( _hsecond < 0 ) {
			// Need to subtract a minute and add the same to second
			_hsecond += 100;
			addSecond( -1 );
		}
	}

	return 0;
}
// ----------------------------------------------------------------------------
// TSDate::addHour - add hour(s) to a time data structure
// ----------------------------------------------------------------------------
// Copyright:   See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:	(1)	Add hour(s) to time and cascade if necessary.
//		(2)	Hours are allowed to be 0 - 23.
// ----------------------------------------------------------------------------
// History:
//
// 07 Nov 1996	Matthew J. Rutherford, Riverside Technology, inc.
//						Adapted from AddHourToTime.c
// ----------------------------------------------------------------------------
// Variable	I/O	Description
//
// add		I	Hours to add.
// daystoadd	L	Days to add.
// routine	L	Name of this routine.
// ----------------------------------------------------------------------------

#include "resj/TSDate.h"

int TSDate::addHour ( int add )
{	int	daystoadd;
	char	routine[] = "TSDate::addHour";

	if ( add >= 24 || add <= -24 ) {
		// First need to add/subtract days to time...
		daystoadd = add/24;
		addDay( daystoadd );
	}

	// Now add the remainder

	if ( add > 0 ) {
		_hour += (add%24);
		if ( _hour > 23 ) {
			// Have gone into the next day...
			_hour -= 24;
			addDay( 1 );
		}
	}
	else {	_hour += (add%24);
		if ( _hour < 0 ) {
			// Have gone into the previous day...
			_hour += 24;
			addDay( -1 );
		}
	}

	return 0;
}
// ----------------------------------------------------------------------------
// TSDate.addInterval- add interval(s) to a time data structure
// ----------------------------------------------------------------------------
// COPYRIGHT See the ../src/COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:       (1)     This routine can be called if the interval is variable
//                      in the upper-level code.
// ----------------------------------------------------------------------------
// History:
//
// 08 Nov 1996	Matthew J. Rutherford, Riverside Technology, inc.
//					pulled out of ESP TSDate stuff, and
//					added intervals down to HSecond.
// 23 Sep 1997	Steven A. Malers, RTi	Use new static flags.
// ----------------------------------------------------------------------------
// Variable     I/O     Description
//
// add          I       Intervals to add.
// interval     I       Interval.
// routine      L       Name of this routine.
// ----------------------------------------------------------------------------

#include "resj/TSDate.h"
#include "resj/TS.h"

int TSDate::addInterval (  int interval, int add )
{      	char routine[] = "addInterval";

        // Based on the interval, call lower-level routines...

	if( interval == TS::INTERVAL_SECOND ) {
		return ( addSecond( add ) );
	}
	else if( interval == TS::INTERVAL_MINUTE ) {
		return ( addMinute( add ) );
	}
	else if( interval == TS::INTERVAL_HOUR ) {
                return ( addHour( add ) );
        }
        else if ( interval == TS::INTERVAL_DAY ) {
                return ( addDay( add) );
        }
        else if ( interval == TS::INTERVAL_WEEK ) {
		PrintWarning( 1, routine,
		"Incrementing by Weekly not Implemented yet" );
                return ( 1 );
        }
        else if ( interval == TS::INTERVAL_MONTH ) {
                return ( addMonth( add) );
        }
        else if ( interval == TS::INTERVAL_YEAR ) {
                return ( addYear( add) );
        }
        else {  // Unsupported interval...

                PrintWarning ( 2, routine,
		"Interval %d is unsupported", interval );

                return 1;
        }
}
//-----------------------------------------------------------------------------
// TSDate::addMinute - add minute(s) to time data structure
//-----------------------------------------------------------------------------
// Copyright:   See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// History:
//
// 07 Nov 1996	Matthew J. Rutherford, Riverside Technology, inc.
//					Adapted from AddMinuteToTime.c
//-----------------------------------------------------------------------------
// Notes:
//
//-----------------------------------------------------------------------------
// Variable	I/O	Description
//
// add		I	Number of minutes to add
// hrs		L	Number of hours to add
// routine	L	Routine name
//-----------------------------------------------------------------------------

#include "resj/TSDate.h"

int TSDate::addMinute ( int add )
{	int	hrs;
	char	routine[]="TSDate::addMinute";

	if ( add >= 60 || add <= -60 ) {
		// Need to add/subtract hour(s) first
		hrs = add / 60;
		addHour( hrs );
	}

	if ( add > 0 ) {
		_minute += add % 60;
		if ( _minute > 59 ) {
			// Need to add an hour and subtract the same from minute
			_minute -= 60;
			addHour( 1 );
		}
	}
	else {	_minute += add % 60;
		if ( _minute < 0 ) {
			// Need to subtract an hour and add the same to minute
			_minute += 60;
			addHour( -1 );
		}
	}

	return 0;
}
// ----------------------------------------------------------------------------
// TSDate::addMonth - add month(s) to a time data structure
// ----------------------------------------------------------------------------
// Copyright:   See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// History:
//
// 07 Nov 1996	Matthew J. Rutherford, Riverside Technology, inc.
//						Adapted from AddMonthToTime.c
// ----------------------------------------------------------------------------
// Variable	I/O	Description
//
// add		I	Months to add.
// i		L	Loop counter for months to add.
// routine	L	Name of this routine.
// ----------------------------------------------------------------------------

#include "resj/TSDate.h"

int TSDate::addMonth ( int add )
{	char	routine[] = "TSDate::addMonth";
	int	i;

	if ( add == 0 ) {
		return 0;
	}
	else if ( (add != 1) && (add != -1) ) {
		// Loop through the number to add/subtract...
		if ( add > 0 ) {
			for ( i = 0; i < add; i++ ) {
				addMonth ( 1 );
			}
		}
		else {	for ( i = 0; i > add; i-- ) {
				addMonth ( -1 );
			}
		}
		return 0;
	}
	else {	// We are dealing with one month...
		_month += add;
		if ( add > 0 ) {
			// We have added one month so check if we
			// went into the next year
			if ( _month > 12 ) {
				// Have gone into the next year...
				_month = 1;
				addYear( 1 );
			}
		}
		else {	// We have subtracted the specified number so check if
			// we are in the previous year
			if ( _month < 1 ) {
				// Have gone into the previous year...
				_month = 12;
				addYear( -1 );
			}
		}
	}
	// Reset time
	setAbsMonth();

	setYearDay();

	return 0;
}
//-----------------------------------------------------------------------------
// TSDate::addSecond - add second(s) to a time data structure
//-----------------------------------------------------------------------------
// Copyright:   See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// History:
//
// 07 Nov 1996	Matthew J. Rutherford, Riverside Technology, inc.
//					Adapted from AddSecondToTime.c
//-----------------------------------------------------------------------------
// Notes:
//
//-----------------------------------------------------------------------------
// Variable	I/O	Description
//
// add		I	Number of seconds to add
// mins		L	Number of minutes to add
// routine	L	Routine name
//-----------------------------------------------------------------------------

#include "resj/TSDate.h"

int TSDate::addSecond ( int add )
{	int	mins;
	char	routine[]="TSDate::addSecond";

	if ( add >= 60 || add <= -60 ) {
		// Need to add/subtract minute(s) first
		mins = add / 60;
		addMinute( mins );
	}

	if ( add > 0 ) {
		_second += add % 60;
		if ( _second > 59 ) {
			// Need to add a minute and subtract the same from
			// second
			_second -= 60;
			addMinute( 1 );
		}
	}
	else {	_second += add % 60;
		if ( _second < 0 ) {
			// Need to subtract a minute and add the same to second
			_second += 60;
			addMinute( -1 );
		}
	}

	return 0;
}
// ----------------------------------------------------------------------------
// TSDate::addYear - add year(s) to a time data structure
// ----------------------------------------------------------------------------
// Copyright:   See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// History:
//
// 07 Nov 1996	Matthew J. Rutherford, Riverside Technology, inc.
//						Adapted from AddYearToTime.c
// ----------------------------------------------------------------------------
// Variable	I/O	Description
//
// add		I	Years to add.
// routine	L	Name of this routine.
// ----------------------------------------------------------------------------

#include "resj/TSDate.h"

int TSDate::addYear ( int add )
{	char routine[] = "TSDate::addYear";

	_year += add;

	setAbsMonth();

	setYearDay();

	_isleap = IsLeapYear( _year );

	return 0;
}
// ----------------------------------------------------------------------------
// TSDate::formatTimeString - format as a string
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:	(1)	Not implemented in C++ yet.
//
// ----------------------------------------------------------------------------
// History:
//
// 05 Jan 1998	Steven A. Malers, RTi	Add to parallel Java version.
// ----------------------------------------------------------------------------
// Variables:	I/O	Description
//
// routine	L	name of routine.
//
// ----------------------------------------------------------------------------

#include "resj/TSDate.h"

char * TSDate::formatTimeString ( void )
{	char routine[] = "TSDate.formatTimeString";

	PrintWarning ( 1, routine, "NOT IMPLEMENTED" );
	return "";
}

char * TSDate::formatTimeString ( char *format )
{	char routine[] = "TSDate.formatTimeString(format)";

	PrintWarning ( 1, routine, "NOT IMPLEMENTED" );
	return "";
}
// ----------------------------------------------------------------------------
// TSDate.getAbsMonth - get the absolute month
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:
//
// ----------------------------------------------------------------------------
// History:
//
// 05 Jan 1998	Steven A. Malers, RTi	Split out of SetGet code.
// ----------------------------------------------------------------------------

#include "resj/TSDate.h"

int TSDate::getAbsMonth( void )
{
	return _abs_month;
}
//------------------------------------------------------------------------------
// TSDate::getDateFromIndex - returns the date for a number of intervals
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 22 Nov 1996	Matthew J. Rutherford,	Created member function.
//		RTi
// 23 Sep 1997	Steven A. Malers, RTi	Move code from TSGetDateFromIndex to
//					TSDate::getDateFromIndex.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TSDate.h"

TSDate TSDate::getDateFromIndex( 	TSDate t1, int base, int mult,
					int index )
{
	char	routine[]="TSDate::getDateFromIndex";
	int	count=0;
	TSDate	t;

	// Start at the specified date, and count up.

	t = t1;

	for( count = 0; count < index; count ++ ){
		t.addInterval( base, mult );
	}

	PrintDebug( 30, routine, "Date for %d is %d/%d/%d %d",
	index, t.getMonth(), t.getDay(), t.getYear() );

	return( t );
}
// ----------------------------------------------------------------------------
// TSDate.getDay - get the day
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:
//
// ----------------------------------------------------------------------------
// History:
//
// 05 Jan 1998	Steven A. Malers, RTi	Split the code out of the SetGet file.
// ----------------------------------------------------------------------------

#include "resj/TSDate.h"

int TSDate::getDay( void )
{
	return _day;
}
// ----------------------------------------------------------------------------
// TSDate.getHSecond - return the hundredth of a second
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:
//
// ----------------------------------------------------------------------------
// History:
//
// 05 Jan 1998	Steven A. Malers, RTi	Split out of SetGet code.
// ----------------------------------------------------------------------------

#include "resj/TSDate.h"

int TSDate::getHSecond( void )
{
	return _hsecond;
}
// ----------------------------------------------------------------------------
// TSDate Set/Get Routines
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:
//
// ----------------------------------------------------------------------------
// History:
//
// 07 Nov 1996	MJR, RTi		Added Member functions
// 30 Jan 1997	MJR, RTi		Removed the reset() function to improve
//					performance.
// 23 Sep 1997	Steven A. Malers, RTi	Use new library flags.
// ----------------------------------------------------------------------------

#include "resj/TSDate.h"

int TSDate::getHour( void )
{
	return _hour;
}
// ----------------------------------------------------------------------------
// TSDate.getMinute - get the minute
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:
//
// ----------------------------------------------------------------------------
// History:
//
// 05 Jan 1998	Steven A. Malers, RTi	Split out of SetGet code.
// ----------------------------------------------------------------------------

#include "resj/TSDate.h"

int TSDate::getMinute( void )
{
	return _minute;
}
// ----------------------------------------------------------------------------
// TSDate.getMonth - get the month
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:
//
// ----------------------------------------------------------------------------
// History:
//
// 05 Jan 1998	Steven A. Malers, RTi	Split code out of SetGet file.
// ----------------------------------------------------------------------------

#include "resj/TSDate.h"

int TSDate:: getMonth( void )
{
	return _month;
}
//------------------------------------------------------------------------------
// TSDate::getNumIntervals - returns the number of intervals between two dates.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 15 Nov 1996	Matthew J. Rutherford,	Created member function.
//		RTi
// 23 Sep 1997	Steven A. Malers, RTi	Moved TSGetNumIntervals int a static
//					function inside TSDate.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TSDate.h"

int TSDate::getNumIntervals( TSDate t1, TSDate t2, int base, int mult )
{
	char	routine[]="TSDate::getNumIntervals";
	int	intervals=0;
	TSDate	t;

	if( t2 < t1 ) {
		PrintWarning( 1, routine,
		"End Date (%d/%d/%d) is before Start date (%d/%d/%d)",
		t2.getMonth(), t2.getDay(), t2.getYear(),
		t1.getMonth(), t1.getDay(), t1.getYear() );
		return( 0 );
	}

	//We want to remain less than t2, so if the two dates are the
	//same we will return 0.

	for( t = t1; t < t2; t.addInterval( base, mult ) ) {
		intervals++;
	}

	PrintDebug( 30, routine,
	"%d Intervals (%d,%d) between %d/%d/%d and %d/%d/%d",
	intervals, base, mult,
	t1.getMonth(), t1.getDay(), t1.getYear(),
	t2.getMonth(), t2.getDay(), t2.getYear() );

	return( intervals );
}
// ----------------------------------------------------------------------------
// TSDate.getSecond - get the second
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:
//
// ----------------------------------------------------------------------------
// History:
//
// 05 Jan 1998	Steven A. Malers, RTi	Split out of SetGet file.
// ----------------------------------------------------------------------------

#include "resj/TSDate.h"

int TSDate::getSecond( void )
{
	return _second;
}
// ----------------------------------------------------------------------------
// TSDate.getTimeZone - get the time zone
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:
//
// ----------------------------------------------------------------------------
// History:
//
// 05 Jan 1998	Steven A. Malers, RTi	Get the time zone.
// ----------------------------------------------------------------------------

#include "resj/TSDate.h"

int TSDate::getTimeZone( void )
{
	return _itz;
}
// ----------------------------------------------------------------------------
// TSDate.getTimeZoneAbbr - get the time zone abbreviation
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:
//
// ----------------------------------------------------------------------------
// History:
//
// 05 Jan 1998	Steven A. Malers, RTi	Split out of SetGet file.
// ----------------------------------------------------------------------------

#include "resj/TSDate.h"

char *TSDate::getTimeZoneAbbr( void )
{
	return _tz;
}
// ----------------------------------------------------------------------------
// TSDate.getWeekday - get the weekday
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:
//
// ----------------------------------------------------------------------------
// History:
//
// 09 Mar 2004	James R. VanShaar, RTi	Created original.
// ----------------------------------------------------------------------------

#include "resj/TSDate.h"

int TSDate:: getWeekday( int recalc )
{
	if( recalc ) {
		setWeekday();
	}

	return _weekday;
}
// ----------------------------------------------------------------------------
// TSDate.getYear - get the year
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:
//
// ----------------------------------------------------------------------------
// History:
//
// 05 Jan 1998	Steven A. Malers, RTi	Split out code from SetGet file.
// ----------------------------------------------------------------------------

#include "resj/TSDate.h"

int TSDate::getYear( void )
{
	return _year;
}
// ----------------------------------------------------------------------------
// TSDate.getYearDay - get the day of the year
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:
//
// ----------------------------------------------------------------------------
// History:
//
// 05 Jan 1998	Steven A. Malers, RTi	Split out of SetGet file.
// ----------------------------------------------------------------------------

#include "resj/TSDate.h"

int TSDate::getYearDay()
{
	return _yearday;
}
// ----------------------------------------------------------------------------
// TSDate::init - initialized all the data members to appropriate values
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:
//
// ----------------------------------------------------------------------------
// History:
//
// 07 Nov 1996	Matthew J. Rutherford,	Wrote member function.
//		Riverside Technology,
//		inc.
// 23 Sep 1997	Steven A. Malers, RTi	Add static data initialization.
// 27 Jan 1998	SAM, RTi		Update to include static flags for
//					equals().
// ----------------------------------------------------------------------------
// Variables:	I/O	Description
//
// routine	L	name of routine.
// ----------------------------------------------------------------------------

#include "resj/TSDate.h"

int TSDate::init ( void )
{	char	routine[] = "TSDate::init";

	_hsecond 	= 0;
	_second		= 0;
	_minute		= 0;
	_hour		= 0;
	_day		= 1;
	_month		= 1;
	_year		= 0;
	_isleap		= 0;
	_weekday	= 0;
	_yearday	= 0;
	_abs_month	= 0;
	_itz		= 0;
	_dsflag		= 0;
	_tz		= (char*)NULL;
	_behavior_flag	= 0;

	setTimeZone( "" );

	return STATUS_SUCCESS;
}

// Now initialize the static data...

const unsigned int TSDate::DATE_STRICT	= 0x1;
const unsigned int TSDate::DATE_FAST		= 0x2;
const unsigned int TSDate::DATE_ZERO		= 0x4;

const unsigned int TSDate::YEAR		= 0x1;
const unsigned int TSDate::MONTH		= 0x2;
const unsigned int TSDate::DAY		= 0x4;
const unsigned int TSDate::HOUR		= 0x8;
const unsigned int TSDate::MINUTE		= 0x10;
const unsigned int TSDate::SECOND		= 0x20;
const unsigned int TSDate::HSECOND		= 0x40;
const unsigned int TSDate::TIME_ZONE		= 0x80;
// ----------------------------------------------------------------------------
// TSDate::init - initialized all the data members to appropriate values
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:	(1)	This routine sets the date to zero.  "init" sets to the
//			current time.
// ----------------------------------------------------------------------------
// History:
//
// 05 Jan 1998	Steven A. Malers, RTi	Pull over from Java.
// ----------------------------------------------------------------------------
// Variables:	I/O	Description
//
// routine	L	name of routine.
//
// ----------------------------------------------------------------------------

#include "resj/TSDate.h"

int TSDate::init0 ( void )
{	char	routine[] = "TSDate::init0";

	_hsecond 	= 0;
	_second		= 0;
	_minute		= 0;
	_hour		= 0;
	_day		= 1;
	_month		= 1;
	_year		= 0;
	_isleap		= 1;
	_weekday	= 0;
	_yearday	= 0;
	_abs_month	= 0;
	_itz		= 0;
	_dsflag		= 0;
	_tz		= (char *)NULL;
	_behavior_flag	= 0;

	setTimeZone( "" );

	return 0;
}
// ----------------------------------------------------------------------------
// TSDate.setAbsMonth - set the absolute month
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:
//
// ----------------------------------------------------------------------------
// History:
//
// 05 Jan 1997	Steven A. Malers, RTi	Split out of SetGet code.
// ----------------------------------------------------------------------------

#include "resj/TSDate.h"

int TSDate::setAbsMonth()
{
	_abs_month = ( _year * 12 ) + _month;

	return( 0 );
}
// ----------------------------------------------------------------------------
// TSDate.setDay - set the day
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:
//
// ----------------------------------------------------------------------------
// History:
//
// 05 Jan 1998	Steven A. Malers, RTi	Split code out of SetGet file.
// ----------------------------------------------------------------------------

#include "resj/TSDate.h"

int TSDate::setDay( int day )
{	char	routine[]="TSDate::setDay";

	_day = day;

	if( _behavior_flag & DATE_STRICT ){
                if( _day > NumDaysInMonth( _month, _year ) || _day < 1 ) {
                        PrintWarning( 2, routine,
                        "day has been set to %d for year %d", _day, _year );
                }
	}

	setYearDay();

	return 0;
}
// ----------------------------------------------------------------------------
// TSDate.setHSecond - set the hundredth of a second
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:
//
// ----------------------------------------------------------------------------
// History:
//
// 05 Jan 1998	Steven A. Malers, RTi	Break out of SetGet file.
// ----------------------------------------------------------------------------

#include "resj/TSDate.h"

int TSDate::setHSecond( int hsecond )
{	char	routine[]="TSDate::setHSecond";

	_hsecond = hsecond;

	if( _behavior_flag & DATE_STRICT ){
                if( _hsecond > 99 || _hsecond < 0 ) {
                        PrintWarning( 1, routine,
                        "hsecond has been set to %d", _hsecond );
                }
	}
	return 0;
}
// ----------------------------------------------------------------------------
// TSDate.setHour - set the hour
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:
//
// ----------------------------------------------------------------------------
// History:
//
// 05 Jan 1998	Steven A. Malers, RTi	Break out of SetGet code.
// ----------------------------------------------------------------------------

#include "resj/TSDate.h"

int TSDate::setHour(  int hour )
{	char	routine[]="TSDate::setHour";

	_hour = hour;

	if( _behavior_flag & DATE_STRICT ){
                if( _hour > 23 || _hour < 0 ) {
                        PrintWarning( 2, routine,
                        "hour has been set to %d", _hour );
                }
	}

	return 0;
}
// ----------------------------------------------------------------------------
// TSDate.setMinute - set the minute
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:
//
// ----------------------------------------------------------------------------
// History:
//
// 05 Jan 1998	Steven A. Malers, RTi	Split out of SetGet code.
// ----------------------------------------------------------------------------

#include "resj/TSDate.h"

int TSDate::setMinute( int minute )
{	char	routine[]="TSDate::setMinute";

	_minute = minute;

	if( _behavior_flag & DATE_STRICT ){
                if( _minute > 59 || _minute < 0 ) {
                        PrintWarning( 2, routine,
                        "minute has been set to %d", _minute );
                }
	}

	return 0;
}
// ----------------------------------------------------------------------------
// TSDate.setMonth - set the month
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:
//
// ----------------------------------------------------------------------------
// History:
//
// 05 Jan 1998	Steven A. Malers, RTi	Split code out of SetGet file.
// ----------------------------------------------------------------------------

#include "resj/TSDate.h"

int TSDate::setMonth ( int month )
{	char	routine[]="TSDate::setMonth";

	_month = month;

	if( _behavior_flag & DATE_STRICT ){
                if( _month > 12 || _month < 1 ) {
                        PrintWarning( 2, routine,
                        "month has been set to %d", _month );
                }
	}

	setYearDay();

	setAbsMonth();

	return 0;
}
// ----------------------------------------------------------------------------
// TSDate.setSecond - set the second
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:
//
// ----------------------------------------------------------------------------
// History:
//
// 23 Sep 1997	Steven A. Malers, RTi	Split out of SetGet file.
// ----------------------------------------------------------------------------

#include "resj/TSDate.h"

int TSDate::setSecond( int second )
{	char	routine[]="TSDate::setSecond";

	_second = second;

	if( _behavior_flag & DATE_STRICT ){
                if( _second > 59 || _second < 0 ) {
                        PrintWarning( 2, routine,
                        "second has been set to %d", _second );
                }
	}

	return 0;
}
// ----------------------------------------------------------------------------
// TSDate.setTimeZone - set the time zone
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:
//
// ----------------------------------------------------------------------------
// History:
//
// 05 Jan 1998	Steven A. Malers, RTi	Split code out of SetGet file.
// ----------------------------------------------------------------------------

#include "resj/TSDate.h"
#include "resj/TSTimeZone.h"

int TSDate::setTimeZone( int zone, int dsflag )
{
	_itz = zone;
	_dsflag = dsflag;
	setTimeZone ( TSTimeZone::getDefinedCode(zone, dsflag) );
	return 0;
}

int TSDate::setTimeZone( char *zone )
{	char	routine[]="TSDate::setTimeZone";

	if ( !zone ) {
		return 1;
	}

	if( _tz ) {
		delete [] _tz;
		_tz 	= (char*)NULL;
	}
	_tz = new char [strlen(zone)+1];

	if( !_tz ){
		PrintWarning( 2, routine,
		"Unable to allocate memory for %s", zone );
		return( 1 );
	}

	strcpy( _tz, zone );
	_itz = TSTimeZone::getDefinedNumber ( zone );
	_dsflag = TSTimeZone::getDefinedDSFlag ( zone );

	return 0;
}
// ----------------------------------------------------------------------------
// TSDate.setWeekday - set the weekday
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:
//
// ----------------------------------------------------------------------------
// History:
//
// 09 Mar 2004	James R. VanShaar, RTi	Created original.
// ----------------------------------------------------------------------------

#include "resj/TSDate.h"

int TSDate::setWeekday( )
{	char	routine[]="TSDate::setWeekday";

	// Sunday = 0, Monday = 1, . . . Saturday = 7.

	// January 1, 1900 was a monday, which corresponds to Julian1900 day of
	// one (1) and also weekday one (1).
	int jDay1900;
        //int leap = IsLeapYear ( _year );
        //jDay1900 = _day +                                   /* day of month */
        //        NumDaysInMonthS (1, _year, (_month-1))+ /* days in prev months*/
        //        365*_year +                              /* days in prev years */
        //        _year/4 -                                /* 1 if leap year */
        //        _year/100 +                              /* -3 every 400 years */
        //        _year/400 -                              /* 1 every 400 years */
        //        JULIAN1900DAYS -                      /* Dec 31, 1899 */
        //        leap;                                   /* cancel /4 term -
        //                                                   will be added by
        //                                                   HMNumDaysInMonths
        //                                                   term */
	if(  GetJulianDay1900FromDate( _month, _day, _year, &jDay1900 ) ) {
		// We are in trouble
		printf( "ARGH!!!\n" );
	}

	// Modulo the 1900 Julian day to see what remains.  In this fashion, we
	// remain consistent with the above definition of Sunday = 0, etc.
	_weekday = jDay1900 % 7;

	if( _behavior_flag & DATE_STRICT ){
                if( _weekday < 0 ) {
                        PrintWarning( 2, routine,
                        "weekday has been set to %d", _weekday );
                }
	}

	return 0;
}
// ----------------------------------------------------------------------------
// TSDate.setWeekday - set the weekday
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:
//
// ----------------------------------------------------------------------------
// History:
//
// 09 Mar 2004	James R. VanShaar, RTi	Created original.
// ----------------------------------------------------------------------------

#include "resj/TSDate.h"

int TSDate::setWeekday( int weekday )
{	char	routine[]="TSDate::setWeekday";

	_weekday = weekday;

	if( _behavior_flag & DATE_STRICT ){
                if( _weekday < 0 ) {
                        PrintWarning( 2, routine,
                        "weekday has been set to %d", _weekday );
                }
	}

	return 0;
}

// ----------------------------------------------------------------------------
// TSDate.setYear - set the year
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:
//
// ----------------------------------------------------------------------------
// History:
//
// 05 Jan 1998	Steven A. Malers, RTi	Split code out of SetGet file.
// ----------------------------------------------------------------------------

#include "resj/TSDate.h"

int TSDate::setYear( int year )
{	char	routine[]="TSDate::setYear";

	_year = year;

	if( _behavior_flag & DATE_STRICT ){
                if( _year < 0 ) {
                        PrintWarning( 2, routine,
                        "year has been set to %d", _year );
                }
	}

	setYearDay();

	setAbsMonth();

        _isleap = IsLeapYear( _year );

	return 0;
}
// ----------------------------------------------------------------------------
// TSDate.setYearDay
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:
//
// ----------------------------------------------------------------------------
// History:
//
// 05 Jan 1998	Steven A. Malers, RTi	Split out of SetGet file.
// ----------------------------------------------------------------------------

#include "resj/TSDate.h"

int TSDate::setYearDay()
{	int i;

        // Calculate the year day

        _yearday = 0;

        // Get the days from the previous months

        for( i=1; i<_month; i++ ) {
                _yearday += NumDaysInMonth( i, _year );
        }

        // Add the days from the current month

        _yearday += _day;

	return( 0 );
}
// ----------------------------------------------------------------------------
// TSDate.shiftTimeZone - shift the current time zone to another by adjusting
//				the hour, etc.
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// History:
//
// 05 Jan 1998	Steven A. Malers, RTi	Add to parallel Java version.
// ----------------------------------------------------------------------------
// Variables:	I/O	Description
//
// routine	L	name of routine.
// ----------------------------------------------------------------------------

#include "resj/TSDate.h"
#include "resj/TSTimeZone.h"

int TSDate::shiftTimeZone ( char *tz )
{	char routine[] = "TSDate.shiftTimeZone";

	int offset = TSTimeZone::calculateOffsetHours ( _tz, tz );
	addHour ( offset );
	setTimeZone ( tz );
	return 0;
}
//------------------------------------------------------------------------------
// TSDate::toDouble - generate a floating point representation of a date
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//------------------------------------------------------------------------------
// History:
//
// 03 Mar 1997	Matthew J. Rutherford,	Copied functionality out of FloatYear.
//		Riverside Technology,
//		inc.
// 23 Sep 1997	Steven A. Malers, RTi	Moved TSDoubleYear int TSDate::toDouble.
// 07 Nov 1997	Daniel Weiler, RTi	Fixed year-normalization bug, optimized
// 05 Jan 1998	SAM, RTi		Change so that constants are already
//					multiplied out and don't need to be
//					processed on the fly (the compiler may
//					already take care of this.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
// dt		I/O	floating point representation of a date.
// ydays	L	Number of days in a year.
//------------------------------------------------------------------------------

#include "resj/TSDate.h"

double TSDate::toDouble ( void )
{	double	dt, day;
	double	ydays = (double)NumDaysInYear(_year);

	day	= ((double)(NumDaysInMonthS(1, _year, (_month-1))));

	day	+= (double)(_day - 1);

	// Normalize to day for hours, minutes, seconds, etc.

	day	+= ((double)(_hour))/24.0;
	day	+= ((double)(_minute))/1440.0;		// 60*24
	day	+= ((double)(_second))/86400.0;		// 60*60*24
	day	+= ((double)(_hsecond))/8640000;	// 100*60*60*24

	dt = _year + day/ydays;

	return dt;
}
//------------------------------------------------------------------------------
// TSDate::toNoYearJulianDouble - generate a floating point representation of
//                                the julian day within a year
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//------------------------------------------------------------------------------
// History:
//
// 14 Feb 2006	James R. VanShaar	Original, based on TSDate::ToDouble.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
// dt		I/O	floating point representation of a date.
// ydays	L	Number of days in a year.
//------------------------------------------------------------------------------

#include "resj/TSDate.h"

double TSDate::toNoYearJulianDouble ( void )
{	double	day;

	day	= ((double)(NumDaysInMonthS(1, _year, (_month-1))));

	day	+= (double)(_day - 1);

	// Normalize to day for hours, minutes, seconds, etc.

	day	+= ((double)(_hour))/24.0;
	day	+= ((double)(_minute))/1440.0;		// 60*24
	day	+= ((double)(_second))/86400.0;		// 60*60*24
	day	+= ((double)(_hsecond))/8640000;	// 100*60*60*24

	return day;
}
//----------------------------------------------------------------------------
// TSDate.toString - convert to string representation
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------
// Notes:
//
//----------------------------------------------------------------------------
// History:
//
// 07 Nov 1996	Matthew J. Rutherford,	Copied from ESP TSDate stuff.
//		Riverside Technology,
//		inc.
// 08 Jan 1998	Steven A. Malers, RTi	Split out of Operators code.
//----------------------------------------------------------------------------
// Variables:	I/O	Description
//
// t		I	incoming TSDate
//
//----------------------------------------------------------------------------

#include "resj/TSDate.h"

char * TSDate::toString ( void )
{
	sprintf( _date_string, "%d/%d/%d %02d:%02d:%02d.%02d",
	_month, _day, _year, _hour, _minute, _second, _hsecond );
	return( _date_string );
}
#if 0
//----------------------------------------------------------------------------
// TSDistData Constructors
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------

#include "TSDistData.h"

//----------------------------------------------------------------------------
// TSDistData Constructor
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------
// Notes:
//
//----------------------------------------------------------------------------
// History:
//
// 05 Mar 1998	Daniel Weiler, Riverside Technology, inc.
//					Initial version.
//----------------------------------------------------------------------------
// Variables:		I/O	Description
//
//
//----------------------------------------------------------------------------

TSDistData::TSDistData( ) : TSData()
{
	initialize();
}

TSDistData::TSDistData( const TSDistData& t )
{
	initialize();
	_n_dist = t._n_dist;
	_distrib = t._distrib;
}
//----------------------------------------------------------------------------
// TSDistData Destructor
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------
// Notes:
//
//----------------------------------------------------------------------------
// History:
//
// 22 Sep 1997	Matthew J. Rutherford,	Initial version.
//		Riverside Technology,
//		inc.
// 05 Jan 1998	Steven A. Malers, RTi	Change _quality_flag to _data_flag.
//----------------------------------------------------------------------------
// Variables:	I/O	Description
//
// routine	L	name of function
//----------------------------------------------------------------------------

#include "TSDistData.h"

TSDistData::~TSDistData ( )
{
	if( _distrib ){
		delete [] _distrib;
	}
}
//------------------------------------------------------------------------------
// TSDistData::getDistArray() - returns the internal array.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 07 Apr 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------
#include "TSDistData.h"

float* TSDistData::getDistArray()
{
	return( _distrib );
}
//-----------------------------------------------------------------------------
// TSDistData::getDistribution - get distribution values
//-----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//-----------------------------------------------------------------------------
// Notes:
//
//-----------------------------------------------------------------------------
// History:
//
// 05 Mar 1998	Daniel Weiler,	Created initial version.
//		Riverside Technology, inc.
//-----------------------------------------------------------------------------
// Variables:	I/O	Description
//
// routine	L	name of routine.
//-----------------------------------------------------------------------------

#include "TSDistData.h"

float TSDistData::getDistribution ( int index )
{
	char	routine[] = "TSDistData::getDistribution";

	// Lookup the _dsitrib[index] value
	return( _distrib[index] );
}
//------------------------------------------------------------------------------
// TSDistData::getNDist() - returns the side of the distribution.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 05 May 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------
#include "TSDistData.h"

int TSDistData::getNDist()
{
	return( _n_dist );
}
//-----------------------------------------------------------------------------
// TSDistData::init - initialized all the data members to appropriate values
//-----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//-----------------------------------------------------------------------------
// Notes:
//
//-----------------------------------------------------------------------------
// History:
//
// 05 Mar 1998	Daniel Weiler,	Created initial version.
//		Riverside Technology, inc.
//-----------------------------------------------------------------------------
// Variables:	I/O	Description
//
// routine	L	name of routine.
//-----------------------------------------------------------------------------

#include "TSDistData.h"

int TSDistData::initialize ( )
{	char	routine[] = "TSDistData::initialize";

	_distrib = NULL;

	_n_dist = 0;

	return (STATUS_SUCCESS);
}
//-----------------------------------------------------------------------------
// TSDistData::setDistribuition - sets the _distrib array
//-----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//-----------------------------------------------------------------------------
// Notes:
//
//-----------------------------------------------------------------------------
// History:
//
// 05 Mar 1998	Daniel Weiler,	Created initial version.
//		Riverside Technology, inc.
//-----------------------------------------------------------------------------
// Variables:	I/O	Description
//
// routine	L	name of routine.
//-----------------------------------------------------------------------------

#include "TSDistData.h"

int TSDistData::setDistribution ( float* dist, int n_dist )
{
	char	routine[] = "TSDistData::setDistribution";
	int i;

	// Set the private data members...
	_n_dist = n_dist;
	if( _distrib ) {
		delete [] _distrib;
	}
	_distrib = new float [ n_dist ];

	if( dist == NULL ) {
		PrintDebug( 10, routine, "Distribution is not set, using "
			"default uniform distribution for %d values.", n_dist );
		for( i = 0; i < _n_dist; i++ ) {
			_distrib[i] =  1.0 / (float)n_dist;
		}
	}
	else {
		for( i = 0; i < _n_dist; i++ ) {
			_distrib[i] = dist[i];
		}
	}

	return (STATUS_SUCCESS);
}


//------------------------------------------------------------------------------
// TSIdent constructors
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	The TSIdent constructors allow a TSIdent object to be
//			intantiated with various identifier components.
//------------------------------------------------------------------------------
// History:
//
// 08 Nov 1996	Matthew J. Rutherford,	Created member functions.
//		RTi
// 16 Sep 1997	Steven A. Malers, RTi	Update to use new suite of TSIdent
//					member functions.
// 07 Jan 1998	SAM, RTi		Update to agree with Java version and
//					add time series alias.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

//------------------------------------------------------------------------------
// Default Constructor - default
//------------------------------------------------------------------------------
// History:
//
// 12 Nov 1996	Matthew J. Rutherford, RTi	created member function
//------------------------------------------------------------------------------
TSIdent::TSIdent ( void )
{
	init();
}

// Variant with behavior mask...
TSIdent::TSIdent ( unsigned int mask )
{
	init ();
	setBehaviorMask ( mask );
}

//------------------------------------------------------------------------------
// TSIdent Constructor - create using full identifier
//------------------------------------------------------------------------------
// History:
//
// 12 Nov 1996	Matthew J. Rutherford, RTi	created member function
//------------------------------------------------------------------------------
TSIdent::TSIdent ( char *identifier )
{	char	routine[] = "TSIdent::TSIdent(char*)";

	// Initialize the data members...

	PrintDebug ( 50, routine, "Enter constructor" );
	init();
	setIdentifier ( identifier );

}

// Variant with behavior mask...
TSIdent::TSIdent ( char *identifier, unsigned int mask )
{	char	routine[] = "TSIdent::TSIdent(char*,mask)";

	// Initialize the data members...

	PrintDebug ( 50, routine, "Enter constructor" );
	init();
	setBehaviorMask ( mask );
	setIdentifier ( identifier );
}

//------------------------------------------------------------------------------
// TSIdent Constructor - use separate identifier components
//------------------------------------------------------------------------------
// History:
//
// 12 Nov 1996	Matthew J. Rutherford,	Created member function.
//		RTi
// 16 Sep 1997	Steven A. Malers, RTi	Update to use new suite of member
//					functions.
//------------------------------------------------------------------------------
TSIdent::TSIdent (	char *full_location, char *full_source, char *type,
			char *interval_string, char *scenario )
{	char	routine[] = "TSIdent::TSIdent(parts)";

	PrintDebug ( 50, routine, "Enter constructor" );
	init();
	setIdentifier ( full_location, full_source, type, interval_string,
			scenario );
}

// Variant with behavior mask...
TSIdent::TSIdent (	char *full_location, char *full_source, char *type,
			char *interval_string, char *scenario,
			unsigned int mask )
{	char	routine[] = "TSIdent::TSIdent(parts,mask)";

	PrintDebug ( 50, routine, "Enter constructor" );
	init();
	setBehaviorMask ( mask );
	setIdentifier ( full_location, full_source, type, interval_string,
			scenario );
}

//------------------------------------------------------------------------------
// TSIdent Copy Constructor
//------------------------------------------------------------------------------
// History:
//
// 12 Nov 1996	Matthew J. Rutherford,	Created member function.
//		RTi
//------------------------------------------------------------------------------
TSIdent::TSIdent ( const TSIdent& ts )
{	char	routine[] = "TSIdent::TSIdent(copy)";

	//Identifier will get set from its parts

	PrintDebug ( 50, routine, "Enter copy constructor" );
	init();
	setBehaviorMask ( ts._behavior_mask );
	// Do not use the following!  It triggers infinite recursion!
	//setIdentifier ( ts._identifier );
	setIdentifier ( ts._full_location, ts._full_source, ts._type,
			ts._interval_string, ts._scenario );
	setAlias( ts._alias );
	_interval_base = ts._interval_base;
	_interval_mult = ts._interval_mult;
}
//------------------------------------------------------------------------------
// TSIdent destructor
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This is the destructor for the TSIdent class.  At this
//			time, it deletes all strings that have been allocated.
//------------------------------------------------------------------------------
// History:
//
// 11 Nov 1996	Matthew J. Rutherford,	Initial version.
//		Riverside Technology,
//		inc.
// 16 Sep 1997	Steven A. Malers, RTi	Update to delete data members for
//					enhanced TSIdent class.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

TSIdent::~TSIdent( void )
{
	if( _alias ) {
		delete [] _alias;
	}
	if( _identifier ) {
		delete [] _identifier;
	}
	if( _full_location ) {
		delete [] _full_location;
	}
	if( _main_location ) {
		delete [] _main_location;
	}
	if( _sub_location ) {
		delete [] _sub_location;
	}
	if( _full_source ) {
		delete [] _full_source;
	}
	if( _main_source ) {
		delete [] _main_source;
	}
	if( _sub_source ) {
		delete [] _sub_source;
	}
	if( _type ) {
		delete [] _type;
	}
	if( _interval_string ) {
		delete [] _interval_string;
	}
	if( _scenario ) {
		delete [] _scenario;
	}
}
//------------------------------------------------------------------------------
// TSIdent - overloaded operators
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 12 Nov 1996	Matthew J. Rutherford,	Created member function.
// 16 Sep 1997	Steven A. Malers, RTi	Update to use new data members.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

TSIdent& TSIdent::operator= ( const TSIdent& id )
{	char	routine[] = "TSIdent::operator=";
	int	dl = 50;

	// Don't need to call setIdentifier() because it will be set by its
	// parts.

	PrintDebug ( dl, routine, "Copying data..." );
	setAlias ( id._alias );
	setBehaviorMask ( id._behavior_mask );
	setLocation( id._full_location );
	setSource( id._full_source );
	setType( id._type );
	setInterval( id._interval_string );
	_interval_base = id._interval_base;
	_interval_mult = id._interval_mult;
	setScenario( id._scenario );

	// Now reset the parts...
	setIdentifier();

	PrintDebug ( dl, routine, "...done copying data." );

	return *this;
}

int TSIdent::operator== ( const TSIdent& id )
{
	// Just call the named function...
	return equals ( id );
}

// Named function...
int TSIdent::equals ( const TSIdent& id ) {
	return( !strcmp( _identifier, id._identifier ) );
}

int TSIdent::operator!= ( const TSIdent& id )
{
	// Just call the named function...
	return notEquals ( id );
}

int TSIdent::notEquals ( const TSIdent& id )
{
	if ( equals(id) ) {
		return 0;
	}
	else {	return 1;
	}
}

TSIdent::operator char *( void )
{
	// Call the named function...

	return toString ();
}
//------------------------------------------------------------------------------
// TSIdent::getAlias - get the time series alias
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	The time series alias is returned.
//------------------------------------------------------------------------------
// History:
//
// 07 Jan 1998	Steven A. Malers,	Initial version.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

char * TSIdent::getAlias ( void )
{
	return( _alias );
}
//------------------------------------------------------------------------------
// TSIdent::getBehaviorMask - get the behavior mask
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	The full location is returned and is based on the
//			current value controlled by _behavior_mask.
//------------------------------------------------------------------------------
// History:
//
// 22 Sep 1997	Steven A. Malers, RTi	Initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

unsigned int TSIdent::getBehaviorMask( void )
{
	return( _behavior_mask );
}
//------------------------------------------------------------------------------
// TSIdent::getIdentifier - get the full identifier
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	The full identifier is based on _behavior_mask and is
//			set when identifier components are set.
//------------------------------------------------------------------------------
// History:
//
// 11 Nov 1996	Matthew J. Rutherford,	Initial version.
//		Riverside Technology,
//		inc.
// 16 Sep 1997	Steven A. Malers, RTi	Split code out of SetGet.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

char * TSIdent::getIdentifier( void )
{
	return( _identifier );
}
//------------------------------------------------------------------------------
// TSIdent.getIdentifierFromParts - create identifier string from its parts
//------------------------------------------------------------------------------
// Notes:	(1)	When this routine is called from the TSIdent::ident()
//			function, some fields will be NULL.  In all likelyhood,
//			this routine will be called several times and when all
//			the data are initialized the identifier will be final.
//			So, to make sure that everything works at all stages,
//			deal with the NULLs by ignoring them.
//------------------------------------------------------------------------------
// History:
//
// 17 Sep 1997	Steven A. Malers, RTi	Create function - model after
//					GetTSIDFromParts.
// 07 Jan 1998	SAM, RTi		Minor changes to bring in line with
//					Java.
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

int TSIdent::getIdentifierFromParts (	char *full_location, char *full_source,
					char *type, char *interval_string,
					char *scenario, char *full_identifier )
{	char	routine[] = "TSIdent.getIdentifierFromParts";

	full_identifier[0] = '\0';
	if ( full_location ) {
		strcpy ( full_identifier, full_location );
	}
	strcat ( full_identifier, SEPARATOR );
	if ( full_source ) {
		strcat ( full_identifier, full_source );
	}
	strcat ( full_identifier, SEPARATOR );
	if ( type ) {
		strcat ( full_identifier, type );
	}
	strcat ( full_identifier, SEPARATOR );
	if ( interval_string ) {
		strcat ( full_identifier, interval_string );
	}
	if ( scenario ) {
		if ( scenario[0] != '\0' ) {
			strcat ( full_identifier, SEPARATOR );
			strcat ( full_identifier, scenario );
		}
	}
	return 0;
}
//------------------------------------------------------------------------------
// TSIdent::getInterval - get the full interval string
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine returns the full interval, which is set
//			from its parts.
//------------------------------------------------------------------------------
// History:
//
// 11 Nov 1996	Matthew J. Rutherford,	Initial version.
//		Riverside Technology,
//		inc.
// 16 Sep 1997	Steven A. Malers, RTi	Split code out of SetGet.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

char * TSIdent::getInterval ( void )
{
	return( _interval_string );
}
//------------------------------------------------------------------------------
// TSIdent::getIntervalBase - get the base interval code (integer)
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine returns the interval base.
//------------------------------------------------------------------------------
// History:
//
// 16 Sep 1997	Steven A. Malers, RTi	Initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

int TSIdent::getIntervalBase ( void )
{
	return( _interval_base );
}
//------------------------------------------------------------------------------
// TSIdent::getIntervalMult - get the interval multiplier
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine returns the interval multiplier.
//------------------------------------------------------------------------------
// History:
//
// 16 Sep 1997	Steven A. Malers, RTi	Initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

int TSIdent::getIntervalMult ( void )
{
	return( _interval_mult );
}
//------------------------------------------------------------------------------
// TSIdent::getLocation - get the full location string
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	The full location is returned and is based on the
//			current value controlled by _behavior_mask.
//------------------------------------------------------------------------------
// History:
//
// 11 Nov 1996	Matthew J. Rutherford,	Initial version.
//		Riverside Technology,
//		inc.
// 16 Sep 1997	Steven A. Malers, RTi	Split out of SetGet code.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

char * TSIdent::getLocation( void )
{
	return( _full_location );
}
//------------------------------------------------------------------------------
// TSIdent::getMainLocation - get the main location
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine returns the main location.
//------------------------------------------------------------------------------
// History:
//
// 16 Sep 1997	Steven A. Malers, RTi	Initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

char * TSIdent::getMainLocation( void )
{
	return( _main_location );
}
//------------------------------------------------------------------------------
// TSIdent::getMainSource - get the main source
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine returns the main source.
//------------------------------------------------------------------------------
// History:
//
// 16 Sep 1997	Steven A. Malers, RTi	Initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

char * TSIdent::getMainSource( void )
{
	return( _main_source );
}
//------------------------------------------------------------------------------
// TSIdent::getScenario
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine returns the scenario string.
//------------------------------------------------------------------------------
// History:
//
// 11 Nov 1996	Matthew J. Rutherford,	Initial version.
//		Riverside Technology,
//		inc.
// 16 Sep 1997	Steven A. Malers, RTi	Split out of SetGet code.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

char * TSIdent::getScenario( void )
{
	return( _scenario );
}
//------------------------------------------------------------------------------
// TSIdent::getSource - get the full source string
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine returns the full source string as
//			controlled by the _behavior_mask flag.
//------------------------------------------------------------------------------
// History:
//
// 11 Nov 1996	Matthew J. Rutherford,	Initial version.
//		Riverside Technology,
//		inc.
// 16 Sep 1997	Steven A. Malers, RTi	Break out of SetGet code.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

char * TSIdent::getSource( void )
{
	return( _full_source );
}
//------------------------------------------------------------------------------
// TSIdent::getSubLocation - get the sub-location
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine returns the sub-location, which will be
//			an empty string if _behavior_mask has
//			TSIDENT_NO_SUB_LOCATION set.
//------------------------------------------------------------------------------
// History:
//
// 11 Nov 1996	Matthew J. Rutherford,	Initial version.
//		Riverside Technology,
//		inc.
// 16 Sep 1997	Steven A. Malers, RTi	Break out of SetGet code.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

char * TSIdent::getSubLocation( void )
{
	return( _sub_location );
}
//------------------------------------------------------------------------------
// TSIdent::getSubSource - get the sub-source
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine returns the sub-source, which will be
//			an empty string if _behavior_mask has
//			TSIDENT_NO_SUB_SOURCE set.
//------------------------------------------------------------------------------
// History:
//
// 16 Sep 1997	Steven A. Malers, RTi	Initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

char * TSIdent::getSubSource( void )
{
	return( _sub_source );
}
//------------------------------------------------------------------------------
// TSIdent::getType - get the data type
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	Return the data type.
//------------------------------------------------------------------------------
// History:
//
// 11 Nov 1996	Matthew J. Rutherford,	Initial version.
//		Riverside Technology,
//		inc.
// 16 Sep 1997	Steven A. Malers, RTi	Break out of SetGet code.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

char * TSIdent::getType( void )
{
	return( _type );
}
//------------------------------------------------------------------------------
// TSIdent::init - initialize private data members
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This function should be called from constructors, etc.,
//			when data members need to be initialied.
//------------------------------------------------------------------------------
// History:
//
// 11 Nov 1996	Matthew J. Rutherford,	Initial version.
//		Riverside Technology,
//		inc.
// 16 Sep 1997	Steven A. Malers, RTi	Update for new data members.
// 07 Jan 1998	SAM, RTi		Add _alias.  Make sure that code agrees
//					with Java version.
//------------------------------------------------------------------------------
// Variable	I/O	Description
//
// error_count	L	Counter for errors during initialization.
// routine	L	Name of this routine.
// _*		C	All other variables are TSIdent class variables.
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

int TSIdent::init ( void )
{	char	routine[] = "TSIdent::init";
	int	dl = 50, error_count = 0;

	PrintDebug ( dl, routine, "Initializing TSIdent..." );
	_behavior_mask = 0;	// Default is to process sub-location and
				// sub-source

	// Initialize to null strings so that we do not have problems with the
	// recursive stuff...

	_alias = (char *)NULL;
	_identifier = (char *)NULL;
	_full_location = (char *)NULL;
	_main_location = (char *)NULL;
	_sub_location = (char *)NULL;
	_full_source = (char *)NULL;
	_main_source = (char *)NULL;
	_sub_source = (char *)NULL;
	_type = (char *)NULL;
	_interval_string = (char *)NULL;
	_scenario = (char *)NULL;

	if ( setAlias ("") ) {
		PrintWarning( 1, routine,
		"Unable to initialize alias" );
		++error_count;
	}

	// Initialize the overall identifier to an empty string...

	if ( setFullIdentifier ("") ) {
		PrintWarning( 1, routine,
		"Unable to initialize full identifier" );
		++error_count;
	}

	// Initialize the location components...

	if ( setMainLocation("") ){
		PrintWarning( 1, routine,
		"Unable to initialize main location" );
		++error_count;
	}

	if ( setSubLocation("") ){
		PrintWarning( 1, routine,
		"Unable to initialize sub-location" );
		++error_count;
	}

	// Initialize the source...

	if ( setMainSource("") ){
		PrintWarning( 1, routine,
		"Unable to initialize main source" );
		++error_count;
	}
	if ( setSubSource("") ){
		PrintWarning( 1, routine, "Unable to initialize sub-source" );
		++error_count;
	}

	// Initialize the data type...

	if ( setType("") ){
		PrintWarning( 1, routine, "Unable to initialize type" );
		++error_count;
	}

	// Initialize the interval...

	_interval_base = 0;
	_interval_mult = 0;

	if ( setInterval("") ){
		PrintWarning( 1, routine, "Unable to initialize interval" );
		++error_count;
	}

	// Initialize the scenario...

	if ( setScenario("") ){
		PrintWarning( 1, routine, "Unable to initialize scenario" );
		++error_count;
	}

	PrintDebug ( dl, routine, "...done initializing TSIdent." );
        return error_count;
}
//------------------------------------------------------------------------------
// TSIdent Constructor - create using full identifier
//------------------------------------------------------------------------------
// History:
//
// 17 Sep 1997	Steven A. Malers, RTi	This routine supersedes SplitTSID.
//------------------------------------------------------------------------------
// Variable	I/O	Description
//
// dl		L	Debug level.
// full_location L	Location of TS.
// full_source	L	Source of the time series data.
// i		L	Loop counter for "list" strings.
// identifier	I	5-part time series identifier string.
// interval_string L	TS interval, as string.
// list		L	List of period-separated strings that comprise "tsid".
// nlist*	L	Number of strings in "list".
// quote	L	Quote character used for locations.
// scenario	L	Scenario identifier.
// type		L	TS type.
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

#include <ctype.h>
#include <string.h>

TSIdent TSIdent::parseIdentifier ( char *identifier )
{	char	routine[] = "TSIdent.parseIdenifier(char*)";
	TSIdent	tsident;

	tsident = parseIdentifier ( identifier, 0 );
	return tsident;
}

TSIdent TSIdent::parseIdentifier (	char *identifier,
					unsigned int behavior_flag )
{	char	routine[]="TSIdent::parseIdentifier(char*,flag)";
	int	dl = 50;

	// Declare a TSIdent which we will fill and return...

	PrintDebug ( dl, routine, "Declare TSIdent within this routine..." );
	TSIdent	tsident ( behavior_flag );
	PrintDebug ( dl, routine, "...done declaring TSIdent" );

	// Parse the identifier using the code from SplitTSID...

	char	full_location[256], full_source[256], interval_string[256],
		**list, quote, scenario[256], type[256];
	int	i, nlist1;

	interval_string[0]	= '\0';
	full_location[0]	= '\0';
	full_source[0]		= '\0';
	scenario[0]		= '\0';
	type[0]			= '\0';

	// Figure out whether we are using the new or old conventions.  First
	// check to see if the number of fields is small.  Then check to see if
	// the data type and interval are combined.

	list = BreakStringList ( identifier, ".", 0, &nlist1 );
	for ( i = 0; i < nlist1; i++ ) {
		PrintDebug ( dl, routine,
		"TS ID list[%d]:  \"%s\"", i, list[i] );
	}
	list = FreeStringList ( list );

	PrintDebug ( dl, routine, "Full TS ID:  \"%s\"", identifier );

	// Parse out location and split the rest of the ID...
	//
	// This field is allowed to be surrounded by quotes since some
	// locations cannot be identified by a simple string.  Allow
	// either ' or " to be used and bracket it.

	quote = '\0';
	if ( (identifier[0] == '\'') || (identifier[0] == '\"') ) {
		StringReadToDelim ( &identifier[1], identifier[0],
		full_location );
		// Get the 2nd+ fields...
		quote = identifier[0];
		list =	BreakStringList (&identifier[strlen(full_location)+1],
			".", 0, &nlist1 );
	}
	else {	list =	BreakStringList ( identifier, ".", 0, &nlist1 );
		if ( nlist1 >= 1 ) {
			strcpy ( full_location, list[0] );
		}
	}
	// Data source...
	if ( nlist1 >= 2 ) {
		strcpy ( full_source, list[1] );
	}
	// Data type...
	if ( nlist1 >= 3 ) {
		strcpy ( type, list[2] );
	}
	// Data interval...
	if ( nlist1 >= 4 ) {
		strcpy ( interval_string, list[3] );
	}
	// Scenario...  It is possible that the scenario has delimeters
	// in it.  Therefore, we need to concatenate all the remaining
	// fields to compose the complete scenario...
	if ( nlist1 >= 5 ) {
		strcpy ( scenario, list[4] );
		for ( i = 5; i < nlist1; i++ ) {
			strcat ( scenario, "." );
			strcat ( scenario, list[i] );
		}
	}
	// Now free the memory for the list...
	FreeStringList ( list );
	list = (char **)NULL;
	// Now split the location again into major area (to replace
	// "location") and subarea.  They are divided by an underscore.
	// Only do this for non-quoted locations.
	//
	// This is probably unneeded given the new TSIdent set routine
	// functionality so comment out for now...
	/*
	if ( !quote && strchr(location,'_') ) {
		list = BreakStringList ( location, "_", 0, &nlist1 );
		strcpy ( location, list[0] );
		if ( nlist1 > 1 )
			strcpy ( subarea, list[1] );
		else	*subarea = '\0';
		FreeStringList ( list );
		list = (char **)NULL;
	}
	*/
	PrintDebug ( dl, routine,
	"After split: fullloc=\"%s\" fullsrc=\"%s\" type=\"%s\" int=\"%s\" scen=\"%s\"",
	full_location, full_source, type, interval_string, scenario );

	// Now set the identifier component parts...

	tsident.setLocation ( full_location );
	tsident.setSource ( full_source );
	tsident.setType ( type );
	tsident.setInterval ( interval_string );
	tsident.setScenario ( scenario );

	// Return the TSIdent object for use elsewhere...

	PrintDebug ( dl, routine, "Returning local TSIdent..." );
	return tsident;
}
//------------------------------------------------------------------------------
// TSIdent.setAlias - set the time series alias
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine sets the time series alias.
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Initial version.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variable	I/O	Description
//
// alias	I	Alias to set.
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

int TSIdent::setAlias ( char *alias )
{	char	routine[]="TSIdent.setAlias";
	int	dl = 50;

	if ( !alias ) {
		return 0;
	}

	PrintDebug ( dl, routine, "Trying to set alias to \"%s\"", alias );

	if( _alias ){
        	delete [] _alias;
	}

	_alias = new char[strlen( alias )+1];

	if( !_alias ){
		PrintWarning( 1, routine,
		"Unable to Allocate memory for alias \"%s\"", alias );
                return 1;
	}

	strcpy( _alias, alias );

        return 0;
}
//------------------------------------------------------------------------------
// TSIdent::setBehaviorMask - set the behavior mask for the identifier
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	The behavior mask controls how identifier sub-parts
//			are joined into the full identifier.   Currently this
//			routine does a full reset (not bit-wise).
//------------------------------------------------------------------------------
// History:
//
// 18 Sep 1997	Steven A. Malers, RTi	Initial version.
//------------------------------------------------------------------------------
// Variable	I/O	Description
//
// behavior_mask I	Behavior mask for identifier.
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

int TSIdent::setBehaviorMask( unsigned int behavior_mask )
{	char	routine[] = "TSIdent::setBehaviorMask";

	_behavior_mask = behavior_mask;
	return 0;
}
//------------------------------------------------------------------------------
// TSIdent::setFullIdentifier - set the full identifier
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine sets the full identifier.  It is only
//			called from within this class.
//------------------------------------------------------------------------------
// History:
//
// 17 Sep 1997	Steven A. Malers, RTi	Create routine.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
// full_identifier I	Full identifier.
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

int TSIdent::setFullIdentifier ( char *full_identifier )
{	char	routine[] = "TSIdent::setFullIdentifier";

	if ( !full_identifier ) {
		return 0;
	}

	if( _identifier ) {
        	delete [] _identifier;
	}

	_identifier = new char[strlen( full_identifier )+1];

	if( !_identifier ) {
		PrintWarning( 1, routine,
		"Unable to allocate memory for full identifier \"%s\"",
		full_identifier );
                return( STATUS_FAILURE );
	}

	strcpy( _identifier, full_identifier );

	// DO NOT call setIdentifier() from here!

        return( STATUS_SUCCESS );
}
//------------------------------------------------------------------------------
// TSIdent::setFullLocation - set the full location
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine sets the full location.  It is only called
//			from within this class.
//------------------------------------------------------------------------------
// History:
//
// 17 Sep 1997	Steven A. Malers, RTi	Create routine.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
// full_location I	Main location part of location.
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

int TSIdent::setFullLocation ( char *full_location )
{	char	routine[] = "TSIdent::setFullLocation";
	int	dl = 50;

	if ( !full_location ) {
		return 0;
	}

	PrintDebug ( dl, routine, "Resetting full location to \"%s\"",
	full_location );

	if( _full_location ) {
        	delete [] _full_location;
	}

	_full_location = new char[strlen( full_location )+1];

	if( !_full_location ) {
		PrintWarning( 1, routine,
		"Unable to allocate memory for full location \"%s\"",
		full_location );
                return( STATUS_FAILURE );
	}

	strcpy( _full_location, full_location );

	// DO NOT call setIdentifier() from here!

        return( STATUS_SUCCESS );
}
//------------------------------------------------------------------------------
// TSIdent::setFullSource - set the full source
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine sets the full source.  It is only called
//			from within this class.
//------------------------------------------------------------------------------
// History:
//
// 17 Sep 1997	Steven A. Malers, RTi	Create routine.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
// full_source I	Main source part of source.
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

int TSIdent::setFullSource ( char *full_source )
{	char	routine[] = "TSIdent::setFullSource";
	int	dl = 50;

	if ( !full_source ) {
		return 0;
	}

	PrintDebug ( dl, routine, "Setting full source to \"%s\"",
	full_source );

	if( _full_source ) {
        	delete [] _full_source;
	}

	_full_source = new char[strlen( full_source )+1];

	if( !_full_source ) {
		PrintWarning( 1, routine,
		"Unable to allocate memory for full source \"%s\"",
		full_source );
                return( STATUS_FAILURE );
	}

	strcpy( _full_source, full_source );

	// DO NOT call setIdentifier() from here!

        return( STATUS_SUCCESS );
}
//------------------------------------------------------------------------------
// TSIdent::setIdentifier - set the full identifier
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine forms the full identifier.  Depending on
//			the arguments, this routine may decompose the data sent
//			in and set the subcomponents.  The full identifier is
//			ultimately set by the setIdentifier(void) function.
//------------------------------------------------------------------------------
// History:
//
// 11 Nov 1996	Matthew J. Rutherford,	Initial version.
//		Riverside Technology,
//		inc.
// 17 Sep 1997	Steven A. Malers, RTi	Update to have a void version to reset
//					the identifier.
// 08 Jan 1997	SAM, RTi		Clean up to match Java version.
//------------------------------------------------------------------------------
// Variable	I/O	Description
//
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

// Notes:	(1)	Version to compose the identifier from its components.
int TSIdent::setIdentifier ( void )
{	char	routine[] = "TSIdent::setIdentifier(void)";
	int	dl = 50;

	// We assume that all the individual set routines have handled the
	// _behavior_mask accordingly and therefore we can just concatenate
	// strings here...

	PrintDebug ( dl, routine, "Setting full identifier from parts..." );

	char	full_identifier[256];
	PrintDebug ( dl, routine, "Calling getIdentifierFromParts..." );
	if (	getIdentifierFromParts(_full_location, _full_source, _type,
		_interval_string, _scenario, full_identifier ) ) {
		PrintWarning ( 2, routine,
		"Problem getting full identifier from parts" );
		return 1;
	}
	PrintDebug ( dl, routine,
	"...successfully called getIdentifierFromParts..." );

	setFullIdentifier ( full_identifier );
	return 0;
}

// Notes:	(1)	Version to set the identifier given a full identifier.
//			The identifier will be split first so that the
//			component parts can be set.
int TSIdent::setIdentifier( char *identifier )
{	char	routine[] = "TSIdent::setIdentifier";
	int	dl = 20;

	if ( !identifier ) {
		return 1;
	}

	PrintDebug ( dl, routine, "Trying to set identifier to \"%s\"",
	identifier );

	if ( !identifier[0] ) {
		// We cannot parse the identifier because doing so would get us
		// into an infinite loop.  If this routine is being called with
		// an empty string, it is a mistake.  The initialization code
		// will call setFullIdentifier() directly.
		PrintDebug ( dl, routine,
		"Identifier string is empty, not processing!" );
		return 1;
	}

	// Parse the identifier using the public static function to create a
	// temporary identifier object...

	TSIdent tsident;
	PrintDebug ( dl, routine, "Done declaring temp TSIdent." );
	PrintDebug ( dl, routine, "Parsing identifier..." );
	tsident = parseIdentifier ( identifier, _behavior_mask );
	PrintDebug ( dl, routine, "...back from parsing identifier" );

	// Now copy the temporary copy into this instance...

	PrintDebug ( dl, routine, "Setting the individual parts..." );
	setLocation( tsident.getLocation() );
	setSource( tsident.getSource() );
	setType( tsident.getType() );
	setInterval( tsident.getInterval() );
	setScenario( tsident.getScenario() );
	PrintDebug ( dl, routine, "... done setting the individual parts" );

	// The temporary object will be deleted when we leave this function.
	return 0;
}

// Notes:	(1)	This function sets the idenfier using its component
//			parts.
int TSIdent::setIdentifier (	char *full_location, char *full_source,
				char *type, char *interval_string,
				char *scenario )
{
	setLocation ( full_location );
	setSource ( full_source );
	setType ( type );
	setInterval ( interval_string );
	setScenario ( scenario );
	return 0;
}
//------------------------------------------------------------------------------
// TSIdent.setInterval - set TS identifier interval
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine sets the interval.  Depending on the
//			arguments, it may have to parse the interval into its
//			components.
//------------------------------------------------------------------------------
// History:
//
// 11 Nov 1996	Matthew J. Rutherford,	Initial version.
//		Riverside Technology,
//		inc.
// 17 Sep 1997	Steven A. Malers, RTi	Overload to be more useful.
// 08 Jan 1998	SAM, RTi		Update to use TSInterval to be
//					compatible with Java.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//------------------------------------------------------------------------------

#include "resj/TS.h"
#include "resj/TSIdent.h"
#include "TSInterval.h"

int TSIdent::setInterval ( char *interval_string )
{	char		interval_base_string[256],
			routine[]="TSIdent::setInterval(char*)";
	int		dl = 50;
	TSInterval	tsinterval;

	if ( !interval_string ) {
		return 1;
	}

	PrintDebug ( dl, routine, "Setting interval to \"%s\"...",
	interval_string );

	if ( !interval_string[0] ) {
		// First split the string into its base and multiplier...

		tsinterval = TSInterval::parseInterval ( interval_string );

		// Now set the base and multiplier...

		_interval_base = tsinterval.getBase();
		_interval_mult = tsinterval.getMultiplier();
	}
	// Else, don't do anything (leave as zero initialized values).

	// Now set the interval string.  Use the given interval base string
	// because we need to preserve existing file names, etc.

	setIntervalString ( interval_string );
	setIdentifier();

	return 0;
}

// Notes:	(1)	Set the interval from the base and multiplier, both
//			as integers.
//		(2)	Need to put in checks at some point to check for valid
//			multipliers given the base, but for now assume that
//			any positive multiplier is valid.
int TSIdent::setInterval ( int interval_base, int interval_mult )
{	char	routine[] = "TSIdent.setInterval";

	if ( interval_mult <= 0 ) {
		PrintWarning ( 2, routine,
		"Interval multiplier (%d) must be greater than zero",
		interval_mult );
		return 1;
	}
	if (	(interval_base != TS::INTERVAL_SECOND) &&
		(interval_base != TS::INTERVAL_MINUTE) &&
		(interval_base != TS::INTERVAL_HOUR) &&
		(interval_base != TS::INTERVAL_DAY) &&
		(interval_base != TS::INTERVAL_WEEK) &&
		(interval_base != TS::INTERVAL_MONTH) &&
		(interval_base != TS::INTERVAL_YEAR) &&
		(interval_base != TS::INTERVAL_IRREGULAR) ) {
		PrintWarning ( 2, routine,
		"Base interval (%d) is not recognized", interval_base );
		return 1;
	}
	_interval_base = interval_base;
	_interval_mult = interval_mult;

	// Now we need to set the string representation of the interval...

	char	interval_string[256];
	interval_string[0] = '\0';
	if (	(interval_base != TS::INTERVAL_IRREGULAR) ) {
		sprintf ( interval_string, "%d", interval_mult );
	}

	if ( interval_base == TS::INTERVAL_SECOND ) {
		strcat ( interval_string, "sec" );
	}
	else if	( interval_base == TS::INTERVAL_MINUTE ) {
		strcat ( interval_string, "min" );
	}
	else if	( interval_base == TS::INTERVAL_HOUR ) {
		strcat ( interval_string, "hour" );
	}
	else if	( interval_base == TS::INTERVAL_DAY ) {
		strcat ( interval_string, "day" );
	}
	else if	( interval_base == TS::INTERVAL_WEEK ) {
		strcat ( interval_string, "week" );
	}
	else if	( interval_base == TS::INTERVAL_MONTH ) {
		strcat ( interval_string, "month" );
	}
	else if	( interval_base == TS::INTERVAL_YEAR ) {
		strcat ( interval_string, "year" );
	}
	else if	( interval_base == TS::INTERVAL_IRREGULAR ) {
		strcat ( interval_string, "irreg" );
	}

	setIntervalString ( interval_string );
	setIdentifier ();
	return 0;
}
//------------------------------------------------------------------------------
// TSIdent::setIntervalString - set the full interval string
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine sets the interval string.  It is only
//			called from within this class.
//------------------------------------------------------------------------------
// History:
//
// 17 Sep 1997	Steven A. Malers, RTi	Create routine.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
// interval_string I	Main location part of location.
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

int TSIdent::setIntervalString ( char *interval_string )
{	char	routine[] = "TSIdent::setIntervalString";

	if ( !interval_string ) {
		return 0;
	}

	if( _interval_string ) {
        	delete [] _interval_string;
	}

	_interval_string = new char[strlen( interval_string )+1];

	if( !_interval_string ) {
		PrintWarning( 1, routine,
		"Unable to allocate memory for interval string \"%s\"",
		interval_string );
                return( STATUS_FAILURE );
	}

	strcpy( _interval_string, interval_string );

        return 0;
}
//------------------------------------------------------------------------------
// TSIdent.setLocation - set the full location
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine sets the full location.  Depending on
//			the information specified, this may also set the
//			location parts.
//------------------------------------------------------------------------------
// History:
//
// 11 Nov 1996	Matthew J. Rutherford,	Initial version.
//		Riverside Technology,
//		inc.
// 17 Sep 1997	Steven A. Malers, RTi	Break out of SetGet code.
//------------------------------------------------------------------------------
// Variable	I/O	Description
//
// location	I	The full location.
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

// Notes:	(1)	Reset the location from its component parts.  This
//			routine is generally called from setMainLocation and
//			setSubLocation to reset _full_location.
int TSIdent::setLocation ( void )
{	char	routine[] = "TSIdent.setLocation(void)";
	int	dl = 50;

	PrintDebug ( dl, routine, "Resetting full location from parts..." );
	if ( _behavior_mask & NO_SUB_LOCATION ) {
		// Just use the main location as the full location...
		if ( _main_location ) {
			// There should always be a main location after the
			// object is initialized...
			setFullLocation ( _main_location );
		}
	}
	else {	// Concatenate the main and sub-locations to get the full
		// location.
		char	full_location[256];
		// We may want to check for _main_location[] also...
		if ( _main_location ) {
			// This should always be the case after the object is
			// initialized...
			strcpy ( full_location, _main_location );
			if ( _sub_location ) {
				// We only want to add the sublocation if it is
				// not an empty string (it will be an empty
				// string after the object is initialized).
				if ( _sub_location[0] ) {
					// We have a sub_location so append it
					// to the main location...
					strcat ( full_location,
					LOCATION_SEPARATOR );
					strcat ( full_location, _sub_location );
				}
			}
			setFullLocation ( full_location );
		}
	}
	// Now reset the full identifier...
	setIdentifier ();
	return 0;
}

// Set the location from the component parts...
int TSIdent::setLocation ( char *main_location, char *sub_location )
{
	setMainLocation ( main_location );
	setSubLocation ( sub_location );
	// The full location will be set when the parts are set.
	return 0;
}

// Notes:	(1)	We first split the location into its components and
//			then set the sub-components.
int TSIdent::setLocation( char *location )
{	char	routine[] = "TSIdent::setLocation(char*)";
	int	dl = 20;

	if ( !location ) {
		return 0;
	}

	PrintDebug ( dl, routine, "Trying to set location to \"%s\"",
	location );

	if ( _behavior_mask & NO_SUB_LOCATION ) {
		// The entire string passed in is used for the main location...
		setMainLocation ( location );
	}
	else {	// Need to split the location into main and sub-location...
		char	**list, sub_location[256];
		int	nlist;
		list =	BreakStringList ( location,
			LOCATION_SEPARATOR, 0, &nlist );
		if ( nlist >= 1 ) {
			// Set the main location...
			setMainLocation ( list[0] );
		}
		if ( nlist >= 2 ) {
			// Now set the sub-location...
			sub_location[0] = '\0';
			int iend = nlist - 1;
			for ( int i = 1; i <= iend; i++ ) {
				strcat ( sub_location, list[i] );
				if ( i != iend ) {
					strcat ( sub_location,
					LOCATION_SEPARATOR );
				}
			}
			setSubLocation ( sub_location );
		}
		else {	// Since we are only setting the main location we
			// need to set the sub-location to an empty string...
			setSubLocation ( "" );
		}
		list = FreeStringList( list );
	}

	return 0;
}
//------------------------------------------------------------------------------
// TSIdent::setMainLocation - set the main location
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine sets the main location and then triggers
//			a reset of the full location.
//------------------------------------------------------------------------------
// History:
//
// 11 Nov 1996	Matthew J. Rutherford,	Initial version.
//		Riverside Technology,
//		inc.
// 17 Sep 1997	Steven A. Malers, RTi	Break out of SetGet code.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
// main_location I	Main location part of location.
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

int TSIdent::setMainLocation ( char *main_location )
{	char	routine[] = "TSIdent::setMainLocation";
	int	dl = 50;

	if ( !main_location ) {
		return 0;
	}

	PrintDebug ( dl, routine, "Trying to set main location to \"%s\"",
	main_location );

	if( _main_location ) {
        	delete [] _main_location;
	}

	_main_location = new char[strlen( main_location )+1];

	if( !_main_location ) {
		PrintWarning( 1, routine,
		"Unable to allocate memory for main location \"%s\"",
		main_location );
                return 1;
	}

	strcpy( _main_location, main_location );

	setLocation();

	return 0;
}
//------------------------------------------------------------------------------
// TSIdent::setMainSource - set the main source
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine sets the main source and then triggers
//			a reset of the full source.
//------------------------------------------------------------------------------
// History:
//
// 17 Sep 1997	Steven A. Malers, RTi	Initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
// main_source I	Main source part of source.
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

int TSIdent::setMainSource ( char *main_source )
{	char	routine[] = "TSIdent.setMainSource";
	int	dl = 50;

	if ( !main_source ) {
		return 0;
	}

	PrintDebug ( dl, routine, "Setting main source to \"%s\"",
	main_source );

	if( _main_source ) {
        	delete [] _main_source;
	}

	_main_source = new char[strlen( main_source )+1];

	if( !_main_source ) {
		PrintWarning( 1, routine,
		"Unable to allocate memory for main source \"%s\"",
		main_source );
		return 1;
	}

	strcpy( _main_source, main_source );

	setSource();

        return 0;
}
//------------------------------------------------------------------------------
// TSIdent::setScenario - set the scenario part of the identifier
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
// 17 Sep 1997	Steven A. Malers, RTi	Split code out of SetGet.
//------------------------------------------------------------------------------
// Variable	I/O	Description
//
// scenario	I	Scenario to use in identifier.
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

int TSIdent::setScenario( char *scenario )
{	char	routine[] = "TSIdent.setScenario";
	int	dl = 50;

	if ( !scenario ) {
		return 0;
	}

	PrintDebug ( dl, routine, "Setting scenario to \"%s\"...", scenario );

	if( _scenario ) {
		delete [] _scenario;
	}

	_scenario = new char[strlen( scenario )+1];

	if( !_scenario ){
		PrintWarning( 1, routine,
		"Unable to allocate memory for scenario \"%s\"", scenario );
		return 1;
	}

	strcpy( _scenario, scenario );

	setIdentifier ();

        return 0;
}
//------------------------------------------------------------------------------
// TSIdent::setSource - set the full source
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine sets the full source.  Depending on
//			the information specified, this may also set the
//			source parts.
//------------------------------------------------------------------------------
// History:
//
// 11 Nov 1996	Matthew J. Rutherford,	Initial version.
//		Riverside Technology,
//		inc.
// 17 Sep 1997	Steven A. Malers, RTi	Break out of SetGet code.
// 08 Jan 1998	SAM, RTi		Update to be compatible with Java.
//------------------------------------------------------------------------------
// Variable	I/O	Description
//
// source	I	The full source.
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

// Notes:	(1)	Reset the source from its component parts.  This
//			routine is generally called from setMainSource and
//			setSubSource to reset _full_source.
int TSIdent::setSource ( void )
{	char	routine[] = "TSIdent.setSource";
	int	dl = 50;

	PrintDebug ( dl, routine, "Resetting full source from its parts" );

	if ( _behavior_mask & NO_SUB_SOURCE ) {
		// Just use the main source as the full source...
		if ( _main_source ) {
			// There should always be a main source after the
			// object is initialized...
			setFullSource ( _main_source );
		}
	}
	else {	// Concatenate the main and sub-sources to get the full
		// source.
		char	full_source[256];
		if ( _main_source ) {
			// We only want to add the subsource if it is not an
			// empty string (it will be an empty string after the
			// object is initialized).
			strcpy ( full_source, _main_source );
			if ( _sub_source ) {
				// We have sub_source so append it to the main
				// source...
				// We have a sub_source so append it to the
				// main source...
				if ( _sub_source[0] ) {
					strcat ( full_source,
					SOURCE_SEPARATOR );
					strcat ( full_source, _sub_source );
				}
			}
			setFullSource ( full_source );
		}
	}
	// Now reset the full identifier...
	setIdentifier ();
	return 0;
}

// Set the source from the component parts...
int TSIdent::setSource ( char *main_source, char *sub_source )
{	char	routine[] = "TSIdent.setSource(char*,char*)";
	int	dl = 50;

	PrintDebug ( dl, routine, "Setting source using \"%s\" and \"%s\"",
	main_source, sub_source );
	setMainSource ( main_source );
	setSubSource ( sub_source );
	// The full source will be set when the parts are set.
	return 0;
}

// Notes:	(1)	We first split the source into its components and
//			then set the sub-components.
int TSIdent::setSource( char *source )
{	char	routine[] = "TSIdent.setSource(char*)";
	int	dl = 50;

	if ( !source ) {
		return 0;
	}

	PrintDebug ( dl, routine, "Setting source using \"%s\"", source );

	if ( _behavior_mask & NO_SUB_SOURCE ) {
		// The entire string passed in is used for the main source...
		setMainSource ( source );
	}
	else {	// Need to split the source into main and sub-source...
		char	**list, sub_source[256];
		int	nlist;
		list =	BreakStringList ( source,
			SOURCE_SEPARATOR, 0, &nlist );
		if ( nlist >= 1 ) {
			// Set the main source...
			setMainSource ( list[0] );
		}
		if ( nlist >= 2 ) {
			// Now set the sub-source...
			sub_source[0] = '\0';
			int iend = nlist - 1;
			for ( int i = 1; i <= iend; i++ ) {
				strcat ( sub_source, list[i] );
				if ( i != iend ) {
					strcat ( sub_source,
					SOURCE_SEPARATOR );
				}
			}
			setSubSource ( sub_source );
		}
		else {	// Since we are only setting the main location we
			// need to set the sub-location to an empty string...
			setSubSource ( "" );
		}
	}

        return 0;
}
//------------------------------------------------------------------------------
// TSIdent::setSubLocation - set the sub-location part of the location
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine sets the sublocation and then calls
//			setLocation() to reset the full location.
//------------------------------------------------------------------------------
// History:
//
// 11 Nov 1996	Matthew J. Rutherford,	Initial version.
//		Riverside Technology,
//		inc.
// 17 Sep 1997	Steven A. Malers, RTi	Break out of SetGet code.
// 08 Jan 1998	SAM, RTi		Update to agree with Java.
//------------------------------------------------------------------------------
// Variable	I/O	Description
//
// sub_location	I	Sub-location to set.
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

int TSIdent::setSubLocation ( char *sub_location )
{	char	routine[]="TSIdent.setSubLocation";
	int	dl = 50;

	if ( !sub_location ) {
		return 0;
	}

	PrintDebug ( dl, routine, "Trying to set sub-location to \"%s\"",
	sub_location );

	if( _sub_location ){
        	delete [] _sub_location;
	}

	_sub_location = new char[strlen( sub_location )+1];

	if( !_sub_location ){
		PrintWarning( 1, routine,
		"Unable to Allocate memory for sub-location \"%s\"",
		sub_location );
                return 1;
	}

	strcpy( _sub_location, sub_location );

	setLocation();

        return 0;
}
//------------------------------------------------------------------------------
// TSIdent.setSubSource - set the sub-source part of the source
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine sets the subsource and then calls
//			setsource() to reset the full source.
//------------------------------------------------------------------------------
// History:
//
// 17 Sep 1997	Steven A. Malers, RTi	Initial version.
// 08 Jan 1997	SAM, RTi		Update to be consistent with Java.
//------------------------------------------------------------------------------
// Variable	I/O	Description
//
// sub_source	I	Sub-source to set.
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

int TSIdent::setSubSource ( char *sub_source )
{	char	routine[]="TSIdent.setSubSource";
	int	dl = 50;

	if ( !sub_source ) {
		return 0;
	}

	PrintDebug ( dl, routine, "Setting sub-source to \"%s\"",
	sub_source );

	if( _sub_source ){
        	delete [] _sub_source;
	}

	_sub_source = new char[strlen( sub_source )+1];

	if( !_sub_source ){
		PrintWarning( 1, routine,
		"Unable to Allocate memory for sub-source \"%s\"",
		sub_source );
                return 1;
	}

	strcpy( _sub_source, sub_source );

	setSource();

        return 0;
}
//------------------------------------------------------------------------------
// TSIdent.setType - set the data type part of the TS identifier
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	Data types are the NWS data types or other
//			abbreviations.  Maybe we need a _behavior_flag setting
//			that forces a check for a valid data type?
//------------------------------------------------------------------------------
// History:
//
// 11 Nov 1996	Matthew J. Rutherford,	Initial version.
//		Riverside Technology,
//		inc.
// 17 Sep 1997	Steven A. Malers, RTi	Break out of SetGet code.
// 08 Jan 1998	SAM, RTi		Update to agree with Java port.
//------------------------------------------------------------------------------
// Variable	I/O	Description
//
// type		I	Time series data type.
//------------------------------------------------------------------------------

#include "resj/TSIdent.h"

int TSIdent::setType ( char *type )
{	char 	routine[] = "TSIdent.setType";
	int	dl = 50;

	if ( !type ) {
		return 0;
	}

	PrintDebug ( dl, routine, "Setting type to \"%s\"...", type );

	if( _type ) {
        	delete [] _type;
	}

	_type = new char[strlen( type )+1];

	if( !_type ){
		PrintWarning( 1, routine,
		"Unable to allocate memory for data type \"%s\"", type );
                return 1;
	}

	strcpy( _type, type );

	setIdentifier();

        return 0;
}
//----------------------------------------------------------------------------
// TSIdent.toString - convert to string representation
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------
// Notes:
//
//----------------------------------------------------------------------------
// History:
//
// 07 Jan 1998	Steven A. Malers,	Initial version.
//		Riverside Technology,
//		inc.
//----------------------------------------------------------------------------
// Variables:	I/O	Description
//
// id		I	Incoming TSIdent.
//----------------------------------------------------------------------------

#include "resj/TSIdent.h"

char TSIdentString[256];  //cfan

char * TSIdent::toString ( void )
{
//cfan	char	string[256];

//cfan	sprintf( string,
	sprintf( TSIdentString,  //cfan
	"Loc=\"%s\",Src=\"%s\",Type=\"%s\",Int=\"%s\",Scen=\"%s\"",
	_full_location, _full_source, _type, _interval_string, _scenario );

//cfan	return string;
	return ( TSIdentString );  //cfan
}

// ----------------------------------------------------------------------------
// TSInterval Constructors
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:
//
// ----------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Port from Java version.
//		Riverside Technology,
//		inc.
// ----------------------------------------------------------------------------
// Variables:		I/O	Description
//
// ----------------------------------------------------------------------------

#include "TSInterval.h"

TSInterval::TSInterval ( void )
{
	init ();
}

// ----------------------------------------------------------------------------
// TSLimits Copy Constructor
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:
//
// ----------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	SAM, RTi	Port from Java.
// ----------------------------------------------------------------------------
// Variables:	I/O	Description
//
// t		I	incoming date
// ----------------------------------------------------------------------------

TSInterval::TSInterval ( const TSInterval& interval )
{	char	routine[] = "TSInterval.CopyConstructor";

	init ();

 	_interval_base = interval._interval_base;
 	_interval_mult = interval._interval_mult;
 	setBaseString ( interval._interval_base_string );
}
// ----------------------------------------------------------------------------
// TSInterval Destructor
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:
//
// ----------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Initial version.
//		Riverside Technology,
//		inc.
// ----------------------------------------------------------------------------
// Variable	I/O	Description
//
// routine	L	name of function
// ----------------------------------------------------------------------------

#include "TSInterval.h"

TSInterval::~TSInterval ( void )
{	char	routine[] = "TSInterval::~TSInterval";

	delete [] _interval_base_string;
}
//----------------------------------------------------------------------------
// TSInterval Operators
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------
// Notes:	(1)	For Java, we do not have overloaded operators, but
//			for the C++ versions we have now implemented Java-like
//			functions so that it is easier to keep the C++ and
//			Java versions compatible.
//----------------------------------------------------------------------------

#include "TSInterval.h"

//----------------------------------------------------------------------------
// TSInterval overload = operator
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------
// Notes:
//
//----------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Initial C++ version.
//		Riverside Technology,
//		inc.
//----------------------------------------------------------------------------
// Variables:	I/O	Description
//
// interval	I	Incoming TSInterval.
//----------------------------------------------------------------------------

TSInterval& TSInterval::operator= ( const TSInterval& interval )
{
	if ( &interval == this) {
                // self declaration
        }
        else {	_interval_base		= interval._interval_base;
		_interval_mult		= interval._interval_mult;
		setBaseString ( interval._interval_base_string );
        }
	return *this;
}

TSInterval::operator char *( void )
{
	// Call the named function...

	return toString ();
}
//------------------------------------------------------------------------------
// TSInterval.getBase
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine returns the interval base as an int.
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers, RTi	Port to C++ from Java.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//------------------------------------------------------------------------------

#include "TSInterval.h"

int TSInterval::getBase( void )
{
	return _interval_base;
}
//------------------------------------------------------------------------------
// TSInterval::getBaseString
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine returns the interval base as a string.
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers, RTi	Port to C++ from Java.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//------------------------------------------------------------------------------

#include "TSInterval.h"

char * TSInterval::getBaseString( void )
{
	return _interval_base_string;
}
//------------------------------------------------------------------------------
// TSInterval.getMultiplier
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine returns the multiplier as an int.
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers, RTi	Port to C++ from Java.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//------------------------------------------------------------------------------

#include "TSInterval.h"

int TSInterval::getMultiplier( void )
{
	return _interval_mult;
}
//------------------------------------------------------------------------------
// TSInterval.getName
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine returns the character name corresponding
//			to an interval.  In the future, we may want to
//			overload to allow abbreviations, etc., and perhaps to
//			concatenate the multiplier.
//		(2)	Also need a static array to cross-reference the
//			integer intervals with the character string names.
//------------------------------------------------------------------------------
// History:
//
// 04 Feb 1998	Steven A. Malers, RTi	Initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//------------------------------------------------------------------------------

#include "TSInterval.h"
#include "resj/TS.h"

char * TSInterval::getName ( int interval )
{	char name[32];

	name[0] = '\0';
	if ( interval == TS::INTERVAL_YEAR ) {
		strcpy ( name, "Year" );
	}
	else if ( interval == TS::INTERVAL_MONTH ) {
		strcpy ( name, "Month" );
	}
	else if ( interval == TS::INTERVAL_DAY ) {
		strcpy ( name, "Day" );
	}
	else if ( interval == TS::INTERVAL_DAY ) {
		strcpy ( name, "Day" );
	}
	else if ( interval == TS::INTERVAL_HOUR ) {
		strcpy ( name, "Hour" );
	}
	else if ( interval == TS::INTERVAL_MINUTE ) {
		strcpy ( name, "Minute" );
	}
	else if ( interval == TS::INTERVAL_SECOND ) {
		strcpy ( name, "Second" );
	}
	char *pt = name;
	return pt;
}
// ----------------------------------------------------------------------------
// TSInterval.init - initialize the data
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers, RTi	Port from Java.
// ----------------------------------------------------------------------------

#include "TSInterval.h"

int TSInterval::init ()
{
	_interval_base = 0;
	_interval_base_string = (char *)NULL;
	setBaseString ( "" );
	_interval_mult = 0;
	return 0;
}
//------------------------------------------------------------------------------
// TSInterval.parseInterval - parses an interval string
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine takes a string like "12min" and splits
//			it into the base and multiplier for the data interval.
//			Several different long and abbreviated names intervals
//			accepted.
//------------------------------------------------------------------------------
// History:
//
// 03 Dec 96	Matthew J. Rutherford,	Initial version for ESPADP.
//		Riverside Technology,
//		inc.
// 12 Jun 97	Steven A. Malers, RTi	Copy to TS++ and add minute
//					capability.
// 17 Sep 1997	SAM, RTi		Rename TSIntervalFromString to
//					TSIdent::parseInterval and also pass
//					back the interval base as a string.
//					Use new static data flags.
// 07 Jan 1998	SAM, RTi		Update to agree with Java.  Return a
//					TSInterval.  This code is being
//					moved to the TSInterval class.
// 08 Jan 1998	SAM, RTi		Code is now in TSInterval.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
// base		O	Base interval.
// base_string	O	Base interval as a string.
// digit	L	The part of "string" that is the multiplier.
// digit_count	L	The number of characters in "digit".
// dl		L	Debug level for this routine.
// i		L	Loop counter for characters in "string".
// interval_string L	String containing interval to be split.
// length	L	Length of "string".
// mult		O	Interval multiplier.
// routine	L	Name of this routine.
//------------------------------------------------------------------------------

#include "ResJ.h"
#include "resj/TS.h"
#include "TSInterval.h"
#include <string.h>
#include <ctype.h>

TSInterval TSInterval::parseInterval ( char* interval_string )
{	char		digit[250], routine[] = "TSInterval.parseInterval";
	int		digit_count=0, dl = 30, i=0, length;
	TSInterval	tsinterval;

	length = strlen( interval_string );

	//
	// Need to strip of any leading digits.
	//

	while( i < length ){
		if( isdigit(interval_string[i]) ){
			digit_count++;
			i++;
		}
		else {	// We have reached the end of the digit part
			// of the string.
			break;
		}
	}

	if( digit_count == 0 ){
		//
		// The string had no leading digits, interpret as one.
		//
		tsinterval.setMultiplier ( 1 );
	}
	else if( digit_count == length ){
		//
		// The whole string is a digit.
		//
		tsinterval.setBase ( TS::INTERVAL_HOUR );
		tsinterval.setMultiplier ( atoi( interval_string ) );

		PrintDebug( dl, routine, "%d Hourly",
		tsinterval.getMultiplier() );
		return tsinterval;
	}
	else {	strncpy( digit, interval_string, digit_count );

		digit[digit_count] = '\0';

		tsinterval.setMultiplier ( atoi( digit ) );
	}

	PrintDebug ( dl, routine, "Multiplier: %d",
	tsinterval.getMultiplier() );

	//
	// Now parse out the Base interval
	//

	char base_string[256];
	base_string[0] = '\0';
	if(	!ESPstrncasecmp( &interval_string[digit_count], "minute", 6) ) {
		strncpy ( base_string, &interval_string[digit_count], 6 );
		base_string[6] = '\0';
		tsinterval.setBaseString ( base_string );
		tsinterval.setBase ( TS::INTERVAL_MINUTE );
	}
	else if (!ESPstrncasecmp( &interval_string[digit_count], "min", 3 ) ) {
		strncpy ( base_string, &interval_string[digit_count], 3 );
		base_string[3] = '\0';
		tsinterval.setBaseString ( base_string );
		tsinterval.setBase ( TS::INTERVAL_MINUTE );
	}
	else if( !ESPstrncasecmp( &interval_string[digit_count], "hour", 4 ) ) {
		strncpy ( base_string, &interval_string[digit_count], 4 );
		base_string[4] = '\0';
		tsinterval.setBaseString ( base_string );
		tsinterval.setBase ( TS::INTERVAL_HOUR );
	}
	else if(!ESPstrncasecmp( &interval_string[digit_count], "hr", 2 ) ) {
		strncpy ( base_string, &interval_string[digit_count], 2 );
		base_string[2] = '\0';
		tsinterval.setBaseString ( base_string );
		tsinterval.setBase ( TS::INTERVAL_HOUR );
	}
	else if( !ESPstrncasecmp( &interval_string[digit_count], "day", 3 ) ){
		strncpy ( base_string, &interval_string[digit_count], 3 );
		base_string[3] = '\0';
		tsinterval.setBaseString ( base_string );
		tsinterval.setBase ( TS::INTERVAL_DAY );
	}
	else if( !ESPstrncasecmp( &interval_string[digit_count], "week", 4) ){
		strncpy ( base_string, &interval_string[digit_count], 4 );
		base_string[4] = '\0';
		tsinterval.setBaseString ( base_string );
		tsinterval.setBase ( TS::INTERVAL_WEEK );
	}
	else if(!ESPstrncasecmp( &interval_string[digit_count], "wk", 2) ){
		strncpy ( base_string, &interval_string[digit_count], 2 );
		base_string[2] = '\0';
		tsinterval.setBaseString ( base_string );
		tsinterval.setBase ( TS::INTERVAL_WEEK );
	}
	else if( !ESPstrncasecmp( &interval_string[digit_count], "month", 5 ) ) {
		strncpy ( base_string, &interval_string[digit_count], 5 );
		base_string[5] = '\0';
		tsinterval.setBaseString ( base_string );
		tsinterval.setBase ( TS::INTERVAL_MONTH );
	}
	else if (!ESPstrncasecmp( &interval_string[digit_count], "mon", 3 ) ){
		strncpy ( base_string, &interval_string[digit_count], 3 );
		base_string[3] = '\0';
		tsinterval.setBaseString ( base_string );
		tsinterval.setBase ( TS::INTERVAL_MONTH );
	}
	else if( !ESPstrncasecmp( &interval_string[digit_count], "year", 4 ) ) {
		strncpy ( base_string, &interval_string[digit_count], 4 );
		base_string[4] = '\0';
		tsinterval.setBaseString ( base_string );
		tsinterval.setBase ( TS::INTERVAL_YEAR );
	}
	else if (!ESPstrncasecmp( &interval_string[digit_count], "yr", 2 ) ){
		strncpy ( base_string, &interval_string[digit_count], 2 );
		base_string[2] = '\0';
		tsinterval.setBaseString ( base_string );
		tsinterval.setBase ( TS::INTERVAL_YEAR );
	}
	else if (!ESPstrncasecmp( &interval_string[digit_count], "irregular", 9 ) ){
		strncpy ( base_string, &interval_string[digit_count], 9 );
		base_string[9] = '\0';
		tsinterval.setBaseString ( base_string );
		tsinterval.setBase ( TS::INTERVAL_IRREGULAR );
	}
	else if (!ESPstrncasecmp( &interval_string[digit_count], "irreg", 5 ) ){
		strncpy ( base_string, &interval_string[digit_count], 5 );
		base_string[5] = '\0';
		tsinterval.setBaseString ( base_string );
		tsinterval.setBase ( TS::INTERVAL_IRREGULAR );
	}
	else if (!ESPstrncasecmp( &interval_string[digit_count], "irr", 3 ) ){
		strncpy ( base_string, &interval_string[digit_count], 3 );
		base_string[3] = '\0';
		tsinterval.setBaseString ( base_string );
		tsinterval.setBase ( TS::INTERVAL_IRREGULAR );
	}
	else {	PrintWarning( 2, routine,
		"Unrecognized Interval \"%s\", returning HOURLY",
		&interval_string[digit_count] );
		tsinterval.setBase ( TS::INTERVAL_HOUR );
	}

	PrintDebug( dl, routine, "Base: %d (%s), Mult: %d",
	tsinterval.getBase (),
	tsinterval.getBaseString (),
	tsinterval.getMultiplier () );

	return tsinterval;
}
// ----------------------------------------------------------------------------
// TSInterval.setBase - set the base interval
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// History:
//
// 06 Jan 1998	Steven A. Malers, RTi	Port from Java.
// ----------------------------------------------------------------------------

#include "TSInterval.h"

int TSInterval::setBase ( int interval_base )
{
	_interval_base = interval_base;
	return 0;
}
//------------------------------------------------------------------------------
// TSInterval::setBaseString - set interval base string
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Port from Java.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variable	I/O	Description
//
// interval_base_string	I Interval base as string.
//------------------------------------------------------------------------------

#include "TSInterval.h"
#include <string.h>

int TSInterval::setBaseString ( char *interval_base_string )
{	char	routine[]="TSInterval.setBaseString";
	int	dl = 50;

	if ( !interval_base_string ) {
		return 0;
	}

	PrintDebug ( dl, routine, "Trying to set sub-location to \"%s\"",
	interval_base_string );

	if( _interval_base_string ){
        	delete [] _interval_base_string;
	}

	_interval_base_string = new char[strlen( interval_base_string )+1];

	if( !_interval_base_string ){
		PrintWarning( 1, routine,
		"Unable to Allocate memory for sub-location \"%s\"",
		interval_base_string );
                return 1;
	}

	strcpy( _interval_base_string, interval_base_string );

        return 0;
}
// ----------------------------------------------------------------------------
// TSInterval.setMultiplier - set the interval multiplier
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers, RTi	Port from Java.
// ----------------------------------------------------------------------------

#include "TSInterval.h"

int TSInterval::setMultiplier ( int interval_mult )
{
	_interval_mult = interval_mult;
	return 0;
}
//----------------------------------------------------------------------------
// TSInterval.toString - convert to string representation
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------
// Notes:
//
//----------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Initial version.  Port from Java.
//		Riverside Technology,
//		inc.
//----------------------------------------------------------------------------
// Variables:	I/O	Description
//
//----------------------------------------------------------------------------

#include "TSInterval.h"

char TSIntervalString[256];  //cfan

char * TSInterval::toString ( void )
{
//cfan	char	string[256];

//cfan	sprintf( string,
	sprintf( TSIntervalString,  //cfan
	"TSInterval Base=%d,Mult=%d,String=\"%s\"",
	_interval_base, _interval_mult, _interval_base_string );

//cfan	return string;
	return ( TSIntervalString );  //cfan
}
#endif
// ----------------------------------------------------------------------------
// TSLimits Constructors
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:
//
// ----------------------------------------------------------------------------
// History:
//
// 05 Jan 1998	Steven A. Malers,	Port from Java version.
//		Riverside Technology,
//		inc.
// 23 Jan 1998	SAM, RTi		Be more rigorous about tracking when
//					dates are set.
// ----------------------------------------------------------------------------
// Variables:		I/O	Description
//
//
// ----------------------------------------------------------------------------

#include "TSLimits.h"

TSLimits::TSLimits ( void )
{
	initialize ();
}

// ----------------------------------------------------------------------------
// TSLimits Copy Constructor
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:
//
// ----------------------------------------------------------------------------
// History:
//
// 06 Jan 1998	SAM, RTi	Port from Java.
// ----------------------------------------------------------------------------
// Variables:	I/O	Description
//
// t		I	incoming date
// ----------------------------------------------------------------------------

TSLimits::TSLimits ( const TSLimits& limits )
{	char	routine[] = "TSLimits.CopyConstructor";

	initialize();

	_date1		= limits._date1;
	_date1_set	= limits._date1_set;
	_date2		= limits._date2;
	_date2_set	= limits._date2_set;
 	_max_value      = limits._max_value;
	_max_value_date = limits._max_value_date;
	_max_value_date_set = limits._max_value_date_set;
	_min_value      = limits._min_value;
	_min_value_date = limits._min_value_date;
	_min_value_date_set = limits._min_value_date_set;
	_non_missing_data_count		= 0;
	_non_missing_data_date1		= limits._non_missing_data_date1;
	_non_missing_data_date1_set	= limits._non_missing_data_date1_set;
	_non_missing_data_date2		= limits._non_missing_data_date2;
	_non_missing_data_date2_set	= limits._non_missing_data_date2_set;
	_found          = limits._found;
}
// ----------------------------------------------------------------------------
// TSLimits Destructor
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:
//
// ----------------------------------------------------------------------------
// History:
//
// 06 Jan 1998	Steven A. Malers,	Port from Java.
//		Riverside Technology,
//		inc.
// ----------------------------------------------------------------------------
// Variable	I/O	Description
//
// routine	L	name of function
// ----------------------------------------------------------------------------

#include "TSLimits.h"

TSLimits::~TSLimits ( void )
{	char	routine[] = "TSLimits::~TSLimits";

}

//----------------------------------------------------------------------------
// TSLimits Operators
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------
// Notes:	(1)	For Java, we do not have overloaded operators, but
//			for the C++ versions we have now implemented Java-like
//			functions so that it is easier to keep the C++ and
//			Java versions compatible.
//----------------------------------------------------------------------------

#include "TSLimits.h"

//----------------------------------------------------------------------------
// TSLimits overload = operator
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------
// Notes:
//
//----------------------------------------------------------------------------
// History:
//
// 06 Jan 1998	Steven A. Malers,	Initial C++ version.
//		Riverside Technology,
//		inc.
// 23 Jan 1998	SAM, RTi		Be more rigorous about date checks.
//----------------------------------------------------------------------------
// Variables:	I/O	Description
//
// limits	I	Incoming TSLimits.
//----------------------------------------------------------------------------

TSLimits& TSLimits::operator= ( const TSLimits& limits )
{
	if ( &limits == this) {
                // self declaration
        }
        else {	_date1		= limits._date1;
		_date1_set	= limits._date1_set;
		_date2		= limits._date2;
		_date2_set	= limits._date2_set;
 		_max_value      = limits._max_value;
		_max_value_date = limits._max_value_date;
		_max_value_date_set = limits._max_value_date_set;
		_min_value      = limits._min_value;
		_min_value_date = limits._min_value_date;
		_min_value_date_set = limits._min_value_date_set;
		_non_missing_data_count		= 0;
		_non_missing_data_date1	 = limits._non_missing_data_date1;
		_non_missing_data_date1_set =limits._non_missing_data_date1_set;
		_non_missing_data_date2	 = limits._non_missing_data_date2;
		_non_missing_data_date2_set =limits._non_missing_data_date2_set;
		_found          = limits._found;
        }
	return *this;
}

TSLimits::operator char *( void )
{
	// Call the named function...

	return toString ();
}
// ----------------------------------------------------------------------------
// TSLimits.areLimitsFound - have the limits been found
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// History:
//
// 06 Jan 1998	Steven A. Malers, RTi	Port from Java.
// ----------------------------------------------------------------------------

#include "TSLimits.h"

int TSLimits::areLimitsFound ()
{
	return _found;
}
// ----------------------------------------------------------------------------
// TSLimits.checkDates - check to see if the dates have been set
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:	(1)	If all of the dates have been set, set _found to true;
// ----------------------------------------------------------------------------
// History:
//
// 23 Jan 1998	Steven A. Malers, RTi	Initial version.
// ----------------------------------------------------------------------------

#include "TSLimits.h"

int TSLimits::checkDates ()
{
	if (	_date1_set &&
		_date2_set &&
		_max_value_date_set &&
		_min_value_date_set &&
		_non_missing_data_date1_set &&
		_non_missing_data_date2_set ) {
		// The dates have been fully processed (set)...
		_found = 1;
	}
	return 0;
}
// ----------------------------------------------------------------------------
// TSLimits.getDate1 - get the first date
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// History:
//
// 07 Jan 1998	Steven A. Malers, RTi	Initial version.
// ----------------------------------------------------------------------------

#include "TSLimits.h"

TSDate TSLimits::getDate1 ()
{
	return _date1;
}
// ----------------------------------------------------------------------------
// TSLimits.getDate2 - get the last date
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// History:
//
// 07 Jan 1998	Steven A. Malers, RTi	Initial version.
// ----------------------------------------------------------------------------

#include "TSLimits.h"

TSDate TSLimits::getDate2 ()
{
	return _date2;
}
// ----------------------------------------------------------------------------
// TSLimits.getMaxValue - get the maximum value
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// History:
//
// 06 Jan 1998	Steven A. Malers, RTi	Port from Java.
// ----------------------------------------------------------------------------

#include "TSLimits.h"

double TSLimits::getMaxValue ()
{
	return _max_value;
}
// ----------------------------------------------------------------------------
// TSLimits.getMaxValueDate - get the date corresponding to the maximum value
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// History:
//
// 06 Jan 1998	Steven A. Malers, RTi	Port from Java.
// ----------------------------------------------------------------------------

#include "TSLimits.h"

TSDate TSLimits::getMaxValueDate ()
{
	return _max_value_date;
}
// ----------------------------------------------------------------------------
// TSLimits.getMinValue - get the minimum value
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// History:
//
// 06 Jan 1998	Steven A. Malers, RTi	Port from Java.
// ----------------------------------------------------------------------------

#include "TSLimits.h"

double TSLimits::getMinValue ()
{
	return _min_value;
}
// ----------------------------------------------------------------------------
// TSLimits.getMinValueDate - get the minimum value for the date
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// History:
//
// 06 Jan 1998	Steven A. Malers, RTi	Port from Java.
// ----------------------------------------------------------------------------

#include "TSLimits.h"

TSDate TSLimits::getMinValueDate ()
{
	return _min_value_date;
}
// ----------------------------------------------------------------------------
// TSLimits.getNonMissingDataCount - get the non-missing data count
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// History:
//
// 23 Jan 1998	Steven A. Malers, RTi	Initial version.
// ----------------------------------------------------------------------------

#include "TSLimits.h"

int TSLimits::getNonMissingDataCount ()
{
	return _non_missing_data_count;
}
// ----------------------------------------------------------------------------
// TSLimits.getNonMissingDataDate1 -	get the date corresponding to the first
//					non-missing data value
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// History:
//
// 07 Jan 1998	Steven A. Malers, RTi	Initial version.
// ----------------------------------------------------------------------------

#include "TSLimits.h"

TSDate TSLimits::getNonMissingDataDate1 ()
{
	return _non_missing_data_date1;
}
// ----------------------------------------------------------------------------
// TSLimits.getNonMissingDataDate2 -	get the date corresponding to the first
//					non-missing data value
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// History:
//
// 07 Jan 1998	Steven A. Malers, RTi	Initial version.
// ----------------------------------------------------------------------------

#include "TSLimits.h"

TSDate TSLimits::getNonMissingDataDate2 ()
{
	return _non_missing_data_date2;
}
// ----------------------------------------------------------------------------
// TSLimits.hasNonMissingData - is some non-missing data available?
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// History:
//
// 23 Jan 1998	Steven A. Malers, RTi	Initial version.
// ----------------------------------------------------------------------------

#include "TSLimits.h"

int TSLimits::hasNonMissingData ()
{
	if ( _non_missing_data_count > 0 ) {
		return 1;
	}
	return 0;
}
// ----------------------------------------------------------------------------
// TSLimits.initialize - initialize the data
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// History:
//
// 06 Jan 1998	Steven A. Malers, RTi	Port from Java.
// 23 Jan 1998	SAM, RTi		Add _non_missing_data_counter.
// ----------------------------------------------------------------------------

#include "TSLimits.h"

int TSLimits::initialize ()
{
	// Dates are initialized in their constructors.

	_date1_set = 0;
	_date2_set = 0;
	_found = 0;
	_max_value = 0.0;
	_max_value_date_set = 0;
	_min_value = 0.0;
	_min_value_date_set = 0;
	_non_missing_data_count = 0;
	_non_missing_data_date1_set = 0;
	_non_missing_data_date2_set = 0;
	return 0;
}
// ----------------------------------------------------------------------------
// TSLimits.setDate1 - set the first date
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// History:
//
// 07 Jan 1998	Steven A. Malers, RTi	Initial version.
// 23 Jan 1998	SAM, RTi		Add the set flag.
// ----------------------------------------------------------------------------

#include "TSLimits.h"

int TSLimits::setDate1 ( TSDate date )
{
	_date1 = date;
	_date1_set = 1;
	checkDates();
	return 0;
}
// ----------------------------------------------------------------------------
// TSLimits.setDate2 - set the first date
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// History:
//
// 07 Jan 1998	Steven A. Malers, RTi	Initial version.
// 23 Jan 1998	SAM, RTi		Add the set flag.
// ----------------------------------------------------------------------------

#include "TSLimits.h"

int TSLimits::setDate2 ( TSDate date )
{
	_date2 = date;
	_date2_set = 1;
	checkDates();
	return 0;
}
// ----------------------------------------------------------------------------
// TSLimits.setMaxValue - set the maximum value
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// History:
//
// 06 Jan 1998	Steven A. Malers, RTi	Port from Java.
// ----------------------------------------------------------------------------

#include "TSLimits.h"

int TSLimits::setMaxValue ( double max_value )
{
	_max_value = max_value;
	return 0;
}

int TSLimits::setMaxValue ( double max_value, TSDate max_value_date )
{
	_max_value = max_value;
	setMaxValueDate ( max_value_date );
	return 0;
}
// ----------------------------------------------------------------------------
// TSLimits.setMaxValueDate - set the date for the maximum value
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// History:
//
// 06 Jan 1998	Steven A. Malers, RTi	Port from Java.
// 23 Jan 1998	SAM, RTi		Add the set flag.
// ----------------------------------------------------------------------------

#include "TSLimits.h"

int TSLimits::setMaxValueDate ( TSDate max_value_date )
{
	_max_value_date = max_value_date;
	_max_value_date_set = 1;
	checkDates();
	return 0;
}
// ----------------------------------------------------------------------------
// TSLimits.setMinValue - set the minimum value
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// History:
//
// 06 Jan 1998	Steven A. Malers, RTi	Port from Java.
// ----------------------------------------------------------------------------

#include "TSLimits.h"

int TSLimits::setMinValue ( double min_value )
{
	_min_value = min_value;
	return 0;
}

int TSLimits::setMinValue ( double min_value, TSDate min_value_date )
{
	_min_value = min_value;
	setMinValueDate ( min_value_date );
	return 0;
}
// ----------------------------------------------------------------------------
// TSLimits.setMinValueDate - set the date for the minimum value
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers, RTi	Port from Java.
// ----------------------------------------------------------------------------

#include "TSLimits.h"

int TSLimits::setMinValueDate ( TSDate min_value_date )
{
	_min_value_date = min_value_date;
	_min_value_date_set = 1;
	checkDates();
	return 0;
}
// ----------------------------------------------------------------------------
// TSLimits.setNonMissingDataCount - set the non-missing data count
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// History:
//
// 23 Jan 1998	Steven A. Malers, RTi	Initial version.
// ----------------------------------------------------------------------------

#include "TSLimits.h"

int TSLimits::setNonMissingDataCount ( int non_missing_data_count )
{
	_non_missing_data_count = non_missing_data_count;
	return 0;
}
// ----------------------------------------------------------------------------
// TSLimits.setNonMissingDataDate1 - set the date for the first non-missing data
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// History:
//
// 07 Jan 1998	Steven A. Malers, RTi	Port from Java.
// 23 Jan 1998	SAM, RTi		Add the set flag.
// ----------------------------------------------------------------------------

#include "TSLimits.h"

int TSLimits::setNonMissingDataDate1 ( TSDate date )
{
	_non_missing_data_date1 = date;
	_non_missing_data_date1_set = 1;
	checkDates();
	return 0;
}
// ----------------------------------------------------------------------------
// TSLimits.setNonMissingDataDate2 - set the date for the last non-missing data
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// History:
//
// 07 Jan 1998	Steven A. Malers, RTi	Port from Java.
// 23 Jan 1998	SAM, RTi		Add the set flag.
// ----------------------------------------------------------------------------

#include "TSLimits.h"

int TSLimits::setNonMissingDataDate2 ( TSDate date )
{
	_non_missing_data_date2 = date;
	_non_missing_data_date2_set = 1;
	checkDates();
	return 0;
}
//----------------------------------------------------------------------------
// TSLimits.toString - convert to a string representation
//----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//----------------------------------------------------------------------------

#include "TSLimits.h"

char TSLimitsString[256];  //cfan

char * TSLimits::toString ( void )
{
//cfan	char string[256];

//cfan	sprintf( string,
	sprintf( TSLimitsString,  //cfan
	"Min: %g on %s Max: %g on %s Non-miss: %s to %s Total: %s to %s",
	_min_value, (char *)_min_value_date,
	_max_value, (char *)_max_value_date,
	(char *)_non_missing_data_date1,
	(char *)_non_missing_data_date2,
	(char *)_date1, (char *)_date2 );

//cfan	return string;
	return ( TSLimitsString );  //cfan
}
//------------------------------------------------------------------------------
// TSList:: initialized instance data members.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 01 Apr 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------
#include "TSList.h"

TS** TSList::_ts_list=NULL;
int* TSList::_io_flags=NULL;
int TSList::_num_ts=0;
//------------------------------------------------------------------------------
// TSList:: DeleteTSList - deletes the list of TS.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 01 Apr 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------
#include "TSList.h"

void TSList::DeleteTSList( )
{
	char routine[]="TSList::DeleteTSList";
	int i;

	if( _ts_list != NULL ) {
		for( i = 0; i < _num_ts; i++ ){
			delete _ts_list[i];
		}
		free( _ts_list );
	}
	_ts_list = NULL;

	_num_ts = 0;

	if( _io_flags != NULL ) {
		free ( _io_flags );
	}
	_io_flags = NULL;
	return;
}

//------------------------------------------------------------------------------
// TSList:: addTSToList adds a TS pointer to the list of TS.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 01 Apr 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------
#include "TSList.h"

int TSList::addTSToList( TS* time_series )
{
	char routine[]="TSList::addTSTList";

	// Add one more data space on the TS list
	_ts_list = (TS**)realloc( _ts_list, (_num_ts + 1)*sizeof( TS* ) );

	if( _ts_list == NULL ) {
		PrintWarning( 1, routine, "Cannot allocate %d TS pointers.",
			_num_ts+1 );
		return( STATUS_FAILURE );
	}

	// Add it to the list...
	_ts_list[ _num_ts ] = time_series;

	_num_ts++;
	return( STATUS_SUCCESS );
}

int TSList::addTSToList( TS* time_series, char* io_set )
{
	char routine [] = "TSList::addTSToList( TS* ,char*  )";

	if( addTSToList( time_series ) ) {
		return( STATUS_FAILURE );
	}

	_io_flags = (int*)realloc( _io_flags, _num_ts*sizeof( int ) );
	if( _io_flags == NULL ) {
		PrintWarning( 2, routine, "Could not allocate %d ints.",
			_num_ts );
		return( STATUS_FAILURE );
	}

	if( !strncasecmp( io_set, "IN", 2 ) ) {
		_io_flags[_num_ts-1] = 1;
	}
	else {
		_io_flags[_num_ts-1] = 0;
	}

	return( STATUS_SUCCESS );
}
//------------------------------------------------------------------------------
// TSList:: getTSFromList gets a TS pointer from the list of TS.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 01 Jun 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------
#include "TSList.h"

int TSList::getNumTS( )
{
	return( _num_ts );
}
//------------------------------------------------------------------------------
// TSList:: getTSFromList gets a TS pointer from the list of TS.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 01 Apr 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------
#include "TSList.h"
#include "resj/TSIdent.h"

TS* TSList::getTSFromList( char* ident )
{
	char routine[]="TSList::getTSFromList", **list = NULL, id_al[MAXC];
	int nlist = 0, i = -1, found = 0;
	TSIdent *ts_ident = NULL;

	// Break the id into parts and deal with the part accordingly.
	// If there is only one item, then it is an alias, if there are
	// more, then assume we are dealing with a full identifier.

	// If we break by a space and find one, then assume TSIdent-style
	// identifier...
	list = BreakStringList( ident, " .\n\t", DELIM_SKIP_BLANKS, &nlist );
	if( list == NULL || nlist == 0 ) {
		PrintWarning( 1, routine, "TS id is malformed." );
		return( (TS*)NULL );
	}
	if( nlist == 3 ) {
		ts_ident = new TSIdent( );
		if( ts_ident->setLocation( list[0] ) ||
			ts_ident->setType( list [1] ) ||
			ts_ident->setInterval( TS::INTERVAL_HOUR,
				atoi( list[2] ) ) ) {
			PrintWarning( 1, routine, "TSIdent %s is malformed.",
			ident );
			delete ts_ident;
			list = FreeStringList( list );
			return( (TS*) NULL );
		}
		 // Now go find the TS* from the list...
		 for( i = 0; i < _num_ts; i++ ){
			strcpy( id_al,
				_ts_list[i]->getIdentifier().getIdentifier());
			if( !strcasecmp( ts_ident->getIdentifier(), id_al ) ) {
				delete ts_ident;
				list = FreeStringList( list );
				return( _ts_list[i] );
			}
		 }
		 // If we get here, we could not find the pointer in the list
		 PrintDebug( 5, routine, "Could not find time series with "
			"identifier %s in list.", ts_ident->getIdentifier() );
		delete ts_ident;
		list = FreeStringList( list );
		return( (TS*) NULL );
	}
	// If we get here we are dealing with an alias
	else if( nlist == 1 ){
		 // Now go find the TS* from the list using the alias...
		 for( i = 0; i < _num_ts; i++ ){
			strcpy( id_al,
				_ts_list[i]->getIdentifier().getAlias());
			if( !strcasecmp( list[0], id_al ) ){
				list = FreeStringList( list );
				return( _ts_list[i] );
			}
		 }
		 // If we get here, we could not find the pointer in the list
		 PrintDebug( 5, routine, "Could not find time series with "
			"alias %s in list.", list[0] );
		 list = FreeStringList( list );
		 return( (TS*) NULL );
	}
	else {
		 PrintWarning( 1, routine, "Could not find time series using "
			"identifier %s.", ident );
		 list = FreeStringList( list );
		 return( (TS*) NULL );
	}
}

//------------------------------------------------------------------------------
// TSTimeZone - Constructors
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 01 Oct 97	Matthew J. Rutherford, Riverside Technology, inc
//					Initial version of functions.
//------------------------------------------------------------------------------
#include "resj/TSTimeZone.h"

/////////////////////////
// Default Constructor //
/////////////////////////

TSTimeZone::TSTimeZone ()
{
	init();
}
//------------------------------------------------------------------------------
// TSTimeZone - Destructor
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 01 Oct 1997	Matthew J. Rutherford, Riverside Technology, inc.
//					Created initial version of function.
//------------------------------------------------------------------------------
// History:
//
//
//------------------------------------------------------------------------------

#include "resj/TSTimeZone.h"

TSTimeZone::~TSTimeZone ()
{
}
//------------------------------------------------------------------------------
// TSTimeZone operators.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 01 Oct 1997	Matthew J. Rutherford, RTi	Created initial version.
//------------------------------------------------------------------------------

#include "resj/TSTimeZone.h"

void TSTimeZone::operator= ( const TSTimeZone& tz )
{
        if ( &tz == this) {
		return;
        }
        else {
		strcpy( _tz, tz._tz );
		_ntz	= _ntz;
		_dsflag	= _dsflag;
	}
}

int TSTimeZone::operator== ( const TSTimeZone& tz )
{
	if( _ntz == tz._ntz && _dsflag == tz._dsflag ){
		// If these two things are equal
		// the rest of the stuff has to be.
		return( 1 );
	}
	// They are not equal.
        return 0;
}
//------------------------------------------------------------------------------
// TSTimeZone.calculateOffsetHours - calculate the offset between two time zones
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This routine computes the offset in hours between two
//			time zones.
//------------------------------------------------------------------------------
// History:
//
// 02 Oct 1997	Matthew J. Rutherford,	Created function.
//		RTi
// 05 Jan 1998	Steven A. Malers, RTi	Split out of SetGet file.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TSTimeZone.h"

int TSTimeZone::calculateOffsetHours( char* new_code )
{
	char	routine[]="TSTimeZone::calculateOffsetHours(char*)";

	if( new_code == NULL ){
		PrintWarning( 2, routine,
		"Incoming Zone is NULL, cannot continue." );
		return 0;
	}

	return ( calculateOffsetHours( _tz, new_code ) );
}

int TSTimeZone::calculateOffsetHours( char* from_tz, char* to_tz )
{
	char	routine[]="TSTimeZone::calculateOffsetHousr(char*,char*)";
	int	diff=0, dl = 50, from_num=0, to_num=0;

	if( !from_tz || !to_tz ){
		PrintWarning( 2, routine,
		"Incoming arguments are NULL, cannot continue." );
		return( 0 );
	}

	from_num 	= getDefinedNumber( from_tz );
	to_num 		= getDefinedNumber( to_tz );

	if( getDefinedDSFlag( from_tz ) == 1 ){
		from_num++;
	}

	if( getDefinedDSFlag( to_tz ) == 1 ){
		to_num++;
	}

	// Take the difference between the two.

	diff = to_num - from_num;

	PrintDebug( dl, routine,
	"Offset from \"%s\" -> \"%s\" is %d.", from_tz, to_tz, diff );

	return( diff );
}
//------------------------------------------------------------------------------
// TSTimeZone.getCode - return the time zone code as a character string
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 05 Jan 1997	Steven A. Malers, RTi	Split out of SetGet.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TSTimeZone.h"

char* TSTimeZone::getCode()
{
	return( _tz );
}
//------------------------------------------------------------------------------
// TSTimeZone.getDSFlag - return the time zone daylight savings flag
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 05 Jan 1997	Steven A. Malers, RTi	Split out of SetGet code.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TSTimeZone.h"

int TSTimeZone::getDSFlag()
{
	return( _dsflag );
}
//------------------------------------------------------------------------------
// TSTimeZone.getDefinedCode - get the defined code string for a number
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 05 Jan 1997	Steven A. Malers, RTi	Break out of GetSet file.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TSTimeZone.h"

char TSTimeZoneCode[256];  //cfan

char* TSTimeZone::getDefinedCode( int zone, int ds_flag )
{
//cfan	char	code[256]="", routine[]="TSTimeZone::getDefinedCode";
	char	routine[]="TSTimeZone::getDefinedCode";  //cfan
	int	dl = 50, found=0, i=0, number=0;

	while( TimeZoneData[i].tz[0] != '\0' ){
		if( 	TimeZoneData[i].ntz == zone &&
			TimeZoneData[i].dsflag == ds_flag ){
			PrintDebug( dl, routine,
			"Code for zone %d (%d) is \"%s\".",
			zone, ds_flag, TimeZoneData[i].tz );
//cfan			strcpy( code, TimeZoneData[i].tz );
			strcpy( TSTimeZoneCode, TimeZoneData[i].tz );  //cfan
			found = 1;
			break;
		}

		i++;
	}

	if( found == 0 ){
//cfan		strcpy( code, "Z" );
		strcpy( TSTimeZoneCode, "Z" );  //cfan
		PrintWarning( 2, routine,
		"Zone %d (%d) is not found in look-up list, returning %s",
//cfan		zone, ds_flag, code );
		zone, ds_flag, TSTimeZoneCode );  //cfan
	}

//cfan	return( code );
	return( TSTimeZoneCode );  //cfan
}
//------------------------------------------------------------------------------
// TSTimeZone.getDefinedDSFlag - get the daylight savings flag for a time zone
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 05 Jan 1997	Steven A. Malers, RTi	Break out of GetSet code.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TSTimeZone.h"

int TSTimeZone::getDefinedDSFlag( char* code )
{
	char	routine[]="TSTimeZone::getDefinedDSFlag";
	int	dl = 50, flag=0, found=0, i=0;

	if( code == NULL ){
		PrintWarning( 2, routine,
		"Incoming Code is NULL, cannot continue." );
		return( 0 );
	}

	while( TimeZoneData[i].tz[0] != '\0' ){
		if( !strcmp( TimeZoneData[i].tz, code ) ){
			PrintDebug( dl, routine,
			"DSFlag for \"%s\" is %d.",
			code, TimeZoneData[i].dsflag );
			flag = TimeZoneData[i].dsflag;
			found = 1;
			break;
		}

		i++;
	}

	if( found == 0 ){
		flag = 0;
		// JRV: Temporary comment out
		//PrintWarning( 2, routine,
		//"Zone %s is not found in look-up list, returning 0", code );
	}

	return( flag );
}
#include "resj/TSTimeZone.h"
char* TSTimeZone::getDefinedDescription( char* string )
{
	char routine [] = "TSTimeZone::getDefinedDescription";
	PrintWarning( 1, routine, "Not enabled yet" );
	return( (char*)NULL );
}
//------------------------------------------------------------------------------
// TSTimeZone.getDefinedNumber - get the time zone number from the time zone
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 05 Jan 1997	Steven A. Malers, RTi	Break out of GetSet code.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TSTimeZone.h"

// Retrieve the defined time zone number given an abbreviation.
int TSTimeZone::getDefinedNumber( char* code )
{
	char	routine[]="TSTimeZone::getDefinedNumber";
	int	dl = 50, found=0, i=0, number=0;

	if( code == NULL ){
		PrintWarning( 2, routine,
		"Incoming Code is NULL, cannot continue." );
		return( STATUS_FAILURE );
	}

	while( TimeZoneData[i].tz[0] != '\0' ){
		if( !strcmp( TimeZoneData[i].tz, code ) ){
			PrintDebug( dl, routine,
			"Zone Number for \"%s\" ( %s ) is %d.",
			code, TimeZoneData[i].desc, TimeZoneData[i].ntz );
			number = TimeZoneData[i].ntz;
			found = 1;
			break;
		}

		i++;
	}

	if( found == 0 ){
		// JRV: Temporary comment out
		//PrintWarning( 2, routine,
		//"Zone \"%s\" is not found in look-up list, returning 0", code );
		number = 0;
	}

	return( number );
}
//------------------------------------------------------------------------------
// TSTimeZone.getDefinedZones - get a list of available time zones.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 05 Jan 1997	Steven A. Malers, RTi	Break out of GetSet code.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TSTimeZone.h"

// Retrieve the defined time zone number given an abbreviation.

char** TSTimeZone::getDefinedZones()
{
	char 	**list=NULL, routine[]="TSTimeZone::getDefinedZones",
		string[MAXC];
	int	i=0, nlist=0;

	list = AddToStringList( list, "TSTimeZone", &nlist );

	while( TimeZoneData[i].tz[0] != '\0' ){
		sprintf( string, "%s (%s)",
		TimeZoneData[i].tz, TimeZoneData[i].desc );
		list = AddToStringList( list, string, &nlist );
		i++;
	}

	return( list );
}
//------------------------------------------------------------------------------
// TSTimeZone.getDescription - return the time zone description string
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 05 Jan 1997	Steven A. Malers, RTi	Split out of SetGet file.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TSTimeZone.h"

char* TSTimeZone::getDescription()
{
	return( getDefinedDescription( _tz ) );
}
//------------------------------------------------------------------------------
// TSTimeZone.getNumber - return the time zone number
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 05 Jan 1997	Steven A. Malers, RTi	Split out of SetGet file.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TSTimeZone.h"

int TSTimeZone::getNumber()
{
	return( _ntz );
}
//------------------------------------------------------------------------------
// TSTimeZone::init
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//------------------------------------------------------------------------------
// History: //
// 01 Oct 1997	Matthew J. Rutherford, RTi	Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TSTimeZone.h"

int TSTimeZone::init()
{
	char	routine[]="TSTimeZone::init";

	_ntz		= 0;

	_dsflag		= 0;

	_tz[0]		= '\0';

	return( 0 );
}

// Now define the static variables in this location:

const int	TSTimeZone::RESET_WITH_CODE	= 1;
const int	TSTimeZone::RESET_WITH_NUMBER	= 3;

const TSTimeZoneData	TSTimeZone::TimeZoneData[] =
	{	{ "GMT", "Greenwich Mean Time",          0,      0 },
		{ "Z", "Zulu Time",          0,      0 },
		{ "UTC", "Coordinated Universal Time",          0,      0 },
		{ "EST", "Eastern Standard Time",        -5,     0 },
		{ "EDT", "Eastern Daylight Time",        -5,     1 },
		{ "CST", "Central Standard Time",        -6,     0 },
		{ "CDT", "Central Daylight Time",        -6,     1 },
		{ "MST", "Mountain Standard Time",        -7,     0 },
		{ "MDT", "Mountain Daylight Time",        -7,     1 },
		{ "PST", "Pacific Standard Time",        -8,     0 },
		{ "PDT", "Pacific Daylight Time",        -8,     1 },
		{ "YST", "Yukon Standard Time",        -9,     0 },
		{ "YDT", "Yukon Daylight Time",        -9,     1 },
		{ "AST", "Alaska Standard Time",        -10,    0 },
		{ "ADT", "Alaska Daylight Time",        -10,    1 },
		{ "NST", "Nome Standard Time",        -11,    0 },
		{ "NDT", "Nome Daylight Time",        -11,    1 },
		{ "INTL", "Internal Clock Time",       -12,    0 },
		{ "Z0",         "", 0,      0 },
		{ "Z+1",        "", 1,      0 },
		{ "Z+2",        "", 2,      0 },
		{ "Z+3",        "", 3,      0 },
		{ "Z+4",        "", 4,      0 },
		{ "Z+5",        "", 5,      0 },
		{ "Z+6",        "", 6,      0 },
		{ "Z+7",        "", 7,      0 },
		{ "Z+8",        "", 8,      0 },
		{ "Z+9",        "", 9,      0 },
		{ "Z+10",       "", 10,     0 },
		{ "Z+11",       "", 11,     0 },
		{ "Z+12",       "", 12,     0 },
		{ "Z-1",        "", -1,     0 },
		{ "Z-2",        "", -2,     0 },
		{ "Z-3",        "", -3,     0 },
		{ "Z-4",        "", -4,     0 },
		{ "Z-5",        "", -5,     0 },
		{ "Z-6",        "", -6,     0 },
		{ "Z-7",        "", -7,     0 },
		{ "Z-8",        "", -8,     0 },
		{ "Z-9",        "", -9,     0 },
		{ "Z-10",       "", -10,    0 },
		{ "Z-11",       "", -11,    0 },
		{ "Z-12",       "", -12,    0 },
		{ "", "", 0, 0 } };
//------------------------------------------------------------------------------
// TSTimeZone::reset - function to reset the data fields based on one of them.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 02 Oct 1997	Matthew J. Rutherford, RTi	Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TSTimeZone.h"

int TSTimeZone::reset( int flag )
{
	char	routine[]="TSTimeZone::reset";

	static	int recursive_flag = 0;

	if( recursive_flag == 1 ){
		return( 0 );
	}

	if( flag == RESET_WITH_CODE ){
		recursive_flag 	= 1;

		// We have a new code, so we have to set the
		// description, number and flag based on that code.

		_dsflag = getDefinedDSFlag( _tz );

		setNumber( getDefinedNumber(_tz), _dsflag );

		recursive_flag 	= 0;

		return( 0 );
	}
	else if( flag == RESET_WITH_NUMBER ){
		recursive_flag 	= 1;
		// We have a new time-zone number and dsflag, and we have to
		// get the code and description.

		setCode( getDefinedCode( _ntz, _dsflag ) );

		recursive_flag 	= 0;

		return( 0 );
	}
	else {
		PrintWarning( 2, routine,
		"Unrecognized reset flag %d.", flag );
		return( 1 );
	}
}
//------------------------------------------------------------------------------
// TSTimeZone.setCode - set the time zone code
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 05 Jan 1997	Steven A. Malers, RTi	Split out of SetGet file.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TSTimeZone.h"

int TSTimeZone::setCode( char* code )
{
	char	routine[]="TSTimeZone::setCode";

	if( code != NULL ){
		strcpy( _tz, code );
	}
	reset( RESET_WITH_CODE );

	return( 0 );
}
//------------------------------------------------------------------------------
// TSTimeZone.setNumber - set the time zone number
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 05 Jan 1997	Steven A. Malers, RTi	Split out of the SetGet file.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TSTimeZone.h"

int TSTimeZone::setNumber( int ntz, int ds_flag )
{
	char	routine[]="TSTimeZone::setNumber";

	// Save the current setting.
	_ntz	= ntz;
	_dsflag	= ds_flag;

	reset( RESET_WITH_NUMBER );

	return( 0 );
}
//------------------------------------------------------------------------------
// TSTimeZone::toString - converts the internals to a string.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 02 Oct 1997	Matthew J. Rutherford, RTi	Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TSTimeZone.h"

char	tmp_string[256];

const char* TSTimeZone::toString()
{
	sprintf( tmp_string, "%s: Zone: %d DSFlag: %d",
	_tz, _ntz, _dsflag );

	return( tmp_string );
}
#if 0
// ----------------------------------------------------------------------------
// TSUtil.changeInterval - change time series from one interval to another
// ----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
// ----------------------------------------------------------------------------
// Notes:	(1)	This is a placeholder.  Need to combine the ESPADP
//			and Java TSUtil code here.
// ----------------------------------------------------------------------------
// History:
//
// 05 Jan 1998	Steven A. Malers, RTi	Initial version in C++.
// ----------------------------------------------------------------------------
// Variables:	I/O	Description
//
// routine	L	name of routine.
//
// ----------------------------------------------------------------------------

#include "TSUtil.h"
#include "resj/TSDate.h"
#include "resj/TS.h"

TS TSUtil::changeInterval (	TS *ts0, int interval_base, int interval_mult,
				unsigned int flags )
{	char routine[] = "TSUtil.changeInterval";

	// Put these in so we don't get irritating compiler messages...

	int base = interval_base;
	int mult = interval_mult;
	int f = flags;
	TS * ts = ts0;

	PrintWarning ( 1, routine, "NOT IMPLEMENTED" );
	return (TS)NULL;
}
// ----------------------------------------------------------------------------
// TSUtil.findTSFile - find a time series file using a path to the file.
// ----------------------------------------------------------------------------
// Notes:	(1)	This routine searches for a time series data file using
//			the following search order:
//
//				I.	If the time series identifier has a
//					scenario that is a file in the current
//					directory or the full path to a file,
//					use it.
//				II.	If the TS ID has a scenario but the file
//					is not in the current directory, use
//					the path information to
//					search.  If a file is found, use it.
//				III.	If the scenario is blank, use the
//					information in the TS ID to form
//					reasonable file names and search the
//					path for a file.  If one is found use
//					it.
//				IV.	If unable to find a file, return non-
//					zero.
// ----------------------------------------------------------------------------
// History:
//
// 05 Jan 1997	Steven A. Malers, RTi	Write initial version.
// 10 Mar 1997	SAM, RTi		Use semi-colons for the path so that
//					this code will work on the PC and UNIX.
// 22 Sep 1997	SAM, RTi		Fold in code from DSSFindTSFile in DSS
//					library.
// 10 Jan 1998	SAM, RTi		Move from DSSApp library to here.
// ----------------------------------------------------------------------------
// Variable	I/O	Description
//
// fulltsdatadirs L	Fill paths to possible time series files.
// i		L	Loop counter for directories.
// nfulltsdatadirs L	Number of "fulltsdatadirs".
// ntsdatadirs	G	Number of "tsdatadirs".
// scenario	L	Scenario part of time series identifier.  Used to
//			specify a file name.
// string	L	Generic string.
// tsdatapath	L	Time series data directory path as a string.
// tsdatadirs	G	Time series data directory path as string list.
// tsfile	O	Full path to time series file.
// tsid_string0	I	Time series identifier string (original).
// tsid_string	L	Time series identifier string.
// ----------------------------------------------------------------------------

#include "TSUtil.h"

char TSUtilTsfile[256];  //cfan

char * TSUtil::findTSFile ( char *tsid_string0, char *tsdatapath0 )
{	char		routine[] = "TSUtil.findTSFile", scenario[256],
			string[256], tsdatapath[1000], tsid_string[256];
	int		i, nfulltsdatadirs = 0;
	static char	**tsdatadirs = (char **)NULL;
	static int	ntsdatadirs = 0;
//cfan	char		tsfile[256];

	// Make sure we have a non-NULL identifier...

	if ( !tsid_string0 ) {
		PrintWarning ( 1, routine, "Time series identifier is NULL" );
		return (char *)NULL;
	}
	else if ( !tsid_string0[0] ) {
		PrintWarning ( 1, routine,
		"Time series identifier is empty" );
		return (char *)NULL;
	}

	// Make sure that the path is non-NULL...

	if ( !tsdatapath0 ) {
		PrintWarning ( 1, routine,
		"Time series path is NULL.  Using \".\"" );
		strcpy ( tsdatapath, "." );
	}
	else if ( !tsdatapath0[0] ) {
		PrintWarning ( 1, routine,
		"Time series path is empty.  Using \".\"" );
		strcpy ( tsdatapath, "." );
	}

	strcpy ( tsid_string, tsid_string0 );
	PrintDebug ( 1, routine,
	"Trying to find time series for ID \"%s\"", tsid_string );

	// If the paths are NULL, initialize so we can use...
	// This will always be the case at this time because of the way that
	// the code was split out of DSSApp.

	if ( tsdatadirs == (char **)NULL ) {
		PrintDebug ( 1, routine,
		"Trying to initialize global path data..." );
/*
		if ( getDef("DSS_TSDATA_PATH", tsdatapath) ) {
			// Trouble, use the default...
			PrintWarning ( 1, routine,
			"Unable to resolve DSS_TSDATA_PATH.  Using \".\"" );
			tsdatadirs = AddToStringList ( tsdatadirs, ".",
			&ntsdatadirs );
		}
		else {
*/
			// We got a path.  Split it into its parts...
			tsdatadirs = BreakStringList ( tsdatapath, "\t; ",
			DELIM_SKIP_BLANKS, &ntsdatadirs );
			if ( !ntsdatadirs ) {
				// Trouble, use the default...
				PrintWarning ( 1, routine,
				"DSS_TSDATA_PATH is empty.  Using \".\"");
				tsdatadirs = AddToStringList ( tsdatadirs, ".",
				&ntsdatadirs );
			}
			else {	for ( i = 0; i < ntsdatadirs; i++ ) {
					PrintDebug ( 10, routine,
					"tsdatadir[%d] = \"%s\"",
					i, tsdatadirs[i] );
				}
			}
/*
		}
*/
	}

	// Now we have a list of directories to search.  Initialize a TSIdent
	// with the character string identifier so that we can get to the
	// parts...

	TSIdent tsident ( tsid_string );

	strcpy ( scenario, tsident.getScenario() );
	if ( scenario[0] ) {
		// We have scenario information...  Let's see if the
		// file exists...
		PrintDebug ( 10, routine,
		"Trying TS file from scenario:  \"%s\"", scenario );
		if ( FileReadable(scenario) ) {
			// It is, use it...
			PrintDebug ( 10, routine,
			"Found TS file from scenario:  \"%s\"", scenario );
//cfan			strcpy ( tsfile, scenario );
			strcpy ( TSUtilTsfile, scenario );  //cfan
//cfan			return tsfile;
			return ( TSUtilTsfile );  //cfan
		}
		// If we have gotten to here, then we could not get the file
		// directly and we need to check the path...
		char	**fulltsdatadirs = GetFilesFromPathList (
			tsdatadirs, scenario, &nfulltsdatadirs );
		for ( i = 0; i < nfulltsdatadirs; i++ ) {
			PrintDebug ( 10, routine,
			"Trying TS file from path:  \"%s\"", fulltsdatadirs[i]);
			if ( FileReadable(fulltsdatadirs[i]) ) {
				// Found a match, use it...
//cfan				strcpy ( tsfile, fulltsdatadirs[i] );
				strcpy ( TSUtilTsfile, fulltsdatadirs[i] );  //cfan
				fulltsdatadirs =
				FreeStringList ( fulltsdatadirs );
//cfan				return tsfile;
				return ( TSUtilTsfile );  //cfan
			}
		}
		fulltsdatadirs = FreeStringList ( fulltsdatadirs );
	}

	// If we have gotten to here, then we do not have scenario information
	// and we need to guess at a file name from the other ID parts...

	// First try the full time series identifier...

	char	**fulltsdatadirs = GetFilesFromPathList (
		tsdatadirs, tsid_string, &nfulltsdatadirs );

	for ( i = 0; i < nfulltsdatadirs; i++ ) {
		PrintDebug ( 10, routine,
		"Trying TS file from path:  \"%s\"", fulltsdatadirs[i]);
		if ( FileReadable(fulltsdatadirs[i]) ) {
			// Found a match, use it...
//cfan			strcpy ( tsfile, fulltsdatadirs[i] );
			strcpy ( TSUtilTsfile, fulltsdatadirs[i] );  //cfan
			fulltsdatadirs = FreeStringList ( fulltsdatadirs );
//cfan			return tsfile;
			return ( TSUtilTsfile );  //cfan
		}
	}
	fulltsdatadirs = FreeStringList ( fulltsdatadirs );

	// If there is no scenario, try composing the file name without the
	// scenario part...

	if ( !scenario[0] ) {
		sprintf ( tsid_string, "%s.%s.%s.%s",
		tsident.getLocation(), tsident.getSource(), tsident.getType(),
		tsident.getInterval() );
		fulltsdatadirs = GetFilesFromPathList (
		tsdatadirs, tsid_string, &nfulltsdatadirs );

		for ( i = 0; i < nfulltsdatadirs; i++ ) {
			PrintDebug ( 10, routine,
			"Trying TS file from path:  \"%s\"", fulltsdatadirs[i]);
			if ( FileReadable(fulltsdatadirs[i]) ) {
				// Found a match, use it...
//cfan				strcpy ( tsfile, fulltsdatadirs[i] );
				strcpy ( TSUtilTsfile, fulltsdatadirs[i] );  //cfan
				fulltsdatadirs =
				FreeStringList ( fulltsdatadirs );
//cfan				return tsfile;
				return ( TSUtilTsfile );  //cfan
			}
		}
		fulltsdatadirs = FreeStringList ( fulltsdatadirs );

		strcpy ( string, tsident.getSource() );
		ToUpper ( string );
		// Try using all uppercase data source...
		sprintf ( tsid_string, "%s.%s.%s.%s",
		tsident.getLocation(), string, tsident.getType(),
		tsident.getInterval() );
		fulltsdatadirs = GetFilesFromPathList (
		tsdatadirs, tsid_string, &nfulltsdatadirs );

		for ( i = 0; i < nfulltsdatadirs; i++ ) {
			PrintDebug ( 10, routine,
			"Trying TS file from path:  \"%s\"", fulltsdatadirs[i]);
			if ( FileReadable(fulltsdatadirs[i]) ) {
				// Found a match, use it...
//cfan				strcpy ( tsfile, fulltsdatadirs[i] );
				strcpy ( TSUtilTsfile, fulltsdatadirs[i] );  //cfan
				fulltsdatadirs =
				FreeStringList ( fulltsdatadirs );
//cfan				return tsfile;
				return ( TSUtilTsfile );  //cfan
			}
		}
		fulltsdatadirs = FreeStringList ( fulltsdatadirs );
	}

	// Need to maybe try different upper/lowercase combinations for the
	// data type and interval??

	// If we have gotten to here we do not know where the time series is...

	PrintWarning ( 1, routine,
	"Unable to find TS file for \"%s\"", tsid_string );

	return (char *)NULL;
}
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

TSLimits TSUtil::getDataLimits ( TS *ts, TSDate& start, TSDate& end )
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

	return limits;
}
//-----------------------------------------------------------------------------
// TSUtil.getPeriodFromTS -	finds the maximum or minimum POR referenced by
//				all entries in a list of time series TS
//-----------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//-----------------------------------------------------------------------------
// Notes:	(1)	For example:
//
//			------------               TS1
//			      -------------------- TS2
//			         ------            TS3
//
//			-------------------------- max
//			         ---               min
//		(2)	Currently, input is an array of time series pointers.
//			It is likely that this will be overloaded to provide
//			more flexibility.
//-----------------------------------------------------------------------------
// History:
//
// ?		Catherine E. Nutting, RTi	Created routine.
// 01/11/96	Peter T. Abplanalp, RTi		Updated to use TSSetTSPt.
//						Changed to include TSLib.h
//						instead of TSDefs.h.
// 01/26/96	CEN, RTi			Added flag: MaxMin (TSPOR_MAX,
//						or TSPOR_MIN ).
// 03 Oct 96	Steven A. Malers, RTi		Clean up code while debugging.
// 08 Jan 1998	SAM, RTi			Copy TSGetPeriodFromTS and put
//						into this TSUtil class.
//-----------------------------------------------------------------------------
// Variable	I/O	Description
//
// am1, am2	L	Absolute months for start and end of window.
// nstats	I	Number of time series entries in TS[].
// m1, y1	O	Months/years which mark earliest absolute months.
// m2, y2	O	Months/years which mark latest absolute months.
// ts		I	Time series list.
//-----------------------------------------------------------------------------

#include <stdio.h>

#include "TSUtil.h"
#include "ResJ.h"

TSLimits TSUtil::getPeriodFromTS ( int nstats, TS *ts[], int flag )
{	char		rtn[] = "TSUtil.getPeriodFromTS";
	TS		*tspt;
	int		i = 0;
	TSDate		end(TSDate::DATE_STRICT),
			tsptr_end(TSDate::DATE_STRICT),
			tsptr_start(TSDate::DATE_STRICT),
			start(TSDate::DATE_STRICT);
	TSLimits	limits;

	PrintDebug ( 10, rtn, "Checking %d TS to find POR limits", nstats );

	// Check for NULL list...

	if ( nstats == 0 ) {
		PrintWarning ( 2, rtn, "Zero time series list" );
		return limits;
	}

	// Set first guess to the limits of the first time series...

	if ( !ts[0] ) {
		PrintWarning ( 2, rtn, "Null first time series" );
		return limits;
	}

	tspt = ts[0];
	start	= tspt->getDate1();
	end	= tspt->getDate2();

	// Now loop through the remaining time series.  Start with i=1 because
	// TS is used to set initial am1, am2 values...

	for (i=1; i<nstats; i++) {
		tspt = ts[i];
		if ( tspt == (TS *)NULL ) {
			PrintWarning ( 2, rtn,
			"Null TS for [%d] time series", i );
			continue;
		}

		tsptr_start = tspt->getDate1();
		tsptr_start = tspt->getDate2();

		if ( flag == MAX_POR ) {
			if ( tsptr_start < start )
				start = tsptr_start;
			if ( tsptr_end > end )
				end = tsptr_end;
		}
		else if ( flag == MIN_POR ) {
			if ( tsptr_start > start )
				start = tsptr_start;
			if ( tsptr_end < end )
				end = tsptr_end;
		}
		else {	PrintWarning ( 2, rtn, "Unknown flag option %u.",
			flag );
			return limits;
		}
	}

	if ( flag == MIN_POR ) {
		PrintDebug ( 10, rtn,
		"Found minimum POR limits to be %s to %s",
		(char*)start, (char*)end );
	}
	else if ( flag == MAX_POR ) {
		PrintDebug ( 10, rtn,
		"Found maximum POR limits to be %s to %s",
		(char*)start, (char*)end );
	}

	limits.setDate1 ( start );
	limits.setDate2 ( end );
	return limits;
}
//------------------------------------------------------------------------------
// TSDateGetDateFromString - converts char string into TSDate
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)This will probably be changed when the NWS reviews batchmode.
//
//------------------------------------------------------------------------------
// History:
//
// 03 Dec 1996	Matthew J. Rutherford, Riverside Technology, inc.
// 17 Feb 1998  Daniel Weiler, RTi	Converted for use in Res-J.
// 20 Apr 1998  DKW, RTi		Reworked to be a lot more flexible.
//
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
// date		L	TSDate that is being created.
// date_string	I	incoming date string in format 120396.
// routine	L	name of function.
// tmp		L	string used in parsing.
//
//------------------------------------------------------------------------------

#include "TSUtil.h"

TSDate TSUtil::getTSDateFromString( char* date_string )
{
	char		tmp[250], routine[] = "TSUtil::getTSDateFromString";
	int		hour_set = 0, minute_set = 0, year_set2 = 0,
			year_set4 = 0;
	TSDate		date;

	strncpy( tmp, date_string, 2 );
	tmp[2] = '\0';

	date.setMonth( atoi( tmp ) );

	strncpy( tmp, &date_string[3], 2 );
	tmp[2] = '\0';

	date.setDay( atoi( tmp ) );


	if( strlen( date_string ) == 8 ){
		//
		// the date is MM/DD/YY or MM/DD HH
		//
		if( date_string[5] == '_' ) {
			strncpy( tmp, &date_string[6], 2 );
			tmp[2] = '\0';
			date.setHour( atoi( tmp ) );
		}
		else {
			sprintf( tmp, "19%s", &date_string[6] );
			tmp[4] = '\0';
			date.setYear( atoi( tmp ) );
		}
	}
	else if( strlen( date_string ) == 10 ){
		//
		// the date is MM/DD/YYYY or MM/DD/YY H
		//
		if( date_string[5] == '_' ) {
			sprintf( tmp, "0%s", &date_string[6] );
			tmp[2] = '\0';
			date.setHour( atoi( tmp ) );
		}
		else {
			strncpy( tmp, &date_string[6], 4 );
			tmp[4] = '\0';
			date.setYear( atoi( tmp ) );

			strncpy( tmp, &date_string[9], 2 );
			tmp[2] = '\0';
			date.setHour( atoi( tmp ) );
		}
	}
	else if( strlen( date_string ) == 11 ){
		//
		// the date is MM/DD/YY HH or MM/DD HH:MM
		//
		if( date_string[5] == '_' ) {
			strncpy( tmp, &date_string[6], 2 );
			tmp[2] = '\0';
			date.setHour( atoi( tmp ) );

			sprintf( tmp, &date_string[9], 2 );
			tmp[2] = '\0';
			date.setMinute( atoi( tmp ) );
		}
		else {
			sprintf( tmp, "19%s", &date_string[6] );
			tmp[4] = '\0';
			date.setYear( atoi( tmp ) );

			strncpy( tmp, &date_string[9], 2 );
			tmp[2] = '\0';
			date.setHour( atoi( tmp ) );
		}
	}
	else if( strlen( date_string ) == 14 ){
		//
		// the date is MM/DD/YY HH:MM
		//
		strncpy( tmp, &date_string[12], 2 );
		tmp[2] = '\0';
		date.setMinute( atoi( tmp ) );

		strncpy( tmp, &date_string[9], 2 );
		tmp[2] = '\0';
		date.setHour( atoi( tmp ) );
	}
	else {
		PrintWarning( 10, routine,
		"Invalid Date String %s, returning empty date", date_string );
		return( date );
	}

	PrintDebug( 10, routine,
	"Converted %s to: %d/%d %02d:%02d",
	date_string, date.getMonth(), date.getDay(), date.getHour(),
	date.getMinute() );

	return( date );
}
//-----------------------------------------------------------------------------
// TSUtil.getValidPeriod -	get a valid period to work on given two
//				suggested dates
//-----------------------------------------------------------------------------
// Notes:	(1)	This routine takes two dates which are generally the
//			start and end dates for an iteration.  If they are
//			specified, they are used (even if they are outside the
//			range of the time series).  If a date is not specified,
//			then the appropriate date from the time series is
//			used.  This routine may require logic at some point to
//			handle special cases.  For example, the incoming
//			arguments may specify a start date but no end date.
//			If the start date from the time series is later than
//			the specified end date, then what?
//-----------------------------------------------------------------------------
// History:
//
// ?		Daniel K. Weiler, RTi	Initial version.
// 09 Jan 1998	Steven A. Malers, RTi	Enhance to return TSLimits.  Port to
//					C++.
//-----------------------------------------------------------------------------

#include "TSUtil.h"
#include "TSLimits.h"

TSLimits TSUtil::getValidPeriod (	TS *ts, TSDate suggested_start,
					TSDate suggested_end )
{	TSLimits dates;

	if ( suggested_start.getYear() == 0 ) {
		dates.setDate1( ts->getDate1() );
	}
	else {	dates.setDate1 ( suggested_start );
	}

	if ( suggested_end.getYear() == 0 ) {
		dates.setDate2( ts->getDate2() );
	}
	else {	dates.setDate2 ( suggested_end );
	}

	return dates;
}
//------------------------------------------------------------------------------
// TSUtil.initialize - initialize static data members to initial values.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Initial version.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//------------------------------------------------------------------------------

#include "TSUtil.h"

// Static data...

const int TSUtil::MAX_POR = 1;
const int TSUtil::MIN_POR = 2;



// ----------------------------------------------------------------------------
// TS::constructors - member functions for TS base class
// ----------------------------------------------------------------------------

#include "resj/TS.h"

// TS constructor
//
// Notes:	(1)	Need to spend time on this
//
TS::TS ( void )
{	char routine[]="TS.TS";

	PrintDebug( 50, routine, "constructing" );

	init();
}

// TS constructor - construct from file
//
// Notes:	(1)	Need to spend time on this
//
TS::TS ( char *input )
{	char	routine[] = "TS.TS(char*)";

	PrintWarning ( 1, routine,
	"Constructing from file (%s) is not enabled", input );
	init();
}

//------------------------------------------------------------------------------
// TS::TS - Cop Constructor
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 05 May 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------
TS::TS ( const TS& ts )
{
	char	routine[] = "TS::TS(const TS&)";

	init();

	// We need to to a piece-by-piece copy of all the data members.

	_date1				= ts._date1;
	_date2				= ts._date2;
	_date1_original 		= ts._date1_original;
	_date2_original 		= ts._date2_original;
	_data_interval_base 		= ts._data_interval_base;
	_data_interval_mult 		= ts._data_interval_mult;
	_data_interval_base_original 	= ts._data_interval_base_original;
	_data_interval_mult_original 	= ts._data_interval_mult_original;

	// Call functions to set the following information since they
	// all have dynamically allocated memory.
	setDataType( ts._data_type );
	setDataUnits( ts._data_units );
	setDataUnitsOriginal( ts._data_units_original );
	setVersion( ts._version );

	_id 				= ts._id;

	_sequence_number		= ts._sequence_number;

	_dirty				= ts._dirty;

	_data_limits			= ts._data_limits;

	_is_instantaneous		= ts._is_instantaneous;

	_missing			= ts._missing;
	_missingl			= ts._missingl;
	_missingu			= ts._missingu;

	setDescription( ts._description );
	setComments( ts._comments );
	setGenesis( ts._genesis );
}

// ~TS destructor - construct from file
//
// Notes:	(1)	Need to spend time on this
//

#include "resj/TS.h"

TS::~TS ( void )
{
	if( _version ){
		delete [] _version;

		_version	= (char*)NULL;
	}

	if( _input_name ){
		delete [] _input_name;

		_input_name 	= (char*)NULL;
	}

	if( _data_type ){
		delete [] _data_type;

		_data_type 	= (char*)NULL;
	}

	if( _description ){
		delete [] _description;

		_description 	= (char*)NULL;
	}

	//We have to add stuff for comments and genesis

	if( _data_units ){
		delete [] _data_units;

		_data_units	= (char*)NULL;
	}

	if( _data_units_original ){
		delete [] _data_units_original;

		_data_units_original = (char*)NULL;
	}
}


// TS overload =
//
// Notes:	(1)	Need to spend time on this
//

#include "resj/TS.h"

TS& TS::operator= ( const TS& ts )
{
	return *this;
}

//------------------------------------------------------------------------------
// TS.addToComments - add to the comments
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This function adds a string to the comments string list.
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers, RTi	Created function.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::addToComments( char *comment )
{	char	routine[]="TS.addToComments";

	int	listlen;
	_comments = AddToStringList ( _comments, comment, &listlen );

	return 0;
}
//------------------------------------------------------------------------------
// TS.addToGenesis - add to the genesis
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This function adds a string to the genesis string list.
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers, RTi	Created function.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::addToGenesis( char *genesis )
{	char	routine[]="TS.addToGenesis";

	int	listlen;
	_genesis = AddToStringList ( _genesis, genesis, &listlen );

	return 0;
}
//------------------------------------------------------------------------------
// TS::allocateDataSpace
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 05 Feb 1997	Steven A. Malers, RTi	Created member function.
//
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::allocateDataSpace ( void )
{	char	routine[] = "TS::allocateDataSpace";

	PrintWarning( 1, routine,
	"This is a virtual function, redefine in lower classes" );

	return 1;
}
//------------------------------------------------------------------------------
// TS.copyHeader - copy a time series' header
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This function used to copy the time series header from
//			another time series to the current time series.
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers, RTi	Created function.  Port from Java.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::copyHeader( TS &ts )
{	char	routine[]="TS.copyHeader";

	setVersion ( ts.getVersion() );
	setIdentifier ( ts.getIdentifier() );
	setDataLimits ( ts.getDataLimits() );
	_date1_original = ts.getDate1Original();
	_date2_original = ts.getDate2Original();
	setDataType ( ts.getDataType() );
	_data_interval_base = ts.getDataIntervalBase();

	_data_interval_mult = ts.getDataIntervalMult();

	_data_interval_base_original = ts.getDataIntervalBaseOriginal ();

	_data_interval_mult_original = ts.getDataIntervalMultOriginal ();

	setDescription( ts.getDescription() );

	int nlist;
	_comments = (char **)NULL;
	AddListToStringList ( _comments, ts.getComments(), &nlist );
	_genesis = (char **)NULL;
	AddListToStringList ( _genesis, ts.getGenesis(), &nlist );

	setDataUnits( ts.getDataUnits() );

	setDataUnitsOriginal( ts.getDataUnitsOriginal() );

	setMissing( ts.getMissing() );

	_dirty = 1;	// We need to recompute limits when we get the chance

	return 0;
}
//------------------------------------------------------------------------------
// TS::formatOutput - format a time series for output
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This is used to write a time series to a simple format.
//			If the format corresponds to a specific time series
//			type in a derived class, use that class'
//			writePersistent routine.
//------------------------------------------------------------------------------
// History:
//
// 30 Sep 1997	Steven A. Malers,	First version to replace some of the
//		Riverside Technology,	TSPrint* routines.
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

// Format as strings...
char ** TS::formatOutput ( int format, unsigned long int flags )
{	char	routine[]="TS::formatOutput(int,long)";

	PrintWarning( 1, routine,
	"This function is virtual, redefine in derived classes" );

	// put in the following so that we don't get warnings about variables
	// not getting used...
	if ( format ) {
		; // do nothing
	}
	if ( flags ) {
		; // do nothing
	}

	return (char **)NULL;
}

// Format and write to a file...
int TS::formatOutput ( ostream &out, int format, unsigned long int flags )
{	char	routine[]="TS::formatOutput(ostream&,int,long)";

	PrintWarning( 1, routine,
	"This function is virtual, redefine in derived classes" );

	// put in the following so that we don't get warnings about variables
	// not getting used...
	if ( out ) {
		; // do nothing
	}
	if ( format ) {
		; // do nothing
	}
	if ( flags ) {
		; // do nothing
	}

	return( STATUS_FAILURE );
}

int TS::formatOutput ( char *fname, int format, unsigned long int flags )
{	char		routine[] = "TS::formatOutput(char*,int,long)";

	// put in the following so that we don't get warnings about variables
	// not getting used...
	if ( fname ) {
		; // do nothing
	}
	if ( format ) {
		; // do nothing
	}
	if ( flags ) {
		; // do nothing
	}

	return( STATUS_FAILURE );
}
//------------------------------------------------------------------------------
// TS::freeDataSpace - free memory used by the data array
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 06 Feb 1997	Steven A. Malers, RTi	Created member function.
//
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::freeDataSpace ( void )
{	char	routine[] = "TS::freeDataSpace";

	PrintWarning( 1, routine,
	"This is a virtual function, redefine in lower classes" );

	return 1;
}
//------------------------------------------------------------------------------
// TS.getComments - return the full comments
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 09 Jan 1998	Steven A. Malers,	Split out of SetGet code.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

char **TS::getComments( void )
{
	return( _comments );
}
//------------------------------------------------------------------------------
// TS::getDataDate - get the date relative to a starting date
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This function is meant to be implemented by regular and
//			irregular interval time series.  Because most time
//			series are regular interval, we implement here and let
//			the IrregularTS code overrule this.
//------------------------------------------------------------------------------
// History:
//
// 30 Sep 1997	Steven A. Malers,	First version.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

TSDate TS::getDataDate ( TSDate &date0, int increment )
{	char	routine[] = "TS::getDataDate";
	TSDate	t;

	// For now, allow the date to go past the end of the time series...

	t = date0;
	t.addInterval ( _data_interval_base,
			increment*_data_interval_mult );

	return t;
}
//------------------------------------------------------------------------------
// TS.getDataIntervalBase - get the data interval base
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Split out of SetGet code.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::getDataIntervalBase( void )
{
	return( _data_interval_base );
}
//------------------------------------------------------------------------------
// TS.getDataIntervalBaseOriginal
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Split out of SetGet code.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::getDataIntervalBaseOriginal( void )
{
	return( _data_interval_base_original );
}
//------------------------------------------------------------------------------
// TS.getDataIntervalMult - get the data interval multiplier
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Split out of SetGet code.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::getDataIntervalMult( void )
{
	return( _data_interval_mult );
}
//------------------------------------------------------------------------------
// TS.getDataIntervalMultOriginal
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Split out of SetGet code.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::getDataIntervalMultOriginal( void )
{
	return( _data_interval_mult_original );
}
//------------------------------------------------------------------------------
// TS.getDataLimits - return the limits of the TS data
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 09 Jan 1998	Steven A. Malers,	Initial version.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

TSLimits TS::getDataLimits( void )
{
	// Make sure that the limits have been set...
	refresh();
	return( _data_limits );
}
//------------------------------------------------------------------------------
// TS::getDataPoint
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 06 Jan 1998	Steven A. Malers, RTi	Created member function.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

TSData TS::getDataPoint ( TSDate& date )
{	char	routine[]="TS::getDataPoint";

	PrintWarning( 1, routine,
	"This is a virtual function, redefine in lower classes" );

	TSData data;
	return data;
}
//------------------------------------------------------------------------------
// TS::getDataPointer
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 12 Nov 1996	Matthew J. Rutherford, RTi	created member function
//
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

double *TS::getDataPointer( TSDate& date )
{
	char	routine[]="TS::getDataPointer";

	PrintWarning( 1, routine,
	"This is a virtual function, redefine in lower classes" );

	return( (double*)NULL );
}
//------------------------------------------------------------------------------
// TS::getDataPosition - get the data array position for a date
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 06 Feb 1997	Steven A. Malers,	Created member function.
//		Riverside Technology,
//		inc.
// 06 Jun 1997	SAM, RTi		Add third positional argument to support
//					minute time series.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
// column	I	Column position in data array.
// date		I	Date to get data position form.
// row		I	Row position in data array.
// third	I	Third array position for data array.
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::getDataPosition( TSDate& date, int *row, int *column, int *third )
{	char	routine[] = "TS::getDataPointer";

	PrintWarning( 1, routine,
	"This is a virtual function, redefine in lower classes" );

	return 1;
}
//------------------------------------------------------------------------------
// TS.getDataType
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Split out of SetGet code.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

char *TS::getDataType( void )
{
	return( _data_type );
}
//------------------------------------------------------------------------------
// TS.getDataUnits
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Split out of SetGet code.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

char *TS::getDataUnits( void )
{
	return( _data_units );
}
//------------------------------------------------------------------------------
// TS.getDataUnitsOriginal
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Split out of SetGet code.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

char *TS::getDataUnitsOriginal( void )
{
	return( _data_units_original );
}
//------------------------------------------------------------------------------
// TS::getDataValue
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 12 Nov 1996	Matthew J. Rutherford, RTi	created member function
//
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

double TS::getDataValue( TSDate& date )
{
	char	routine[]="TS::getDataValue";

	PrintWarning( 1, routine,
	"This is a virtual function, redefine in lower classes" );

	return( -999.0 );
}
//------------------------------------------------------------------------------
// TS.getDate1 - return the date for the first data
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Split out of SetGet code.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

TSDate TS::getDate1( void )
{
	return _date1;
}
//------------------------------------------------------------------------------
// TS.getDate1Original - return the starting date for the original data
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Split out of SetGet code.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

TSDate TS::getDate1Original( void )
{
	return( _date1_original );
}
//------------------------------------------------------------------------------
// TS.getDate2 - return the date for the last data point
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Split out of SetGet code.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

TSDate TS::getDate2( void )
{
	return _date2;
}
//------------------------------------------------------------------------------
// TS.getDate2Original - return the ending date for the original data
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Split out of SetGet code.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

TSDate TS::getDate2Original( void )
{
	return( _date2_original );
}
//------------------------------------------------------------------------------
// TS.getDescription
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Split out of SetGet code.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

char *TS::getDescription( void )
{
	return( _description );
}
//------------------------------------------------------------------------------
// TS.getGenesis - return the full genesis information
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 09 Jan 1998	Steven A. Malers,	Split out of SetGet code.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

char **TS::getGenesis( void )
{
	return( _genesis );
}
//------------------------------------------------------------------------------
// TS.getIdentifier - get the identifier as a TSIdent
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Split out of SetGet code.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

TSIdent TS::getIdentifier()
{
	return( _id );
}
//------------------------------------------------------------------------------
// TS.getIdentifierString - get the identifier as a string
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Initial version.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

char * TS::getIdentifierString()
{
	return( _id.getIdentifier() );
}
//------------------------------------------------------------------------------
// TS.getInstantaneous - returns the _is_instantaneous flag
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 16 Apr 1998	Daniel Weiler,			Created intial version.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::getInstantaneous( )
{
	return( _is_instantaneous );
}
//------------------------------------------------------------------------------
// TS.getLocation
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Split out of SetGet code.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

char *TS::getLocation( void )
{
	return( _id.getLocation() );
}
//------------------------------------------------------------------------------
// TS.getMaxValue
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Split out of SetGet code.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

double TS::getMaxValue( void )
{
	return( _data_limits.getMaxValue() );
}
//------------------------------------------------------------------------------
// TS.getMinValue
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Split out of SetGet code.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

double TS::getMinValue( void )
{
	return( _data_limits.getMinValue() );
}
//------------------------------------------------------------------------------
// TS.getMissing
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Split out of SetGet code.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

double TS::getMissing( void )
{
	return( _missing );
}
//------------------------------------------------------------------------------
// TS.getNonMissingDataDate1 -	get the date corresponding to the first
//				non-missing data
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 27 May 1997	Steven A. Malers,	First version.
//		Riverside Technology,
//		inc.
// 09 Jan 1998	SAM, RTi		Move this code from TS.getDataDate1.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//------------------------------------------------------------------------------

#include "resj/TS.h"
#include "resj/TSIdent.h"

TSDate TS::getNonMissingDataDate1( void )
{
	return( _data_limits.getNonMissingDataDate1() );
}
//------------------------------------------------------------------------------
// TS.getNonMissingDataDate2 -	get the date corresponding to the last
//				non-missing data
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 27 May 1997	Steven A. Malers,	First version.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//------------------------------------------------------------------------------

#include "resj/TS.h"
#include "resj/TSIdent.h"

TSDate TS::getNonMissingDataDate2( void )
{
	return( _data_limits.getNonMissingDataDate2() );
}
//------------------------------------------------------------------------------
// TS.getSequenceNumber - get the time series sequence number
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 09 Jan 1998	Steven A. Malers,	Iniitial version.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::getSequenceNumber ( void )
{
	return( _sequence_number );
}
//------------------------------------------------------------------------------
// TS.getVersion
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Split out of SetGet code.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

char *TS::getVersion( void )
{
	return( _version );
}
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

int TS::init( void )
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
}

// Global static data...

const int TS::DATES_ABSOLUTE		= 1;
const int TS::DATES_RELATIVE		= 2;
const int TS::DATES_UNIT		= 3;

const int TS::FORMAT_MATRIX			= 1;
const int TS::FORMAT_SPREADSHEET		= 2;

const int TS::FORMAT_MATRIX_PRINT_NOTES	= 0x1;
const int TS::FORMAT_MATRIX_PRINT_HEADER	= 0x2;
					// 0x4, 0x8, 0x10, 0x20... 0x800
					// are reserved for future use

const int TS::FORMAT_SPREADSHEET_TAB_DELIM	= 0x1000;
const int TS::FORMAT_SPREADSHEET_COMMA_DELIM	= 0x2000;
const int TS::FORMAT_SPREADSHEET_PIPE_DELIM	= 0x4000;

const int TS::FORMAT_MATRIX_PRINT_STAT_TOT	= 0x1000;
const int TS::FORMAT_MATRIX_PRINT_STAT_AT	= 0x2000;
const int TS::FORMAT_MATRIX_PRINT_STAT_NUM	= 0x4000;
const int TS::FORMAT_MATRIX_PRINT_STAT_MIN	= 0x8000;
const int TS::FORMAT_MATRIX_PRINT_STAT_MAX	= 0x10000;
const int TS::FORMAT_MATRIX_PRINT_STAT_MEAN	= 0x20000;
const int TS::FORMAT_MATRIX_PRINT_STAT_MED	= 0x40000;
const int TS::FORMAT_MATRIX_PRINT_STAT_SDEV	= 0x80000;

const int TS::FORMAT_MATRIX_PRINT_ALL_STATS	=
		TS::FORMAT_MATRIX_PRINT_STAT_TOT 	|
		TS::FORMAT_MATRIX_PRINT_STAT_AT	|
		TS::FORMAT_MATRIX_PRINT_STAT_NUM	|
		TS::FORMAT_MATRIX_PRINT_STAT_MIN	|
		TS::FORMAT_MATRIX_PRINT_STAT_MAX	|
		TS::FORMAT_MATRIX_PRINT_STAT_MEAN	|
		TS::FORMAT_MATRIX_PRINT_STAT_MED	|
		TS::FORMAT_MATRIX_PRINT_STAT_SDEV;

/*  Modified by guoxian Zhou 05/22/2003
const double TS :: INFINITY		= 100000000.0;
*/
const double TS :: u_INFINITY		= 100000000.0;

const int TS::INTERVAL_SECOND 	= 1;
const int TS::INTERVAL_MINUTE		= 60;
const int TS::INTERVAL_HOUR		= 3600;
const int TS::INTERVAL_DAY		= 86400;
const int TS::INTERVAL_WEEK		= 604800;
const int TS::INTERVAL_MONTH		= 2678400;
const int TS::INTERVAL_YEAR		= 31536000;
const int TS::INTERVAL_IRREGULAR	= 999;

const int TS::ISTREAM_STATUS_NONE	= 1;
const int TS::ISTREAM_STATUS_OPEN	= 2;
const int TS::ISTREAM_STATUS_CLOSED	= 3;
//------------------------------------------------------------------------------
// TS::isDataMissing - determines if a data value is missing
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 11 Nov 1996	Matthew J. Rutherford, Riverside Technology, inc.  created funct
//
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::isDataMissing( double value )
{
	char	routine[]="TS::isDataMissing";

	if( 	value >= _missingl &&
		value <= _missingu ) {

		return( 1 );
	}

	return( 0 );
}
//------------------------------------------------------------------------------
// TS::printSample
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)This is a virtual function at this level and should be
//		redefined.
//
//------------------------------------------------------------------------------
// History:
//
// 12 Nov 1996	Matthew J. Rutherford, RTi	Created function
//
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::printSample( ostream& out )
{
	char	routine[]="TS::printSample";

	PrintWarning( 1, routine,
	"This is a virtual function, should be defined lower down" );

	return( STATUS_SUCCESS );
}
//------------------------------------------------------------------------------
// TS::readPersistent
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)This is a virtual function at this level and should be
//		redefined.
//
//------------------------------------------------------------------------------
// History:
//
// 12 Nov 1996	Matthew J. Rutherford, RTi	Created function
//
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::readPersistent( istream& in )
{
	char	routine[]="TS::readPersistent(istream&)";

	PrintWarning( 1, routine,
	"This is a virtual function, should be defined lower down" );

	return( STATUS_SUCCESS );
}

int TS::readPersistent( char *file_name )
{
	char	routine[]="TS::readPersistent(char*)";

	PrintWarning( 1, routine,
	"This is a virtual function, should be defined lower down" );

	return( STATUS_SUCCESS );
}
//------------------------------------------------------------------------------
// TS::readPersistentHeader
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)This is a virtual function at this level and should be
//		redefined.
//
//------------------------------------------------------------------------------
// History:
//
// 13 Nov 1996	Matthew J. Rutherford, RTi	Created function
//
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::readPersistentHeader( istream& in )
{
	char	routine[]="TS::readPersistentHeader(istream&)";

	PrintWarning( 1, routine,
	"This is a virtual function, should be defined lower down" );

	return( STATUS_SUCCESS );
}

int TS::readPersistentHeader( char *fname )
{
	char	routine[]="TS::readPersistentHeader(char*)";

	PrintWarning( 1, routine,
	"This is a virtual function, should be defined lower down" );

	return( STATUS_SUCCESS );
}
//------------------------------------------------------------------------------
// TS.refresh
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This function used to be calcMaxMinValues but was
//			changed to refresh() to be more generic.
//------------------------------------------------------------------------------
// History:
//
// 12 Nov 1996	Matthew J. Rutherford,	Created function.
//		RTi.
// 16 Jun 1997	MJR, RTi		Added overload.
// 06 Jan 1996	Steven A. Malers, RTi	Change function to refresh().
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::refresh( void )
{	char	routine[]="TS.refresh()";

	PrintWarning( 1, routine,
	"This is a virtual function, define in lower classes" );

	return( STATUS_SUCCESS );
}
//------------------------------------------------------------------------------
// TS.setComments - set the full comments information
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Initial version.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::setComments ( char **comments )
{	char	routine[] = "TS.setComments";

	if ( !comments ) {
		return 1;
	}

	if( _comments ) {
        	_comments = FreeStringList ( _comments );
	}

	int nlist;
	_comments = AddListToStringList ( _comments, comments, &nlist );

	return 0;
}
//------------------------------------------------------------------------------
// TS.setDataInterval - set the data interval
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Split out of SetGet code.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::setDataInterval( int base, int mult )
{
	_data_interval_base = base;

	_data_interval_mult = mult;

	return( 0 );
}
//------------------------------------------------------------------------------
// TS.setDataIntervalOriginal
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Split out of SetGet code.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::setDataIntervalOriginal( int base, int mult )
{
	_data_interval_base_original = base;

	_data_interval_mult_original = mult;

	return( 0 );
}
//------------------------------------------------------------------------------
// TS.setDataLimits - set the data limits
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 06 Jan 1998	Steven A. Malers,	First version.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::setDataLimits( TSLimits limits )
{
	_data_limits = limits;

	return( 0 );
}
//------------------------------------------------------------------------------
// TS.setDataType
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Split out of SetGet code.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::setDataType( char *data_type )
{
	char	routine[]="TS::setDataType";

	if ( !data_type ) {
		return 1;
	}

	if( _data_type ){
        	delete [] _data_type;
	}

	_data_type = new char[strlen(data_type)+1];

	if( !_data_type ){
		PrintWarning( 1, routine,
		"Unable to allocate memory for %s", data_type );
		return( STATUS_FAILURE );
	}

	strcpy( _data_type, data_type );

	return( STATUS_SUCCESS );
}
//------------------------------------------------------------------------------
// TS.setDataUnits
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Split out of SetGet code.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::setDataUnits( char *data_units )
{
	char	routine[]="TS::setDataUnits";

	if ( !data_units ) {
		return 1;
	}

	if( _data_units ) {
		delete [] _data_units;
		_data_units = (char*)NULL;
	}

	_data_units = new char [strlen(data_units)+1];

	if( !_data_units ){
		PrintWarning( 1, routine,
		"Unable to allocate memory for %s", data_units );
                return( STATUS_FAILURE );
	}

	strcpy( _data_units, data_units );

	return( STATUS_SUCCESS );
}
//------------------------------------------------------------------------------
// TS.setDataUnitsOriginal
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Split out of SetGet code.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::setDataUnitsOriginal( char *units )
{
	char	routine[] = "TS::setDataUnitsOriginal";

	if ( !units ) {
		return 1;
	}

	if( _data_units_original ){
		delete [] _data_units_original;
	}

	_data_units_original = new char[strlen(units)+1];

	if( !_data_units_original ){
		PrintWarning( 1, routine,
		"Unable to allocate memory for %s", units );
                return( STATUS_FAILURE );
	}

	strcpy( _data_units_original, units );

	return( STATUS_SUCCESS );
}
//------------------------------------------------------------------------------
// TS::setDataValue
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 05 Feb 1997	Steven A. Malers, RTi	Created member function.
//
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::setDataValue( TSDate& date, double val )
{	char	routine[] = "TS::setDataValue";

	PrintWarning( 1, routine,
	"This is a virtual function, redefine in lower classes" );

	return 1;
}
//------------------------------------------------------------------------------
// TS.setDate1 - set the start of data date
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Split out of SetGet code.
//		Riverside Technology,	Do not use _data_limits because
//		inc.			because we need to get at
//					directly in the data access functions!
//					_data_limits is reset by refresh().
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::setDate1( TSDate t )
{
	_date1 = t;

	return 0;
}
//------------------------------------------------------------------------------
// TS.setDate1Original - set the date for the original first data point
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Split out of SetGet code.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::setDate1Original( TSDate t )
{
	_date1_original = t;

	return( 0 );
}
//------------------------------------------------------------------------------
// TS.setDate2 - set the date for the last point
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Split out of SetGet code.
//		Riverside Technology,	Do not set in _data_limits.  It will be
//		inc.			set in refresh().
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::setDate2( TSDate t )
{
	_date2 = t;

	return 0;
}
//------------------------------------------------------------------------------
// TS.setDate2Original - set the ending date for the original data
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Split out of SetGet code.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::setDate2Original( TSDate t )
{
	_date2_original = t;

	return( 0 );
}
//------------------------------------------------------------------------------
// TS.setDescription
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Split out of SetGet code.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::setDescription( char *description )
{
	char	routine[] = "TS::setDescrition";

	if ( !description ) {
		return 1;
	}

	if( _description ) {
        	delete [] _description;
	}

	_description = new char[strlen( description )+1];

	if( !_description ){
		PrintWarning( 1, routine,
		"Unable to allocate memory for %s", description );
                return( STATUS_FAILURE );
	}

	strcpy( _description, description );

	return( STATUS_SUCCESS );
}
//------------------------------------------------------------------------------
// TS.setGenesis - set the full comments information
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Create function.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::setGenesis ( char **genesis )
{	char	routine[] = "TS.setGenesis";

	if ( !genesis ) {
		return 1;
	}

	if( _genesis ) {
        	_genesis = FreeStringList ( _genesis );
	}

	int nlist;
	_genesis = AddListToStringList ( _genesis, genesis, &nlist );

	return 0;
}
//------------------------------------------------------------------------------
// TS.setIdentifier
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Split out of SetGet code.
//		Riverside Technology,
//		inc.
// 01 Apr 1998	Matthew J. Rutherford, RTi
//					Overload the function to take a
//					TSIdent&
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::setIdentifier( TSIdent& identifier )
{
	_id = identifier;

	return( 0 );
}
int TS::setIdentifier( char *identifier )
{
	if ( identifier ) {
		_id.setIdentifier( identifier );
	}

	return( 0 );
}

int TS::setIdentifier( 	char* location,	char* source,
				char* type,	char* interval,
				char* scenario )
{
	if ( location ) {
		_id.setLocation( location );
	}

	if ( source ) {
		_id.setSource( source );
	}

	if ( type ) {
		_id.setType( type );
	}

	if ( interval ) {
		_id.setInterval( interval );
	}

	if ( scenario ) {
		_id.setScenario( scenario );
	}

	return( 0 );
}
//------------------------------------------------------------------------------
// TS.setInstantaneous - sets the _is_instantaneous to INSTANTANEOUS
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 16 Apr 1998	Daniel Weiler,			Created intial version.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::setInstantaneous()
{
	_is_instantaneous = INSTANTANEOUS;
	return( STATUS_SUCCESS );
}
//------------------------------------------------------------------------------
// TS.setLocation - set the location part of TSIdent
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 09 Jan 1998	Steven A. Malers,	Port from Java.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::setLocation ( char *location )
{	char	routine[]="TS_setLocation";

	return _id.setLocation ( location );
}
//------------------------------------------------------------------------------
// TS.setMaxValue
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Split out of SetGet code.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::setMaxValue( double max )
{
	_data_limits.setMaxValue ( max );

	return( 0 );
}
//------------------------------------------------------------------------------
// TS.setMinValue
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Split out of SetGet code.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::setMinValue( double min )
{
	_data_limits.setMinValue ( min );

	return( 0 );
}
//------------------------------------------------------------------------------
// TS.setMissing
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Split out of SetGet code.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::setMissing( double missing )
{
	_missing = missing;

	if( missing < 0 ) {

		_missingl = missing * 1.001;
		_missingu = missing * 0.999;
	}
	else {
		_missingl = missing * 0.999;
		_missingu = missing * 1.001;
	}

        return( STATUS_SUCCESS );
}
//------------------------------------------------------------------------------
// TS::setNonMissingDataDate1 -	set the date corresponding to the first
//				non-missing data
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 27 May 1996	Steven A. Malers,	First version.
//		Riverside Technology,	Change from setDataDate1.
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"
#include "resj/TSIdent.h"

int TS::setNonMissingDataDate1( TSDate t )
{
	_data_limits.setNonMissingDataDate1 ( t );

	return( 0 );
}
//------------------------------------------------------------------------------
// TS::setNonMissingDataDate2 -	set the date corresponding to the last
//				non-missing data
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 27 May 1996	Steven A. Malers,	First version.
//		Riverside Technology,
//		inc.
// 09 Jan 1998	SAM, RTi		Update to use _data_limits.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//------------------------------------------------------------------------------

#include "resj/TS.h"
#include "resj/TSIdent.h"

int TS::setNonMissingDataDate2( TSDate t )
{
	_data_limits.setNonMissingDataDate2 ( t );

	return 0;
}
//------------------------------------------------------------------------------
// TS.setSequenceNumber - set the time series sequence number
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 09 Jan 1998	Steven A. Malers,	Initial version.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::setSequenceNumber ( int sequence_number )
{
	_sequence_number = sequence_number;

	return 0;
}
//------------------------------------------------------------------------------
// TS.setVersion
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 08 Jan 1998	Steven A. Malers,	Split out of SetGet code.
//		Riverside Technology,
//		inc.
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::setVersion( char *version )
{
	char	routine[]="TS::setVersion";

	if ( !version ) {
		return 1;
	}

	if( _version ){
		delete [] _version;
	}

	_version = new char[strlen(version)+1];

	if( !_version ){
		PrintWarning( 1, routine,
		"Unable to allocate memory for %s", version );
                return( STATUS_FAILURE);
	}

	strcpy( _version, version );

	return( STATUS_SUCCESS );
}
//------------------------------------------------------------------------------
// writePersistent -
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This is used to write a time series to a DATACARD
//			format file.
//
//------------------------------------------------------------------------------
// History:
//
// 08 Nov 1996	Matthew J. Rutherford, Riverside Technology, inc.
//
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::writePersistent ( ostream& out )
{
	char	routine[]="TS::writePersistent(ostream&)";

	PrintWarning( 1, routine,
	"This function is virtual, redefine in lower classes" );

	return( STATUS_SUCCESS );
}

int TS::writePersistent ( char *fname )
{
	char	routine[]="TS::writePersistent(char*)";

	PrintWarning( 1, routine,
	"This function is virtual, redefine in lower classes" );

	return( STATUS_SUCCESS );
}
//------------------------------------------------------------------------------
// TS::writePersistentHeader
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)	This is a virtual function at this level and should be
//			redefined in the derived class.
//------------------------------------------------------------------------------
// History:
//
// 26 May 1997	Steven A. Malers, RTi	Created function
//------------------------------------------------------------------------------
// Variables:	I/O	Description
//
//
//------------------------------------------------------------------------------

#include "resj/TS.h"

int TS::writePersistentHeader( ostream& in )
{
	char	routine[]="TS::writePersistentHeader(ostream&)";

	PrintWarning( 1, routine,
	"This is a virtual function, should be defined in the derived class" );

	return( STATUS_SUCCESS );
}

int TS::writePersistentHeader( char *fname )
{
	char	routine[]="TS::writePersistentHeader(char*)";

	PrintWarning( 1, routine,
	"This is a virtual function, should be defined in the derived class" );

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_tsclass/RCS/TSClass.cxx,v $";
 static char rcs_id2[] = "$Id: TSClass.cxx,v 1.12 2006/10/26 15:36:30 hsu Exp $";}
/*  ===================================================  */

}

#endif
