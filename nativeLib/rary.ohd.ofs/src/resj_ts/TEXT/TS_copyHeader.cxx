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

int TS :: copyHeader( TS &ts )
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

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_copyHeader.cxx,v $";
 static char rcs_id2[] = "$Id: TS_copyHeader.cxx,v 1.2 2000/05/19 13:35:21 dws Exp $";}
/*  ===================================================  */

}
