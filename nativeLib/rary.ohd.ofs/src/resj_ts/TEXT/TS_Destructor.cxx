
// ~TS destructor - construct from file
//
// Notes:	(1)	Need to spend time on this
//

#include "resj/TS.h"

TS :: ~TS ( void )
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

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob72/rfc/ofs/src/resj_ts/RCS/TS_Destructor.cxx,v $";
 static char rcs_id2[] = "$Id: TS_Destructor.cxx,v 1.2 2000/05/19 13:35:20 dws Exp $";}
/*  ===================================================  */

}

