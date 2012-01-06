//------------------------------------------------------------------------------
// Node :: Node - constructors.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//------------------------------------------------------------------------------
// History:
// 
// 14 Jan 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
// 17 Aug 2007	Darrin Sharp, Riverside Technology, inc
//					Copy rating curve id and/or rating table
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Node.h"

Node :: Node() : Component() 
{
	initialize();
}

Node :: Node( const Node& node ) : Component( node )
{
	initialize();

	if( node._diversion_ts != NULL ) {
                delete _diversion_ts;
                _diversion_ts = new HourTS( *(node._diversion_ts) );
        }

	_prevInflow = node._prevInflow;
	_startInflow = node._startInflow;
	_endInflow = node._endInflow;
	_discharge = node._discharge;
	_prevDischarge = node._prevDischarge;
	_startDischarge = node._startDischarge;
	_endDischarge = node._endDischarge;
	_diversion = node._diversion;
	_prevDiversion = node._prevDiversion;
	_startDiversion = node._startDiversion;
	_endDiversion = node._endDiversion;
	_min_ctl = node._min_ctl;
	_min = node._min;
	_mode = node._mode;
	_has_rating_table = node._has_rating_table;

  	if ( node._has_rating_table ) {
  		_rating_table = node._rating_table;
  	}
	
	if ( strcmp(node._rating_curve_id ,"NULL") ) {
                strcpy(_rating_curve_id,node._rating_curve_id);
        }


/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Node_Constructors.cxx,v $";
 static char rcs_id2[] = "$Id: Node_Constructors.cxx,v 1.2 2006/10/26 15:27:43 hsu Exp $";}
/*  ===================================================  */

}
