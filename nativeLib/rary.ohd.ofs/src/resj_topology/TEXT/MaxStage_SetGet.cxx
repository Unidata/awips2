//------------------------------------------------------------------------------
// MaxStage :: SetGet - set/gets for MaxStage.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 05 May 1998 	Daniel Weiler, RTi	Created initial version.
// 04 Jun 2001	James R. VanShaar, RTi	Modified addTimeStep to more accurately
//					account for multiple lags which may not
//					be evenly divisible by simulation time
//					step.
// 30 Jan 2002	JRV, RTi	Added setInactiveState().
// 22 Mar 2005	KWZ, OHD	Added setAllGroups() and resetDcp()
// 20 Mar 2006	JRV, RTi	Added getDCP().
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "MaxStage.h"
#include "ComboMethod.h"

//void MaxStage :: addTimeStep( int lag )
void MaxStage :: addTimeStep( int lag, double kkk )
{

	_sumLag += lag;
	_sumK += kkk;
	// If lag time is more than half way between timesteps, we will run to 
	//	the later timestep.  If it is midway, we might as well only run
	//	to the previous as it will save just a little time and make no
	//	predictable difference in resulting lagged flow.
	double roundingSum = _sumLag;
	if ( ((int)_sumLag % _t_mult) > ( (double)(_t_mult) / 2.0) ) {
		// Increment by _t_mult to ensure integer division to next value
		roundingSum += _t_mult;
	}
	_n_tstep = ((int)roundingSum)/_t_mult;
}

//------------------------------------------------------------------------------
// MaxStage :: getDCP - Returns the method's downstream control point (a node).
//------------------------------------------------------------------------------
// Returns the method's downstream control point.
//------------------------------------------------------------------------------
// Return: 
//     None
// Calls:
//     None
// Errors:
//     None
// Warnings:
//     None
// Debug:
//     None
//------------------------------------------------------------------------------

/**
Gets the method's downstream control point.
@return Component
*/

Node* MaxStage :: getDCP( )
{
    return _dcp;
}

void MaxStage :: setInactiveState()
{
	_myValue = MISSING;

	_Active = 0;

	return;
}

void MaxStage :: setNextDSComp( Component* comp, int in_pos ) 
{
	_next_ds_comp = comp;
	_inflow_pos = in_pos;

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */

}

/**MaxStage::setAllGroups().  set up _group for every _mehtod_list[].
*/
void MaxStage::setAllGroups(Component* src, Component* dest)
{	int i;
	
	if (src && dest)
	{
		for (i=0;i<dest->_n_meth;i++)
		{
			if ((strcmp(dest->_method_list[i]->getType(),"SETMIN")==0) ||
				(strcmp(dest->_method_list[i]->getType(),"SETMAX")==0) ||
				(strcmp(dest->_method_list[i]->getType(),"SETSUM")==0))
					((ComboMethod *)dest->_method_list[i])->setGroup((ComboMethod *)src->_method_list[i]);
			else if (strcmp(dest->_method_list[i]->getType(),"MAXSTAGE")==0)
				((MaxStage*)dest->_method_list[i])->resetDcp();
// printf(" maxsolve compname=%s methodname=%s methid=%s \n", src->getID(), dest->_method_list[i]->getType(), dest->_method_list[i]->getID());
		}
		
		//do a recursive setAllGroups
		for (i=0;i<dest->_n_son;i++)
		{
			setAllGroups(src->_son_list[i],dest->_son_list[i]);
		}
	}
}

/**MaxStage::resetDcp().  Reset _dcp for MaxStage method.
*/
void MaxStage::resetDcp()
{	char tmpID[20];

	strcpy(tmpID,_dcp->getID());
	if (_dcp)delete _dcp; //The old one was created, not linked. so delete it.
	_dcp=(Node*)_owner->findRoot() -> getComponentPtr( tmpID );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/MaxStage_SetGet.cxx,v $";
 static char rcs_id2[] = "$Id: MaxStage_SetGet.cxx,v 1.5 2006/10/26 15:26:48 hsu Exp $";}
/*  ===================================================  */

}
