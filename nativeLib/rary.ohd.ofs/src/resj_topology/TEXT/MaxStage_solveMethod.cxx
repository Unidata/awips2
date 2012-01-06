//------------------------------------------------------------------------------
// MaxStage :: solveMethod - Algorithm that solves the MaxStage method.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 04 May 1998	Daniel Weiler, Riverside Technology, inc
//					Created initial version.
// 19 Oct 1998	DKW, RTi	 	Moved the creation/destruction of
//					the sub-tree to be every time in the
//					iterative loop.
// 25 May 2001	James R. VanShaar, RTi	Reset is_co_date prior to any returns
// 01 Jun 2001	JRV, RTi	Added re-initialization of new '_sumLag'
// 05 Jun 2001	JRV, RTi	Improved error handling
// 11 Dec 2002	JRV, RTi	Added setSolutionOrder usage, assignment of 
// 				_Active.
// 17 Dec 2002	JRV, RTi	Added buildSubTree_specialTieOut,
// 				buildSubTree_specialTieIn functionality.
// 30 Jan 2002	JRV, RTi	Modified the handling near the end of the 
// 				simulation period.
// 14 Aug 2007	DJS, RTi	For Reservoir Tools Enhancement, added the 
//				capability to use and existing Rating Curve
//				for lookup of max flow; also added capability
//				to use a max discharge param instead of 
//				max stage.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "MaxStage.h"
#include "ComboMethod.h"

extern "C" {
	extern void intfgq_(const char[8], int *ts,
			    float *fst, float *ffl,
               		    int *jd, int *hr, float *hs);
	extern void intfrc_(const char[8], int *ef);
}

int MaxStage :: solveMethod( TSDate& cur_date, int isPrimarySolution,
	double** group_val )
{
	char routine[]="MaxStage::solveMethod";
	int i, iter = 0, FirstTime = 1, MinTooHigh = 0, nrows;
	Component* sub_root = NULL;
	double difference = _criterion + 1.0, release, sub_stg,
		uplim, lowlim, maxflow = 0; 
	TSDate t1, t2;
	Solver	solve;
	double qdata[1], hdata[1], stage, flow;
	int err_flag=-999, warn_flag, timeInt;
	char rcid[8];
	int rclen,istage;
	int jDay, hour;
        float fstage,fflow,uplimDischarge = 0,highestStage = 0,highestFlow = -999;

	// Set _Active to 1
	_Active = 1;

	// Initialize the release to be tested; only necessary the first time through, when
	// _maxflow has not yet been intialized
	if ( _maxflow == MISSING ) {
		if ( _max_discharge != MISSING) {
			// if _max_discharge is defined, no need to convert from _max_stage to maxflow
			maxflow = _max_discharge;
		}
		else if ( _stage_flow_tbl.getNRows() != 0 ){
			// We have an internal (either from the Node or this method, but now at
                	// this method) rating curve table.
			nrows = _stage_flow_tbl.getNRows(); 
			// a table was defined at the node or in the method, so
			// convert the _max_stage into a maxflow via the table lookup
			maxflow = _stage_flow_tbl.lookup( _max_stage, GETCOLUMN_2, ALLOW_BOUNDS ); 
		}
		else {
			// not a _max_discharge based evaluation, and a rating TABLE has not
			// been defined at either the node or in the method; so, must be doing
			// a lookup based on a rating curve id defined at the node
			// (_max_stage is the indep variable in this case)
	
			// first get the rating curve and load it into the FRATNG common block
			rclen=strlen(_dcp->_rating_curve_id);
			strcpy(rcid,_dcp->_rating_curve_id);
			// pad the rcid with blanks so we don't have garbage in the
			// string when we cross over to fortran
			if (rclen < 8) {
				for ( i=rclen+1; i<9; i++ ) {
					strcat(rcid," ");
				}
			}
			// load the rating curve into the fortran COMMON block;
			// intfrc_ is a wrapper routine to smooth the transition to fortran
			intfrc_(rcid, &err_flag);
			if (err_flag == 1) {
				return (STATUS_FAILURE); // rating curve does not exist         	
			}
			// if err_flag is -999, that means that execution passed through
			// intfrc (which calls fgetrc), but infrc/fgetrc did not pass back
			// either a success (0) or failure (1) err_flag; this most 
			// likely occurred because mcp3 (the calibration program) is 
			// being run - rating curves are not available when running in 
			// calibration mode; fgetrc is stubbed in the calibration case 
			// (hence the lack of any err_flag return value); so, catch 
			// this error and flag it
			if (err_flag == -999) {
				PrintError(routine,"Failure translating stage to "
				   "discharge using a Rating Curve ID. This is most "
				   "likely because the calibration program is being "
				   "run - Rating Curve ID's are not supported in "
				   "calibration mode.");
        			return (STATUS_FAILURE); 
			}
			// here is where the actual lookup of stage vs discharge occurs;
			// intfgq_ is a wrapper routine to smooth the transition to fortran
			fstage =  _max_stage; 		// floating pt stage
			// the date/time is not really used in this application, but
			// is required for completeness in the param list
			timeInt = Method::getTimeInterval();  
			jDay = int(cur_date.toNoYearJulianDouble());
			hour = cur_date.getHour();

			// now call the routine to query the rating curve and get the flow
                	// corresponding to the _max_stage value (now in fstage).
			// note that errors will be flagged in fstgq.f
			intfgq_(rcid,			// rating curve id (input)
			       &timeInt, 		// time step (input)
			       &fstage,			// stage - (input)
			       &fflow,			// flow - (output)
			       &jDay,			// julian day of val to convert (input)
			       &hour,			// hour of value to convert (input)
			       &highestStage		// max stage of the curve (output)
		      	); 
 			maxflow = fflow;
			// since there is no stage_flow_tbl in this case, determine the
 			// biggest discharge defined by the rating curve in the process
  			// db; use the highestStage param that came back from the last
			// intgq_ call (above);	the highestFlow determined here will be 
			// used later during the iterative solution
			fstage=highestStage;
			intfgq_(rcid,			// rating curve id (input)
			       &timeInt, 		// time step (input)
			       &fstage,			// stage - (input)
			       &highestFlow,		// flow - (output)
			       &jDay,			// julian day of val to convert (input)
			       &hour,			// hour of value to convert (input)
			       &highestStage		// max stage of the curve (output)
		      	); 
			// put into the object, since we may skip this conditional on future
			// iterations of RES-J
			_highestFlow = highestFlow;
		}
		// set the method's _maxflow attribute, so we don't do the table
		// lookup for stage vs. flow the next time through
		_maxflow = maxflow;
	}
	// already done the table lookup or otherwise intialized _maxflow, so just use it
	else {
		maxflow = _maxflow;
	}

	release = _min_release;
/*
	uplim = ( _min_release + 2.0 * _stage_flow_tbl.getMax( GETCOLUMN_2 ) )
			/ 2.0;
	lowlim = _min_release;
*/

	// We do not want to be saving carryover states during the iteration.
	// Set the Component-level flag to be false, but we also need to reset 
	// it back to what it orginally was when the iteration converges.
	t1 = cur_date;
	t2 = cur_date;
	int flag = _owner->_is_co_date;
	_owner->_is_co_date = 0;

	// Allocate space for the timeseries to be used as outflow from 
	// the 'owning' reservoir
	HourTS inFromOwnRes;
	inFromOwnRes.setDate1( Method :: getForecastDate1() );
	inFromOwnRes.setDate2( Method :: getForecastDate2() );
	inFromOwnRes.setDataInterval( Method :: getTimeInterval(), 
		Method :: getTimeMult() );
	if( inFromOwnRes.allocateDataSpace() ) {
		PrintWarning( 1, routine, "Troubles allocating data space for "
			"outflow time series on %s.", _id );
		return( STATUS_FAILURE );
	}
// No need to build subtree in each iteration -- ksh
//	while( difference > _criterion && iter <= _max_iter ) {
		if ( _n_tstep != 0  && iter == 0 ) {
			FirstTime = 0;
			// This is not the first call to this MaxStage method.
			// We know how far it is between the reservoir and the
			// downstream control point.  It is also possible that
			// our solution will include timesteps beyond the end
			// of the simulation window
			t2.addInterval( _t_int, _n_tstep * _t_mult );
			// Check to see if MaxStage solution will
			// cross the end of the simulation window
			if( t2 > getForecastDate2() ) {
		/********************
				// Modify t1 and t2 so as to solve for 
				// the last acceptable release period, 
				// that is shift t1 and t2 by the same 
				// value so that t2 == 
				// getForecastDate2() and solve for the
				// release
			TSDate endSim =  getForecastDate2();
			int nIntervals = TSDate::getNumIntervals( 
				endSim, t2, _t_int, _t_mult );
			t2.addInterval( _t_int, - 1 * (nIntervals * 
				_t_mult) );
			t1.addInterval( _t_int, - 1 * (nIntervals * 
				_t_mult) );
		********************/

				// Assign the last Release from the reservoir
				// (startRelease) as the solution to MaxStage
				_myValue= _owner->_startRelease;

				PrintWarning( 1, routine, " %s MaxStage "
				"method %s - D/S control %s require "
				"future date %s exceeds end run date %s "
				"at timestep %s.  Returning last release "
				"(%f)", _owner->getID(), _id, _dcp->getID(), t2.toString(), 
				getForecastDate2().toString(),
				cur_date.toString(), _myValue );
/*
				PrintWarning( 1, routine, "Exiting MaxStage "
				"method %s -  downstream control point stage "
				"check date %s exceeds simulation end date %s "
				"while attempting to solve timestep %s.  "
				"Returning last release from owning Reservoir "
				"(%f)", _id, t2.toString(), getForecastDate2().toString(),
				cur_date.toString(), _myValue );
*/
				// Return all values as prior to call to 
				// MaxStage.
				delete sub_root;
				// Reset the carryover flag...
				_owner->_is_co_date = flag;
				
				if( !group_val ) {
					_owner->_release = _myValue;
					//(_dcp->_stage_ts)->setDataValue( 
					//	cur_date, sub_stg );
				}
				else {
					*group_val[0] = _myValue;
				}
				return( STATUS_SUCCESS );
			}
		}
		// Re-initialize for proper construction
		// in buildSubTree()
		_sumLag = 0;
		_sumK = 0;
		_n_tstep = 0;  

		// build the sub-topology that will be used by the method...
		// First, get the sub topology that represents the system that 
		// the Max Stage is being performed on.
		//sub_root = _dcp->buildSubTree( _owner, this );
// printf("kwz---debug0 buildSubTree\n");
		sub_root = _dcp->buildSubTree( _owner, this, t1, inFromOwnRes );
// printf("kwz---debug1 buildSubTree\n");
		if( sub_root == NULL ) {
			PrintError( routine, "Troubles building "
				"sub-topology MaxStage method %s on %s - "
				"cannot execute MaxStage.", _id, _owner->_id );
			// Reset the carryover flag...
			_owner->_is_co_date = flag;
			return( STATUS_FAILURE );
		}
		//kwz.r2433._group was not set. so must set it
		setAllGroups(_dcp,sub_root);
		// Define _SolutionNumber for each component in the subTree
		sub_root->setSolutionOrder(0);

		// Consider and handle any _specialTie circumstances.
		sub_root->buildSubTree_specialTieOut( sub_root, (Component *)_dcp );
		if( sub_root->buildSubTree_specialTieIn( ) ) {
			PrintError( routine, "Troubles building "
				"sub-topology MaxStage method %s on %s due to "
				"\"ToComp\" SetWithdraw complexities - "
				"cannot execute MaxStage.", _id, _owner->_id );
			// Reset the carryover flag...
			_owner->_is_co_date = flag;
			return( STATUS_FAILURE );
		}

		// We will set the inflow on the next down stream component as
		// part of the solution of the MaxStage.
		if( _next_ds_comp == NULL ) {
			PrintError( routine, "Troubles setting next downstream "
				"component for MaxStage %s on %s.  Cannot "
				"execute MaxStage.", _id, _owner->_id );
			delete sub_root;
			// Reset the carryover flag...
			_owner->_is_co_date = flag;
			return( STATUS_FAILURE );
		}

		// Need to know what position the _owner holds in the
		// _next_ds_comp inflow array if there are multiple inflows.
		if( _inflow_pos == -1 ) {
			PrintError( routine, "Troubles establishing next "
				"downstream component for MaxStage %s on %s.  "
				"Cannot execute MaxStage", _id, _owner->_id );
			delete sub_root;
			// Reset the carryover flag...
			_owner->_is_co_date = flag;
			return( STATUS_FAILURE );
		}

		// Instantiate a solver object that will run until the release 
		// from this reservoir is routed to the _dcp. If there are 
		// reaches between _owner and the dcp, the lag time is 
		// considered in the range of dates supplied to the Solver 
		// constructor.
		if( iter == 0 && FirstTime ) {
			// This is the first call to this MaxStage instance.
			// We need to update t2 here as _n_tstep was not
			// non-zero prior to the most recent buildSubTree call.
			t2.addInterval( _t_int, _n_tstep * _t_mult );
		}
		if( t2 > getForecastDate2() ) {
			// Assign the last Release from the reservoir
			// (startRelease) as the solution to MaxStage
			_myValue= _owner->_startRelease;

			PrintWarning( 1, routine, " %s MaxStage "
				"method %s - D/S control %s requires "
				"future date %s exceeds end run date %s "
				"at timestep %s.  Returning last release "
				"(%f)", _owner->getID(), _id, _dcp->getID(), t2.toString(), 
				getForecastDate2().toString(),
				cur_date.toString(), _myValue );
/*
			PrintWarning( 1, routine, "Exiting MaxStage "
				"method %s -  downstream control point stage "
				"check date %s exceeds simulation end date %s "
				"while attempting to solve timestep %s.  "
				"Returning last release from owning Reservoir "
				"(%f)", _id, t2.toString(), getForecastDate2().toString(),
				cur_date.toString(), _myValue );
*/
			// Return all values as prior to call to MaxStage.
			delete sub_root;
			// Reset the carryover flag...
			_owner->_is_co_date = flag;
				
			if( !group_val ) {
				_owner->_release = _myValue;
				//(_dcp->_stage_ts)->setDataValue( 
				//	cur_date, sub_stg );
			}
			else {
				*group_val[0] = _myValue;
			}
			return( STATUS_SUCCESS );
		}

		if( t1 > t2 ) {
			PrintError( routine, "Start date is after end date for "
				"MaxStage %s on %s.", _id, _owner->_id );
			delete sub_root;
			// Reset the carryover flag...
			_owner->_is_co_date = flag;
			return( STATUS_FAILURE );
		}

		// Give the expression tree a new root and then verify them.
		Expression::setComponentRoot( sub_root );
		if( sub_root->verifyExpressions() ) {
			PrintError( routine, "Couldn't verify the "
				"expressions in the sub topology used by "
				"MaxStage %s on %s.  Cannot execute MaxStage.", 
				_id, _owner->_id );
			delete sub_root;
			// Reset the carryover flag...
			_owner->_is_co_date = flag;
			return( STATUS_FAILURE );
		}

		// We got a sub-topology. Now solve 
		// sub-topology iteratively to come within the _criterion of the
		// _max_stage value.
/*
		if( iter != 0 ) {
			if( sub_stg > _max_stage ){
				uplim = release;
			}
			else {
				lowlim = release;
			}
			release = ( uplim + lowlim ) / 2.0;
		}
*/
	while( difference > _criterion && iter <= _max_iter ) {
		// Manually set the inflow at the next downstream component
		// to the release being tested.
		TSDate t;
		HourTS **dsInflow_ts;
		for( t = t1; t <= t2; t.addInterval( _t_int, _t_mult ) ){
			dsInflow_ts = _next_ds_comp->_inflow_ts;
			if ( dsInflow_ts == NULL ) {
				PrintError( routine, "Inflow for component "
					"downstream from MaxStage %s owner %s "
					"is NULL.  This may be an uncorrected "
					"bug related to nested MaxStages.",
					_id, _owner->_id );
				delete sub_root;
				// Reset the carryover flag...
				_owner->_is_co_date = flag;
				return( STATUS_FAILURE );
			}
			dsInflow_ts[_inflow_pos]->setDataValue( t, release );
			//_next_ds_comp->_inflow_ts[_inflow_pos]->setDataValue(
			//	t, release );
		}
			
		// Solve the sub-topology
		if( solve.run( sub_root, t1, t2, _t_int, _t_mult, 
			0 ) ) {
			PrintError( routine, "Troubles solving sub-topology "
				"for %s on %s." , _id, _owner->_id );
			delete sub_root;
			// Reset the carryover flag...
			_owner->_is_co_date = flag;
			return( STATUS_FAILURE );
		}

		double outflowVal = sub_root->_outflow_ts.getDataValue( t2 );
/*		sub_stg = _stage_flow_tbl.lookup( 
			sub_root->_outflow_ts.getDataValue( t2 ),
			GETCOLUMN_1, ALLOW_BOUNDS ); 
		double maxflow = _stage_flow_tbl.lookup( _max_stage, GETCOLUMN_2, ALLOW_BOUNDS ); 
		difference = fabs( ( _max_stage - sub_stg ) / _max_stage 
			* 100.0 );
 printf(" !!!!MaxStage iter= %d group_id=%d ucpname=%s t1=%s t2=%s \n", iter, _group_id, _owner->getID(), t1.toString(), t2.toString());
 printf(" !!!!MaxStage dcpname=%s release=%f dcpflow=%f dcpmaxflow=%f \n", sub_root->getID(), release, outflowVal, maxflow);
*/		

		difference = fabs( ( maxflow - outflowVal ) / maxflow * 100.0 );

		if( iter == 0 && maxflow  > outflowVal ) {
			if( release < (maxflow - outflowVal) ) release = maxflow - outflowVal;
			difference = 0.0;
			break;
		}
		if( release <= _min_release && maxflow < outflowVal ) break;
/*
		if( sub_root != NULL ) {
			delete sub_root;
		}
		// Test first iteration case where _min_release is too high
		if( iter == 0 && _max_stage < sub_stg ) {
			PrintWarning( 1, routine, "MaxStage %s failed to "
				"converge as MINRELEASE (%f) caused "
				"unacceptably high stage (%f) on %s.", _id, 
				_min_release, sub_stg, cur_date.toString() );
			iter = _max_iter;
			MinTooHigh = 1;
		}
*/
		iter++;
		if ( _sumK <= 0.0 || _sumLag <= 0.0 ) {
		  	uplim = maxflow;
		}
		else {
			// if MAXIMUMDISCHARGE has been defined, use it preferentially to 
			// determine the next uplim(Discharge)
			if (_max_discharge != MISSING) {
				uplimDischarge = _max_discharge * 2;
			}
			else if (_stage_flow_tbl.getNRows() != 0) {
				// calculate the next uplim as a function of the stage table
				// since it exists
				uplimDischarge =  _stage_flow_tbl.getMax( GETCOLUMN_2 );
			} 
			else {
				// no table and MAXDISCHARGE not defined, so a Rating Curve from the 
				// process db is being used; _highestFlow was calculated previously,
				// shortly after the initial stage to flow lookup was done (when 
				// it was determined that a process db table was being used)
				uplimDischarge = _highestFlow;
			}
			uplim = _owner->_startRelease + uplimDischarge * 
			( 1.0 + 2.0*_sumK/_sumLag ) + _owner->_release * 
			( 1.0 - 2.0*_sumK/_sumLag );
		}
		lowlim = _min_release;
		// printf(" !!!!MaxStage _sumK=%f _sumLag=%f I1=%f O1=%f O2=%f uplim=%f \n", _sumK, _sumLag, _owner->_startRelease, _owner->_release, _stage_flow_tbl.getMax( GETCOLUMN_2 ), uplim);
		// printf(" !!!!MaxStage difference= %f _criterion= %f \n", difference, _criterion);
		if( outflowVal > maxflow ){
			uplim = release;
		}
		else {
			lowlim = release;
			// printf(" !!!!MaxStage release=%f uplim=%f lowlim=%f \n", release, uplim, lowlim);
			// this looks like a bug; shouldnt the release = stmnt be outside the conditional?
			release = ( uplim + lowlim ) / 2.0;
		}
	}

	if( sub_root != NULL ) {
		delete sub_root;
	}

	// test if we truely tried all iterations and failed 
	if( iter >= _max_iter && !MinTooHigh) {
		PrintWarning( 1, routine, "MaxStage %s failed to converge "
			"within specified tolerance of %f percent within the "
			"maximum iterations of %d (%s).", _id, _criterion, 
			_max_iter, cur_date.toString() );
	}

// printf(" !!!!MaxStage releasefinal= %f \n", release );
	// Set the owner's release...
	_myValue = release;
	if( !group_val ) {
		_owner->_release = _myValue;
		//(_dcp->_stage_ts)->setDataValue( cur_date, sub_stg );
	}
	else {
		*group_val[0] = _myValue;
	}

	// Reset the Expression root to the original.
	Expression::setComponentRoot( _owner->findRoot() );

	// Reset the carryover flag...
	_owner->_is_co_date = flag;

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */

	return( STATUS_SUCCESS );


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/MaxStage_solveMethod.cxx,v $";
 static char rcs_id2[] = "$Id: MaxStage_solveMethod.cxx,v 1.6 2006/10/26 15:27:07 hsu Exp $";}
/*  ===================================================  */

}
