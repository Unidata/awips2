//------------------------------------------------------------------------------
// Balance :: construct - Reads a stringlist and constructs precip and/or
//				evap timeseries data.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 24 Apr 1998	Daniel K. Weiler, Riverside Technology, inc
//					Created initial version.
// 12 Jun 2002	James R. VanShaar, RTi	Added _max_rel.
// 13 Jun 2002	JRV, RTi	Improved error handling.
//				Added use of _totBalStor and _ace.
// 09 Jul 2002	JRV, RTi	Added preferred keywords BALRES and ENDBALRES 
// 				for original RESERVOIR and ENDRESERVOIR 
// 				keywords.
// 29 Jul 2002	JRV, RTi	Improved error handling associated with ACE.
// 2003-11-21 Luiz Teixeira, RTi - Added list = FreeStringList( list ) at the 
//  				end of the main for loop in the construct method
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------

#include "Balance.h"
#include "ResJSys.h"
#include "TSUtil.h"

int Balance :: construct ( char** re_list, int n_items )  
{
	char routine[] = "Balance :: construct", **list = NULL,
		**sub_list = NULL, temp[MAXC];
	Reservoir*  res = NULL;
	int i, j, nlist = 0, nsub = 0, res_count = 0, totErrs = 0,
		alreadyACE = 0;
	double ffactor = 1.0, vfactor = 1.0, lfactor = 1.0;

	if( Method :: getUnitType() == ENGLISH ) {
		ffactor = 0.028317;
		lfactor = 0.3048;
		vfactor = 1233.5;
	}
	
	// There must be at least one reservoir...
	if( n_items <= 1 ) {
		PrintError( routine, "Must specify at least one "
			"reservoir for Balance method %s on %s.", _id,
			_owner->_id );
		return( STATUS_FAILURE );
	}

	for( i = 0; i < n_items; i++ ) {
		if( strlen( re_list[i] ) == 0 || re_list[i][0] == '#' ) {
			continue;
		}

		list = BreakStringList( re_list[i], " \n\t",
			DELIM_SKIP_BLANKS, &nlist );
		if( list == NULL || nlist < 1 ) {
			PrintError( routine, "Troubles constructing "
				"Balance method %s on %s.", _id, _owner->_id );
			return( STATUS_FAILURE );
		}

		// Determine the mode of balancing.
		if( !strcasecmp( list[0], "VOLUME" ) ) {
			if ( _bal_mode == PERCENT ) {
				PrintWarning( 1, routine, "Balance method %s "
					"on %s previously defined to work in "
					"PERCENT mode.  Changing to VOLUME "
					"mode.", _id, _owner->_id );
			}
			_bal_mode = VOLUME; 
			list = FreeStringList( list );
			continue;
		}
		if( !strcasecmp( list[0], "PERCENT" ) ) {
			if ( _bal_mode == VOLUME ) {
				PrintWarning( 1, routine, "Balance method %s "
					"on %s previously defined to work in "
					"VOLUME mode.  Changing to PERCENT "
					"mode.", _id, _owner->_id );
			}
			_bal_mode = PERCENT; 
			list = FreeStringList( list );
			continue;
		}
		if( !strcasecmp( list[0], "ACE" ) || !strcasecmp( list[0], 
			"ATTENUATION_CONSTANT_FOR_EVACUATION") ) {
			if ( alreadyACE ) {
				PrintWarning( 1, routine, "ACE has been "
					"specified more than once.  Value will "
					"be %f.  Was %f. (BALANCE method %s.)",
					list[2], _ace, _id );
			}
			alreadyACE = 1;
			_ace = atof( list[1] );
			if ( _ace > 1 ) {
				PrintWarning( 1, routine, "ACE--%f > 1.0! "
					"This is likely to increase "
					"oscillation and will not balance the "
					"reservoirs.  (BALANCE method %s.)",
					_ace, _id );
			}
			list = FreeStringList( list );
			continue;
		}
		if( !strcasecmp( list[0], "RESERVOIR" ) || 
			!strcasecmp( list[0], "BALRES" ) ) {
			if( nlist < 2 ) {
				PrintError( routine, "Identifier must "
					"follow keyword %s on BALANCE method "
					"%s.", list[0], _id );
				totErrs++;
				list = FreeStringList( list );
				continue;
			}

			// Give recommended syntax change NOTE, if applicable
			if (!strcasecmp( list[0], "RESERVOIR" ) ) {
				sprintf( temp, "**NOTE** Use of \"RESERVOIR\" "
					"in BALANCE method parameterization is "
					"no longer recommended.  Please use "
					"\"BALRES\"." );
				int length = strlen( temp );
				int ipr = ResJSys::getIPR(); 
				ResJ_ccwrite( &length, temp, &ipr );
			}

			// Get the pointer to the reservoir
			res = (Reservoir*) ((_owner->findRoot())->
				getComponentPtr( list[1] ) );
			if( res == NULL ) {
				PrintError( routine, "Troubles getting "
					"Reservoir %s for BALANCE method %s.", 
					list[1], _id );
				list = FreeStringList( list );
				totErrs++;
			}

			// Now make sure that it is actually a Reservoir...
			if( strcasecmp( res->_type, "RESERVOIR" ) ) {
				PrintError( routine, "Cannot perform "
					"Balance method on Component %s - not "
					"a Reservoir.", list[0] );
				list = FreeStringList( list );
				totErrs++;
			}

			// Since we are allowing upstream and downstream 
			// reservoirs in the Balance, we need to make an
			// internal note as to where the owner sits in the 
			// list.
			if( !strcasecmp( res->_id, _owner->_id ) ) {
				_owner_pos = res_count;
			}

			// So we have the pointer to a bona fide Reservoir.
			_balance_res[ res_count ] = res;

			// Free up the list so we can use it again.
			list = FreeStringList( list );

			// Now we get a substring of the parameters for this
			// reservoir...
			sub_list = GetSubStringList( &re_list[i+1], 
				n_items-i-1, "VALUES", 
				&nsub, RETURN_NO_KEYWORDS );
			if( sub_list == NULL || nsub == 0 ) {
				PrintError( routine, "Troubles getting "
					"Reservoir data for BALANCE method %s.",
					_id );
				totErrs;
				continue;
			}
			for( j = 0; j < nsub; j++ ) {
				list = BreakStringList( sub_list[j], " \n\t",
					DELIM_SKIP_BLANKS, &nlist );
				if( list == NULL || nlist == 0 ) {
					PrintError( routine, "Troubles getting "
						"Reservoir data for BALANCE "
						"method %s.", _id );
					totErrs;
					continue;
				}
				if( !strcasecmp( list[0], "LOWER" ) ) {
					if( nlist != 3 ) {
						PrintError( routine,
						"BALANCE keyword %s must be "
						"followed by either POOL or "
						"STORAGE and a value for "
						"Reservoir %s, method %s.",
						list[0], res->_id, _id );
						list = FreeStringList( list );
						totErrs;
						continue;
					}
					if( !strcasecmp( list[1], "STORAGE" ) ){
						_lower_stor[res_count] = 
							atof( list[2] )*vfactor;
					}
					else if( !strcasecmp( list[1], 
						"POOL" ) ) {
						// BALANCE works with storages.
						// Convert the pool elevation
						// only as part of initializing
						// the storage variable.
						// NOTE: the elevation-storage
						// table has already been 
						// converted.
						_lower_stor[res_count] = 
						  res->_elev_stor_tbl.lookup(
						  atof( list[2] )*lfactor, 
						  GETCOLUMN_2, ALLOW_BOUNDS );
					}
					// Check here to see that we have
					// reasonable values as compared to
					// the owner's values
					double minStorage = res->_elev_stor_tbl.
						lookup(res->_min_pool, 
						GETCOLUMN_2, ALLOW_BOUNDS );;
					if(minStorage > _lower_stor[res_count]){
						PrintWarning( 1, routine, 
						  "Specification of %s %s = %s "
						  "causes minimum storage to "
						  "exceed lower storage bound "
						  "( %f > %f ) on Reservoir %s "
						  "for BALANCE method %s. "
						  "Setting to %f.", list[0], 
						  list[1], list[2], minStorage,
						  _upper_stor[res_count], 
						  res->_id, _id, minStorage );
						_lower_stor[res_count] = 
							minStorage;
					}
				}
				else if( !strcasecmp( list[0], "UPPER" ) ) {
					if( nlist != 3 ) {
						PrintError( routine,
						  "BALANCE keyword %s must be "
						  "followed by either POOL or "
						  "STORAGE and a value for "
						  "Reservoir %s, method %s.",
						  list[0], res->_id, _id );
						list = FreeStringList( list );
						totErrs;
						continue;
					}
					if( !strcasecmp( list[1], "STORAGE" ) ){
						_upper_stor[res_count] = 
							atof( list[2] )*vfactor;
					}
					else if( !strcasecmp( list[1], 
						"POOL" ) ) {
						_upper_stor[res_count] = 
						res->_elev_stor_tbl.lookup(
						atof( list[2] )*lfactor, 
						GETCOLUMN_2, ALLOW_BOUNDS );
					}
					// Check here to see that we have
					// reasonable values as compared to
					// the owner's values
					double maxStorage = res->_elev_stor_tbl.
						getMax( GETCOLUMN_2 );
					if(maxStorage < _upper_stor[res_count]){
						PrintWarning( 1, routine, 
						  "Specification of %s %s = %s "
						  "causes upper storage bound "
						  "to exceed maximum storage "
						  "( %f > %f ) on Reservoir %s "
						  "for BALANCE method %s. "
						  "Setting to %f.", list[0], 
						  list[1], list[2], 
						  _upper_stor[res_count], 
						  maxStorage, res->_id, _id,
						  maxStorage );
						_upper_stor[res_count] = 
							maxStorage;
					}
				}
				else if( !strcasecmp( list[0], "MINRELEASE" ) ){
					_min_rel[res_count] = atof( list[1] ) *
						ffactor;
					if( _min_rel[res_count] < 
						res->_min_release ) {
						PrintWarning(1, routine, 
						 "Minimum release specified on "
						 "Balance method %s less than "
						 "Reservoir %s min release. "
						 "( %f < %f ).  Setting to %f.",
						 _id, res->_id, 
						 _min_rel[res_count], 
						 res->_min_release, 
						 res->_min_release);
						_min_rel[res_count] = 
							res->_min_release;
					}
				}
				else if( !strcasecmp( list[0], "MAXRELEASE" ) ){
					_max_rel[res_count] = atof( list[1] ) *
						ffactor;
					if ( _min_rel[res_count] > 
						_max_rel[res_count] ) {
						PrintWarning(1, routine, 
						  "MINRELEASE specified to be "
						  "larger than MAXRELEASE for "
						  "reservoir %s in BALANCE "
						  "method %s. MAXRELEASE set "
						  "to MINRELEASE.", res->_id, 
						  _id);
						_max_rel[res_count] = 
							_min_rel[res_count];
					}
				}
				else if( !strcasecmp( list[0], "ACE" ) || 
					!strcasecmp( list[0], 
					"ATTENUATION_CONSTANT_FOR_EVACUATION") ) {
					PrintError( routine, "ACE is a global "
						"parameter for the BALANCE "
						"method and should not be "
						"included in the BALRES block. "
						"Error in BALANCE method %s.", 
						_id );
					totErrs++;
				}
				list = FreeStringList( list );
			}
			sub_list = FreeStringList( sub_list );

			// Check here to make sure that we got everything we 
			// need for the Balance method.
			// For all reservoirs:
			if( _upper_stor[res_count] == MISSING ) {
				PrintError( routine, "Must specify UPPER POOL "
					"or UPPER STORAGE for reservoir %s in "
					"BALANCE method %s.", res->_id, _id );
				totErrs++;
			}
			// For the owning reservoir:
			if( !strcmp( res->_id, _owner->_id ) ) {
				if ( _min_rel[res_count] == MISSING || 
					_max_rel[res_count] ==	MISSING ) { 
					PrintError( routine, "Must specify "
						"MINRELEASE and MAXRELEASE for "
						"reservoir %s in BALANCE "
						"method %s.", res->_id, _id);
					totErrs++;
				}
				if (_min_rel[res_count] > _max_rel[res_count]) {
					PrintError( routine, "MINRELEASE > "
						"MAXRELEASE ( %f > %f ) for "
						"reservoir %s in BALANCE "
						"method %s.", 
						_min_rel[res_count] / ffactor, 
						_max_rel[res_count] / ffactor, 
						res->_id, _id);
					totErrs++;
				}
				if ( _bal_mode == PERCENT && 
					_lower_stor[res_count] == MISSING ) {
					PrintError( routine, "Must specify "
						"LOWER POOL or LOWER STORAGE "
						"for reservoir %s in BALANCE "
						"method %s when in PERCENT "
						"mode.", res->_id, _id);
					totErrs++;
				}
			}
			res_count++;
			i += nsub + 1;
			continue;
		}

		if ( !strcasecmp(list[0], "ENDVALUES") ||
			!strcasecmp(list[0], "ENDRESERVOIR") ||
			!strcasecmp(list[0], "ENDBALRES")) {
			// Give recommended syntax change NOTE, if applicable
			if (!strcasecmp( list[0], "ENDRESERVOIR" ) ) {
				sprintf( temp, "**NOTE** Use of "
					"\"ENDRESERVOIR\" in BALANCE method "
					"parameterization is no longer "
					"recommended.  Please use "
					"\"ENDBALRES\"." );
				int length = strlen( temp );
				int ipr = ResJSys::getIPR(); 
				ResJ_ccwrite( &length, temp, &ipr );
			}
			list = FreeStringList( list );
			continue;
		}

		// We got here because the keyword was not recognized
		PrintWarning( 1, routine, "Keyword %s not recognized for "
			"BALANCE method %s.", list[0], _id );
			
		// Freeing memory 
		if ( list ) {
			list = FreeStringList( list );  // 2003-11-21 LT
		}	
		continue;
	}
	_n_res = res_count;

	// Make sure we got all of the things from the control file we need
	// to perform a balance operation.
	if( _n_res < 1 ) {
		PrintError( routine, "Balance method %s must have at "
			"least one reservoir.", _id );
		totErrs++;
	}
	if( _bal_mode == 0 ) {
		PrintError( routine, "Balance method %s must have a "
			"VOLUME or PERCENT mode specified.", _id );
		totErrs++;
	}
	if( _owner_pos == -1 ) {
		PrintError( routine,"Troubles finding owner in Balance "
			"system." );
		totErrs++;
	}

	// If PERCENT mode, determine the total balancing storage available
	for( i = 0; i < _n_res; i++ ) {
		_totBalStor += _upper_stor[i] - _lower_stor[i];
	}

	// Check for errors
	if ( totErrs > 0 ) {
		return( STATUS_FAILURE );
	}

	_is_constructed = 1;
	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_topology/RCS/Balance_construct.cxx,v $";
 static char rcs_id2[] = "$Id: Balance_construct.cxx,v 1.7 2006/10/26 15:11:08 hsu Exp $";}
/*  ===================================================  */

}		


