//------------------------------------------------------------------------------
// ResJSys :: parseParameters - parses the topology info out of the file.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 13 Feb 1998  Daniel Weiler, RTi	Created initial version.
// 04 Mar 1998	Matthew J. Rutherford, RTi
//				Changed some of the logic in here to make the
//				routine work for all Methods that are added
//				in the future.
// 07 Apr 1998	DKW		Added the state variable parsing on the 
//				component.
// 14 May 2001	James R. VanShaar, RTi	Improved error handling
// 08 Jul 2002	JRV, RTi	Modified algorithm to parse Components on a 
//				first pass and Methods on a second pass.
// 31 Oct 2003	JRV, RTi	Modified to free memory completely.
// 16 Mar 2006	JRV, RTi	Added checks for MaxStage issues required by
// 				Lookup3.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//	fname	I	Control file name.
//
//------------------------------------------------------------------------------
#include "Lookup3.h"
#include "MaxStage.h"
#include "ResJSys.h"
#include "Method.h"

int ResJSys :: parseParameters( char** param_list, int n_items )
{
	char	routine[]="ResJSys :: parseParameters", **list=NULL, 
		**sub_list=NULL, **method_list=NULL, temp[256];
	int 	nlist=0, i=0, j=0, k=0, count=0, totErrs=0, method_listSize=0,
		endFound;
	// Arrays for Lookup3 / MaxStage checks.
	// If the user has more than 20 MaxStage methods . . . Oh Boy.
	char **MaxStageOWN=NULL, **MaxStageID=NULL, 
	    **Lookup3OWN=NULL, **Lookup3TOCOMP=NULL;
	int nMS=0, nLUOwn=0, nLUToComp=0;
	Method	*method=NULL;
	Component *comp=NULL;

	if( !param_list || !n_items ){
		PrintWarning( 1, routine, "Empty parameters list." );
		return( STATUS_FAILURE );
	}

	// Parse out component parameters and construct a secondary list 
	// containing method parameters
	for( i = 0; i< n_items; i++ ) {
		if( strlen( param_list[i] ) == 0 || param_list[i][0] == '#' ){
			continue;
		}

		// Break each string from the param_list  
		list = BreakStringList( param_list[i], " \n\t", 
			DELIM_SKIP_BLANKS, &nlist);

		// Units need to be set first
		if( !strcasecmp( list[0], "UNITS" ) && i != 0 ) {
			totErrs++;
			PrintError( routine, "If explicitly defined, UNITS "
				"must be first item in PARAMETERS." );
			list = FreeStringList( list );
			continue;
		}

		if( !strcasecmp( list[0], "UNITS" ) ) {
			if( !strcasecmp( list[1], "ENGLISH" ) ) {
				Method :: setUnitType( ENGLISH );
			}
			else if ( !strcasecmp(list[1], "SI") || 
				!strcasecmp(list[1], "METRIC") ) {
				Method :: setUnitType( SI );
			}
			else {
				// Something irregular explicitly set--default 
				// to SI, but print warning.
				PrintWarning( 1, routine, "UNITS assignment "
					"\"%s\" irregular.  Set to default, "
					"METRIC.", list[1] );
				Method :: setUnitType( SI );
			}
			list = FreeStringList( list );
			continue;
		}

		// If a state variable data member of a Component is being 
		// defined, parsing (and storing) will be done by the Component
		// function.
		if( !strcasecmp( list[0], "RESERVOIR" ) ||
			!strcasecmp( list[0], "REACH" ) ||
			!strcasecmp( list[0], "NODE" ) ) {
			if ( list[1] == NULL ) {
				totErrs++;
				PrintError( routine, "No identifier supplied "
					"with keyword \"%s\" in PARAMETERS.", 
					list[0] );
				list = FreeStringList( list );
				continue;
			}

			// Break out appropriate parameter information for the
			// component type.
			sub_list = GetSubStringList( &param_list[i], n_items-i,
				list[0], &count, RETURN_NO_KEYWORDS );
			if ( sub_list == NULL ) {
				totErrs++;
				PrintError( routine, "Could not create "
					"sub-list beginning with line \"%s\". "
					" Perhaps no 'END%s'?", param_list[i], 
					list[0] );
				list = FreeStringList( list );
				continue;
			}
			// Note: if there is nothing between the keywords, 
			//	sub_list was set with one value "****NADA****" 
			// 	and the value of count was set to 0.  Count will
			//	be used to ensure that the list won't really be
			//	used.

			// Find the component and let it parse its information
			comp = _root->getComponentPtr( list[1] );
			if( comp == NULL ) {
				totErrs++;
				PrintError( routine, "Cannot find "
					"component for \"%s\".", list[1] );
				sub_list = FreeStringList( sub_list );
				list = FreeStringList( list );
				continue;
			}
			if( comp->setStates( sub_list, count ) ) {
				// Error handling occurs within the function 
				// itself.
				totErrs++;
				sub_list = FreeStringList( sub_list );
				list = FreeStringList( list );
				i += (count+1);
				continue;
			}

			sub_list = FreeStringList( sub_list );
			list = FreeStringList( list );
			i += (count+1);
			continue;
		}

		if( !list || nlist != 3 ) {
			totErrs++;
			PrintError( routine, 
				"Expecting 3 parts to Parameter Line \"%s\".",
				 param_list[i] );
			if( list ) {
				list = FreeStringList( list );
			}
			continue;
		}

		// If we are dealing with a constant line then we just need
		// to skip over it, since these are dealt with in the
		// parseConstants routine....
		if( !strcasecmp( list[0], "CONSTANT" ) ) {
			PrintDebug( 15, routine,
				"Skipping over CONSTANT line: \"%s\".", 
				param_list[i] );
			list = FreeStringList( list );
			continue;
		}

		// If we get this far, likely, we are working with a method

		// Construct the keyword signaling the end of the method's 
		// parameterization
		sprintf( temp, "END%s", list[0] );
		list = FreeStringList( list );

		// Copy the method lines to another string list which we will 
		// process shortly.
		method_list = AddToStringList( method_list, param_list[i], 
			&method_listSize );
		endFound = 0;
		while ( i < n_items-1 ) {
			i++;
			// Break each string from the param_list  
			list = BreakStringList( param_list[i], " \n\t", 
				DELIM_SKIP_BLANKS, &nlist);
			if ( !strcasecmp( list[0], temp ) ) {
				// We're to the end.  Add it and get out of loop
				method_list = AddToStringList( method_list, 
					param_list[i], &method_listSize );
				endFound = 1;
				list = FreeStringList( list );
				break;
			}
			// Add it and increment the counter
			method_list = AddToStringList( method_list, 
				param_list[i], &method_listSize );
			list = FreeStringList( list );
		}
		if ( endFound == 0 ) {
			// If we get here we did not find the end of the 
			// keyword.  Perhaps it was not a method, but it didn't 
			// fit into the category of a component either.  Sound 
			// a message.
			totErrs++;
			PrintError( routine, "Parameter keyword does not "
				"have matching end keyword (%s).", temp );
		}
	}

	// Handle the secondary list which is a list of the methods contained in
	// the original param_list
	for( i = 0; i< method_listSize; i++ ) {
		// Break each string from the method_list  
		list = BreakStringList( method_list[i], " \n\t", 
			DELIM_SKIP_BLANKS, &nlist);

		// First we need to make a sub-list based on the list[0] 
		// token. This sub list needs to be taken out even if we don't
		// use the parameters...
		count = 0;
		sub_list = GetSubStringList( &method_list[i], method_listSize-i,
			list[0], &count, RETURN_NO_KEYWORDS );
		if ( sub_list == NULL ) {
			totErrs++;
			PrintError( routine, "Could not create sub-list "
				"beginning with line \"%s\".  Perhaps "
				"no 'END%s'?", 
				method_list[i], list[0] );
			i = method_listSize;
			list = FreeStringList( list );
			continue;
		}

		// We have to update the indexer so that we do not try to
		// deal with any of the sub_list lines again...We have to 
		// add 1 to the count so that we account for the first and
		// keywords (the second gets taken care of in the i++ in the 
		// main for loop above.
		i += (count + 1);

		// Next, we need to make sure that this method has been 
		// allocated (should have happened in parseRules). The order for
		// the getMethod function is ComponentID, MethodID, 
		// MethodType...

		// ####### The following has been added and subsequently 
		// ####### commented out following an alternative handling of 
		// ####### the problem.  It remains here for potential debugging
		// ####### purposes.
		// We are going to do this twice to allow new code in the first
		// attempt to give warnings on duplicate method identifiers.
		// However, the second time will use all pieces of the method
		// identifier (type, component, and identifier) to do the best
		// job in getting the correct one.  The issue at stake is that
		// carryover does not preserve the component (except for LagK),
		// so same identifier methods could get confused.
		// 
		// method = _root->getMethod( list[2] );
		// ####### End

		method = _root->getMethod( list[0], list[1], list[2] );

		if( !method ){
			PrintWarning( 1, routine,
				"%s %s %s was not initialized in parseRules "
				"section...skipping.", list[0], list[1], 
				list[2] );
			list = FreeStringList( list );
			sub_list = FreeStringList( sub_list );
			continue;
		}
		// Ok so we found a method for this group of parameters, we
		// just need to parse it...
		// NOTE: Currently (5/15/01) error handling occurs here!!
		if( method->construct( sub_list, count ) ) {
			totErrs++;
			PrintError( routine, "Troubles constructing %s.",
			 	method_list[i - count -1] );
			list = FreeStringList( list );
			sub_list = FreeStringList( sub_list );
			continue;
		}

		// Track any MaxStage methods
		if( !strcasecmp( list[0], "MAXSTAGE" ) )
		{
		    // Add strings to lists.  Counter handled carefully.
		    int tI=nMS;
		    MaxStageOWN = AddToStringList( MaxStageOWN, list[1], &tI );
		    MaxStageID = AddToStringList( MaxStageID, list[2], &nMS );
		}
		// Track any Lookup3 methods
		else if( !strcasecmp( list[0], "LOOKUP3" ) )
		{
		    // Track owners of Lookup3 methods
		    Lookup3OWN = AddToStringList( Lookup3OWN, list[1], &nLUOwn );

		    // Track receivers of any Lookup3 toComp transfers.
		    Component *RComp = ((Lookup3*)method)->getReceivingComp();
		    if( RComp )
		    {
		        Lookup3TOCOMP = AddToStringList( Lookup3TOCOMP, 
		            RComp->getID(), &nLUToComp );
		    }
		}

		list = FreeStringList( list );
		sub_list = FreeStringList( sub_list );
	}

	// Now check any MaxStage / Lookup3 interaction, if necessary
	i = 0;
	while( i < nMS && !totErrs )
	{
	    // Get the current MaxStage method, its downstream control point and
	    // the upstream control point (owner).
	    method = _root->getMethod( "MAXSTAGE", MaxStageOWN[i],
	        MaxStageID[i] );
	    Node *dcp = ((MaxStage*)method)->getDCP();
	    Component *ucp=NULL;
	    ucp = ((MaxStage*)method)->getOwner();

	    // Determine which components are in the MaxStage subtree
	    char** MSComp = NULL;
	    int nComp = 0;
	    MSComp = dcp->buildSubTree_CompIDList( ucp, MSComp, &nComp );

	    for( j=0; j<nComp; j++ )
	    {
	        // Now compare this subtree with owners of Lookup3 Methods
	        for( k = 0; k < nLUOwn; k++ )
	        {
	            // Are any of these components owners of a Lookup3 method?
	            if( !strcasecmp( MSComp[j], Lookup3OWN[k] ) )
	            {
	                totErrs++;
	                char *own = ucp->getID();
	                char *id = method->getID();
	                PrintError( routine, "Lookup3 method owned by %s is "
	                    "located within subtree of MaxStage %s %s.",
	                    Lookup3OWN[k], ucp->getID(), method->getID() );
	            }
	        }

	        // Now compare this subtree with receivers of Lookup3 ToComp
	        for( k = 0; k < nLUToComp; k++ )
	        {
	            // Are any of these components receivers of a Lookup3 method
	            // toComp transfer?
	            if( !strcasecmp( MSComp[j], Lookup3TOCOMP[k] ) )
	            {
	                totErrs++;
	                PrintError( routine, "Lookup3 method transfers to "
	                    "Component %s, located within subtree of MaxStage "
	                    "%s %s.", Lookup3TOCOMP[k], ucp->getID(),
	                    method->getID() );
	            }
	        }
	    }
	    MSComp = FreeStringList( MSComp );
	    i++;
	}
	if( MaxStageOWN )
	{
	    MaxStageOWN = FreeStringList( MaxStageOWN );
	    MaxStageID = FreeStringList( MaxStageID );
	}
	if( Lookup3OWN )
	{
	    Lookup3OWN = FreeStringList( Lookup3OWN );
	    if( Lookup3TOCOMP )
	    {
	        Lookup3TOCOMP = FreeStringList( Lookup3TOCOMP );
	    }
	}
	
	if( method_list ) {
		method_list = FreeStringList( method_list );
	}

	// Parse method parameters

	if ( totErrs > 0 ) {
		return( STATUS_FAILURE );
	}

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_system/RCS/ResJSys_parseParameters.cxx,v $";
 static char rcs_id2[] = "$Id: ResJSys_parseParameters.cxx,v 1.8 2006/10/26 15:31:15 hsu Exp $";}
/*  ===================================================  */

}
