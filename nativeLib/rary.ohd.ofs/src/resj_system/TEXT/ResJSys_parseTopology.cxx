//------------------------------------------------------------------------------
// ResJSys :: parseTopology - parses the topology info out of the file.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
// 
// 13 Jan 1998  Daniel Weiler, RTi	Created initial version.
// 18 Feb 1998	Matthew J. Rutherford, RTi	Updated some of the calls to
//						fit in with the new design.
// 14 May 2001	James R. VanShaar, RTi	Improved error handling
// 11 Oct 2001  JRV, RTi        Added check for ID length
// 11 Dec 2002	JRV, RTi	Added setSolutionOrder usage
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//	fname	I	Control file name.
//
//------------------------------------------------------------------------------
#include "ResJSys.h"
#include "Reservoir.h"
#include "Reach.h"
#include "Node.h"

int ResJSys :: parseTopology( char** topo_list, int n_items )
{
	char		**list=NULL, 
			routine[]="ResJTopology :: parseControlFile";
	int 		found=0, nlist=0, first_line_found=0, n_comp=0, i, j, k;
	int		totErrs=0;
	Component	*comp=NULL, **comp_list=NULL;

	// NOTE on error handling:
	//	The first line must have 2 parts to describe the first component
	//	with no reference to other components.  If this condition is 
	//	met, this function will proceed to test all other components.  
	//	This may result in multiple errors dependent on a single 
	//	problem.
	// 	Nevertheless, this approach should provide the most valuable
	// 	information (plus some extraneous info) on input errors for a 
	//	given run. 

	// Local list of all the Components constructed
	comp_list = new Component* [n_items];

	if( comp_list == NULL ){
		PrintError( routine,
			"Unable to allocate memory for %d Component(s)", 
			n_items );
		delete [] comp_list;
		return( STATUS_FAILURE );
	}

	for( i = 0; i< n_items; i++ ) {
		if( strlen( topo_list[i] ) == 0 || topo_list[i][0] == '#' ){
			continue;
		}

		// Break each string from the topo_list  
		list = BreakStringList(topo_list[i], " \n\t", DELIM_SKIP_BLANKS,
			&nlist);

		// Test first line
		if( !first_line_found ) {
			if( nlist != 2 ){
				list = FreeStringList(list);
				PrintError( routine, 
					"First line of Topology section \"%s\" "
					"is malformed.", topo_list[i] );
				delete [] comp_list;
				return(STATUS_FAILURE);
			}
			else {
				first_line_found = 1;
			}
		}

		// Test for minimum valuable list size
		if( list == NULL || nlist < 2 ){
			totErrs++;
			PrintError( routine,
				"Topology line \"%s\" is malformed. " 
				"Number of components is now erroneous as "
				"count equals total topology lines.", 
				topo_list[i] );
			if( list ){
				list = FreeStringList( list );
			}
			continue;
		}

		// Identify type and create component
		if( !strcmp( "RESERVOIR", list[0] ) ){
			comp = new Reservoir;
		}
		else if( !strcmp( "REACH", list[0] ) ) {
			comp = new Reach;
		}
		else if( !strcmp( "NODE" , list[0] ) ) {
			comp = new Node;
		}	
		else {
			totErrs++;
			PrintError( routine, "Topology line \"%s\": "
				"Unrecognized component type \"%s\".", 
				topo_list[i], list[0] );
			list = FreeStringList( list );
			continue;
		}

		// Now that we have allocated the correct type, we just
		// need to set the id and save it in our local list.  We will
		// also check for size limitations on the ID.
		if( !comp ){
			totErrs++;
			PrintError( routine, "Unable to allocate memory for "
				"%s.", topo_list[i] );
			list = FreeStringList( list );
			continue;
		}
		// Check ID size
		if(strlen(list[1]) > 12 ) {
			totErrs++;
			PrintError( routine, 
				"Component ID \"%s\" is too long ( > 12 chars.",
				list[1] );
			list = FreeStringList( list );
			continue;
		}
		// Set the ID...
		if( comp->setID( list[1] ) ) {
			totErrs++;
			PrintError( routine, 
				"Troubles setting compnent ID to \"%s\".",
				list[1] );
			list = FreeStringList( list );
			continue;
		}

		// Place the component on the temporary list...
		comp_list[n_comp] = comp;

		// We need to loop through the rest of the strings
		// on this line and attach things as necessary...
		for( j = 2; j < nlist; j++ ) {
			if(!strcmp( "BELOW", list[j] ) ) {
				found = 0;
				for( k=0; k<n_comp; k++ ) {
					if( !strcmp( comp_list[k]->getID(), 
						list[j+1])){
						found = 1;
						break;
					}
				}
				if( found ){
					if( comp->addSon( comp_list[k] ) ){
						// This will happen if we have
						// two (or more) components
						// downstream of the same 
						// component.
						totErrs++;
						PrintError( routine, 
						     "Topology line \"%s\": "
						     "Topological linkage not "
						     "set between %s and %s.", 
						     topo_list[i], 
						     comp->getID(),
						     comp_list[k]->getID() );
						continue;
					}

					// Wire the inflow and outflow together.
					comp->setInflowTS( 
					comp_list[k]->getOutflowTS() ); 
				}
				else {
					totErrs++;
					PrintError( routine,
						"Topology line \"%s\": "
						"Referenced component \"%s\" "
						"has not been defined yet.",
						topo_list[i], list[j+1] );
					continue;
				}
			}
		}
		n_comp++;
		list = FreeStringList( list );
	}

	if( n_comp == 0 ){
		delete [] comp_list;
		PrintError( routine, "Topology section has no valid items." );
		return( STATUS_FAILURE );
	}

	if ( totErrs > 0 ) {
		delete [] comp_list;
		return( STATUS_FAILURE );
	}

	// We need to use any of the components to find the root...

	_root = comp_list[0]->findRoot();

	// Define the SolutionNumber values for each component
	_root->setSolutionOrder(0);

	// Delete the temporary component list.
	delete [] comp_list;

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_system/RCS/ResJSys_parseTopology.cxx,v $";
 static char rcs_id2[] = "$Id: ResJSys_parseTopology.cxx,v 1.5 2006/10/26 15:31:25 hsu Exp $";}
/*  ===================================================  */

}
