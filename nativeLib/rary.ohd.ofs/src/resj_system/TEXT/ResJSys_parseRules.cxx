//------------------------------------------------------------------------------
// ResJSys::parseRules - top level code to parse out the rules.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1)The rules section of the control file comes in as follows:
//	
//	[ EXPRESSION ]
//		::Action CompID MethID
//		::Action CompID MethID
//
// Where EXPRESSION is any conditional expression. If Actions are always to be 
// performed the expression TRUE is used.
//------------------------------------------------------------------------------
// History:
// 
// 17 Feb 1998	Matthew J. Rutherford, Riverside Technology, inc
//					Created initial version.
// 18 Feb 1998	MJR	Put in the guts to do the actual parsing.
// 24 Feb 1998	MJR	Change the code so that it matches with the current
//			setup described in the Notes section.
// 04 Mar 1998	MJR	Put in a check so that no method can be in the rules
//			twice.
// 22 Apr 1998	Daniel Weiler, RTi	Instantiated all of the methods
//					on the param list. This must be done
//					at this level so that Methods that
//					derive from ComboMethod can solve.
// 14 Oct 1998  DKW, RTi	Dynamically allocated expr_str to accomodate
//				large expression strings.
// 14 May 2001	James R. VanShaar, RTi	Improved error handling
// 11 Oct 2001  JRV, RTi        Added check for ID length
// 24 Dec 2002	JRV, RTi 	Added Spillway.
// 23 AUG 2004	KSH, OHD	Added Passflow method
// 06 Feb 2006  JRV, RTi    Added Lookup3 and extra work associated with
//                          creation of ComponentMethod.
//------------------------------------------------------------------------------
// Variables:	I/O	Description		
//
//
//------------------------------------------------------------------------------
#include "Adjust.h"
#include "Balance.h"
#include "CalcInflow.h"
#include "ExprParser.h"
#include "Expression.h"
#include "LagK.h"
#include "Lookup3.h"
#include "MaxDecrease.h"
#include "MaxIncrease.h"
#include "MaxStage.h"
#include "RainEvap.h"
#include "Passflow.h"
#include "Reservoir.h"
#include "ResJSys.h"
#include "SetElevation.h"
#include "SetMax.h"
#include "SetMin.h"
#include "SetRelease.h"
#include "SetSum.h"
#include "SetWithdraw.h"
#include "Spillway.h"
#include "ReachMethod.h"
#include "ReservoirMethod.h"

#define GLOBAL_STR "TRUE"
#define GLOBAL_EXP "1 == 1"
#define RULE_TOK "::"
#define LBRACE '['
#define RBRACE ']'

int ResJSys::parseRules( char** list, int nlist, char** param_list,
	int n_param )
{
	char		expr_str[5000] = "", routine[]="ResJSys::parseRules", 
			rule[5000], **sub_list=NULL, res_name[13] = "";
	Component 	*comp=NULL;
	Expression 	*expr=NULL;
	int		i=0, j, pos, nsub_list=0, meth_count=0, totErrs=0,
			nspil=0, nrel_spil=0;
	Method		*method=NULL,
			*intermed_list[500];

	// First off, lets instantiate all of the Methods in the param
	// list.
	for( i = 0; i < n_param; i++ ) {
		sub_list = BreakStringList( param_list[i], " ", 
			DELIM_SKIP_BLANKS, &nsub_list );
		if( !sub_list || nsub_list != 3 ){
			if( sub_list ){
				sub_list = FreeStringList( sub_list );
			}
			continue;
		}

		// Check for keywords, if none found, continue
		if( strcasecmp( sub_list[0], "ADJUST" ) && 
			strcasecmp( sub_list[0], "BALANCE" ) && 
			strcasecmp( sub_list[0], "CALCINFLOW" ) &&
			strcasecmp( sub_list[0], "LAGK" ) && 
			strcasecmp( sub_list[0], "LOOKUP3" ) && 
			strcasecmp( sub_list[0], "MAXDECREASE" ) && 
			strcasecmp( sub_list[0], "MAXINCREASE" ) && 
			strcasecmp( sub_list[0], "MAXSTAGE" ) && 
			strcasecmp( sub_list[0], "RAINEVAP" ) && 
			strcasecmp( sub_list[0], "PASSFLOW" ) && 
			strcasecmp( sub_list[0], "SETELEVATION" ) && 
			strcasecmp( sub_list[0], "SETMAX" ) && 
			strcasecmp( sub_list[0], "SETMIN" ) && 
			strcasecmp( sub_list[0], "SETRELEASE" ) && 
			strcasecmp( sub_list[0], "SETSUM" ) && 
			strcasecmp( sub_list[0], "SETWITHDRAW" ) &&
			strcasecmp( sub_list[0], "SPILLWAY" ) ) {
			
			if ( sub_list) {
				sub_list = FreeStringList( sub_list );
			}
			continue;
		}

		// Before we instantiate a new one, we need to make
		// sure that this method/component combination doesn't
		// already exist. This could happen, for example, when we come
		// to a SETMAX method that uses two other methods which were
		// previously defined.  If it does, we just continue...
		if( _root->getMethod( sub_list[0], sub_list[1], 
			sub_list[2] ) ) {
			// This is NOT an error.
			PrintDebug( 10, routine,
				"Duplicate Method: %s %s %s.",
				sub_list[0], sub_list[1], sub_list[2] );
			if ( sub_list) {
				sub_list = FreeStringList( sub_list );
			}
			continue;
		}

		// Now we need to get the name of the component
		// that we are dealing with...

		PrintDebug( 10, routine,
			"Searching tree for component \"%s\".",
			sub_list[1] );

		comp = _root->getComponentPtr( sub_list[1] );

		if( comp == NULL ) {
			totErrs++;
			PrintError( routine,
				"PARAMETERS line \"%s\": "
				"Unable to find \"%s\" in Component tree. "
				"Check Topology.", param_list[i], sub_list[1] );
			if ( sub_list) {
				sub_list = FreeStringList( sub_list );
			}
			continue;
		}

		// Now we do a big check for its type and instatiate the method
		// we are dealing with.  NOTE: This only creates an object and
		// allocates memory--it does not parameterize the method
		if( !strcmp( sub_list[0], "ADJUST" ) ) {
			// We have a Adjust method...
			method = new Adjust( (Reservoir*)comp);
		}
		else if( !strcmp( sub_list[0], "BALANCE" ) ) {
			// We have a Balance method...
			method = new Balance( (Reservoir*)comp );
		}
		else if( !strcmp( sub_list[0], "CALCINFLOW" ) ) {
			// We have a CalcInflow method...
			method = new CalcInflow( (Reservoir*)comp );
		}
		else if( !strcmp( sub_list[0], "LAGK" ) ) {
			// We have a LagK method...
			method = new LagK( (Reach*)comp );
		}
		else if( !strcmp( sub_list[0], "LOOKUP3" ) ) {
			// We have a Lookup3 method...
			method = new Lookup3( comp, comp->getType() );
		}
		else if( !strcmp( sub_list[0], "MAXDECREASE" ) ) {
			// We have a MaxDecrease method...
			method = new MaxDecrease( (Reservoir*)comp );
		}
		else if( !strcmp( sub_list[0], "MAXINCREASE" ) ) {
			// We have a MaxIncrease method...
			method = new MaxIncrease( (Reservoir*)comp );
		}
		else if( !strcmp( sub_list[0], "MAXSTAGE" ) ) {
			// We have a MaxStage method...
			method = new MaxStage( (Reservoir*)comp );
		}
		else if( !strcmp( sub_list[0], "RAINEVAP" ) ) {
			// We have a RainEvap method...
			method = new RainEvap( (Reservoir*)comp );
		}
		else if( !strcmp( sub_list[0], "PASSFLOW" ) ) {
			// We have a Passflow method...
			method = new Passflow( (Reservoir*)comp );
		}
		else if( !strcmp( sub_list[0], "SETELEVATION" ) ) {
			// We have a SetElevation method...
			method = new SetElevation( (Reservoir*)comp );
		}
		else if( !strcmp( sub_list[0], "SETMAX" ) ) {
			// We have a SetMax method...
			method = new SetMax( comp, comp->getType() );
		}
		else if( !strcmp( sub_list[0], "SETMIN" ) ) {
			// We have a SetMin method...
			method = new SetMin( comp, comp->getType() );
		}
		else if( !strcmp( sub_list[0], "SETRELEASE" ) ) {
			// We have a SetRelease method...
			method = new SetRelease( (Reservoir*)comp );
		}
		else if( !strcmp( sub_list[0], "SETSUM" ) ) {
			// We have a SetSum method...
			method = new SetSum( comp, comp->getType() );
		}
		else if( !strcmp( sub_list[0], "SETWITHDRAW" ) ) {
			// We have a SetWithdraw method...
			method = new SetWithdraw( (Reservoir*)comp );
		}
		else if( !strcmp( sub_list[0], "SPILLWAY" ) ) {
			// We have a Spillway method...
			method = new Spillway( (Reservoir*)comp );
		}
		else {
			PrintWarning( 1, routine,
				"The \"%s\" method is not supported yet.",
				 sub_list[0] );
			if ( sub_list) {
				sub_list = FreeStringList( sub_list );
			}
			comp = NULL;
			continue;
		}

		// Ensure method was instatiated
		if( !method ){
			totErrs++;
			PrintError( routine,
				"Unable to allocate memory for method \"%s\".",
				param_list[i] );
			if ( sub_list) {
				sub_list = FreeStringList( sub_list );
			}
			comp = NULL;
			continue;
		}

		// Check ID size
		if(strlen(sub_list[2]) > 12 ) {
			totErrs++;
			PrintError( routine,
				"Method ID \"%s\" is too long ( > 12 chars).",
				sub_list[2] );
			if ( sub_list) {
				sub_list = FreeStringList( sub_list );
			}
			if ( method != NULL ) {
				delete method;
				method = NULL;
			}
			comp = NULL;
			continue;
		}

		if( method->setID( sub_list[2] ) ){
			totErrs++;
			PrintError( routine,
				"Troubles setting Identifier \"%s\" for %s "
				"method.", sub_list[2], sub_list[0] );
			if ( sub_list) {
				sub_list = FreeStringList( sub_list );
			}
			if ( method != NULL ) {
				delete method;
				method = NULL;
			}
			comp = NULL;
			continue;
		}

		// add it to the component...
		if( comp->addMethod( NULL, method ) ){
			totErrs++;
			PrintError( routine, "Troubles adding the method "
				"\"%s\" to Component \"%s\".", method->getID(),
				 comp->getID() );
			if ( method != NULL ) {
				delete method;
				method = NULL;
			}
			comp = NULL;
			continue;
		}

		// Add to the intermediate list:
		intermed_list[meth_count] = method;
		meth_count++;

		if ( sub_list) {
			sub_list = FreeStringList( sub_list );
		}
		comp = NULL;
		method = NULL;
	}

	// Now go thru and process the expression stuff (list)
	for( i=0; i<nlist; i++ ) {
		PrintDebug( 20, routine, "Rule Line[%d]: %s", i, list[i] );

		// Handle the RULE Conditional Expression
		if( !strncmp( list[i], "[", 1 ) ) {
			// We just got a new expression...

			// Make a copy of the whole line.

			strcpy( expr_str, list[i] );

			UnpadString( expr_str, " ", PAD_FRONT_BACK );

			if( expr_str[0] != LBRACE || 
				expr_str[strlen(expr_str)-1] != RBRACE ){
				totErrs++;
				PrintError( routine,
					"Conditional expression \"%s\" is "
					"malformed.", expr_str );
				continue;
			}
			// OK, lets get rid of the square brackets and replace
			// them with spaces...
			expr_str[0] = ' ';
			expr_str[strlen(expr_str)-1] = ' ';

			ReplaceChar( expr_str, LBRACE, '(' );
			ReplaceChar( expr_str, RBRACE, ')' );

			UnpadString( expr_str, " ", PAD_FRONT_BACK );

			// Now we need to make sure that the expression isn't 
			// the GLOBAL_STR.
			if( !strcmp( expr_str, GLOBAL_STR ) ){
				strcpy( expr_str, GLOBAL_EXP );
			}
		}

		// Handle the RULE Rule
		else if( !strncmp( list[i], RULE_TOK, strlen( RULE_TOK ) ) ) {
			// We are on a rule...need to make sure that we have 
			// found an expression...

			if( strlen( expr_str ) == 0 ){
				totErrs++;
				PrintError( routine,
					"Rule line \"%s\" is not within any "
					"expressions.", list[i] );
				continue;
			}

			strcpy( rule, &list[i][strlen( RULE_TOK )] );

			sub_list = BreakStringList( rule, " ", 
				DELIM_SKIP_BLANKS, &nsub_list );
  

			// Check to make sure spillway method is used together with
			// at least one setrelease method

			if( !strcasecmp( sub_list[0], "SETRELEASE" ) ){
				if( !strcasecmp( sub_list[1], res_name ) || 
				!strcasecmp( res_name, "" ) ) ++nrel_spil;
				else nrel_spil = 0;
			res_name[0] = '\0';
			strcpy( res_name, sub_list[1] );
			}  
			if( !strcasecmp( sub_list[0], "SPILLWAY" ) ) {
				++nspil;
				if( !strcasecmp( sub_list[1], res_name ) ) ++nrel_spil;
				else nrel_spil = 0;
			res_name[0] = '\0';
			strcpy( res_name, sub_list[1] );
			}

// printf( "\n !!str previous string= %s, nspil= %d. nrel_spil= %d\n", res_name, nspil, nrel_spil);

			PrintDebug( 10, routine,
				"Using expression \"%s\" for rule \"%s\".",
				expr_str, list[i] );

			// Now we need to get the name of the component
			// that we are dealing with...

			PrintDebug( 10, routine,
				"Searching tree for component \"%s\".",
				sub_list[1] );

			comp = _root->getComponentPtr( sub_list[1] );

			if( comp == NULL ){
				totErrs++;
				PrintError( routine,
					"Unable to find \"%s\" in Component "
					"tree.", sub_list[1] );
				if ( sub_list) {
					sub_list = FreeStringList( sub_list );
				}
				continue;
			}

			// Loop through the intermediate list and find the
			// method and the position of the method in the 
			// intermediate list.
			for ( j = 0; j < meth_count; j++ ) {
				char *meth_tempid;
				meth_tempid = intermed_list[j]->getID();
				char *comp_tempid;
				comp_tempid = intermed_list[j]->getOwner()->_id;
				char *type_temp;
				type_temp = intermed_list[j]->getType();
				if( !strcmp( sub_list[2], meth_tempid ) &&
					!strcmp( sub_list[1], comp_tempid ) &&
					!strcmp( sub_list[0], type_temp ) ) {

				//if( !strcmp( sub_list[2], 
					//intermed_list[j]->getID() ) &&
					//!strcmp( sub_list[1],
					//intermed_list[j]->getOwner()->_id ) && 
					//!strcmp( sub_list[0],
					//intermed_list[j]->getType() ) ) 

					method = intermed_list[j];	
					break;
				}
			}

			if( method == NULL ){
				totErrs++;
				PrintError( routine, "RULE- %s: "
					"Unable to find \"%s\" method \"%s\" ",
					rule, sub_list[0], sub_list[2] );
				if ( sub_list) {
					sub_list = FreeStringList( sub_list );
				}
				comp = NULL;
				continue;
			}

			// Now we have to parse the current expression and 
			// save it onto the Component...

			// We have to parse the expression for each rule
			// so that each component has unique memory for
			// the expression.
			ExprParser parser;

			expr = parser.parseString( expr_str );

			if( !expr ){
				totErrs++;
				PrintError( routine,
					"Expression \"%s\" could not be parsed!",
					expr_str );
				if ( sub_list) {
					sub_list = FreeStringList( sub_list );
				}
				comp = NULL;
				method = NULL;
				continue;
			}

			if( expr->verify() ){
				totErrs++;
				PrintError( routine,
					"Unable to verify constant values for "
					"\"%s\".", expr_str );
				if ( expr != NULL ) {
					delete expr;
					expr = NULL;
				}
				if ( sub_list) {
					sub_list = FreeStringList( sub_list );
				}
				comp = NULL;
				method = NULL;
				continue;
			}

			if( comp->addExpression( expr, sub_list[0],
				sub_list[1], sub_list[2] ) ){
				totErrs++;
				PrintError( routine,
					"Troubles adding the expression \"%s\" "
					"at postion %d to Component %s.",
					expr_str, pos, comp->getID() );
				if ( expr != NULL ) {
					delete expr;
					expr = NULL;
				}
				expr = NULL;
				if ( sub_list) {
					sub_list = FreeStringList( sub_list );
				}
				comp = NULL;
				method = NULL;
				continue;
			}

			if ( sub_list) {
				sub_list = FreeStringList( sub_list );
			}
			comp	= NULL;
			method 	= NULL;
			expr	= NULL;
		}
	}
	if( nspil >= 1 && nrel_spil <= 1 ) {
		totErrs++;
		PrintError( routine,
			"SPILLWAY method can not be used alone, "
			"must be used with SETRELEASE method. "
			"You can overwrite with SETSUM, SETMAX "
			"or SETMIN method for reservoir %12s", res_name );
	}

	if ( totErrs > 0 ) {
		return( STATUS_FAILURE );
	}

	return( STATUS_SUCCESS );

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_system/RCS/ResJSys_parseRules.cxx,v $";
 static char rcs_id2[] = "$Id: ResJSys_parseRules.cxx,v 1.8 2006/10/26 15:31:19 hsu Exp $";}
/*  ===================================================  */

}
