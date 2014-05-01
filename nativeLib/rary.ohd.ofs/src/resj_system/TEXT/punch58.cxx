//------------------------------------------------------------------------------
// punch58.cc : C++ function called by the FORTRAN wrapper to ResJ.
//			It will ouput the control file with some comments to
// 			the ipu unit number.
//------------------------------------------------------------------------------
// Copyright:   See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 02 Jun 1998  Daniel Weiler, Riverside Technology, inc 	Initial version.
// 16 Oct 1998  DKW, RTi	Added some more carryover data.
// 20 Oct 1998	DKW, RTi	Took care of some units issues.
// 11 Apr 2001  James R. VanShaar, RTi	Fixed LagK matching with char_co and
//					introduced check for future redundancy
// 12 Apr 2001  JRV, Rti	Changed string and error writing from 
//				ResJ_fwrite to ResJ_ccwrite, for correct 
//				parsing of long strings to continuation lines
// 14 Dec 2001  JRV, RTi        Removed file_len from function parameter set
// 09 Jul 2002	JRV, RTi	Added special handling of BALANCE method to
//				deal with BALANCE keyword RESERVOIR independant
//				of a RESERVOIR component keyword.
// 01 Aug 2002  KSH, HRL        Added BLENDTBL keyword to indicate table data
//                              blending. BLEND is retained for old datasets.
// 08 Aug 2002	JRV, RTi	Corrected a problem with the BALANCE method
// 				being part of a combo-method.
// 13 Dec 2002	JRV, RTi	Added INITIALTRANSFER for SetWithdraw.
// 18 Dec 2002	JRV, RTi	Revised method carryover to work with entire
// 				method identification set (type, owner_id,
// 				method_id).
// 14 Jan 2002	JRV, RTi	Improved error handling.
// 				Added Spillway work.
// 24 Feb 2003	JRV, RTi	Fixed bug with Node flow unit conversion.
// 11 Mar 2004	JRV, RTi	Added CalcInflow work.
// 19 Apr 2004	JRV, RTi	Handled various punching bugs.
// 10 Feb 2006  JRV, RTi   Added Lookup3 work.  Because much of the carryover is
//                         hidden to the user, some of it won't be punched
//                         (BLENDTBL and BLENDTS values).
// 27 Mar 2006  JRV, RTi   Added additional Node carryover.
// 06 Apr 2006  Marc L. Baldo, RTi 	Added support for INITIALSTORAGE and 
//					INITIALLAGGEDINFLOW for LagK
//------------------------------------------------------------------------------
// Variables:   I/O     Description
//
//
//------------------------------------------------------------------------------

#include "ResJSys.h"
#include <stdio.h>
#include <stdlib.h>

void punch58( char* fname, float* CO, int* co_size, int* ipu, 
	int* ipr, int* ibug, int* ierr )
{
	char routine[]="punch58", string[MAXC], **list = NULL, name[256],
		*value = NULL, tmp_str[256], *char_co = NULL, error[256],
		ctl_file[MAXC], val_str[10],
		tmpval[256], tmp_str0[256],tmp_str1[256], tmp_str2[256];
	FILE* fp = (FILE*)NULL;
	int i, length = 0, nlist = 0, val_found = 0, reach_found = 0, index = 0,
		par_found = 0, eighty = 80;
	double lfactor = 1.0, vfactor = 1.0, ffactor = 1.0, conv_val;

	int ts_count = 0, n_ints = 0, n_outts = 0, t_step = 0, 
		Reservoir_found = 0, Node_found = 0, Combo_found = 0,
		LagK_found = 0, sizeInflowCO = 0, initialOutflow = 0,
		initialStorage = 0, initialLaggedInflow = 0,
		oldindex = 0, lag = 0, SetR_W_E_found = 0, conv_int = 0,
		Adjust_found = 0, Balance_found = 0, typeOK, compOK, NewCO,
		surematch, Spillway_found = 0, CalcInflow_found = 0,
		Lookup3_found = 0; 
	int whichCO[] = {0, 0, 0, 0, 0, 0, 0, 0};
	int lag_found;
	int j;
	int inflow_lag_found = 0;
	int numLagTableEntries = 0;
	// NOTE: MAXC = 512

	char *temp;		// Used to ensure unique match for LagK
	char LagKName[13];	// Used to ensure unique match for LagK

	if( *ibug == 0 ) {
		return;
	}

	if( fname == NULL ) {
		*ierr = 0;
		sprintf( error, "Cannot punch Carryover - ResJ fs5file not "
			"specified." );
		PrintError( routine, error );
		//length = strlen( error );
		//ResJ_ccwrite( &length, error, ipr );
		//ResJ_fwrite( &length, error, ipr );
		return;
	}

	// Otherwise, use the length of the file name
	// to determine the actual file name. We have to do this explicitly
	// to cleanly handle the Fortran to C++ char* interface, which is
	// not a pretty one.
        for( i = 0; i < strlen( fname ); i++ ) {
		if( fname[i] == ' ' ) {
			break;
		}
		length++;
	}
	strncpy( ctl_file, fname, length );
	if( ctl_file == NULL ) {
		*ierr = 0;
		sprintf( error, "Cannot execute ResJ - fs5file non-existent." );
		length = strlen( error );
		PrintError( routine, error );
		//ResJ_ccwrite( &length, error, ipr );
		//ResJ_fwrite( &length, error, ipr );
	}
	else {
		ctl_file[ length ] = '\0';
	}

	fp = fopen( ctl_file, "r" );
	if( fp == NULL ) {
		*ierr = 1;
		sprintf( error, "Troubles opening ResJ fs5file %s", ctl_file );
		PrintError( routine, error );
		//length = strlen( error );
		//ResJ_ccwrite( &length, error, ipr );
		//ResJ_fwrite( &length, error, ipr );
		return;
	}
	
	// EJM - removed 10/22/98
	// sprintf(string,"Punching Carryover to unit number %d.", *ipu );
	//length = strlen( string );
	//ResJ_fwrite( &length, string, ipr );  

	// Convert the incoming float* to a char*
	char_co = (char*)malloc( (*co_size)*(sizeof(float) ) );
	if( char_co == NULL ) {
		*ierr = 1;
		sprintf( error, "Troubles allocating memory for CO array." );
		PrintError( routine, error );
		//length = strlen( error );
		//ResJ_ccwrite( &length, error, ipr );
		//ResJ_fwrite( &length, error, ipr );
		return;
	}
	memcpy( char_co, CO, (*co_size)*4 );

//JRV: Why only 256 when string = 512 characters?
	while( fgets( string, 256, fp ) != NULL ) {
		UnpadString( string, " \t\n", PAD_BACK );
		length = strlen( string );
		list = BreakStringList( string, " \t\n", DELIM_SKIP_BLANKS,
			&nlist );
		if( list == NULL || nlist == 0 ) {
			list = FreeStringList( list );
			continue;
		}

		// ---------------------------------------- //
		// Handle non-parameteric information
		if ( !par_found ) {
			// Do a count of the timeseries. By doing this here when
			// we read the control file, this prevents us from 
			// having to run a syntax check to get it.
			if( ts_count == 1 ) {
				if( !strcasecmp( list[0], "INPUT" ) ) {
					n_ints++;
				}
				else if( !strcasecmp( list[0], "OUTPUT" ) ) {
					n_outts++;
				}
				else if( !strcasecmp( list[0], 
					"ENDTIMESERIES" ) ) {
					ts_count = 0;
				}
				else if( !strcasecmp( list[0], "TIMESTEP" ) ) {
					t_step = atoi(list[1]);
				}
			}
			else if( !strcasecmp( list[0], "TIMESERIES" ) ) {
				ts_count = 1;
			}
			else if( !strcasecmp( list[0], "PARAMETERS" ) ) {
				par_found = 1;
			}
			length = strlen( string );
			ResJ_ccwrite( &length, string, ipu );
			list = FreeStringList( list );
			continue;
		}
		// End of handling non-parameteric information
		// ---------------------------------------- //

		// Handle BALANCE method information
		// While no carry over exists for the BALANCE method use of the
		// keyword "RESERVOIR" must be handled within the BALANCE 
		// method or else the punch routine mistakes it for a component
		// and adds reservoir carry over data.
		// ---------------------------------------- //
		if( Balance_found ) {
			if( !strcasecmp(list[0], "ENDBALANCE") ) {
				Balance_found = 0;
			}
			if ( !strcasecmp( list[0], "RESERVOIR" ) ) {
				// Swap out the RESERVOIR keyword for the new 
				// keyword BALRES
				sprintf( tmp_str, "%s", "BALRES" );
				for ( i = 1; i < nlist; i++ ) {
					sprintf( tmp_str, "%s %s",tmp_str, 
						list[i] );
				}
				list = FreeStringList( list );
				length = strlen( tmp_str );
				ResJ_ccwrite( &length, tmp_str, ipu );
				continue;
			}
			if ( !strcasecmp( list[0], "ENDRESERVOIR" ) ) {
				// Swap out the ENDRESERVOIR keyword for the new 
				// keyword ENDBALRES
				sprintf( tmp_str, "%s", "ENDBALRES" );
				for ( i = 1; i < nlist; i++ ) {
					sprintf( tmp_str, "%s %s",tmp_str, 
						list[i] );
				}
				list = FreeStringList( list );
				length = strlen( tmp_str );
				ResJ_ccwrite( &length, tmp_str, ipu );
				continue;
			}
			// Otherwise, we simply print the line
			list = FreeStringList( list );
			length = strlen( string );
			ResJ_ccwrite( &length, string, ipu );
			continue;
		}
		else if( !strcasecmp(list[0], "BALANCE") ) {
			// The keyword BALANCE can be found as part of a combo-
			// method.  The ordering of the BALANCE check before
			// the check for a reservoir requires that we now check
			// to ensure that we are not in a combo-method before
			// we address this BALANCE stuff.
			if( !Combo_found ) {
				Balance_found = 1;
				length = strlen( string );
				ResJ_ccwrite( &length, string, ipu );
				list = FreeStringList( list );
				continue;
			}
		}

		// End of handling BALANCE method information
		// ---------------------------------------- //

		// Handle RESERVOIR information
		// Carry over data consists of: INITIALRELEASE, INITIALPOOL,
		//	INITIALWITHDRAW, INITIALINFLOW, PREVIOUSRELEASE,
		//	PREVIOUSPOOL, PREVIOUSWITHDRAW, and PREVIOUSINFLOW
		// ---------------------------------------- //
		if ( Reservoir_found ) {
			if( !strcasecmp( list[0], "ENDRESERVOIR" ) ) {
				// Check & Handle missing CO values
				for (i = 0; i < 8; i++) {
					if( !whichCO[i] ) {
					   switch (i) {
					      case 0:
						strncpy(tmpval, &value[28], 8);
						tmpval[8] = '\0';
						conv_val = atof(tmpval) / 
							ffactor;
						sprintf(tmp_str, "%s %f", 
						   "INITIALRELEASE",conv_val);
						length = strlen( tmp_str );
						ResJ_ccwrite( &length, tmp_str, 
							ipu );
						break;
					      case 1:
						strncpy(tmpval, &value[36], 8);
						tmpval[8] = '\0';
						conv_val = atof(tmpval) / 
							lfactor;
						sprintf(tmp_str, "%s %f", 
						   "INITIALPOOL",conv_val);
						length = strlen( tmp_str );
						ResJ_ccwrite( &length, tmp_str, 
							ipu );
						break;
					      case 2:
						strncpy(tmpval, &value[44], 8);
						tmpval[8] = '\0';
						conv_val = atof(tmpval) / 
							ffactor;
						sprintf(tmp_str, "%s %f", 
						   "INITIALWITHDRAW",conv_val);
						length = strlen( tmp_str );
						ResJ_ccwrite( &length, tmp_str, 
							ipu );
						break;
					      case 3:
						strncpy(tmpval, &value[52], 8);
						tmpval[8] = '\0';
						conv_val = atof(tmpval) / 
							ffactor;
						sprintf(tmp_str, "%s %f", 
						   "INITIALINFLOW",conv_val);
						length = strlen( tmp_str );
						ResJ_ccwrite( &length, tmp_str, 
							ipu );
						break;
					      case 4:
						strncpy(tmpval, &value[60], 8);
						tmpval[8] = '\0';
						conv_val = atof(tmpval) / 
							ffactor;
						sprintf(tmp_str, "%s %f", 
						   "PREVIOUSRELEASE",conv_val);
						length = strlen( tmp_str );
						ResJ_ccwrite( &length, tmp_str, 
							ipu );
						break;
					      case 5:
						strncpy(tmpval, &value[68], 8);
						tmpval[8] = '\0';
						conv_val = atof(tmpval) / 
							lfactor;
						sprintf(tmp_str, "%s %f", 
						   "PREVIOUSPOOL",conv_val);
						length = strlen( tmp_str );
						ResJ_ccwrite( &length, tmp_str, 
							ipu );
						break;
					      case 6:
						strncpy(tmpval, &value[76], 8);
						tmpval[8] = '\0';
						conv_val = atof(tmpval) / 
							ffactor;
						sprintf(tmp_str, "%s %f", 
						   "PREVIOUSWITHDRAW",conv_val);
						length = strlen( tmp_str );
						ResJ_ccwrite( &length, tmp_str, 
							ipu );
						break;
					      case 7:
						strncpy(tmpval, &value[84], 8);
						tmpval[8] = '\0';
						conv_val = atof(tmpval) / 
							ffactor;
						sprintf(tmp_str, "%s %f", 
						   "PREVIOUSINFLOW",conv_val);
						length = strlen( tmp_str );
						ResJ_ccwrite( &length, tmp_str, 
							ipu );
						break;
					   }
					}
					// reinitialize whichCO
					whichCO[i] = 0;
				}
				Reservoir_found = 0;
				length = strlen( string );
				ResJ_ccwrite( &length, string, ipu );
				list = FreeStringList( list );
				continue;
			}
			// Look for keyword and get CO value
			else if( !strcasecmp( list[0], "INITIALRELEASE" ) ) {
				strncpy( tmpval, &value[28], 8 );
				whichCO[0] = 1;
			}
			else if( !strcasecmp( list[0], "INITIALPOOL" ) ) { 
				strncpy( tmpval, &value[36], 8 );
				whichCO[1] = 1;
			}
			else if( !strcasecmp( list[0], "INITIALWITHDRAW" ) ) {
				strncpy( tmpval, &value[44], 8 );
				whichCO[2] = 1;
			}
			else if( !strcasecmp( list[0], "INITIALINFLOW" ) ) {
				strncpy( tmpval, &value[52], 8 );
				whichCO[3] = 1;
			}
			else if( !strcasecmp( list[0], "PREVIOUSRELEASE" ) ) {
				strncpy( tmpval, &value[60], 8 );
				whichCO[4] = 1;
			}
			else if( !strcasecmp( list[0], "PREVIOUSPOOL" ) ) { 
				strncpy( tmpval, &value[68], 8 );
				whichCO[5] = 1;
			}
			else if( !strcasecmp( list[0], "PREVIOUSWITHDRAW" ) ) {
				strncpy( tmpval, &value[76], 8 );
				whichCO[6] = 1;
			}
			else if( !strcasecmp( list[0], "PREVIOUSINFLOW" ) ) {
				strncpy( tmpval, &value[84], 8 );
				whichCO[7] = 1;
			}
			else {
				// Print whatever else is found
				length = strlen( string );
				ResJ_ccwrite( &length, string, ipu );
				list = FreeStringList( list );
				continue;
			}
			// Handle carry over and print line
			tmpval[8] = '\0';
			if ( !strcasecmp( list[0], "INITIALPOOL" ) ||
				!strcasecmp( list[0], "PREVIOUSPOOL" ) ) {
				// convert using lengths
				conv_val = atof( tmpval ) / lfactor;	
			}
			else {
				conv_val = atof( tmpval ) / ffactor;	
			}
			sprintf( tmp_str, "%s  %f", list[0], conv_val );
			length = strlen( tmp_str );
			ResJ_ccwrite( &length, tmp_str, ipu );
			list = FreeStringList( list );
			continue;
		}
		else if( !strcasecmp( list[0], "RESERVOIR" ) ) {
			Reservoir_found = 1;
			sprintf( name, "%-12.12s%-12.12s", "RESERVOIR", 
				list[1] );
			ToUpper( name );
			value = strstr( char_co, name ); 
			if( value == NULL ) {
				*ierr = 1;
				//fclose(tfp);
				fclose(fp);
				sprintf( error, "Unable to find a "
					"match for %s.", name );
				PrintError( routine, error );
				list = FreeStringList( list );
				return;
			}
			list = FreeStringList( list );
			length = strlen( string );
			ResJ_ccwrite( &length, string, ipu );
			continue;
		}
		// End handling RESERVOIR information
		// ---------------------------------------- //

		// ---------------------------------------- //
		// Deal with NODE carry over
		// The CO data are DISCHARGE and PREVIOUSDISCHARGE
		if( Node_found ) {
			char *tempXYZ = list[0];
			if( strstr( list[0], "DISCHARGE" ) )
			{
			    if( !strcasecmp( list[0], "DISCHARGE" ) )
			    {
			        // copy the value following two identifiers, and
			        // the index
			        strncpy(tmpval, &value[12+12+4], 8); 
			        tmpval[8] = '\0';
			        conv_val = atof( tmpval ) / ffactor;
			        sprintf(tmp_str, "   %s %f", list[0],conv_val);
			        length = strlen( tmp_str );
			        ResJ_ccwrite( &length, tmp_str, ipu );
			        whichCO[0] = 1;
			    }
			    else if(!strcasecmp( list[0], "PREVIOUSDISCHARGE" ))
			    {
			        // copy the value following two identifiers, and
			        // the index and DISCHARGE
			        strncpy(tmpval, &value[12+12+4+8], 8); 
			        tmpval[8] = '\0';
			        conv_val = atof( tmpval ) / ffactor;
			        sprintf(tmp_str, "   %s %f", list[0],conv_val);
			        length = strlen( tmp_str );
			        ResJ_ccwrite( &length, tmp_str, ipu );
			        whichCO[1] = 1;
			    }
			    continue;
			}
			// Check for new inflow states.  At this point, we are
			// looking at the input file.  There will not be INFLOW
			// keywords if there are no INFLOW values in the
			// carryover array.
			else if( strstr( list[0], "INFLOW" ) && NewCO )
			{
			    if( !strcasecmp( list[0], "INITIALINFLOW" ) ||
                                !strcasecmp( list[0], "INFLOW" ) )
			    {
			        // copy the value following two identifiers, and
			        // the index, DISCHARGE, PREVIOUSDISCHARGE
			        strncpy(tmpval, &value[12+12+4+8+8], 8); 
			        tmpval[8] = '\0';
			        conv_val = atof( tmpval ) / ffactor;
			        sprintf(tmp_str, "   %s %f", list[0],conv_val);
			        length = strlen( tmp_str );
			        ResJ_ccwrite( &length, tmp_str, ipu );
			        whichCO[2] = 2;
			    }
			    else if( !strcasecmp( list[0], "PREVIOUSINFLOW" ) )
			    {
			        // copy the value following two identifiers, and
			        // the index, DISCHARGE, PREVIOUSDISCHARGE, AND
			        // INITIALINFLOW
			        strncpy(tmpval, &value[12+12+4+8+8+8], 8); 
			        tmpval[8] = '\0';
			        conv_val = atof( tmpval ) / ffactor;
			        sprintf(tmp_str, "   %s %f", list[0],conv_val);
			        length = strlen( tmp_str );
			        ResJ_ccwrite( &length, tmp_str, ipu );
			        whichCO[3] = 3;
			    }
			    continue;
			}
			// Check for new diversion states.  At this point, we
			// are looking at the input file.  There will not be
			// DIVERSION keywords if there are no DIVERSION values
			// in the carryover array.
			else if( strstr( list[0], "DIVERSION" ) && NewCO )
			{
			    if( !strcasecmp( list[0], "INITIALDIVERSION" ) ||
                                !strcasecmp( list[0], "DIVERSION" ) )
			    {
			        // copy the value following two identifiers, and
			        // the index, DISCHARGE, PREVIOUSDISCHARGE,
			        // INITIALINFLOW, PREVIOUSINFLOW
			        strncpy(tmpval, &value[12+12+4+8+8+8+8], 8); 
			        tmpval[8] = '\0';
			        // NOTE: It is possible that the carryover value
			        // begins with "-999".  When the array was
			        // written, the "-999" was replaced with "MISS"
			        // to prevent problems.  Make special provision
			        // to deal with that case.
			        if( !strncmp( tmpval, "MISS", 4 ) )
			        {
			            strncpy( tmpval, "-999", 4 );
			        }
			        conv_val = atof( tmpval ) / ffactor;
			        sprintf(tmp_str, "   %s %f", list[0],conv_val);
			        length = strlen( tmp_str );
			        ResJ_ccwrite( &length, tmp_str, ipu );
			        whichCO[4] = 4;
			    }
			    else if( !strcasecmp( list[0], "PREVIOUSDIVERSION" ) )
			    {
			        // copy the value following two identifiers, and
			        // the index, DISCHARGE, PREVIOUSDISCHARGE,
			        // INITIALINFLOW, PREVIOUSINFLOW,
			        // INITIALDIVERSION
			        strncpy(tmpval, &value[12+12+4+8+8+8+8+8], 8); 
			        tmpval[8] = '\0';
			        // NOTE: It is possible that the carryover value
			        // begins with "-999".  When the array was
			        // written, the "-999" was replaced with "MISS"
			        // to prevent problems.  Make special provision
			        // to deal with that case.
			        if( !strncmp( tmpval, "MISS", 4 ) )
			        {
			            strncpy( tmpval, "-999", 4 );
			        }
			        conv_val = atof( tmpval ) / ffactor;
			        sprintf(tmp_str, "   %s %f", list[0],conv_val);
			        length = strlen( tmp_str );
			        ResJ_ccwrite( &length, tmp_str, ipu );
			        whichCO[5] = 5;
			    }
			    continue;
			}
			else if( !strcasecmp( list[0], "ENDNODE" ) )
			{
			    // Check for required CO values
			    for (i = 0; i < 6; i++) {
			        if( !whichCO[i] )
			        {
			           switch (i)
			           {
			              case 0:
			                strncpy(tmpval, &value[28], 8);
			                tmpval[8] = '\0';
			                conv_val = atof( tmpval ) / ffactor;	
			                sprintf(tmp_str, "   %s %f", 
			                   "DISCHARGE",conv_val);
			                length = strlen( tmp_str );
			                ResJ_ccwrite( &length, tmp_str, ipu );
			                break;
			              case 1:
			                strncpy(tmpval, &value[36], 8);
			                tmpval[8] = '\0';
			                conv_val = atof( tmpval ) / ffactor;	
			                sprintf(tmp_str, "   %s %f", 
			                   "PREVIOUSDISCHARGE",conv_val);
			                length = strlen( tmp_str );
			                ResJ_ccwrite( &length, tmp_str, ipu );
			                break;
			              case 2:
			                if( !NewCO )
			                {
			                    break;
			                }
			                strncpy(tmpval, &value[44], 8);
			                tmpval[8] = '\0';
			                conv_val = atof( tmpval ) / ffactor;	
			                sprintf(tmp_str, "   %s %f", 
			                   "INITIALINFLOW",conv_val);
			                length = strlen( tmp_str );
			                ResJ_ccwrite( &length, tmp_str, ipu );
			                break;
			              case 3:
			                if( !NewCO )
			                {
			                    break;
			                }
			                strncpy(tmpval, &value[52], 8);
			                tmpval[8] = '\0';
			                conv_val = atof( tmpval ) / ffactor;	
			                sprintf(tmp_str, "   %s %f", 
			                   "PREVIOUSINFLOW",conv_val);
			                length = strlen( tmp_str );
			                ResJ_ccwrite( &length, tmp_str, ipu );
			                break;
			              case 4:
			                if( !NewCO )
			                {
			                    break;
			                }
			                strncpy(tmpval, &value[60], 8);
			                tmpval[8] = '\0';
			                // NOTE: It is possible that the
			                // carryover value begins with "-999".
			                // When the array was written, the "-999"
			                // was replaced with "MISS" to prevent
			                // problems.  Make special provision to
			                // deal with that case.
			                if( !strncmp( tmpval, "MISS", 4 ) )
			                {
			                    strncpy( tmpval, "-999", 4 );
			                }
			                conv_val = atof( tmpval ) / ffactor;	
			                sprintf(tmp_str, "   %s %f", 
			                   "INITIALDIVERSION",conv_val);
			                length = strlen( tmp_str );
			                ResJ_ccwrite( &length, tmp_str, ipu );
			                break;
			              case 5:
			                if( !NewCO )
			                {
			                    break;
			                }
			                strncpy(tmpval, &value[68], 8);
			                tmpval[8] = '\0';
			                // NOTE: It is possible that the
			                // carryover value begins with "-999".
			                // When the array was written, the "-999"
			                // was replaced with "MISS" to prevent
			                // problems.  Make special provision to
			                // deal with that case.
			                if( !strncmp( tmpval, "MISS", 4 ) )
			                {
			                    strncpy( tmpval, "-999", 4 );
			                }
			                conv_val = atof( tmpval ) / ffactor;	
			                sprintf(tmp_str, "   %s %f", 
			                   "PREVIOUSDIVERSION",conv_val);
			                length = strlen( tmp_str );
			                ResJ_ccwrite( &length, tmp_str, ipu );
			                break;
			           }
			        }
			        // reinitialize whichCO
			        whichCO[i] = 0;
			    }
			    length = strlen( string );
			    ResJ_ccwrite( &length, string, ipu );
			    // We are done with this node
			    Node_found = 0;
			    continue;
			}
			else
			{
			    // There is nothing carryover-specific
			    list = FreeStringList( list );
			    length = strlen( string );
			    ResJ_ccwrite( &length, string, ipu );
			    continue;
			}
		}
		else if( !strcasecmp( list[0], "NODE" ) ) {
			Node_found = 1;
			// Find the method information in the carry over array
			sprintf( name, "%-12.12s%-12.12s", list[0], list[1] );
			ToUpper( name );
			value = strstr( char_co, name );
			if( value == NULL ) {
				*ierr = 1;
				//fclose(tfp);
				fclose(fp);
				sprintf( error, "Troubles with NODE %s.",
					name );
				PrintError( routine, error );
				list = FreeStringList( list );
				return;
			}
			// Check for the new INFLOW and DISCHARGE carryover
			if( strncasecmp( &value[12+12+4+8+8], "*FUTURE*", 8 ) )
			{
			    // We didn't match.  We have a new carryover.
			    NewCO = 1;
			}
			else
			{
			    // We matched, so we have old carryover.
			    NewCO = 0;
			}
                        list = FreeStringList( list );
			length = strlen( string );
			ResJ_ccwrite( &length, string, ipu );
			continue;
		}
		// End Dealing with NODE carry over
		// ---------------------------------------- //


		// ---------------------------------------- //
		// Deal with Combo Methods: They contain identification of their
		//	sub-methods, of course without the sub-method 
		//	parameters.  Therefore, we need to ignore the sub-method
		//	keywords.
		if( Combo_found ) {
			if( (!strcasecmp(list[0], "ENDSETSUM")) || 
			(!strcasecmp( list[0], "ENDSETMAX")) || 
			(!strcasecmp(list[0], "ENDSETMIN")) ) {
				Combo_found = 0;
			}
			// No matter what, we still print the line
			list = FreeStringList( list );
			length = strlen( string );
			ResJ_ccwrite( &length, string, ipu );
			continue;
		}
		else if( (!strcasecmp(list[0], "SETSUM")) || 
			(!strcasecmp( list[0], "SETMAX")) || 
			(!strcasecmp(list[0], "SETMIN")) ) {
			Combo_found = 1;
			list = FreeStringList( list );
			length = strlen( string );
			ResJ_ccwrite( &length, string, ipu );
			continue;
		}
		// End of Combo Method handling
		// ---------------------------------------- //


		// ---------------------------------------- //
		// Handle LagK method carry over
		// Carry over data consists of inflows and INITIALOUTLFOW
		if( LagK_found ) {
			if( val_found == 1 ) {
				if( !strcasecmp( list[0], "ENDVALUES" ) ) {
					sizeInflowCO = index;
					if( sizeInflowCO < numLagTableEntries ) {
						sizeInflowCO = numLagTableEntries;
					}
					index = 0;
					val_found = 0;	
					length = strlen( string );
					ResJ_ccwrite( &length, string, ipu );
				}
				else {
					// This works with the LagK COINFLOWS
					if( !strncmp(list[0], "#", 1) ) {
						// This is a comment line
						length = strlen( string );
						ResJ_ccwrite( &length, string, 
							ipu );
						list = FreeStringList( list );
						continue;
					}
					strncpy(tmpval, 
						&value[40+NewCO*12+index*8], 8);
					tmpval[8] = '\0';
					conv_val = atof( tmpval ) / ffactor;
					sprintf(tmp_str, "%f", conv_val);
					length = strlen( tmp_str );
					ResJ_ccwrite( &length, tmp_str, ipu );
					index++;
				}
				list = FreeStringList( list );
				continue;
			}
			else if( !strcasecmp( list[0], "INITIALOUTFLOW" ) ) {
				// This works with LagK INITIALOUTFLOW
				initialOutflow = 1;
				strncpy( tmpval, 
					&value[40+NewCO*12+sizeInflowCO*8], 8);
				tmpval[8] = '\0';
				conv_val = atof( tmpval ) / ffactor;
				sprintf(tmp_str, "%s  %f", list[0], conv_val);
				length = strlen( tmp_str );
				ResJ_ccwrite( &length, tmp_str, ipu );
				list = FreeStringList( list );
				continue;
			}
			else if( !strcasecmp( list[0], "INITIALSTORAGE" ) ) {
				// This works with LagK INITIALSTORAGE
				initialStorage = 1;
				strncpy( tmpval, 
					&value[48+NewCO*12+sizeInflowCO*8], 8);
				tmpval[8] = '\0';
				conv_val = atof( tmpval ) / ffactor;
				sprintf(tmp_str, "%s  %f", list[0], conv_val);
				length = strlen( tmp_str );
				ResJ_ccwrite( &length, tmp_str, ipu );
				list = FreeStringList( list );
				continue;
			}
			else if( !strcasecmp( list[0], "INITIALLAGGEDINFLOW" ) ) {
				// This works with LagK INITIALLAGGEDINFLOW
				initialLaggedInflow = 1;
				strncpy( tmpval, 
					&value[56+NewCO*12+sizeInflowCO*8], 8);
				tmpval[8] = '\0';
				conv_val = atof( tmpval ) / ffactor;
				sprintf(tmp_str, "%s  %f", list[0], conv_val);
				length = strlen( tmp_str );
				ResJ_ccwrite( &length, tmp_str, ipu );
				list = FreeStringList( list );
				continue;
			}
			else if( !strcasecmp( list[0], "VALUES" ) ) {
				val_found = 1;	
				index = 0;
			}
			else if( !strcasecmp( list[0], "LAG" ) ) {
				if( list[1] && strlen( list[1] ) > 0 ) {
					lag = atoi( list[1] );
				}
				lag_found = 1;
			}
			else if( !strcasecmp( list[0], "TABLE" ) && list[1] && 
			         !strcasecmp( list[1], "inflow_lag" ) ) {
				inflow_lag_found = 1;
			}
                        else if( inflow_lag_found && ( !strcasecmp( list[0], "ENDTABLE" ) ) ) {
                                inflow_lag_found = 0;
                        }
                        else if( inflow_lag_found && ( list[0][0] != '#' ) ) {
                                numLagTableEntries++;
                        }
			else if( !strcasecmp( list[0], "ENDLAG" ) ) {
				lag_found = 0;
				inflow_lag_found = 0;
			}
			else if( !strcasecmp( list[0], "ENDLAGK" ) ) {
				if( sizeInflowCO == 0) {
					// We did not find any values.  We need 
					// to print the block with COINFLOW, 
					// VALUES, the data (from carry over), 
					// ENDVALUES and COINFLOW before ending 
					// the LAGK parameterization
					sprintf(tmp_str, "%s", "COINFLOW");
					length = strlen( tmp_str );
					ResJ_ccwrite( &length, tmp_str, ipu );
					sprintf(tmp_str, "%s", "VALUES");
					length = strlen( tmp_str );
					ResJ_ccwrite( &length, tmp_str, ipu );
					// Calculate and print actual values
					if( lag == 0 ) {
						sizeInflowCO = 1;
					}
					if( lag < t_step ) {
						sizeInflowCO = 2;
					}
					else if( lag % t_step != 0 ) {
						sizeInflowCO = lag / t_step + 2;
					}
					else {
						sizeInflowCO = lag / t_step + 1;
					}
					if( sizeInflowCO < numLagTableEntries ) {
						sizeInflowCO = numLagTableEntries;
					}
					for( i = 0; i < sizeInflowCO; i++ ) {
						sprintf(tmpval, "%8.8s", 
						       &value[40+NewCO*12+i*8]);
						tmpval[8] = '\0';
						conv_val = atof( tmpval ) / 
							ffactor;
						sprintf(tmp_str, "%f",conv_val);
						tmp_str[8] = '\0';
						length = strlen( tmp_str );
						ResJ_ccwrite( &length, tmp_str, 
							ipu );
					}
					sprintf(tmp_str, "%s", "ENDVALUES");
					length = strlen( tmp_str );
					ResJ_ccwrite( &length, tmp_str, ipu );
					sprintf(tmp_str, "%s", "ENDCOINFLOW");
					length = strlen( tmp_str );
					ResJ_ccwrite( &length, tmp_str, ipu );
				}
				if( !initialOutflow ) {
					// We did not find an initial outflow
					// value.
					sprintf(tmpval, "%8.8s",
						&value[40+NewCO*12+
						sizeInflowCO*8]);
					tmpval[8] = '\0';
					conv_val = atof( tmpval ) / ffactor;
					sprintf(tmp_str, "%s  %f", 
						"INITIALOUTFLOW", conv_val);
					length = strlen( tmp_str );
					ResJ_ccwrite( &length, tmp_str, ipu );
				}
				if( !initialStorage ) {
					// We did not find an initial storage
					// value.
					sprintf(tmpval, "%8.8s",
						&value[48+NewCO*12+
						sizeInflowCO*8]);
					tmpval[8] = '\0';
					conv_val = atof( tmpval ) / ffactor;
					sprintf(tmp_str, "%s  %f", 
						"INITIALSTORAGE", conv_val);
					length = strlen( tmp_str );
					ResJ_ccwrite( &length, tmp_str, ipu );
				}
				if( !initialLaggedInflow ) {
					// We did not find an initial lagged 
					// inflow value.
					sprintf(tmpval, "%8.8s",
						&value[56+NewCO*12+
						sizeInflowCO*8]);
					tmpval[8] = '\0';
					conv_val = atof( tmpval ) / ffactor;
					sprintf(tmp_str, "%s  %f", 
						"INITIALLAGGEDINFLOW", 
						conv_val);
					length = strlen( tmp_str );
					ResJ_ccwrite( &length, tmp_str, ipu );
				}
				lag = 0;
				initialOutflow = 0;
				initialStorage = 0;
				initialLaggedInflow = 0;
				sizeInflowCO = 0;
				LagK_found = 0;
			}
			length = strlen( string );
			ResJ_ccwrite( &length, string, ipu );
			list = FreeStringList( list );
			continue;
		}
		else if( !strcasecmp( list[0], "LAGK" ) ) {
			sprintf( name, "%-12.12s%-12.12s", "REACH", list[1] );
			ToUpper( name );
			// We need to test the LagK method name before we are 
			// sure that we have found the correct information in 
			// the char_co
			// iterate until value becomes null or we find a sure 
			// match
			// NOTE: "name" contains "REACH" and the REACH_ID
			//   tmp_str0 is used to check method type for "LAGK"
			//   tmp_str and tmp_str are comparing the LAGK_ID
			surematch = 0;
			value = strstr( char_co, name );

			// Reformat list[2] as tmp_str2 for full comparison
			sprintf( tmp_str2, "%-12.12s", list[2] );

			// Check for modified carryover structure
			// We used to have 3*4 + 3*4 + 4 +3*4 + N*8 characters 
			// before "*FUTURE*".  This adds up to be evenly 
			// divisible by 8.  The new carryover structure adds
			// 12 characters causing it not to be divisible by 8.
			length = strcspn( value, "*" );
			NewCO = 1;
			if( length % 8 == 0) {
				// We have not added modified carryover yet
				NewCO = 0;
				typeOK = 1;
			}

			while (value != NULL && !surematch) {
				if( NewCO ) {
					sprintf( tmp_str0, "%-12.12s", 
						&value[24+4+12]);
					sprintf( tmp_str, "%-12.12s", 
						list[0]);
					typeOK = !strncasecmp(tmp_str0, 
						tmp_str, 12);
				}
				// Get and test Lagk name from carry over
				sprintf( tmp_str, "%-12.12s", &value[28] );
				// Compare carry over LagK name with parameter
				// file LagK name and test previously determined
				// typeOK value.  
				if( typeOK && strstr(tmp_str, tmp_str2)!=NULL) {
					surematch = 1;
				}
				else {
					// increment the string to the next 
					// character to try to find the next 
					// match
					value++;
					value = strstr( value, name );
				}
			}
			if( value == NULL ) {
				*ierr = 1;
				//fclose(tfp);
				fclose(fp);
				sprintf( error, "Unable to find a "
					"match for %s %s on %s.", 
					list[0], list[1], list[2] );
				PrintError( routine, error );
				list = FreeStringList( list );
				return;
			}
			LagK_found = 1;
			length = strlen( string );
			ResJ_ccwrite( &length, string, ipu );
			continue;
		}
		// End handling LagK method carry over
		// ---------------------------------------- //


		// ---------------------------------------- //
		// Deal with Lookup3 carryover over beginning here
		// The CO data are on the BLENDTS and BLENDTBL rows.
		// Note: The VALUES keyword is not of interest to us here
		if( Lookup3_found )
		{
		    if( !strcasecmp( list[0], "BLENDTS" ) )
		    {
		        // Copy the value following two identifiers, the index,
		        // and two more identifiers
		        // We can handle it as a string.
		      //strncpy(tmp_str0, &value[12+12+4+12+12], 4);
		      //tmp_str0[4] = '\0';
		        // And the value after the BLENDTBL step
		      //strncpy(tmp_str1, &value[12+12+4+12+12+4+4], 4);
		      //tmp_str1[4] = '\0';
		        // And the value after the Column Index value
		      //strncpy(tmp_str2, &value[12+12+4+12+12+4+4+4], 4);
		      //tmp_str2[4] = '\0';
		        // Copy the value following two identifiers, the index,
		        // two more identifiers, BLENDTS step, BLENDTBL step,
		        // the column Index value, and the row Index value.
		        // (the last value).
		      //strncpy(tmpval, &value[12+12+4+12+12+4+4+4+4], 8); 
		      //tmpval[8] = '\0';
		      //conv_val = atof( tmpval ) / ffactor;
		      //fprintf(tfp, "%s  %s  %s %s %s %f\n", list[0], list[1],
                      //    tmp_str0, tmp_str1, tmp_str2, conv_val);

		        // HOWEVER, they are internal to RES-J and not included
		        // in user parameterization.  Therefore, we will not
		        // punch them.
			length = strlen( string );
			ResJ_ccwrite( &length, string, ipu );
		    }
		    else if( !strcasecmp( list[0], "BLENDTBL" ) )
		    {
		        // We have a BLENDTBL step, column Index value and a row
		        // Index value.  None of these need units conversion.
		        // They may handled as strings.
		        // Copy the values following two identifiers, the index,
		        // two more identifiers, and BLENDTS step, BLENDTBL step

		      //strncpy(tmp_str0, &value[12+12+4+12+12+4], 4);
		      //tmp_str0[4] = '\0';
		        // And the value after the BLENDTBL step
		      //strncpy(tmp_str1, &value[12+12+4+12+12+4+4], 4);
		      //tmp_str1[4] = '\0';
		        // And the value after the Column Index value
		      //strncpy(tmp_str2, &value[12+12+4+12+12+4+4+4], 4);
		      //tmp_str2[4] = '\0';
		        // And the value after the Row Index value (the last
		        // value).
		      //strncpy(tmpval, &value[12+12+4+12+12+4+4+4+4], 8); 
		      //tmpval[8] = '\0';
		      //conv_val = atof( tmpval ) / ffactor;
		      //fprintf(tfp, "%s  %s  %s %s %s %f\n", list[0], list[1],
                      //    tmp_str0, tmp_str1, tmp_str2, conv_val);

		        // HOWEVER, they are internal to RES-J and not included
		        // in user parameterization.  Therefore, we will not
		        // punch them.
		        length = strlen( string );
		        ResJ_ccwrite( &length, string, ipu );
		    }
		    else if( !strcasecmp( list[0], "INITIALTRANSFER" ) )
		    {
		        // Copy the value following two identifiers, the index,
		        // two more identifiers, BLENDTS step, BLENDTBL step,
		        // the column Index value, and the row Index value.
		        // (the last value).
		        strncpy(tmpval, &value[12+12+4+12+12+4+4+4+4], 8); 
		        tmpval[8] = '\0';
		        // NOTE: It is possible that the carryover value begins
		        // with "-999".  When the array was written, the "-999"
		        // was replaced with "MISS" to prevent problems.  Make
		        // special provision to deal with that case.
		        if( !strncmp( tmpval, "MISS", 4 ) )
		        {
		            strncpy( tmpval, "-999", 4 );
		        }
		        conv_val = atof( tmpval ) / ffactor;
		        sprintf(tmp_str, "   %s %f", list[0], conv_val);
		        length = strlen( tmp_str );
		        ResJ_ccwrite( &length, tmp_str, ipu );
		    }
		    else if( !strcasecmp( list[0], "ENDLOOKUP3" ) )
		    {
		        Lookup3_found = 0;
		        length = strlen( string );
		        ResJ_ccwrite( &length, string, ipu );
		    }
		    else
		    {
		        // Anything else
		        length = strlen( string );
		        ResJ_ccwrite( &length, string, ipu );
		    }
		    list = FreeStringList( list );
		    continue;
		}
		else if( !strcasecmp( list[0], "LOOKUP3" ) )
		{

		    // Find the method information in the carry over array.
		    // The first part of the carry over contains "METHOD" and 
		    // METHOD_ID (METHOD <method_id>).
		    // For example, "METHOD resA_Lookup".
		    sprintf( name, "%-12.12s%-12.12s", "METHOD", list[2] );
		    ToUpper( name );
		    // This finds the first possible match.
		    value = strstr( char_co, name );

		    // We found the first part of a potential match.  Now we
		    // need to check the second part which is 
		    // METHOD_TYPE COMPONENT_ID. For example, "LOOKUP3 RESA".

		    // We need to test all three parts which uniquely define the
		    //  method--TYPE, COMPONENT_ID, and the METHOD_ID.
		    // NOTE: the name variable contains "METHOD" and the 
		    //       METHOD_ID.
		    //   tmp_str and tmp_str0 are used to check method type.
		    //   tmp_str and tmp_str1 are used to check the owning
		    //       component

		    // Reformat list[I] as tmp_strI for full comparison
		    sprintf( tmp_str0, "%-12.12s", list[0] );
		    sprintf( tmp_str1, "%-12.12s", list[1] );
		    surematch = 0;
		    // Work through potential matches until we run out of
		    // carryover or we find a sure match.
		    while (value != NULL && !surematch)
		    {
		        // Get and test method type from carry 
		        // over
		        sprintf( tmp_str, "%-12.12s", &value[24+4] );
		        typeOK = !strncasecmp(tmp_str0, tmp_str, 12);
		        // Get and test owning component from 
		        // carry over
		        sprintf( tmp_str, "%-12.12s", &value[24+4+12] );
		        compOK = !strncasecmp(tmp_str1, tmp_str, 12);
		        // Check the results of the two tests.
		        if( typeOK && compOK )
		        {
		            // We found it!
		            surematch = 1;
		        }
		        else
		        {
		            // We didn't find it.
		            // Increment our string pointer to the
		            // next character to try to find the
		            // next possible match
		            value++;
		            value = strstr( value, name );
		        }
		    }

		    // Did we run out of carryover or fail to find
		    // a match?
		    if( value == NULL || !surematch)
		    {
		        // We did not find it
		        *ierr = 1;
		        //fclose(tfp);
		        fclose(fp);
		        sprintf( error, "Unable to find a sure match for %s %s "
		            "on %s.", list[0], list[1], list[2] );
		        PrintError( routine, error );
		        list = FreeStringList( list );
		        return;
		    }
		    Lookup3_found = 1;
		    length = strlen( string );
		    ResJ_ccwrite( &length, string, ipu );
		    list = FreeStringList( list );
		    continue;
		}
		// End of Deal with LOOKUP3 carry over
		// ---------------------------------------- //


		// ---------------------------------------- //
		// Deal with SetRelease, SetWithdraw, and SetElevation carry 
		//	over beginning here
		// The CO data are on the BLENDTS and BLENDTBL rows.
		// Note: The VALUES keyword is not of interest to us here
		if( SetR_W_E_found ) {
			if( !strcasecmp( list[0], "BLENDTS" ) ) {
				// copy the value following two identifiers, the
				// index 
				strncpy(tmpval, &value[12+12+4+NewCO*24], 4);
				tmpval[4] = '\0';
				conv_int = atoi( tmpval );
				sprintf(tmp_str, "%s  %s  %d", list[0], 
					list[1], conv_int);
				length = strlen( tmp_str );
				ResJ_ccwrite( &length, tmp_str, ipu );
			}
			else if( !strcasecmp( list[0], "BLEND" ) ||
				!strcasecmp( list[0], "BLENDTBL" ) ) {
				// copy the value following two identifiers, and
				// the index and BLENDTS step
				strncpy(tmpval, &value[12+12+4+NewCO*24+4], 4); 
				tmpval[4] = '\0';
				conv_int = atoi( tmpval );
				sprintf(tmp_str, "%s  %s  %d", list[0], 
					list[1], conv_int);
				length = strlen( tmp_str );
				ResJ_ccwrite( &length, tmp_str, ipu );
			}
			else if( !strcasecmp( list[0], "INITIALTRANSFER" ) ) {
				if( SetR_W_E_found != 2 ) {
					// INITIALTRANSFER is only valid for
					// SETWITHDRAW
					list = FreeStringList( list );
					continue;
				}
				// copy the value following two identifiers, 
				// the index, BLENDTS step, and BLEND or 
				// BLENDTBL step.
				strncpy(tmpval, &value[12+12+4+NewCO*24+4+4], 8); 
				tmpval[8] = '\0';
				conv_val = atof( tmpval ) / ffactor;
				sprintf(tmp_str, "%s %f", list[0], conv_val);
				length = strlen( tmp_str );
				ResJ_ccwrite( &length, tmp_str, ipu );

			}
			else if( !strcasecmp( list[0], "ENDSETRELEASE" ) || 
				!strcasecmp( list[0], "ENDSETWITHDRAW") ||
				!strcasecmp( list[0], "ENDSETELEVATION") ) {
				SetR_W_E_found = 0;
				length = strlen( string );
				ResJ_ccwrite( &length, string, ipu );
			}
			else {
				// Anything else
				length = strlen( string );
				ResJ_ccwrite( &length, string, ipu );
			}
			list = FreeStringList( list );
			continue;
		}
		else if( !strcasecmp( list[0], "SETRELEASE" ) || 
			!strcasecmp( list[0], "SETWITHDRAW") ||
			!strcasecmp( list[0], "SETELEVATION") ) {

			// Find the method information in the carry over
			// array ( METHOD and METHOD_ID )
			sprintf( name, "%-12.12s%-12.12s", "METHOD", list[2] );
			ToUpper( name );
			value = strstr( char_co, name );

			// Check for modified carryover structure
			// We used to have 9*4 characters before "*FUTURE*"
			// in the SetRelease, SetWithdraw, SetElevation method
			// carryover and 8*4 characters before "*FUTURE*" in
			// the Adjust method carryover.
			// Now (12/2002), we have 15*4, 17*4, 15*4 and 14*4
			// characters, respectively.
			// It is anticipated that nothing less than 14*4 will
			// be developed (12 for lables, 1 for index, and at
			// least one for some other value).
			length = strcspn( value, "*" );
			NewCO = 1;
			if( length < 14 * 4 ) {
				// We have not added modified carryover yet
				NewCO = 0;
			}

			if( !NewCO ) {
				if( value == NULL ) {
					// We did not find it
					*ierr = 1;
					//fclose(tfp);
					fclose(fp);
					sprintf( error, "Unable to find a "
						"match for %s %s on %s.", 
						list[0], list[1], list[2] );
					PrintError( routine, error );
					list = FreeStringList( list );
					return;
				}
			}
			else {
				// We need to test all three parts which
				// uniquely define the method--TYPE, 
				// COMPONENT_ID, and the METHOD_ID.
				// NOTE: "name" contains "METHOD" and the 
				// 	METHOD_ID
				//   tmp_str and tmp_str0 are used to check 
				//   	method type.
				//   tmp_str and tmp_str1 are used to check the
				//   	owning component

				// Reformat list[I] as tmp_strI for full 
				// comparison
				sprintf( tmp_str0, "%-12.12s", list[0] );
				sprintf( tmp_str1, "%-12.12s", list[1] );
				surematch = 0;
				while (value != NULL && !surematch) {
					// Get and test method type from carry 
					// over
					sprintf( tmp_str, "%-12.12s", 
						&value[24+4] );
					typeOK = !strncasecmp(tmp_str0, 
						tmp_str, 12);
					// Get and test owning component from 
					// carry over
					sprintf( tmp_str, "%-12.12s", 
						&value[24+4+12] );
					compOK = !strncasecmp(tmp_str1, 
						tmp_str, 12);
					// Compare method type and owning 
					// component 
					if( typeOK && compOK ) {
						surematch = 1;
					}
					else {
						// increment the string to the 
						// next character to try to find
						// the next match
						value++;
						value = strstr( value, name );
					}
				}
				if( value == NULL || !surematch) {
					// We did not find it
					*ierr = 1;
					//fclose(tfp);
					fclose(fp);
					sprintf( error, "Unable to find a sure "
						"match for %s %s on %s.", 
						list[0], list[1], list[2] );
					PrintError( routine, error );
					list = FreeStringList( list );
					return;
				}
			}
			if( !strcasecmp( list[0], "SETRELEASE" ) ) {
				SetR_W_E_found = 1;
			}
			else if( !strcasecmp( list[0], "SETWITHDRAW" ) ) {
				SetR_W_E_found = 2;
			}
			else if( !strcasecmp( list[0], "SETELEVATION" ) ) {
				SetR_W_E_found = 3;
			}

			length = strlen( string );
			ResJ_ccwrite( &length, string, ipu );
			list = FreeStringList( list );
			continue;
		}
		// End of Deal with SetRelease, SetWithdraw, and SetElevation 
		// carry over
		// ---------------------------------------- //


		// ---------------------------------------- //
		// Deal with Adjust carry over beginning here
		// The CO data is on the BLENDTBL row.
		// Note: The VALUES keyword is not of interest to us here
		if( Adjust_found ) {
			if( !strcasecmp( list[0], "BLEND" ) ||
				!strcasecmp( list[0], "BLENDTBL" ) ) {
				// copy the value following two identifiers, the
				// index 
				strncpy(tmpval, &value[12+12+4+NewCO*24], 4);
				tmpval[4] = '\0';
				conv_int = atoi( tmpval );
				sprintf(tmp_str, "%s  %s  %d", list[0], 
					list[1], conv_int);
				length = strlen( tmp_str );
				ResJ_ccwrite( &length, tmp_str, ipu );
			}
			else if( !strcasecmp( list[0], "ENDADJUST" ) ) {
				Adjust_found = 0;
				length = strlen( string );
				ResJ_ccwrite( &length, string, ipu );
			}
			else {
				// Anything else
				length = strlen( string );
				ResJ_ccwrite( &length, string, ipu );
			}
			list = FreeStringList( list );
			continue;
		}
		else if( !strcasecmp( list[0], "ADJUST" ) ) {
			// Check for modified carryover structure
			// We used to have 9*4 characters before "*FUTURE*"
			// in the SetRelease, SetWithdraw, SetElevation method
			// carryover and 8*4 characters before "*FUTURE*" in
			// the Adjust method carryover.
			// Now (12/2002), we have 15*4, 17*4, 15*4 and 14*4
			// characters, respectively.
			length = strcspn( value, "*" );
			NewCO = 1;
			if( length < 14*4 ) {
				// We have not added modified carryover yet
				NewCO = 0;
			}

			if( !NewCO ) {
				// Find the method information in the carry over
				// array
				sprintf( name, "%-12.12s%-12.12s", "METHOD", 
					list[2] );
				ToUpper( name );
				value = strstr( char_co, name );
				if( value == NULL ) {
					// We did not find it
					*ierr = 1;
					//fclose(tfp);
					fclose(fp);
					list = FreeStringList( list );
					sprintf( error, "Unable to find a "
						"match for %s %s on %s.", 
						list[0], list[1], list[2] );
					PrintError( routine, error );
					return;
				}
			}
			else {
				// We need to test all three parts which
				// uniquely define the method--TYPE, 
				// COMPONENT_ID, and the METHOD_ID.
				// NOTE: "name" contains "METHOD" and the 
				// 	METHOD_ID
				//   tmp_str and tmp_str0 are used to check 
				//   	method type.
				//   tmp_str and tmp_str1 are used to check the
				//   	owning component

				// Find the method information in the carry over
				// array ( METHOD and METHOD_ID )
				sprintf( name, "%-12.12s%-12.12s", "METHOD", 
					list[2] );
				ToUpper( name );
				value = strstr( char_co, name );

				// Reformat list[I] as tmp_strI for full 
				// comparison
				sprintf( tmp_str0, "%-12.12s", list[0] );
				sprintf( tmp_str1, "%-12.12s", list[1] );
				surematch = 0;
				while (value != NULL && !surematch) {
					// Get and test method type from carry 
					// over
					sprintf( tmp_str, "%-12.12s", 
						&value[24+4] );
					typeOK = !strncasecmp(tmp_str0, 
						tmp_str, 12);
					// Get and test owning component from 
					// carry over
					sprintf( tmp_str, "%-12.12s", 
						&value[24+4+12] );
					compOK = !strncasecmp(tmp_str1, 
						tmp_str, 12);
					// Compare method type and owning
					// component
					if( typeOK && compOK ) {
						surematch = 1;
					}
					else {
						// increment the string to the 
						// next character to try to find
						// the next match
						value++;
						value = strstr( value, name );
					}
				}
				if( value == NULL || !surematch) {
					// We did not find it
					*ierr = 1;
					//fclose(tfp);
					fclose(fp);
					sprintf( error, "Unable to find a sure "
						"match for %s %s on %s.", 
						list[0], list[1], list[2] );
					PrintError( routine, error );
					list = FreeStringList( list );
					return;
				}
			}
			Adjust_found = 1;
			length = strlen( string );
			ResJ_ccwrite( &length, string, ipu );
			list = FreeStringList( list );
			continue;
		}
		// End of Deal Adjust carry over
		// ---------------------------------------- //

		// ---------------------------------------- //
		// Deal with Spillway carry over beginning here
		// The CO data is on the INITIALSPILL row.
		// Note: The VALUES keyword is not of interest to us here
		if( Spillway_found ) {
			if( !strcasecmp( list[0], "INITIALSPILL" ) ) {
				// copy the value following two identifiers, the
				// index, and the other two identifiers.
				strncpy(tmpval, &value[12+12+4+12+12], 8);
				tmpval[8] = '\0';
				conv_val = atof( tmpval ) / ffactor;
				sprintf(tmp_str, "%s  %f\n", list[0], conv_val);
				length = strlen( tmp_str );
				ResJ_ccwrite( &length, tmp_str, ipu );
				whichCO[0] = 1;
			}
			else if( !strcasecmp( list[0], "ENDSPILLWAY" ) ) {
				// Check for required CO values
				for (i = 0; i < 1; i++) {
					if( !whichCO[i] ) {
					   switch (i) {
					      case 0:
						strncpy(tmpval, &value[52], 8);
						tmpval[8] = '\0';
						conv_val = atof(tmpval) / 
							ffactor;
						sprintf(tmp_str, "%s %s", 
						   "INITIALSPILL",conv_val);
						length = strlen( tmp_str );
						ResJ_ccwrite( &length, tmp_str, 
							ipu );
						break;
					   }
					}
					// reinitialize whichCO
					whichCO[i] = 0;
				}
				Spillway_found = 0;
				length = strlen( string );
				ResJ_ccwrite( &length, string, ipu );
			}
			else {
				// Anything else
				length = strlen( string );
				ResJ_ccwrite( &length, string, ipu );
			}
			list = FreeStringList( list );
			continue;
		}
		else if( !strcasecmp( list[0], "SPILLWAY" ) ) {
			// We need to test all three parts which uniquely define
			// the method--TYPE, COMPONENT_ID, and the METHOD_ID.
			// NOTE: "name" contains "METHOD" and the METHOD_ID
			//   tmp_str and tmp_str0 are used to check method type.
			//   tmp_str and tmp_str1 are used to check the owning 
			//   component

			// Find the method information in the carry over array 
			// ( METHOD and METHOD_ID )
			sprintf( name, "%-12.12s%-12.12s", "METHOD", list[2] );
			ToUpper( name );
			value = strstr( char_co, name );

			// Reformat list[I] as tmp_strI for full 
			// comparison
			sprintf( tmp_str0, "%-12.12s", list[0] );
			sprintf( tmp_str1, "%-12.12s", list[1] );
			surematch = 0;
			while (value != NULL && !surematch) {
				// Get and test method type from carry over
				sprintf( tmp_str, "%-12.12s", &value[24+4] );
				typeOK = !strncasecmp(tmp_str0, tmp_str, 12);

				// Get and test owning component from carry over
				sprintf( tmp_str, "%-12.12s", &value[24+4+12] );
				compOK = !strncasecmp(tmp_str1, tmp_str, 12);

				// Compare method type and owning component
				if( typeOK && compOK ) {
					surematch = 1;
				}
				else {
					// increment the string to the next 
					// character to try to find the next 
					// match
					value++;
					value = strstr( value, name );
				}
			}
			if( value == NULL || !surematch) {
				// We did not find it
				*ierr = 1;
				//fclose(tfp);
				fclose(fp);
				sprintf( error, "Unable to find a sure "
					"match for %s %s on %s.", 
					list[0], list[1], list[2] );
				PrintError( routine, error );
				list = FreeStringList( list );
				return;
			}

			Spillway_found = 1;
			length = strlen( string );
			ResJ_ccwrite( &length, string, ipu );
			list = FreeStringList( list );
			continue;
		}
		// End of Deal Spillway carry over
		// ---------------------------------------- //

		// ---------------------------------------- //
		// Deal with CalcInflow carry over beginning here
		// The CO data is on the INITIALSPILL row.
		// Note: The VALUES keyword is not of interest to us here
		if( CalcInflow_found ) {
			if( !strcasecmp( list[0], "REMAININGVOL" ) ) {
				// copy the value following two identifiers, the
				// index, and the other two identifiers.
				strncpy(tmpval, &value[12+12+4+12+12], 16);
				tmpval[16] = '\0';
				if( !strcasecmp( tmpval, "MISSINGX" ) ) {
					conv_val=MISSING;
				}
				else {
					conv_val = atof( tmpval ) / vfactor;
				}
				sprintf(tmp_str, "%s  %f", list[0], conv_val);
				length = strlen( tmp_str );
				ResJ_ccwrite( &length, tmp_str, ipu );
				whichCO[0] = 1;
			}
			else if( !strncasecmp( list[0], "START", 5 ) ) {
				if(!strcasecmp( list[0], "STARTINFLOW")) {
					// copy the value following two 
					// identifiers, the
					// index, two other identifiers, and the
					// other value.
					strncpy(tmpval, 
						&value[12+12+4+12+12+2*8], 8);
					tmpval[8] = '\0';
					if( !strcasecmp( tmpval, "MISSINGX" )) {
						conv_val=MISSING;
					}
					else {
						conv_val = atof( tmpval ) / 
							ffactor;
					}
					sprintf(tmp_str, "%s  %f", list[0], 
						conv_val);
					length = strlen( tmp_str );
					ResJ_ccwrite( &length, tmp_str, ipu );
					whichCO[1] = 1;
				}
				else if( !strcasecmp( list[0], "STARTPOOL" ) ) {
					// copy the value following two 
					// identifiers, the
					// index, two other identifiers, and the
					// other value.
					strncpy(tmpval, 
						&value[12+12+4+12+12+3*8], 8);
					tmpval[8] = '\0';
					if( !strcasecmp( tmpval, "MISSINGX" )) {
						conv_val=MISSING;
					}
					else {
						conv_val = atof( tmpval ) / 
							lfactor;
					}
					sprintf(tmp_str, "%s  %f", list[0], 
						conv_val);
					length = strlen( tmp_str );
					ResJ_ccwrite( &length, tmp_str, ipu );
					whichCO[2] = 1;
				}
				else if( !strcasecmp( list[0], "STARTRELEASE" ) ) {
					// copy the value following two 
					// identifiers, the
					// index, two other identifiers, and the
					// other value.
					strncpy(tmpval, 
						&value[12+12+4+12+12+4*8], 8);
					tmpval[8] = '\0';
					if( !strcasecmp( tmpval, "MISSINGX" )) {
						conv_val=MISSING;
					}
					else {
						conv_val = atof( tmpval ) / 
							ffactor;
					}
					sprintf(tmp_str, "%s  %f", list[0], 
						conv_val);
					length = strlen( tmp_str );
					ResJ_ccwrite( &length, tmp_str, ipu );
					whichCO[3] = 1;
				}
				else if( !strcasecmp( list[0], "STARTWITHDRAWAL" ) ||
					!strcasecmp( list[0], "STARTWITHDRAW" ) ) {
					// copy the value following two 
					// identifiers, the
					// index, two other identifiers, and the
					// other value.
					strncpy(tmpval, 
						&value[12+12+4+12+12+5*8], 8);
					tmpval[8] = '\0';
					if( !strcasecmp( tmpval, "MISSINGX" )) {
						conv_val=MISSING;
					}
					else {
						conv_val = atof( tmpval ) / 
							ffactor;
					}
					sprintf(tmp_str, "%s  %f", list[0], 
						conv_val);
					length = strlen( tmp_str );
					ResJ_ccwrite( &length, tmp_str, ipu );
					whichCO[4] = 1;
				}
			}
			else if( !strcasecmp( list[0], "ENDCALCINFLOW" ) ) {
				// Check for required CO values
				for (i = 0; i < 2; i++) {
					if( !whichCO[i] ) {
					   switch (i) {
					      case 0:
						// REMAININGVOL:
						strncpy(tmpval, &value[52], 16);
						tmpval[8] = '\0';
						if( !strcasecmp( tmpval, 
							"MISSINGX" )) {
							conv_val=MISSING;
						}
						else {
							conv_val = atof(tmpval)
							       / vfactor;
						}
						sprintf(tmp_str, "%s %f", 
						   "REMAININGVOL",conv_val);
						length = strlen( tmp_str );
						ResJ_ccwrite( &length, tmp_str, 
							ipu );
						break;
					      case 1:
						// STARTINFLOW:
						strncpy(tmpval, &value[68], 8);
						tmpval[8] = '\0';
						if( !strcasecmp( tmpval, 
							"MISSINGX" )) {
							conv_val=MISSING;
						}
						else {
							conv_val = atof(tmpval)
							       / ffactor;
						}
						sprintf(tmp_str, "%s %f", 
						   "STARTINFLOW",conv_val);
						length = strlen( tmp_str );
						ResJ_ccwrite( &length, tmp_str, 
							ipu );
						break;
					      case 2:
						// STARTPOOL:
						strncpy(tmpval, &value[76], 8);
						tmpval[8] = '\0';
						if( !strcasecmp( tmpval, 
							"MISSINGX" )) {
							conv_val=MISSING;
						conv_val = atof(tmpval) / 
							lfactor;
						}
						else {
							conv_val = atof(tmpval)
							       / vfactor;
						}
						sprintf(tmp_str, "%s %f", 
						   "STARTPOOL",conv_val);
						length = strlen( tmp_str );
						ResJ_ccwrite( &length, tmp_str, 
							ipu );
						break;
					      case 3:
						// STARTRELEASE:
						strncpy(tmpval, &value[84], 8);
						tmpval[8] = '\0';
						if( !strcasecmp( tmpval, 
							"MISSINGX" )) {
							conv_val=MISSING;
						}
						else {
							conv_val = atof(tmpval)
							       / ffactor;
						}
						sprintf(tmp_str, "%s %f", 
						   "STARTRELEASE",conv_val);
						length = strlen( tmp_str );
						ResJ_ccwrite( &length, tmp_str, 
							ipu );
						break;
					      case 4:
						// STARTWITHDRAWAL:
						strncpy(tmpval, &value[92], 8);
						tmpval[8] = '\0';
						if( !strcasecmp( tmpval, 
							"MISSINGX" )) {
							conv_val=MISSING;
						}
						else {
							conv_val = atof(tmpval)
							       / ffactor;
						}
						sprintf(tmp_str, "%s %f", 
						   "STARTWITHDRAWAL",conv_val);
						length = strlen( tmp_str );
						ResJ_ccwrite( &length, tmp_str, 
							ipu );
						break;
					   }
					}
					// reinitialize whichCO
					whichCO[i] = 0;
				}

				// Write the current line
				CalcInflow_found = 0;
				length = strlen( string );
				ResJ_ccwrite( &length, string, ipu );
			}

			else {
				// Anything else
				length = strlen( string );
				ResJ_ccwrite( &length, string, ipu );
			}
			list = FreeStringList( list );
			continue;
		}
		else if( !strcasecmp( list[0], "CALCINFLOW" ) ) {
			// We need to test all three parts which uniquely define
			// the method--TYPE, COMPONENT_ID, and the METHOD_ID.
			// NOTE: "name" contains "METHOD" and the METHOD_ID
			//   tmp_str and tmp_str0 are used to check method type.
			//   tmp_str and tmp_str1 are used to check the owning 
			//   component

			// Find the method information in the carry over array 
			// ( METHOD and METHOD_ID )
			sprintf( name, "%-12.12s%-12.12s", "METHOD", list[2] );
			ToUpper( name );
			value = strstr( char_co, name );

			// Reformat list[I] as tmp_strI for full 
			// comparison
			sprintf( tmp_str0, "%-12.12s", list[0] );
			sprintf( tmp_str1, "%-12.12s", list[1] );
			surematch = 0;
			while (value != NULL && !surematch) {
				// Get and test method type from carry over
				sprintf( tmp_str, "%-12.12s", &value[24+4] );
				typeOK = !strncasecmp(tmp_str0, tmp_str, 12);

				// Get and test owning component from carry over
				sprintf( tmp_str, "%-12.12s", &value[24+4+12] );
				compOK = !strncasecmp(tmp_str1, tmp_str, 12);

				// Compare method type and owning component
				if( typeOK && compOK ) {
					surematch = 1;
				}
				else {
					// increment the string to the next 
					// character to try to find the next 
					// match
					value++;
					value = strstr( value, name );
				}
			}
			if( value == NULL || !surematch) {
				// We did not find it
				*ierr = 1;
				//fclose(tfp);
				fclose(fp);
				sprintf( error, "Unable to find a sure "
					"match for %s %s on %s.", 
					list[0], list[1], list[2] );
				PrintError( routine, error );
				list = FreeStringList( list );
				return;
			}

			CalcInflow_found = 1;
			length = strlen( string );
			ResJ_ccwrite( &length, string, ipu );
			list = FreeStringList( list );
			continue;
		}
                if( !strcasecmp( list[0], "UNITS" ) ) {
                        if( !strcasecmp( list[1], "ENGLISH" ) ) {
                                lfactor = 0.3048;
                                vfactor = 1233.5;
                                ffactor = 0.028317;
                        }
		}

		list = FreeStringList( list );
		length = strlen( string );
		ResJ_ccwrite( &length, string, ipu );
	}

	sprintf( error, "ENDRES-J" );
	length = strlen( error );
	ResJ_ccwrite( &length, error, ipr );
	//ResJ_fwrite( &length, error, ipr );
	ResJ_ccwrite( &length, error, ipu );
	//ResJ_fwrite( &length, error, ipu );

	fclose( fp );
	free( char_co );

	return;

/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_system/RCS/punch58.cxx,v $";
 static char rcs_id2[] = "$Id: punch58.cxx,v 1.10 2006/10/26 15:37:46 hsu Exp $";}
/*  ===================================================  */

}
