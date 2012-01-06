//------------------------------------------------------------------------------
// execute58.cc : C++ function called by the FORTRAN wrapper to ResJ.
//		This function will construct TS and start and end dates from
//		the info being passed in so that ResJ can be run successfully.
//------------------------------------------------------------------------------
// Copyright:   See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:
//
//------------------------------------------------------------------------------
// History:
//
// 02 Jun 1998  Daniel Weiler, Riverside Technology, inc 	Initial version.
// 12 Apr 2001  James R. VanShaar, RTi	Corrected maximum counting for carry-
//					over writing from size-1 to num_co
// 10 May 2001	JRV, RTi	Rerouted error writing from ResJ_fwrite to
//				PrintError
// 19 Nov 2001	JRV, RTi	Fixed problem parsing LagK information and
//				matching with carry over array.
// 21 Nov 2001	JRV, RTi	Handle LagK INITIALOUTFLOW
// 21 Nov 2001	JRV, RTI	Handle carry over for SetRelease, SetWithdraw, 
//				SetElevation
// 24 Nov 2001	JRV, RTI	Added handling of Combo methods
// 24 Nov 2001	JRV, RTI	Added handling of NODES
// 26 Nov 2001	JRV, RTI	Enhanced Reservoir handling with new carry over
// 28 Nov 2001	JRV, RTI	Added handling of Adjust method
// 14 Dec 2001  JRV, RTi        Removed file_len from function parameter set
// 09 Jul 2002	JRV, RTi	Added special handling of BALANCE method to
//				deal with BALANCE keyword RESERVOIR independant
//				of a RESERVOIR component keyword.
// 29 Jul 2002	JRV, RTi	Corrected a problem with the BALANCE method
// 				being part of a combo-method.
// 01 Aug 2002  KSH, HRL        Added BLENDTBL keyword to indicate table data
//                              blending. BLEND is retained for old datasets.
// 13 Dec 2002	JRV, RTi	Added INITIALTRANSFER for SetWithdraw.
// 18 Dec 2002	JRV, RTi	Revised method carryover to work with entire
// 				method identification set (type, owner_id,
// 				method_id).
// 14 Jan 2003	JRV, RTi	Added handling of Spillway carryover.
// ** *** 2003	JRV, RTi	Fixed sizing of arrays used in setting 
// 				startDate and endDate.
// 16 Feb 2004	JRV, RTi	Added handling of CalcInflow method.
// 10 Feb 2006	JRV, RTi	Added Lookup3 method.
// 18 Feb 2006	JRV, RTi	Addressed carryover issues in hind-casting
//                              and handling of new Node states.
// 1  Mar 2006  Marc L. Baldo, RTi   Added support of the INITIALSTORAGE,
//                                   INITIALLAGGEDINFLOW, and KFTWLOSS
//                                   Added handling associated with the
//                                   number of carryover locations
//                                   for ENDTABLE for Inflow_Lag, and
//                                   TABLE for Inflow_Lag (These were handled
//                                   previously, by pass through.)
// 15 Mar 2006  MLB, RTi        Added support of Variable Lag/Updated Variable 
//                              K with Fort Worth Transmission Loss Recession 
//                              calculations
//------------------------------------------------------------------------------
// Variables:   I/O     Description
//
//
//------------------------------------------------------------------------------
//

#include "ResJSys.h"
#include "TSList.h"
#include "resj/TSDate.h"
#include "HourTS.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

void execute58( char* fname, float* CO, float* PO, float* D, 
	int* d_index, int* co_da, int* co_hr, int* num_co, int* st_da,
	int* st_hr, int* end_da, int* end_hr, int* ifillc, int* iusec, 
	int* ibug, int* ipr, int* iodebug, int* ierr )
{
	char routine[]="execute58", *char_co = NULL, *char_po = NULL,
		**in_tsid_list = NULL, **out_tsid_list = NULL, *value = NULL,
		**co_array = NULL, string[2000], **list = NULL, name[256],
		tmp_str[256], error[256], ctl_file[MAXC], tmp_file[256],
		tmpval[256], *tmp_charCO = NULL, tmp_str0[256], tmp_str1[256],
		tmp_str2[256], *temp;  
          
 //BY HANK...
 char procid[16];
 int procidnum;
 char hostid[16];
 long int hostidnum;

	int i, j, ints, totalts,  n_ts, offset = 0, n_ints = 0,
		n_outts = 0, *out_ts_index = NULL, t_step = 0, nlist = 0,
		ts_count = 0, par_found = 0, LagK_found = 0, val_found = 0,
		index = 0, length = 0, *year = NULL, *month = NULL, 
		*day = NULL, *hour = NULL, float_len, oldindex = 0, 
		SetR_W_E_found = 0, SetMaxI_D_found = 0, Combo_found = 0, 
		Node_found = 0, Reservoir_found = 0, conv_int, 
		sizeInflowCO = 0, initialOutflow = 0, lag = 0, Adjust_found = 0,
		Balance_found = 0, NewCO, surematch, Spillway_found = 0,
		CalcInflow_found = 0, Lookup3_found = 0;
	int numLagTableEntries = 0;
	int inflow_lag_found = 0;
	int whichCO[] = {0, 0, 0, 0, 0, 0, 0, 0};
	int typeOK, compOK;
	float **tsdata = NULL, *float_co = NULL;
	TS** 	outts = NULL;
	TSDate start_date, end_date;
	FILE* fp = NULL, *tfp = NULL;
	double lfactor = 1.0, ffactor = 1.0, vfactor = 1.0, conv_val;
	ResJSys resjsys;
	ResJSys::setIPR_IODEBUG (*ipr, *iodebug);
	int initialLaggedInflow, initialStorage;
	int transLossFound;

	// set the destinations for the debug (and status) and warning messages 
	// to the appropriate levels...
	ResJSys :: setDebugSaveLevel( *ibug );
	// The use of setWarningSaveLevel is currently required to reach the
	// 	SetWarningLevel(...) function
	ResJSys :: setWarningSaveLevel( 1 + *ibug );
		// value of 2 will cause functions printing the warning to 
		// appear

	// Make sure we have a control file to work with...
	if( fname == NULL ) {
		*ierr = 0;
		sprintf( error, "Cannot execute ResJ - fs5file non-existent." );
		PrintError( routine, error );
		return;
	}
	// Otherwise, use the length of the file name
	// to determine the actual file name. We have to do this explicitly
	// to cleanly handle the Fortran to C++ char* interface, which is
	// not a pretty one.
	for( i = 0; i < strlen( fname ); i++ ) {
		if( fname[i] == ' ') {
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
		//ResJ_fwrite( &length, error, ipr );
	}
	else {
		ctl_file[ length ] = '\0';
	}

//cew split if on open into two pieces
	fp = fopen( ctl_file, "r" );
	if( fp == NULL ) {
		*ierr = 1;
		sprintf( error, "Troubles opening fs5file --> %s", ctl_file );
		PrintError( routine, error );
		return;
        }

//hdh get the process id and put it into a string.
	procidnum = getpid();
	sprintf(procid, "%d", procidnum);
	hostidnum = gethostid();
	sprintf(hostid, "%d", hostidnum);

// cew put temp file into users home directory.
//hdh        strcpy(tmp_file, getenv("HOME"));
//hdh	strcat(tmp_file,"/.resj_tmp_file");
    strcpy(tmp_file,"/tmp");
	strcat(tmp_file,"/resj_tmp_file.");
	strcat(tmp_file,procid);
	strcat(tmp_file,".");
	strcat(tmp_file,hostid);
	tfp = fopen( tmp_file, "w+" );
	if( tfp == NULL ) {
		*ierr = 1;
		sprintf( error, "Troubles opening temporary fs5file %s", 
			tmp_file );
		PrintError( routine, error );
		return;
	}

	// Convert the incoming float* to a char*
	char_co = (char*)malloc( (*iusec)*sizeof(float)+1 );
	if( char_co == NULL ) {
		*ierr = 1;
		sprintf( error, "Troubles allocating memory for CO array." );
		PrintError( routine, error );
		return;
	}
	memcpy( char_co, CO, (*iusec)*sizeof(float) );
	char_co[ (*iusec)*sizeof(float) ] = '\0';

	// Get the number of time series
	n_ts = (int)PO[1];

	while( fgets( string, 256, fp ) != NULL ) {
		if( list ) {
			list = FreeStringList( list );
		}

		UnpadString( string, " \t\n", PAD_BACK );
		list = BreakStringList( string, " \t\n", DELIM_SKIP_BLANKS,
			&nlist );
		if( list == NULL || nlist == 0 ) {
			continue;
		}

		if( !strcasecmp( list[0], "UNITS" ) ) {
			if( !strcasecmp( list[1], "ENGLISH" ) ) {
				lfactor = 0.3048;
				vfactor = 1233.5;
				ffactor = 0.028317;
			}
		}
	        Component::_lfactor = lfactor;
	        Component::_vfactor = vfactor;
        	Component::_ffactor = ffactor;
            
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

			fprintf(tfp, "%s\n", string );
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
			// Don't bother with concern about RESERVOIR vs 
			// BALRES and ENDRESEROVOR vs
			// ENDBALRES.  The parsing of the system
			// will provide the warnings.
			// Simply print the line.
			fprintf(tfp, "%s\n", string);
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
				fprintf(tfp, "%s\n", string);
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
						fprintf(tfp, "%s\n", tmp_str);
						break;
					      case 1:
						strncpy(tmpval, &value[36], 8);
						tmpval[8] = '\0';
						conv_val = atof(tmpval) / 
							lfactor;
						sprintf(tmp_str, "%s %f", 
						   "INITIALPOOL",conv_val);
						fprintf(tfp, "%s\n", tmp_str);
						break;
					      case 2:
						strncpy(tmpval, &value[44], 8);
						tmpval[8] = '\0';
						conv_val = atof(tmpval) / 
							ffactor;
						sprintf(tmp_str, "%s %f", 
						   "INITIALWITHDRAW",conv_val);
						fprintf(tfp, "%s\n", tmp_str);
						break;
					      case 3:
						strncpy(tmpval, &value[52], 8);
						tmpval[8] = '\0';
						conv_val = atof(tmpval) / 
							ffactor;
						sprintf(tmp_str, "%s %f", 
						   "INITIALINFLOW",conv_val);
						fprintf(tfp, "%s\n", tmp_str);
						break;
					      case 4:
						strncpy(tmpval, &value[60], 8);
						tmpval[8] = '\0';
						conv_val = atof(tmpval) / 
							ffactor;
						sprintf(tmp_str, "%s %f", 
						   "PREVIOUSRELEASE",conv_val);
						fprintf(tfp, "%s\n", tmp_str);
						break;
					      case 5:
						strncpy(tmpval, &value[68], 8);
						tmpval[8] = '\0';
						conv_val = atof(tmpval) / 
							lfactor;
						sprintf(tmp_str, "%s %f", 
						   "PREVIOUSPOOL",conv_val);
						fprintf(tfp, "%s\n", tmp_str);
						break;
					      case 6:
						strncpy(tmpval, &value[76], 8);
						tmpval[8] = '\0';
						conv_val = atof(tmpval) / 
							ffactor;
						sprintf(tmp_str, "%s %f", 
						   "PREVIOUSWITHDRAW",conv_val);
						fprintf(tfp, "%s\n", tmp_str);
						break;
					      case 7:
						strncpy(tmpval, &value[84], 8);
						tmpval[8] = '\0';
						conv_val = atof(tmpval) / 
							ffactor;
						sprintf(tmp_str, "%s %f", 
						   "PREVIOUSINFLOW",conv_val);
						fprintf(tfp, "%s\n", tmp_str);
						break;
					   }
					}
					// reinitialize whichCO
					whichCO[i] = 0;
				}
				Reservoir_found = 0;
				fprintf(tfp, "%s\n", string);
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
				fprintf(tfp, "%s\n", string);
				continue;
			}
			// Handle carry over and print line
			tmpval[8] = '\0';
			if ( !strcasecmp( list[0], "INITIALPOOL" ) ||
				!strcasecmp( list[0], "PREVIOUSPOOL" ) ) {
				// comvert using lengths
				conv_val = atof( tmpval ) / lfactor;	
			}
			else {
				conv_val = atof( tmpval ) / ffactor;	
			}
			sprintf( tmp_str, "%s  %f", list[0], conv_val );
			fprintf(tfp, "%s\n", tmp_str);
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
				fclose(tfp);
				fclose(fp);
				sprintf( error, "Troubles with RESERVOIR %s.",
					name );
				PrintError( routine, error );
				list = FreeStringList( list );
				return;
			}
			fprintf(tfp, "%s\n", string);
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
			    if( !strcasecmp( list[0], "INITIALDISCHARGE" ) ||
			        !strcasecmp( list[0], "DISCHARGE" ) )
			    {
			        // copy the value following two identifiers, and
			        // the index
			        strncpy(tmpval, &value[12+12+4], 8); 
			        tmpval[8] = '\0';
			        conv_val = atof( tmpval ) / ffactor;
			        whichCO[0] = 1;
			    }
			    else if(!strcasecmp( list[0], "PREVIOUSDISCHARGE" ))
			    {
			        // copy the value following two identifiers, and
			        // the index and DISCHARGE
			        strncpy(tmpval, &value[12+12+4+8], 8); 
			        tmpval[8] = '\0';
			        conv_val = atof( tmpval ) / ffactor;
			        whichCO[1] = 1;
			    }
			    else
			    {
			        // We don't recognize what this will be.
			        // Write the line and let the parser deal with it.
			        fprintf(tfp, "%s\n", string);
			        continue;
			    }
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
			        whichCO[3] = 3;
			    }
			    else
			    {
			        // We don't recognize what this will be.
			        // Write the line and let the parser deal with it.
			        fprintf(tfp, "%s\n", string);
			        continue;
			    }
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
			        whichCO[5] = 5;
			    }
			    else
			    {
			        // We don't recognize what this will be.
			        // Write the line and let the parser deal with it.
			        fprintf(tfp, "%s\n", string);
			        continue;
			    }
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
			                sprintf(tmp_str, "%s %f", 
			                   "DISCHARGE",conv_val);
			                fprintf(tfp, "%s\n", tmp_str);
			                break;
			              case 1:
			                strncpy(tmpval, &value[36], 8);
			                tmpval[8] = '\0';
			                conv_val = atof( tmpval ) / ffactor;	
			                sprintf(tmp_str, "%s %f", 
			                   "PREVIOUSDISCHARGE",conv_val);
			                fprintf(tfp, "%s\n", tmp_str);
			                break;
			              case 2:
			                if( !NewCO )
			                {
			                    break;
			                }
			                strncpy(tmpval, &value[44], 8);
			                tmpval[8] = '\0';
			                conv_val = atof( tmpval ) / ffactor;	
			                sprintf(tmp_str, "%s %f", 
			                   "INITIALINFLOW",conv_val);
			                fprintf(tfp, "%s\n", tmp_str);
			                break;
			              case 3:
			                if( !NewCO )
			                {
			                    break;
			                }
			                strncpy(tmpval, &value[52], 8);
			                tmpval[8] = '\0';
			                conv_val = atof( tmpval ) / ffactor;	
			                sprintf(tmp_str, "%s %f", 
			                   "PREVIOUSINFLOW",conv_val);
			                fprintf(tfp, "%s\n", tmp_str);
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
			                sprintf(tmp_str, "%s %f", 
			                   "INITIALDIVERSION",conv_val);
			                fprintf(tfp, "%s\n", tmp_str);
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
			                sprintf(tmp_str, "%s %f", 
			                   "PREVIOUSDIVERSION",conv_val);
			                fprintf(tfp, "%s\n", tmp_str);
			                break;
			           }
			        }
			        // reinitialize whichCO
			        whichCO[i] = 0;
			    }
			    Node_found = 0;
			    fprintf(tfp, "%s\n", string);
			    continue;
			}
			else {
			        fprintf(tfp, "%s\n", string);
			        continue;
			}
			// Print the carryover line
			sprintf(tmp_str, "%s  %f", list[0], conv_val);
			fprintf(tfp, "%s\n", tmp_str);
			continue;
		}
		else if( !strcasecmp( list[0], "NODE" ) ) {
			Node_found = 1;
			// Find the method information in the carry over array
			sprintf( name, "%-12.12s%-12.12s", list[0], list[1] );
			ToUpper( name );
			value = strstr( char_co, name );
			if( value == NULL ) {
				*ierr = 1;
				fclose(tfp);
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
			fprintf(tfp, "%s\n", string);
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
			fprintf(tfp, "%s\n", string);
			continue;
		}
		else if( (!strcasecmp(list[0], "SETSUM")) || 
			(!strcasecmp( list[0], "SETMAX")) || 
			(!strcasecmp(list[0], "SETMIN")) ) {
			Combo_found = 1;
			fprintf(tfp, "%s\n", string);
			continue;
		}
		// End of Combo Method handling
		// ---------------------------------------- //
		
		// ---------------------------------------- //
		// Handle LagK method carry over
		// Carry over data consists of inflows and INITIALOUTFLOW
		if( LagK_found ) {
			if( val_found == 1 ) {
				if( !strcasecmp( list[0], "ENDVALUES" ) ) {
					sizeInflowCO = index;
					index = 0;
					val_found = 0;	
					fprintf(tfp, "%s\n", string );
				}
				else {
					// This works with the LagK COINFLOWS
					if( !strncmp(list[0], "#", 1) ) {
						// This is a comment line
						fprintf(tfp, "%s\n", string );
						continue;
					}
					strncpy(tmpval, 
						&value[40+NewCO*12+index*8], 8);
					tmpval[8] = '\0';
					conv_val = atof( tmpval ) / ffactor;
					fprintf(tfp, "%f\n", conv_val);
					index++;
				}
				continue;
			}
			else if( !strcasecmp( list[0], "INITIALOUTFLOW" ) ) {
				// This works with LagK INITIALOUTFLOW
				initialOutflow = 1;
				strncpy( tmpval, 
					&value[40+NewCO*12+sizeInflowCO*8], 8);
				tmpval[8] = '\0';
				conv_val = atof( tmpval ) / ffactor;
				fprintf(tfp, "%s  %f\n", list[0], conv_val);
				continue;
			}
			else if( !strcasecmp( list[0], "INITIALSTORAGE" ) ) {
				// works with LagK INITIALSTORAGE
				initialStorage = 1;
				strncpy( tmpval, 
					&value[48+NewCO*12+sizeInflowCO*8], 8);
				tmpval[8] = '\0';
				conv_val = atof( tmpval ) / ffactor;
				fprintf(tfp, "%s  %f\n", list[0], conv_val);
				continue;
			}
			else if( !strcasecmp( list[0], "INITIALLAGGEDINFLOW" ) ) {
				// This works with LagK INITIALLAGGEDINFLOW
				initialLaggedInflow = 1;
				strncpy( tmpval, 
					&value[56+NewCO*12+sizeInflowCO*8], 8);
				tmpval[8] = '\0';
				conv_val = atof( tmpval ) / ffactor;
				fprintf(tfp, "%s  %f\n", list[0], conv_val);
				continue;
			}
			else if( !strcasecmp( list[0], "KFTWLOSS" ) ) {
				transLossFound = 1;
			}
			else if( !strcasecmp( list[0], "VALUES" ) ) {
				val_found = 1;	
				index = 0;
			}
			else if( !strcasecmp( list[0], "LAG" ) ) {
                                if(list[1] && strlen(list[1]) > 0) {
					lag = atoi( list[1] );
				}
			}
			else if( inflow_lag_found && ( !strcasecmp( list[0], "ENDTABLE" ) ) ) {
				inflow_lag_found = 0;
			}
			else if( inflow_lag_found ) {
				numLagTableEntries++;
				// Keep track of the maximum lag in table
                                if(list[1] && strlen(list[1]) > 0) {
					int lagtemp = (int) ( atof ( list[1] ) + 0.5 ) ;
					if( lagtemp > lag ) {
						lag = lagtemp;
					}
				}
			}
			else if( ( !strcasecmp( list[0], "TABLE" ) ) && 
                                 list[1] &&
                                 ( !strcasecmp ( "inflow_lag", list[1] ) ) ) {
				inflow_lag_found = 1;
			}
			else if( !strcasecmp( list[0], "ENDLAGK" ) ) {
				if( sizeInflowCO == 0) {
					// We did not find any values.  We need 
					// to print the block with COINFLOW, 
					// VALUES, the data (from carry over), 
					// ENDVALUES and COINFLOW before ending 
					// the LAGK parameterization
					fprintf(tfp, "%s\n", "COINFLOW");
					fprintf(tfp, "%s\n", "VALUES");
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
						fprintf(tfp, "%8.8f\n", 
							conv_val);
					}
					fprintf(tfp, "%s\n", "ENDVALUES");
					fprintf(tfp, "%s\n", "ENDCOINFLOW");
				}
				if( !initialOutflow ) {
					// We did not find an initial outflow
					// value.
					sprintf(tmpval, "%8.8s", 
						&value[40+NewCO*12+
						sizeInflowCO*8]);
					tmpval[8] = '\0';
					conv_val = atof( tmpval ) / ffactor;
					fprintf(tfp, "%s %8.8f\n", 
						"INITIALOUTFLOW", conv_val);
				}
				if( !initialStorage ) {
					// We did not find an initial 
					// storageCO value.
					sprintf(tmpval, "%8.8s", 
						&value[48+NewCO*12+
						sizeInflowCO*8]);
					tmpval[8] = '\0';
					conv_val = atof( tmpval ) / ffactor;
					fprintf(tfp, "%s %8.8f\n", 
						"INITIALSTORAGE", conv_val);
				}
				if( !initialLaggedInflow ) {
					// We did not find an initial 
					// lagged Inflow value.
					sprintf(tmpval, "%8.8s", 
						&value[56+NewCO*12+
						sizeInflowCO*8]);
					tmpval[8] = '\0';
					conv_val = atof( tmpval ) / ffactor;
					fprintf(tfp, "%s %8.8f\n", 
						"INITIALLAGGEDINFLOW", conv_val);
				}
				lag = 0;
				initialOutflow = 0;
				initialStorage = 0;
				initialLaggedInflow = 0;
				sizeInflowCO = 0;
				transLossFound = 0;
				LagK_found = 0;
				NewCO = 0;
			}
			fprintf(tfp, "%s\n", string);
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
				sprintf( tmp_str, "%-12.12s", &value[24+4] );
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
				fclose(tfp);
				fclose(fp);
				sprintf( error, "Unable to find a sure match "
					"for %s %s on %s.", list[0], list[1], 
					list[2] );
				PrintError( routine, error );
				list = FreeStringList( list );
				return;
			}
			LagK_found = 1;
			fprintf(tfp, "%s\n", string);
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
		        strncpy(tmp_str0, &value[12+12+4+12+12], 4);
		        tmp_str0[4] = '\0';
		        // And the value after the BLENDTBL step
		        strncpy(tmp_str1, &value[12+12+4+12+12+4+4], 4);
		        tmp_str1[4] = '\0';
		        // And the value after the Column Index value
		        strncpy(tmp_str2, &value[12+12+4+12+12+4+4+4], 4);
		        tmp_str2[4] = '\0';
		        // Copy the value following two identifiers, the index,
		        // two more identifiers, BLENDTS step, BLENDTBL step,
		        // the column Index value, and the row Index value.
		        // (the last value).
		        strncpy(tmpval, &value[12+12+4+12+12+4+4+4+4], 8); 
		        tmpval[8] = '\0';
		        conv_val = atof( tmpval ) / ffactor;
		        fprintf(tfp, "%s  %s  %s %s %s %f\n", list[0], list[1],
                            tmp_str0, tmp_str1, tmp_str2, conv_val);
		    }
		    else if( !strcasecmp( list[0], "BLENDTBL" ) )
		    {
		        // We have a BLENDTBL step, column Index value and a row
		        // Index value.  None of these need units conversion.
		        // They may handled as strings.
		        // Copy the values following two identifiers, the index,
		        // two more identifiers, and BLENDTS step, BLENDTBL step
		        strncpy(tmp_str0, &value[12+12+4+12+12+4], 4);
		        tmp_str0[4] = '\0';
		        // And the value after the BLENDTBL step
		        strncpy(tmp_str1, &value[12+12+4+12+12+4+4], 4);
		        tmp_str1[4] = '\0';
		        // And the value after the Column Index value
		        strncpy(tmp_str2, &value[12+12+4+12+12+4+4+4], 4);
		        tmp_str2[4] = '\0';
		        // And the value after the Row Index value (the last
		        // value).
		        strncpy(tmpval, &value[12+12+4+12+12+4+4+4+4], 8); 
		        tmpval[8] = '\0';
		        conv_val = atof( tmpval ) / ffactor;
		        fprintf(tfp, "%s  %s  %s %s %s %f\n", list[0], list[1],
                            tmp_str0, tmp_str1, tmp_str2, conv_val);
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
		        fprintf(tfp, "%s %f\n", list[0], conv_val);
		    }
		    else if( !strcasecmp( list[0], "ENDLOOKUP3" ) )
		    {
		        Lookup3_found = 0;
		        fprintf(tfp, "%s\n", string);
		    }
		    else
		    {
		        // Anything else
		        fprintf(tfp, "%s\n", string );
		    }
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
		        fclose(tfp);
		        fclose(fp);
		        sprintf( error, "Unable to find a sure match for %s %s "
		            "on %s.", list[0], list[1], list[2] );
		        PrintError( routine, error );
		        list = FreeStringList( list );
		        return;
		    }
		    Lookup3_found = 1;
		    fprintf(tfp, "%s\n", string);
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
				fprintf(tfp, "%s  %s  %d\n", list[0], list[1], 
					conv_int);
			}
			else if( !strcasecmp( list[0], "BLEND" ) ||
			         !strcasecmp( list[0], "BLENDTBL" ) ) {
				// copy the value following two identifiers, and
				// the index and BLENDTS step
				strncpy(tmpval, &value[12+12+4+NewCO*24+4], 4); 
				tmpval[4] = '\0';
				conv_int = atoi( tmpval );
				fprintf(tfp, "%s  %s  %d\n", list[0], list[1], 
					conv_int);
			}
			else if( !strcasecmp( list[0], "INITIALTRANSFER" ) ) {
				if( SetR_W_E_found != 2 ) {
					// INITIALTRANSFER is only valid for
					// SETWITHDRAW
					continue;
				}
				// copy the value following two identifiers, 
				// the index, BLENDTS step, and BLEND or 
				// BLENDTBL step.
				strncpy(tmpval, &value[12+12+4+NewCO*24+4+4], 8); 
				tmpval[8] = '\0';
				conv_val = atof( tmpval ) / ffactor;
				fprintf(tfp, "%s %f\n", list[0], conv_val);
			}
			else if( !strcasecmp( list[0], "ENDSETRELEASE" ) || 
				!strcasecmp( list[0], "ENDSETWITHDRAW") ||
				!strcasecmp( list[0], "ENDSETELEVATION") ) {
				SetR_W_E_found = 0;
				fprintf(tfp, "%s\n", string);
				NewCO = 0;
			}
			else {
				// Anything else
				fprintf(tfp, "%s\n", string );
			}
			continue;
		}
		else if( !strcasecmp( list[0], "SETRELEASE" ) || 
			!strcasecmp( list[0], "SETWITHDRAW") ||
			!strcasecmp( list[0], "SETELEVATION") ) {

			// Find the method information in the carry over
			// array ( METHOD and METHOD_ID )
			sprintf( name, "%-12.12s%-12.12s", "METHOD", 
				list[2] );
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
					fclose(tfp);
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
					fclose(tfp);
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
			fprintf(tfp, "%s\n", string);
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
				fprintf(tfp, "%s  %s  %d\n", list[0], list[1], 
					conv_int);
			}
			else if( !strcasecmp( list[0], "ENDADJUST" ) ) {
				Adjust_found = 0;
				fprintf(tfp, "%s\n", string);
				NewCO = 0;
			}
			else {
				// Anything else
				fprintf(tfp, "%s\n", string );
			}
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
					fclose(tfp);
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
					fclose(tfp);
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
			fprintf(tfp, "%s\n", string);
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
				fprintf(tfp, "%s  %f\n", list[0], conv_val);
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
						fprintf(tfp, "%s\n", tmp_str);
						break;
					   }
					}
					// reinitialize whichCO
					whichCO[i] = 0;
				}

				// Write the current line
				Spillway_found = 0;
				fprintf(tfp, "%s\n", string);
			}
			else {
				// Anything else
				fprintf(tfp, "%s\n", string );
			}
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
				fclose(tfp);
				fclose(fp);
				sprintf( error, "Unable to find a sure "
					"match for %s %s on %s.", 
					list[0], list[1], list[2] );
				PrintError( routine, error );
				list = FreeStringList( list );
				return;
			}

			Spillway_found = 1;
			fprintf(tfp, "%s\n", string);
			continue;
		}
		// End of Deal Spillway carry over
		// ---------------------------------------- //


		// ---------------------------------------- //
		// Deal with CalcInflow carry over beginning here
		// The CO data is on the STARTCALCINFLOW and REMAININGVOL rows.
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
				fprintf(tfp, "%s  %f\n", list[0], conv_val);
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
					fprintf(tfp, "%s  %f\n", list[0], 
						conv_val);
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
					fprintf(tfp, "%s  %f\n", list[0], 
						conv_val);
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
					fprintf(tfp, "%s  %f\n", list[0], 
						conv_val);
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
					fprintf(tfp, "%s  %f\n", list[0], 
						conv_val);
					whichCO[4] = 1;
				}
			}
			else if( !strcasecmp( list[0], "ENDCALCINFLOW" ) ) {
				// Check for required CO values
				for (i = 0; i < 5; i++) {
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
						fprintf(tfp, "%s\n", tmp_str);
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
						fprintf(tfp, "%s\n", tmp_str);
						break;
					      case 2:
						// STARTPOOL:
						strncpy(tmpval, &value[76], 8);
						tmpval[8] = '\0';
						if( !strcasecmp( tmpval, 
							"MISSINGX" )) {
							conv_val=MISSING;
						}
						else {
							conv_val = atof(tmpval)
							       / lfactor;
						}
						sprintf(tmp_str, "%s %f", 
						   "STARTPOOL",conv_val);
						fprintf(tfp, "%s\n", tmp_str);
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
						fprintf(tfp, "%s\n", tmp_str);
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
						fprintf(tfp, "%s\n", tmp_str);
						break;
					   }
					}
					// reinitialize whichCO
					whichCO[i] = 0;
				}

				// Write the current line
				CalcInflow_found = 0;
				fprintf(tfp, "%s\n", string);
			}
			else {
				// Anything else
				fprintf(tfp, "%s\n", string );
			}
			continue;
		}
		else if( !strcasecmp( list[0], "CALCINFLOW" ) ) {
			if( FindMatch( char_co, list, &value ) ) {
				// We did not find it
				*ierr = 1;
				fclose(tfp);
				fclose(fp);
				sprintf( error, "Unable to find a sure "
					"match for %s %s on %s.", 
					list[0], list[1], list[2] );
				PrintError( routine, error );
				list = FreeStringList( list );
				return;
			}

			CalcInflow_found = 1;
			fprintf(tfp, "%s\n", string);
			continue;
		}
		// End of Deal CalcInflow carry over
		// ---------------------------------------- //

/*
		if( !strcasecmp( list[0], "UNITS" ) ) {
			if( !strcasecmp( list[1], "ENGLISH" ) ) {
				lfactor = 0.3048;
				vfactor = 1233.5;
				ffactor = 0.028317;
			}
		}
	        Component::_lfactor = lfactor;
	        Component::_vfactor = vfactor;
        	Component::_ffactor = ffactor;
*/
		fprintf(tfp, "%s\n", string );
	}
	fclose( tfp );
	if( list ) {
		list = FreeStringList( list );
	}

	if( *num_co == 0 ) {
		year = new int[1];
		month = new int[1];
		day = new int[1];
		hour = new int[1];
	}
	else {
		year = new int[*num_co];
		month = new int[*num_co];
		day = new int[*num_co];
		hour = new int[*num_co];
	}
	if( !year || !month || !day || !hour ) {
		*ierr = 0;
		sprintf( error, "Troubles allocating memory." );
		length = strlen( error );
		PrintError( routine, error );
		//ResJ_fwrite( &length, error, ipr );
	}
	// Set the beginning and ending forecast dates for simulation run...
	// First check that the beginning date is less than the end date
	if( ( (float)(*st_da) + (float)(*st_hr)/24.0 ) >= 
		( (float)(*end_da) + (float)(*end_hr)/24.0) ) {
		sprintf( error, "Start Date is after End Date." );
		length = strlen( error );
		PrintError( routine, error );
		//ResJ_fwrite( &length, error, ipr );
		*ierr = 1;
	}
	GetDateFromJulianHour1900( (*st_da-1)*24+(*st_hr-1), &month[0], 
		&day[0], &year[0], &hour[0] );
	resjsys.setStartDate( hour[0], day[0], month[0], year[0] );
	start_date.setYear( year[0] );
	start_date.setMonth( month[0] );
	start_date.setDay( day[0] );
	start_date.setHour( hour[0] );
	GetDateFromJulianHour1900( (*end_da-1)*24+(*end_hr-1), &month[0], 
		&day[0], &year[0], &hour[0] );
	resjsys.setEndDate( hour[0], day[0], month[0], year[0] );
	end_date.setYear( year[0] );
	end_date.setMonth( month[0] );
	end_date.setDay( day[0] );
	end_date.setHour( hour[0] );
	for( i = 0; i < *num_co; i++ ) {
		GetDateFromJulianHour1900( (co_da[i]-1)*24+(co_hr[i]-1), 
			&month[i], &day[i], &year[i], &hour[i] );
	}
	resjsys.setCODates( hour, day, month, year, *num_co );
	delete [] year;
	delete [] month;
	delete [] day;
	delete [] hour;

	// The ResJSys :: run will take care of filling in the TS from PO
	// with the data from D. We just need to make sure that we provide
	// the correct Identifier and Data on this end.
	char_po = (char*)malloc( (n_ts*5)*sizeof(float)+1 );
	if( char_po == NULL ) {
		*ierr = 1;
		sprintf( error, "Troubles allocating memory for PO array." );
		PrintError( routine, error );
		return;
	}
	memcpy( char_po, &PO[4], ((n_ts*5)*sizeof(float) ) );
	char_po[ (n_ts*5)*sizeof(float) ] = '\0';
	
	// Getting all of the Identifiers from the PO array ( from the input
	// deck ) that are marked as input and sending those off to the 
	// run function along with the data from D.
	in_tsid_list = new char*[n_ints];
	out_tsid_list = new char*[n_outts];
	if( in_tsid_list == NULL || out_tsid_list == NULL ) {
		*ierr = 1;
		sprintf( error, "Troubles allocating memory." );
		PrintError( routine, error );
		return;
	}
	tsdata = new float*[n_ints];
	out_ts_index = new int[n_outts];
	if( tsdata == NULL || out_ts_index == NULL ) {
		*ierr = 1;
		sprintf( error, "Troubles allocating memory." );
		PrintError( routine, error );
		return;
	}

	// Build the list of TS identifiers to be passed to ResJ :: run
	for( i = 0, totalts = 0, ints = 0; i < strlen( char_po ); i+=20 ) {
		if( char_po[i+18] == 'I' && char_po[i+19] == 'N' ) {
			in_tsid_list[ints] = new char[24]; 
			in_tsid_list[ints][0] = '\0';
			strncpy( in_tsid_list[ints], &char_po[i], 8 );
			in_tsid_list[ints][8] = '\0';
			strncat( in_tsid_list[ints], " ", 2 );
			strncat( in_tsid_list[ints], &char_po[i+8], 6 );
			in_tsid_list[ints][14] = '\0';
			strncat( in_tsid_list[ints], " ", 2 );
			strncat( in_tsid_list[ints], &char_po[i+14], 4 );
			if( in_tsid_list[ints] == NULL ) {
				*ierr = 1;
				sprintf( error, "Troubles allocating memory." );
				PrintError( routine, error );
				return;
			}
			in_tsid_list[ints][18] = '\0';
			tsdata[ints] = &D[d_index[totalts]];
			ints++;
		}
		else {
			out_tsid_list[totalts-ints] = new char[24]; 
			out_tsid_list[totalts-ints][0] = '\0';
			strncpy( out_tsid_list[totalts-ints], &char_po[i], 8 );
			out_tsid_list[totalts-ints][8] = '\0';
			strncat( out_tsid_list[totalts-ints], " ", 2 );
			strncat( out_tsid_list[totalts-ints], &char_po[i+8],6);
			out_tsid_list[totalts-ints][14] = '\0';
			strncat( out_tsid_list[totalts-ints], " ", 2 );
			strncat( out_tsid_list[totalts-ints],&char_po[i+14],4);
			if( out_tsid_list[totalts-ints] == NULL ) {
				*ierr = 1;
				sprintf( error, "Troubles allocating memory." );
				PrintError( routine, error );
				return;
			}
			out_tsid_list[totalts-ints][18] = '\0';
			out_ts_index[totalts-ints] = d_index[totalts];
		}
		totalts++;
	}

	// Run ResJ...
	*ierr = resjsys.run( tmp_file, ints, in_tsid_list, tsdata ); 
	if( *ierr > 0 ) { 
		sprintf( error, "ResJ did not execute." );
		PrintError( routine, error );
		return;
	}
	remove( tmp_file );

	outts = resjsys.getResJOutput( out_tsid_list, n_outts );

	// Now read the outputTS(s) into the D array...
	TSDate idate( TSDate :: DATE_FAST );
	for( i = 0; i < n_outts; i++ ) {
		// all of outts will be NULL if outts[i] is NULL for any i
		if( outts == NULL ) {
			sprintf( error, "Unable to get one or more output "
				"TS.  See the log file." );
			PrintError( routine, error );
			*ierr = 1;
			return;
		}
		else {
			for( idate = start_date, j = 0 ; idate <= end_date; 
			idate.addInterval( TS :: INTERVAL_HOUR, t_step ), 
				j++ ) {
				D[out_ts_index[i]+j] = 
					outts[i]->getDataValue( idate ); 
			}
		}
	}

	// Finally, manage the carryover strings written during the simulation
	// run at the CO dates.
	int size, writeSize = 0;
	co_array = ResJSys :: getCOArray( &size, &writeSize );
	if( co_array == NULL || size == 0 ) {
		*ierr = 1;
		sprintf( error, "Troubles getting the CO array from ResJ.");
		PrintError( routine, error );
		return;
	}
	if( *ifillc == 1 && *num_co == 0 ) {
		length = strlen( co_array[size-1] );
		memcpy( CO, co_array[size-1], length );
	}
	if( *ifillc == 1 && *num_co > 0 ) {
		length = strlen( co_array[size-1] );
		memcpy( CO, co_array[size-1], length );
		for( i = 0; i < writeSize; i++ ) {
			length = strlen( co_array[i] );
			float_len = strlen( co_array[i] )/4;
			float_co = new float[ float_len ];
			memcpy( float_co, co_array[i], length );
			ResJ_ccfcwtco( &co_da[i], &co_hr[i], float_co, 
				&float_len ); 
			delete [] float_co;
		}
	}

	// Clean up some memory allocation
	if( in_tsid_list != NULL ) {
		for( i = 0; i < ints; i++ ) {
			if( in_tsid_list[i] != NULL ) {
				delete [] in_tsid_list[i];
			}
		}
		delete [] in_tsid_list;
	}
	if( out_tsid_list != NULL ) {
		for( i = 0; i < totalts-ints; i++ ) {
			if( out_tsid_list[i] != NULL ) {
				delete [] out_tsid_list[i];
			}
		}
		delete [] out_tsid_list;
	}

	delete [] outts; 
	delete [] tsdata; 
	delete [] out_ts_index; 
	free( char_co );
	free( char_po );

	fclose( fp );

	sprintf( error, "Successfully ran ResJ.");
	length = strlen( error );
	ResJ_fwrite( &length, error, ipr );

	return;
}

int FindMatch ( char *char_co, char **list, char **value )
{
	char name[256], tmp_str[256], tmp_str0[256], tmp_str1[256];
	int surematch, typeOK, compOK;

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
	*value = strstr( char_co, name );

	// Reformat list[I] as tmp_strI for full 
	// comparison
	sprintf( tmp_str0, "%-12.12s", list[0] );
	sprintf( tmp_str1, "%-12.12s", list[1] );
	surematch = 0;
	while (*value != NULL && !surematch) {
		// Get and test method type from carry over
		sprintf( tmp_str, "%-12.12s", &((*value)[24+4]) );
		typeOK = !strncasecmp(tmp_str0, tmp_str, 12);

		// Get and test owning component from carry over
		sprintf( tmp_str, "%-12.12s", &((*value)[24+4+12]) );
		compOK = !strncasecmp(tmp_str1, tmp_str, 12);

		// Compare method type and owning component
		if( typeOK && compOK ) {
			surematch = 1;
		}
		else {
			// increment the string to the next 
			// character to try to find the next 
			// match
			*value++;
			*value = strstr( *value, name );
		}
	}
	if( *value == NULL || !surematch) {
		// We did not find it
		return STATUS_FAILURE;
	}

	return STATUS_SUCCESS;
/*  ==============  Statements containing RCS keywords:  */
/*  ===================================================  */


/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_system/RCS/execute58.cxx,v $";
 static char rcs_id2[] = "$Id: execute58.cxx,v 1.18 2006/10/26 15:37:31 hsu Exp $";}
/*  ===================================================  */

}

