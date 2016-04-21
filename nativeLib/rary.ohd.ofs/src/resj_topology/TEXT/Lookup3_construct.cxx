//------------------------------------------------------------------------------
// Lookup3::construct - Reads a stringlist and constructs the Lookup3 method.
//------------------------------------------------------------------------------
// History:
// 
// 06 Feb 2006  James R. VanShaar, RTi    Created initial version
//------------------------------------------------------------------------------
#include "ExprParser.h"
#include "Lookup3.h"
#include "TSUtil.h"
#include <stdio.h>
#include "ResJSys.h"   // Required to use ResJSys::getIPR to call ResJ_ccwrite()

//------------------------------------------------------------------------------
// Lookup :: construct - Parameterizes the method.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     integer specifying success of construct method.
// Calls:
//     Method::getUnitType()
//     PrintError()
//     Component::getID()
//     BreakStringList()
//     FreeStringList()
//     key_TSINPUT()
//     key_TSINDEX()
//     key_COLUMNVAR()
//     key_ROWVAR()
//     key_TABLEVAR()
//     key_WEEKLYVARIATION()
//     key_BLENDTBL_BLENDTS()
//     key_INTERPOLATE()
//     getType()
//     key_INITIALTRANSFER()
//     key_VALUES()
//     key_TOCOMP()
//     PrintWarning()
//     setCOstring();
// Errors:
//     Attempt to parameterize on a Reach component.
//     Inability to parse an input line.
//     Parameterization with INITIALTRANSFER keyword before successful usage of
//         TOCOMP keyword.
//     Lack of VALUES keyword and associated table values.
//     Attempt to index the table columns with a time series not parameterized
//         in the method.
//     Attempt to index the table rows with a time series not parameterized in
//         the method.
//     Negative values in a RELEASE-type table.
//     No INITIALTRANSFER parameterization with TOCOMP NEXTSTEP functionality.
//     Attempt at TOCOMP functionality with RELEASE- or AUGMENTATION-type
//         table.
// Warnings:
//     Inclusion of an unrecognized keyword.
//     Parameterization of negative values in a table associated with TOCOMP
//         functionality.
// Debug:
//     None
//------------------------------------------------------------------------------
/**
Parameterizes the method
@return int value signifying success
@param re_list character string list containing the user-input method
 parameters.
@param n_items integer number of strings in re_list (rows in input).
*/
int Lookup3::construct ( char** re_list, int n_items )  
{
    char routine[]="Lookup3::construct", **list=NULL;
    int i, nlist=0, totErrs = 0, WarnedOnce = 0, unknownKey = 0, toCompBad=1,
        valTable=0;

    if( Method::getUnitType() == ENGLISH )
    {
        _tableConv = 0.028317;
    }

    // Check to ensure owning component is not REACH.
    if( _ownerType == CM_REACH )
    {
            PrintError( routine, "LOOKUP3 %s cannot function on REACH "
                "component '%s'.", _id, _owner->getID() );
            return( STATUS_FAILURE );
    }

    // If execution reaches here, the LOOKUP3 and matching ENDLOOKUP3 keywords
    // have been found (and removed).
    for( i = 0; i < n_items; i++ )
    {
        // Check for empty or commented lines.
        if( strlen( re_list[i] ) == 0 || re_list[i][0] == '#' )
        {
            continue;
        }

        // Parse the line.
        list = BreakStringList( re_list[i], " \n\t", 
            DELIM_SKIP_BLANKS, &nlist );
        if( nlist == 0 || list == NULL )
        {
            totErrs++;
            PrintError( routine, "Troubles getting data for Lookup3 %s %s.", 
                _owner->getID(), _id );
            if( list )
            {
                list = FreeStringList( list );
            }
            continue;
        }

        // Begin to check the first "word" for keyword significance.

        // Check for input time series.
        // Keywords begin with TS and include TSINPUT and TSINDEX
        if( !strncasecmp( list[0], "TS", 2 ) )
        {
            // Check for input time series -- observed withdrawal, diversion,
            // augmentation and / or release time series are acceptable.
            if( !strcasecmp( list[0], "TSINPUT" ) )
            {
                totErrs += key_TSINPUT(list, nlist, re_list[i]);
            }
            // Check for, and handle the TSINDEX keyword -- ROWS and COLUMNS
            // sub-keywords are acceptable.
            else if( !strcasecmp( list[0], "TSINDEX" ) )
            {
                totErrs += key_TSINDEX(list, nlist, re_list[i]);
            }
            // If execution reaches here the key is unrecognized.
            else
            {
                unknownKey++;
            }
        }

        // Check for keywords containing "VAR".  These include COLUMNVAR, 
        // ROWVAR, TABLEVAR and WEEKLYVARIATION.
        else if( strstr( list[0], "VAR" ) )
        {
            // Check for column variable definition
            if( !strcasecmp( list[0], "COLUMNVAR" ) )
            {
                totErrs += key_COLUMNVAR(list, nlist, re_list[i]);
            }
            // Check for row variable definition
            else if( !strcasecmp( list[0], "ROWVAR" ) )
            {
                totErrs += key_ROWVAR(list, nlist, re_list[i]);
            }
            // Check for table variable definition
            else if( !strcasecmp( list[0], "TABLEVAR" ) )
            {
                totErrs += key_TABLEVAR(list, nlist, re_list[i]);
            }
            // Handle weekly variation.
            else if( !strcasecmp( list[0], "WEEKLYVARIATION" ) )
            {
                totErrs += key_WEEKLYVARIATION(list, nlist);
            }
            // If execution reaches here the key is unrecognized.
            else
            {
                unknownKey++;
            }
        }

        // Check for blending keywords, including BLENDTBL, BLENDTS
        else if( !strncasecmp( list[0], "BLEND", 5 ) )
        {
            // Check for table blending
            if( !strcasecmp( list[0], "BLENDTBL" ) )
            {
                totErrs += key_BLENDTBL_BLENDTS(list, nlist, 0);
            }
            // Check for time series to table blending
            else if( !strcasecmp( list[0], "BLENDTS" ) )
            {
                totErrs += key_BLENDTBL_BLENDTS(list, nlist, 1);
            }
            // If execution reaches here the key is unrecognized.
            else
            {
                unknownKey++;
            }
        }

        // Check for keywords beginning with "IN", including INTERPOLATE and 
        // INITIALTRANSFER
        else if( !strncasecmp( list[0], "IN", 2 ) )
        {
            // Check for interpolation
            if( !strcasecmp( list[0], "INTERPOLATE" ) )
            {
                totErrs += key_INTERPOLATE(list, nlist);
            }
            // Check for the intialtransfer keyword for ToComp functionality
            else if( !strcasecmp( list[0], "INITIALTRANSFER" ) )
            {
                // TOCOMP parameterization is required prior to INITIALTRANSFER
                if( toCompBad )
                {
                    totErrs++;
                    PrintError( routine, "TOCOMP must be successfully "
                        "processed before INITIALTRANSFER.  (%s %s %s)",
                        getType(), _owner->_id, _id );
                }
                else
                {
                    totErrs += key_INITIALTRANSFER(list, nlist );
                }
            }
            // If execution reaches here the key is unrecognized.
            else
            {
                unknownKey++;
            }
        }

        // Check for the beginning of the Lookup table, keyword VALUES.
        else if( !strcasecmp( list[0], "VALUES" ) )
        {
            totErrs += key_VALUES( list, n_items, re_list, &i );
            // Set a trigger defining that the with VALUES section has been
            // processed.
            valTable = 1;
        }
    
        // Check for transfer of withdrawal / diversion values
        else if( !strcasecmp( list[0], "TOCOMP" ) )
        {
            toCompBad = key_TOCOMP(list, nlist);
            totErrs += toCompBad;
        }

        // If execution enters the following "else", a known keyword was not
        // found.
        else
        {
            unknownKey++;
        }

        // If this is an unknown keyword and there was no previous unknown
        // keyword, warn about it now.
        if ( unknownKey && !WarnedOnce )
        {
                WarnedOnce++;
                PrintWarning( 1, routine, "'%s' is an unrecognized (or "
                    "misplaced) keyword.  If there are other errors or "
                    "warnings for Lookup3 %s, this may disappear following "
                    "their correction.", list[0], _id );
                unknownKey=0;
        }
        
        // Freeing memory 
        list = FreeStringList( list );
    }

    // Done reading and working with the parameters.
    // Now check for successful method parameterization.

    // No need to check TSInput parameters further--they are optional.

    // Check for values table
    if ( !valTable )
    {
	totErrs++;
        PrintError( routine, "Keyword VALUES not found. (%s %s %s)", getType(),
             _owner->_id, _id );
    }

    // Check that if a time series will be used as a table index that indeed 
    // the associated time series was parameterized and found.
    if( _colVar == INDEX_TS && _colIndex == NULL )
    {
        totErrs++;
        PrintError( routine, "Columns indexed using a non-parameterized "
	    "time series.  Please review \"INPUT_TS\" and \"COLUMNVAR\" "
	    "keywords. (%s %s %s)", getType(), _owner->_id, _id );
    }
    if( _rowVar == INDEX_TS && _rowIndex == NULL )
    {
        totErrs++;
        PrintError( routine, "Rows indexed using a non-parameterized "
	    "time series.  Please review \"INPUT_TS\" and \"ROWVAR\" "
	    "keywords. (%s %s %s)", getType(), _owner->_id, _id );
    }

    // Check for negative values in a RELEASE-type table
    if( _tableVar == TVAR_RELEASE && _fromComp )
    {
        totErrs++;
        PrintError( routine, "Negative values not allowed in table when "
            "TABLEVAR is RELEASE. (%s %s %s)", getType(), _owner->_id, _id );
    }

    // Check for special concerns about TOCOMP
    if( _receivingComp )
    {
        // The following tests for ToComp activity and a NextStep transfer lag
        if( _toCompMode && _myValue == MISSING )
        {
            totErrs++;
            PrintError( routine, "Lookup3 keyword 'INITIALTRANSFER' not found, "
                "but required with ToComp NextStep parameterization for %s "
                "on %s.", _id, _owner->_id );
        }
        // The following addresses the chance of TOCOMP resulting in FROMCOMP
        // functionality--that is transfer from elsewhere into the owner.
        if( _tableVar == TVAR_AUGMENTATION || _tableVar == TVAR_RELEASE )
        {
            // TOCOMP is not allowed with AUGMENTATION or RELEASE
            totErrs++;
            PrintError( routine, "TABLEVAR AUGMENTATION and RELEASE not "
                "compatibile with TOCOMP functionality.  Please consider using "
                "WITHDRAWAL or DIVERSION instead for %s %s %s.", getType(), 
                _owner->getID(), _id );
        }
        // Negative table values in a DIVERSION or WITHDRAWAL table will be
        // warned against.
        else if( _fromComp )
        {
            PrintWarning( 1, routine, "Negative table values suggest water "
                "being transferred from an external component onto the owner "
                "of the %s method.  Negative values will be used in mass "
                "balance, but will be will be replaced with 0 during transfer "
                "ToComp. This is allowed but discouraged. (%s %s %s)",
                getType(), _owner->getID(), _id, getType() );
        }

        // Check for negative value--TOCOMP reversed to FROMCOMP.
        if( _myValue < 0 )
        {
            // If TOCOMP mode is NEXTSTEP, then we should deal with additional
            // details.
            if( _toCompMode )
            {
                // FROMCOMP (negative TOCOMP INTIALTRANSFER) value not allowed.
                PrintWarning( 1, routine, "INITIALTRANSFERs value is negative "
                    "(%f).  Replacing with 0 in %s %s %s.", _myValue, getType(),
                    _owner->getID(), _id );
                // Put it in the transfer time series at the first simulation
                // time step. _toCompMode value is non-zero, so it was a
                // NEXTSTEP thing. Apply it to the time series.
                TSDate date;
                date = Method::getForecastDate1();
                _Comp_ts.setDataValue( date, _myValue );
            }
            _myValue = 0;
        }
    }

    if ( totErrs > 0 )
    {
        return( STATUS_FAILURE );
    }

// I don't know what this is. It appears to be some inefficient code that will
// force blending algorithms, even for a 1 time step blend (no real blend)!
    if( _n_blend_tbl < 0 )
    {
        _n_blend_tbl = 0;
    }
    if( _n_blend_ts < 0 )
    {
        _n_blend_ts = 0;
    }

    // Write the carryover to the CO array at the RES-J System level...
    setCOstring();

    _is_constructed = 1;
    return( STATUS_SUCCESS );
}

//------------------------------------------------------------------------------
// Lookup :: buildTable - Builds the Lookup3 values table
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     integer specifying success of table build.
// Calls:
//     BreakStringList()
//     PrintError()
//     FreeStringList()
//     TSUtil::getTSDateFromString()
//     TSDate::getMonth()
//     TSDate::getDay()
//     TSDate::setYear()
//     getType()
//     Component::getID()
//     TSDate::toNoYearJulianDouble()
//     HourTS::getDataUnits()
//     GetConversion()
//     HourTS::getIdentifier()
//     IsDouble()
//     atof()
//     TSDate::toNoYearJulianDouble()
//     ReferencedMatrix::populate()
// Errors:
//     Inability to parse an input line.
//     Usage of Februrary 29 in date indexes
//     Invalid values of mm/dd (negative months, days or mm > 12, dd > 31)
//     Inability to convert units specified for an input index time series.
//     Attempt to use non-date, non-numerical values as indexes.
//     Incompatible number of column indexes and values in a row of the table.
//     Non-ascending column indexes (numerically or chronologically).
//     Non-ascending row indexes (numerically or chronologically).
//     Problems creating the table Matrix.
// Warnings:
//     None
// Debug:
//     None
//------------------------------------------------------------------------------
/**
Parameterizes the method
@return int value signifying success
@param strTbl character string list containing the user-input values table
 parameters.
@param nlines integer number of lines in the user-input values table (strTbl
 variable).
*/
int Lookup3::buildTable( char** strTbl, int nlines )
{
    // This function will create a Table.  This function does not care what the
    // values represent nor what the indexes are, EXCEPT for the cases when an
    // index is a DATE.

    // Input to this method is the entire table:
    //   several rows of data, the first being column indexes and the first
    //   value of each of the successive rows being the row index for that row.

    // Actual table creation occurs near the very end with Matrix::populate().
    // Before reaching there, however, parse out an array of column indexes, row
    // indexes and table values.  Also convert these values, if necessary, to
    // the internal units.

    char routine[]="Lookup3::buildTable", **list=NULL;

    int n_col, nlist = 0, totErrs = 0;

    // START: Handle the columns------------------------------{
    //   Parse them and count them
    list = BreakStringList( strTbl[0], " \n\t", DELIM_SKIP_BLANKS, &nlist );
    if( nlist == 0 || list == NULL )
    {
        PrintError( routine, "Troubles getting data for %s '%s'.", getType(),
            _id );
        if( list )
        {
            list = FreeStringList( list );
        }
        return( STATUS_FAILURE );
    }
    n_col = nlist;

    // Size the column array
    double *colI = new double[n_col];

    // Convert the indexes to an array of doubles, converting units as
    // necessary.
    int i, j;
    if( _colVar == INDEX_DATE )
    {
        // Read in the dates and convert them into julian days (fractions of
        // julian days where 01/01 any-year = 0.0 and 12/31 is 365.0).
        // NOTE: leap year is not considered here.

        TSDate date;
        int month, day;
        for( i = 0; i < n_col; i++ )
        {
            date = TSUtil::getTSDateFromString( list[i] );
            month = date.getMonth();
            day = date.getDay();
            date.setYear(1);
    
            if( month == 2 && day == 29 )
            {
                totErrs++;
                PrintError( routine, "Index date \"%s\" invalid--No leap years. "
                    "(%s %s %s).", list[i], getType(), _owner->getID(), _id );
            }
            else if( month > 12 || month < 1 || day > 31 || day < 1 )
            {
                totErrs++;
                PrintError( routine, "Column index date \"%s\" invalid "
                    "(%s %s %s).", getType(), getType(), _owner->getID(), _id );
                colI[i] = -999.0;
            }
            else
            {
                colI[i] = date.toNoYearJulianDouble();
            }

        }

        if( totErrs )
        {
            PrintError( routine, "Trouble with column index dates. (%s %s %s).",
                getType(), _owner->getID(), _id );
            if( list )
            {
                list = FreeStringList( list );
            }
            delete [] colI;
            return( STATUS_FAILURE );
        }
    
	// Wrap around requirements will be handled later.
    }
    else
    {
        // Values are either from a time series or a component state.
        // If the are from a time series, a conversion factor may still remain
        // to be defined.
        if( _colVar == INDEX_TS && strcmp( _colOrigUnits, "\0" ) )
        {
            // Get the units on the time series
            // (NWSRFS standard units associated with the time series type).
            char* tsUnits = _colIndex->getDataUnits();
            float zero = 0.0;
            if( _colOrigUnits[0] != '*' )
            {
                if( GetConversion ( _colOrigUnits, tsUnits, &_colConv, &zero,
                    &zero, "" ) )
                {
                    // Unable to convert the units
                    totErrs++;
                    TSIdent id = _rowIndex->getIdentifier();
                    PrintError( routine, "Unable to convert units %s to %s "
                        "for %s-type time series (%s %s %s).", _colOrigUnits, 
                        tsUnits, id.getType(), getType(), _owner->getID(),
                        _id );
                    _colConv = 1.0;
                }
            }
        }

        for( i = 0; i < n_col; i++ )
        {
            if( !IsDouble( list[0] ) )
            {
                totErrs++;
                PrintError( routine, "Non-date column indexes must be "
                    "numerical values for %s %s %s.", getType(),
                    _owner->getID(), _id );
            }
            else
            {
                colI[i] = atof( list[i] ) * _colConv;
            }
        }
        if( totErrs )
        {
            if( list )
            {
                list = FreeStringList( list );
            }
            delete [] colI;
            return( STATUS_FAILURE );
        }
    }
    // Free the list variable as it is no longer needed.
    list = FreeStringList( list );
    // END: Handle the columns------------------------------}
    
    // START: Handle the rows and values--------------------{

    // Based on the number of columns and nlines - 1, size an array of doubles
    // to contain the table values.
    int n_row = nlines - 1;
    double *rowI = new double[n_row];
    double *data = new double[n_col * n_row];


    // If the are from a time series, a conversion factor may still remain
    // to be defined.
    if( _rowVar == INDEX_TS && strcmp( _rowOrigUnits, "\0" ) )
    {
        // Get the units on the time series
        // (NWSRFS standard units associated with the time series type).
        char* tsUnits = _rowIndex->getDataUnits();
        float zero = 0.0;
        if( _colOrigUnits[0] != '*' )
        {
            if( GetConversion ( _rowOrigUnits, tsUnits, &_rowConv, &zero, &zero,
                "" ) )
            {
                // Unable to convert the units
                totErrs++;
                TSIdent id = _rowIndex->getIdentifier();
                PrintError( routine, "Unable to convert units %s to %s for "
                    "%s-type time series (%s %s %s).", _rowOrigUnits, tsUnits, 
                    id.getType(), getType(), _owner->getID(), _id );
                _rowConv = 1.0;
            }
        }
    }

    // Now work with the rows in the table.  The column indexes were done
    // previously, at row = 0.
    int dataI, posVal = 0, negVal = 0, month, day;
    TSDate date;
    for( i=1; i<nlines; i++ )
    {
        //   Parse them and count them
        list = BreakStringList( strTbl[i], " \n\t", DELIM_SKIP_BLANKS, &nlist );
        if( nlist != n_col + 1 )
        {
            PrintError( routine, "Number of columns (%d) and number of data "
                "fields on row %d (%d) do not agree. (%s %s %s)", n_col, i,
                nlist - 1, getType(), _owner->getID(),_id );
            if( list )
            {
                list = FreeStringList( list );
            }
            delete [] colI;
            delete [] rowI;
            delete [] data;
            return( STATUS_FAILURE );
        }
    
        // Convert the row index to a double and store in the array, converting
        // units as necessary.
        if( _rowVar == INDEX_DATE )
        {
            // Read in the dates and convert them into julian days (fractions of
            // julian days where 01/01 any-year = 0.0 and 12/31 is 365.0).
            // NOTE: leap year is not considered here.

            date = TSUtil::getTSDateFromString( list[0] );
            month = date.getMonth();
            day = date.getDay();
            date.setYear(1);

            if( month == 2 && day == 29 )
            {
                totErrs++;
                PrintError( routine, "Index date \"%s\" invalid--No leap years. "
                    "(%s %s %s).", list[0], getType(), _owner->getID(), _id );
            }
            else if( month > 12 || month < 1 || day > 31 || day < 1 )
            {
                totErrs++;
                PrintError( routine, "Row index date \"%s\" invalid "
                    "(%s %s %s).", getType(), getType(), _owner->getID(), _id );
                rowI[i-1] = -999.0;
            }
            else
            {
                rowI[i-1] = date.toNoYearJulianDouble();
            }
        }
        else
        {
            // Values are from either a time series or a component state.
            if( !IsDouble( list[0] ) )
            {
                totErrs++;
                PrintError( routine, "Non-date row indexes must be "
                    "numerical values for %s %s %s.", getType(),
                    _owner->getID(), _id );
            }
            else
            {
                rowI[i-1] = atof( list[0] ) * _rowConv;
            }
        }

        // Handle the values.  Use j=1 to reference the second value on the
        // line (the first is the row index).
        for( j=1; j<nlist; j++ )
        {
            // Since the array starts with index = 0, it is necessary to
            // calculate the index with appropriate offsets.

            // Multiply the array row number minus 1 by the number of columns in
	    // the column index array, then add the array column number minus 1.
	    // i is going from 1 to n_row
	    // j is going from 1 to nlist (nlist = n_col)
	    // The first value is on i=1 and j=1 and should be stored at 0.
            dataI= (i - 1) * n_col + (j - 1);

            // Values are from either a time series or a component state.
            if( !IsDouble( list[j] ) )
            {
                totErrs++;
                PrintError( routine, "Table values must be "
                    "numerical values for %s %s %s.", getType(),
                    _owner->getID(), _id );
            }
            else
            {
                data[dataI] = atof( list[j] ) * _tableConv;
            }

            // While here, track the existence of negative values that are
            // incompatible with TOCOMP functionality.
            if( data[dataI] < 0 )
            {
                _fromComp++;
            }
        }
        // Free the list variable
        list = FreeStringList( list );
    }

    // END: Handle the rows and values--------------------}


    // START: Handle Wrap-around Issues --------------------{

    // Now, if indexing by date, deal with wrap around issues.
    if( _colVar == INDEX_DATE || _rowVar == INDEX_DATE )
    {
        // ---------- Start Column Indexes ----------{
	// The following work deals with a lot of arrays that may be confusing.
	// Here are some variables:
	//    colI----------- The original column index array.
	//    colIX---------- The extended column index array.
	//    n_col---------- Number of columns in the original data table
	//    n_colX--------- Number of columns in the extended data table
	//    QC------------- Trigger whether columns are being extended
	//                    0 = No, 1 = Yes.

        int n_colX = n_col;
        int QC = 0;
        if( _colVar == INDEX_DATE )
        {
            // Assign a trigger for this condition.
            QC = 1;

            // A value equal to the last user-parameterized value is needed
            // in the year before the first user-parameterized value and a value
            // equal to the first user user-parameterized value is needed in the
            // year after the last user-parameterized value.
            n_colX += 2;

            // Size a new, extended array that will be filled.
            double *colIX = new double[n_colX];

            // Deal with extensions.
            // The first wrap around date equals last input julian date - 365.
            colIX[0] = colI[n_col-1] - 365.0;
            // The last wrap around date equals 365 + first input julian date.
            colIX[n_colX-1] = 365.0 + colI[0];
    
            // Now copy the remaining values.
            for( i = 0; i < n_col; i++ )
            {
                colIX[i+QC] = colI[i];
            }
    
            // Delete the old array and point it to the new array.
            delete [] colI;
            colI = colIX;
        }
        // Otherwise, no need to worry about the column indexing array.

        // ---------- End Column Indexes ----------}


        // ---------- Start Row Indexes ----------{
	// Here are some variables:
	//    rowI----------- The original row index array.
	//    rowIX---------- The extended row index array.
	//    n_row---------- Number of rows in the original data table
	//    n_rowX--------- Number of rows in the extended data table
	//    QR------------- Trigger whether rows are being extended
	//                    0 = No, 1 = Yes.

        int n_rowX = n_row;
        int QR = 0;
        if( _rowVar == INDEX_DATE )
        {
            // Assign a trigger for this condition.
            QR = 1;

            // A value equal to the last user-parameterized value is needed
            // in the year before the first user-parameterized value and a value
            // equal to the first user user-parameterized value is needed in the
            // year after the last user-parameterized value.
            n_rowX += 2;

            // Size a new, extended array that will be filled.
            double *rowIX = new double[n_rowX];
    
            // Deal with extensions.
            // The first wrap around date equals last input julian date - 365.
            rowIX[0] = rowI[n_row-1] - 365.0;
            // The last wrap around date equals 365 + first input julian date.
            rowIX[n_rowX-1] = 365.0 + rowI[0];
    
            // Now copy the remaining values.
            for( i = 0; i < n_row; i++ )
            {
                rowIX[i+QR] = rowI[i];
            }
    
            // Delete the old array and point it to the new array.
            delete [] rowI;
            rowI = rowIX;
        }
        // Otherwise, no need to worry about the row indexing array.

        // ---------- End Row Indexes ----------}

        // ---------- Start Handle the Data ----------{
	// This next mess of array manipulation uses the variables shown below.
	// Additonally, at the end of Lookup3::buildTable there are several
	// examples showing the array logic contained in this section.
	// Hopefully they will be of assistence, should you need to understand
	// this manpulation.
	//
	//    n_col---------- Number of columns in the original data table
	//    n_colX--------- Number of columns in the extended data table
	//    QC------------- Trigger whether columns are being extended
	//                    0 = No, 1 = Yes.
	//    n_row---------- Number of rows in the original data table
	//    n_rowX--------- Number of rows in the extended data table
	//    QR------------- Trigger whether rows are being extended
	//                    0 = No, 1 = Yes.
	//    data----------- Original data array
	//    s-------------- Pointer to the Original array, used for syntax
	//                    brevity (from the explanation).
	//    dataX---------- Extended data array
	//    u-------------- Pointer to the Extended data array, used for
	//                    syntax brevity (from the explanation).
        //    a, b, cn, co--- Counters or constants used for indexing.
	
	// Size the extended array.
        double *dataX = new double[n_colX * n_rowX];

	// Initialize several variables that have not yet been initialized.
        int x,y,a,b,cn,co;
        double* u = dataX;
        double* s = data;
        a=n_col-1;
        b=n_colX-1;

        // First Cut: If necessary, fill the extended columns (first and last)
	//   of the extended table array:
	//   * Only the data in the rows corresponding to the original data rows
	//     are filled.
        //   * The first column of the extended array gets data from the last
	//     column of the original data.
        //   * The last column of the extended array gets data from the first
	//     column of the original data.
	if( QC )
	{
            for( x=QR, y=0; y<n_row; x++, y++ )
            {
                //u[x*n_col] = s[y*n_col+n_col-1];
                //u[x*n_col+n_col-1] = s[y*n_col];
                u[x*n_colX] = s[y*n_col+a];
                u[x*n_colX+b] = s[y*n_col];
            }
    	}
        
        // Second Cut
        for( x=QR,y=0; y<n_row; x++, y++ )
        {
            for( cn=QC, co=0; co<n_col; cn++,co++ )
            {
                u[x*n_colX+cn] = s[y*n_col+co];
            }
        }
        
        // Third Cut
        if( QR )
        {
            x=(n_rowX-2)*n_colX;
            y=(n_rowX-1)*n_colX;
            for( cn=0; cn<n_colX; cn++ )
            {
                //u[cn] = u[(n_row-2)*n_col+cn];
                //u[(n_row-1)*n_col+cn] = u[n_col+cn];
                u[cn] = u[x+cn];
                u[y+cn] = u[n_colX+cn];
            }
        }

        // Having replaced the data and the indexing arrays with extensions,
        // these extended arrays are now THE arrays.
        // Delete the old array and point it to the new array.
        delete [] data;
        data = dataX;
        // Reassign the size counters.
        n_col = n_colX;
        n_row = n_rowX;

        // ---------- END: Handle the Data ----------}

    }
    // END: Handle Wrap-around Issues --------------------}

    // Check for errors.  These were sufficiently minor that execution could
    // continue to check input.
    if( totErrs )
    {
        delete [] colI;
        delete [] rowI;
        delete [] data;
        return( STATUS_FAILURE );
    }
    else
    {
        // The table values appear to be in good shape and the indexing arrays
        // have been built.
        // Check that the index values are increasing.
        for( i = 1; i< n_col; i++ )
        {
            if( colI[i] <= colI[i-1] )
            {
                totErrs++;
                PrintError( routine, "Column indexes must be in ascending (or "
                    "chronological) order. (%s %s %s)", getType(), 
                    _owner->getID(), _id );
            }
        }
        for( i = 1; i< n_row; i++ )
        {
            if( rowI[i] <= rowI[i-1] )
            {
                totErrs++;
                PrintError( routine, "Row indexes must be in ascending (or "
                    "chronological) order. (%s %s %s)", getType(), 
                    _owner->getID(), _id );
            }
        }
        if( totErrs )
        {
            delete [] colI;
            delete [] rowI;
            delete [] data;
            return( STATUS_FAILURE );
        }
    }

    // 3 double arrays now exist: column Indexes, row Indexes and all the
    // table values.  Proceed to build the ReferencedMatrix.

    if( _table.populate( rowI, n_row, colI, n_col, data, n_col * n_row ) )
    {
        // Trouble!!
        PrintError( routine, "Problems creating table.  Review nearby "
            "warnings. chronological) order. (%s %s %s)", getType(), 
            _owner->getID(), _id );
        delete [] colI;
        delete [] rowI;
        delete [] data;
        return( STATUS_FAILURE );
    }

    // Clean-up
    delete [] rowI;
    delete [] colI;
    delete [] data;

    return( STATUS_SUCCESS );
}
/*
////////////////////////////////////////////////////////////////////////////////
Example 1: No Column Extension, Row Extension.  Original 3,5; Extended 5,5
////////////////////////////////////////////////////////////////////////////////

Non-Extended Arrays Indexing (s);
     n_row=3; n_col=5;
    -------------------
    NCO NC1 NC2 NC3 NC4
---                  
NR0  S0  S1  S2  S3  S4   10  20  30  40  50
NR1  S5  S6  S7  S8  S9   60  70  80  90 100
NR2 S10 S11 S12 S13 S14  110 120 130 140 150
---                  

First Cut Extended Arrays Indexing (u)
     n_rowX=5; n_colX=5;
    -------------------
    XC0 XC1 XC2 XC3 XC4
XR0  X0  X1  X2  X3  X4
XR1  X5  X6  X7  X8  X9
XR2 X10 X11 X12 X13 X14
XR3 X15 X16 X17 X18 X19
XR4 X20 X21 X22 X23 X24

First Cut: If necessary, fill the extended columns (first and last)
of the extended table array:
* Only the data in the rows corresponding to the original data rows
    are filled.
* The first column of the extended array gets data from the last
    column of the original data.
* The last column of the extended array gets data from the first
    column of the original data.

>>>>> In this case, columns were not extended, so the First Cut can be skipped.

Second Cut Extended Arrays Indexing (u)
     n_rowX=5; n_colX=5;
    -------------------
    XC0 XC1 XC2 XC3 XC4
XR0  X0  X1  X2  X3  X4
XR1  X5  X6  X7  X8  X9   10  20  30  40  50
XR2 X10 X11 X12 X13 X14   60  70  80  90 100
XR3 X15 X16 X17 X18 X19  110 120 130 140 150
XR4 X20 X21 X22 X23 X24

Second Cut: Copy original column values into the new table array on the
            non-extended rows:
Let QR=0 if rows were not extended, QR=1 if they were.  Therefore QR=1 here.
Let QC=0 if columns were not extended, QC=1 if they were.  Therefore QC=0 here.
Work through the First Cut Extended Indexing:
     x=QR,y=0; y<n_row; x++,y++
     x=1 ,y=0; y<3     ; x++,y++
     x=1 ,y=0; y<3     ; x++,y++
Within the loop, work through the columns
     cn=QC,co=0; co<n_col; cn++,co++
     cn=0 ,co=0; co<5    ; cn++,co++
when x=1 ,y=0:
     cn=0, co=0
u[X5 ] = u[x*n_colX+cn]         = s[S0 ] = s[y*n_col+co]
         u[1*5      +0 ] = u[5 ]          = s[0*5     +0 ] = s[0 ] =  10
     cn=1, co=1
u[X6 ] = u[x*n_colX+cn]         = s[S1 ] = s[y*n_col+co]
         u[1*5      +1 ] = u[6 ]          = s[0*5     +1 ] = s[1 ] =  20
     cn=2, co=2
u[X7 ] = u[x*n_colX+cn]         = s[S2 ] = s[y*n_col+co]
         u[1*5      +2 ] = u[7 ]          = s[0*5     +2 ] = s[2 ] =  30
     cn=3, co=3
u[X8 ] = u[x*n_colX+cn]         = s[S3 ] = s[y*n_col+co]
         u[1*5      +3 ] = u[8 ]          = s[0*5     +3 ] = s[3 ] =  40
     cn=4, co=4
u[X9 ] = u[x*n_colX+cn]         = s[S4 ] = s[y*n_col+co]
         u[1*5      +4 ] = u[9 ]          = s[0*5     +4 ] = s[4 ] =  50
when x=2 ,y=1:
     cn=0, co=0
u[X10] = u[x*n_colX+cn]         = s[S5 ] = s[y*n_col+co]
         u[2*5      +0 ] = u[10]          = s[1*5     +0 ] = s[5 ] =  60
     cn=1, co=1
u[X11] = u[x*n_colX+cn]         = s[S6 ] = s[y*n_col+co]
         u[2*5      +1 ] = u[11]          = s[1*5     +1 ] = s[6 ] =  70
     cn=2, co=2
u[X12] = u[x*n_colX+cn]         = s[S7 ] = s[y*n_col+co]
         u[2*5      +2 ] = u[12]          = s[1*5     +2 ] = s[7 ] =  80
     cn=3, co=3
u[X13] = u[x*n_colX+cn]         = s[S8 ] = s[y*n_col+co]
         u[2*5      +3 ] = u[13]          = s[1*5     +3 ] = s[8 ] =  90
     cn=4, co=4
u[X14] = u[x*n_colX+cn]         = s[S9 ] = s[y*n_col+co]
         u[2*5      +4 ] = u[14]          = s[1*5     +4 ] = s[9 ] = 100
when x=3 ,y=2:
     cn=0, co=0
u[X15] = u[x*n_colX+cn]         = s[S10] = s[y*n_col+co]
         u[3*5      +0 ] = u[15]          = s[2*5     +0 ] = s[10] = 110
     cn=0, co=0
u[X16] = u[x*n_colX+cn]         = s[S11] = s[y*n_col+co]
         u[3*5      +1 ] = u[16]          = s[2*5     +1 ] = s[11] = 120
     cn=2, co=1
u[X17] = u[x*n_colX+cn]         = s[S12] = s[y*n_col+co]
         u[3*5      +2 ] = u[17]          = s[2*5     +2 ] = s[12] = 130
     cn=3, co=2
u[X18] = u[x*n_colX+cn]         = s[S13] = s[y*n_col+co]
         u[3*5      +3 ] = u[18]          = s[2*5     +3 ] = s[13] = 140
     cn=4, co=4
u[X19] = u[x*n_colX+cn]         = s[S14] = s[y*n_col+co]
         u[3*5      +4 ] = u[19]          = s[2*5     +4 ] = s[14] = 150

// LOGIC: Second Cut
for( x=QR,y=0; y<n_row+QR; x++, y++ )
{
    for( cn=QC, co=0; co<n_col; cn++,co++ )
    {
        u[x*n_colX+cn] = s[y*n_col+co];
    }
}

Third Cut Extended Arrays Indexing (u)
     n_rowX=5; n_colX=5;
    -------------------
    XC0 XC1 XC2 XC3 XC4
XR0  X0  X1  X2  X3  X4  110 120 130 140 150
XR1  X5  X6  X7  X8  X9   10  20  30  40  50
XR2 X10 X11 X12 X13 X14   60  70  80  90 100
XR3 X15 X16 X17 X18 X19  110 120 130 140 150
XR4 X20 X21 X22 X23 X24   10  20  30  40  50

Third Cut: If necessary, extend rows by copying from existing rows in the
           extended table.
           The last row gets it from the second row.
           The first row gets it from second to last row.
Let QR=0 if rows were not extended, QR=1 if they were.  Therefore QR=1 here.
If QR != 1, then skip this!
Loop through the columns twice, each time with a different algorithm.
Work through the columns
     cn=0; cn<n_colX; cn++
     cn=0; cn<5     ; cn++
First time: (Note: in the logic, the first and second times can be combined in
            one loop).
when cn=0;
u[X0 ] = u[cn]         = u[X15] = u[(n_rowX-2)*n_colX+cn]
         u[0 ] = u[ 0]          = u[(5      -2)*5      +0 ] = u[15] = 110
when cn=1;
u[X1 ] = u[cn]         = u[X15] = u[(n_rowX-2)*n_colX+cn]
         u[1 ] = u[ 1]          = u[(5      -2)*5      +1 ] = u[16] = 120
when cn=2;
u[X2 ] = u[cn]         = u[X15] = u[(n_rowX-2)*n_colX+cn]
         u[2 ] = u[ 2]          = u[(5      -2)*5      +2 ] = u[17] = 130
when cn=3;
u[X3 ] = u[cn]         = u[X15] = u[(n_rowX-2)*n_colX+cn]
         u[3 ] = u[ 3]          = u[(5      -2)*5      +3 ] = u[18] = 140
when cn=4;
u[X4 ] = u[cn]         = u[X15] = u[(n_rowX-2)*n_colX+cn]
         u[4 ] = u[ 4]          = u[(5      -2)*5      +4 ] = u[19] = 150

Second time: (Note: in the logic, the first and second times can be combined in
            one loop).
Work through the columns
     cn=0; cn<n_colX; cn++
     cn=0; cn<5     ; cn++
when cn=0;
u[X20] = u[(n_rowX-1)*n_colX+cn]         = u[X15] = u[n_colX+cn]
         u[(5      -1)*5      +0 ] = u[20]          = u[5      +0 ] = u[ 5] =  10
when cn=1;
u[X21] = u[(n_rowX-1)*n_colX+cn]         = u[X15] = u[n_colX+cn]
         u[(5      -1)*5      +1 ] = u[21]          = u[5      +1 ] = u[ 6] =  20
when cn=2;
u[X22] = u[(n_rowX-1)*n_colX+cn]         = u[X15] = u[n_colX+cn]
         u[(5      -1)*5      +2 ] = u[22]          = u[5      +2 ] = u[ 7] =  30
when cn=3;
u[X23] = u[(n_rowX-1)*n_colX+cn]         = u[X15] = u[n_colX+cn]
         u[(5      -1)*5      +3 ] = u[23]          = u[5      +3 ] = u[ 8] =  40
when cn=4;
u[X24] = u[(n_rowX-1)*n_colX+cn]         = u[X15] = u[n_colX+cn]
         u[(5      -1)*5      +4 ] = u[24]          = u[5      +4 ] = u[ 9] =  50

// LOGIC: Third Cut
if( _rowVar==INDEX_DATE )
{
    x=(n_rowX-2)*n_colX;
    y=(n_rowX-1)*n_colX;
    for( cn=0; cn<n_colX; cn++ )
    {
        //u[cn] = u[(n_rowX-2)*n_colX+cn];
        //u[(n_rowX-1)*n_colX+cn] = u[n_colX+cn];
        u[cn] = u[x+cn];
        u[y+cn] = u[n_colX+cn];
    }
}


////////////////////////////////////////////////////////////////////////////////
Example 2: Column Extension, No Row Extension.  Original 3,3; Extended 3,5

Non-Extended Arrays Indexing (s);
    n_row=3; n_col=3;
    -------------------
        NCO NC1 NC2 NC3
NR0 ---  S0  S1  S2 ---  ---  20  30  40 ---
NR1 ---  S3  S4  S5 ---  ---  70  80  90 ---
NR2 ---  S6  S7  S8 ---  --- 120 130 140 ---

First Cut Extended Arrays Indexing (u)
    n_rowX=3; n_colX=5;
    -------------------
    XC0 XC1 XC2 XC3 XC4
XR0  X0  X1  X2  X3  X4   40              20
XR1  X5  X6  X7  X8  X9   90              70
XR2 X10 X11 X12 X13 X14  140             120

First Cut: If necessary, fill the extended columns (first and last)
of the extended table array:
* Only the data in the rows corresponding to the original data rows
    are filled.
* The first column of the extended array gets data from the last
    column of the original data.
* The last column of the extended array gets data from the first
    column of the original data.

Let QR=0 if rows were not extended, QR=1 if they were.  Therefore QR=0 here.
Work through the First Cut Extended Indexing:
     x=QR,y=0; y<n_row; x++,y++
     x=0 ,y=0; y<3    ; x++,y++
     x=0 ,y=0; y<3    ; x++,y++
when x=0 ,y=0:
u[X0 ] = u[x*n_colX]                   = s[S2 ] = s[y*n_col+n_col-1]
         u[0*5      ]           = u[0 ]          = s[0*3     +3     -1] = s[2 ] =  40
u[X4 ] = u[x*n_colX+n_colX-1]         = s[S0 ] = s[x*n_col]
         u[0*5      +5      -1] = u[4 ]          = s[0*3     ]          = s[0 ] =  20
when x=1 ,y=1:
u[X5 ] = u[x*n_colX]                   = s[S5 ] = s[y*n_col+n_col-1]
         u[1*5      ]           = u[5 ]          = s[1*3     +3     -1] = s[5 ] =  90
u[X9 ] = u[x*n_colX+n_colX-1]         = s[S3 ] = s[y*n_col]
         u[1*5      +5      -1] = u[9 ]          = s[1*3     ]          = s[3 ] =  70
when x=2 ,y=2:
u[X10] = u[x*n_colX]                   = s[S8 ] = s[y*n_col+n_col-1]
         u[2*5      ]           = u[10]          = s[2*3     +3     -1] = s[8 ] = 140
u[X14] = u[x*n_colX+n_colX-1]         = s[S6 ] = s[y*n_col]
         u[2*5      +5      -1] = u[14]          = s[2*3     ]          = s[6 ] = 120

LOGIC:
int QR=0;
if( _rowVar == INDEX_DATE )
{
    QR = 1;
}
int QC=0;
if( _colVar == INDEX_DATE )
{
    QC = 1;
}
int x,y,a,b;
double* u = dataX;
double* s = data;
a=n_col-1;
b=n_colX-1;
// First Cut
for( x=QR, y=0; y<n_row; x++, y++ )
{
    //u[x*n_colX] = s[y*n_col+n_col-1];
    //u[x*n_colX+n_colX-1] = s[y*n_col];
    u[x*n_colX] = s[y*n_col+a];
    u[x*n_colX+b] = s[y*n_col];
}

Second Cut Extended Arrays Indexing (u)
    n_rowX=3; n_colX=5;
    -------------------
    XC0 XC1 XC2 XC3 XC4
XR0  X0  X1  X2  X3  X4   40  20  30  40  20
XR1  X5  X6  X7  X8  X9   90  70  80  90  70
XR2 X10 X11 X12 X13 X14  140 120 130 140 120

Second Cut: Copy original column values into the new table array on the non-extended rows:
Let QR=0 if rows were not extended, QR=1 if they were.  Therefore QR=0 here.
Let QC=0 if columns were not extended, QC=1 if they were.  Therefore QC=0 here.
Work through the First Cut Extended Indexing:
     x=QR,y=0; y<n_row; x++,y++
     x=0 ,y=0; y<3     ; x++,y++
     x=0 ,y=0; y<3     ; x++,y++
Within the loop, work through the columns
     cn=QC,co=0; co<n_col; cn++,co++
     cn=1 ,co=0; co<5    ; cn++,co++
when x=0 ,y=0:
     cn=1, co=0
u[X1 ] = u[x*n_colX+cn]         = s[S0 ] = s[y*n_col+co]
         u[0*5      +1 ] = u[1 ]          = s[0*3     +0 ] = s[0 ] =  20
     cn=2, co=1
u[X2 ] = u[x*n_colX+cn]         = s[S1 ] = s[y*n_col+co]
         u[0*5      +2 ] = u[2 ]          = s[0*3     +1 ] = s[1 ] =  30
     cn=3, co=2
u[X3 ] = u[x*n_colX+cn]         = s[S2 ] = s[y*n_col+co]
         u[0*5      +3 ] = u[3 ]          = s[0*3     +2 ] = s[2 ] =  40
when x=1 ,y=1:
     cn=1, co=0
u[X6 ] = u[x*n_colX+cn]         = s[S3 ] = s[y*n_col+co]
         u[1*5      +1 ] = u[6 ]          = s[1*3     +0 ] = s[3 ] =  70
     cn=2, co=1
u[X7 ] = u[x*n_colX+cn]         = s[S4 ] = s[y*n_col+co]
         u[1*5      +2 ] = u[7 ]          = s[1*3     +1 ] = s[4 ] =  80
     cn=3, co=2
u[X8 ] = u[x*n_colX+cn]         = s[S5 ] = s[y*n_col+co]
         u[1*5      +3 ] = u[8 ]          = s[1*3     +2 ] = s[5 ] =  90
when x=2 ,y=2:
     cn=1, co=0
u[X11] = u[x*n_colX+cn]         = s[S6 ] = s[y*n_col+co]
         u[2*5      +1 ] = u[11]          = s[2*3     +0 ] = s[6 ] = 120
     cn=2, co=1
u[X12] = u[x*n_colX+cn]         = s[S7 ] = s[y*n_col+co]
         u[2*5      +2 ] = u[12]          = s[2*3     +1 ] = s[7 ] = 130
     cn=3, co=2
u[X13] = u[x*n_colX+cn]         = s[S8 ] = s[y*n_col+co]
         u[2*5      +3 ] = u[13]          = s[2*3     +2 ] = s[8 ] = 140

Third Cut: If necessary, extend rows by copying from existing rows in the extended table.
           The last row gets it from the second row.
           The first row gets it from second to last row.
Let QR=0 if rows were not extended, QR=1 if they were.  Therefore QR=0 here.
If QR != 1, then skip this!

No Third Cut!
////////////////////////////////////////////////////////////////////////////////
*/

//------------------------------------------------------------------------------
// Lookup :: key_BLENDTBL_BLENDTS - Handles the keywords BLENDTBL and BLENTS and
//                                  related parameters.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     integer specifying success
// Calls:
//     IsInteger()
//     PrintError()
//     atoi()
// Errors:
//     Non-integer specification for blend period.
//     No specification of blend period.
// Warnings:
//     None
// Debug:
//     None
//------------------------------------------------------------------------------
/**
Handles the keywords BLENDTBL and BLENTS and related parameters.
@return int value signifying success
@param list character string list containing the fields from the line beginning
 with the keyword
@param nlist integer number of fields in the line
@param type integer defining which of the two keywords is being parameterized--
type=0 for BLENDTBL, type=1 for BLENDTS
*/
int Lookup3::key_BLENDTBL_BLENDTS( char **list, int nlist, int type )
{
    int totErrs = 0;
    char routine[]="Lookup3::construct-BLENDTBL";

    // type = 0 for blendTBL
    //      = 1 for blendTS

    // Follwing the keyword a blend period and possibly other carryover blend
    // values, should be found.

    // Handle the blend period
    if( nlist > 1 )
    {
        // A blend period exists and must be an integer.
        if( !IsInteger( list[1] ) )
        {
            // It is not an integer.
            totErrs++;
            PrintError( routine, "%s value '%s' is not an integer.",
                list[0], list[1] );
        }
        else
        {
            if( type )
            {
                _n_blend_ts = atoi( list[1] );
                _ts_step = _n_blend_ts + 1;
            }
            else
            {
                _n_blend_tbl = atoi( list[1] );
                _tbl_step = 1;
            }
        }
    }
    else
    {
        // No blend period was given.
        totErrs++;
        PrintError( routine, "Value required immediately after %s "
            "method keyword %s.", _type, list[0] );
    }

    // Check for internal carryover values.
    // If one exists, then three will.
    if( nlist > 2 )
    {
        // The carryover values are the blend step, the last column index used
	// to reference the table, the last row index used, and the last value
        // prescribed by the method.

        // The blend step should be an integer.
        // Because it is created by the system, don't check it.
        if( type )
        {
            _ts_step = atoi( list[2] );
        }
        else {
            _tbl_step = atoi( list[2] );
        }

        // Now check for the column index value (and if found, the row index
        // value).
        if( strcasecmp( list[3], "SKIP" ) )
        {
            // An integer value should be here, input by the system so don't
            // check it.
            _colI = atoi( list[3] );

            // Now check for the row index value
            if( strcasecmp( list[4], "SKIP" ) )
            {
                // An integer value should be here, input by the system so don't
                // check it.
                _rowI = atoi( list[4] );
                _iValKnown = 1;
            }
    	    // Otherwise, the table was not used last time step so no carryover.
        }
	// Otherwise, the table was not used last time step so no carryover.

        // Now check for the last prescribed value
        _myValue = atof( list[5] ) * _tableConv;
        _lastValKnown = 1;
    }

    return totErrs;
}

//------------------------------------------------------------------------------
// Lookup :: key_COLUMNVAR - Handles the keyword COLUMNVAR and related
//                           parameters.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     integer specifying success
// Calls:
//     PrintError()
//     ExprParser::parseString()
//     getType()
//     Component::getID()
//     Expression::verify()
//     PrintWarning()
// Errors:
//     No user-input subkeywords.
//     Duplicate COLUMNVAR usage.
//     Unable to interpret user-input expression.
//     Unable to access user-input expression value.
// Warnings:
//     Parametered COLUMNVAR to not use an input time series previously
//     specified for that purpose.
// Debug:
//     None
//------------------------------------------------------------------------------
/**
Handles the keyword COLUMNVAR and related parameters.
@return int value signifying success
@param list character string list containing the fields from the line beginning
 with the keyword
@param nlist integer number of fields in the line
@param re_list character string containing the unparsed line beginning with the
 keyword
*/
int Lookup3::key_COLUMNVAR( char **list, int nlist, char *re_list )
{
    int totErrs = 0;
    char routine[]="Lookup3::construct-COLUMNVAR";

    if ( nlist < 2 )
    {
        PrintError( routine, "DATE, INPUT_TS or Component State "
            "sub-keyword required immediately after LOOKUP3 method "
            "keyword %s. (%s %s %s)", list[0], getType(), _owner->getID(),
            _id );
        return STATUS_FAILURE;
    }

    // Check to see if this is a duplicate parameterization
    if( _colVar )
    {
        totErrs++;
        PrintError( routine, "'%s' is a duplicate usage of the "
            "'%s' keyword.", re_list, list[0] );
    }

    // Check the second field for secondary keyword(s).
    // These will be "DATE", "INPUT_TS" or some component state expression.
    if( !strcasecmp( list[1], "DATE" ) )
    {
        _colVar = INDEX_DATE;
    }
    else if( !strcasecmp( list[1], "INPUT_TS" ) )
    {
        _colVar = INDEX_TS;
        // Confirm that a TSINDEX COLUMNS time series was parameterized near
        // the end of _construct.
        
        // Handle the optional UNIT type for conversion of column indexes.
        if( nlist == 3 )
        {
            strncpy( _colOrigUnits, list[2], 24 );
            _colOrigUnits[ strlen(list[2]) ] = '\0';
        }
	// Otherwise, _colOrigUnits remains as initialized (probably "\0").
    }
    else
    {
        // The input is now expected to be a component state expression.
        _colVar = INDEX_COMPSTATE;
        char CompStatement[MAXC];
        strcpy( CompStatement, list[1] );

	// Just like in the Rules, allow complex states with + and - of
	// more than one.  If the user has more than one, s/he probably
	// used spaces.  If so, combine the remaining pieces of list
	// to create one bigger string containing the entire expression.
        if( nlist > 3 )
        {
            // Reconstruct the whole expression from list.
            int i;
            for( i=2; i<nlist; i++ )
            {
                // Add a space
                strcat( CompStatement, " " );
                // Add the next piece
                strcat( CompStatement, list[i] );
            }
        }

        // Tap into where the conditional expressions deal with their stuff.
        ExprParser parser;
        // There is a chance that _colExpr already exists, delete it if it does.
        if( _colExpr )
        {
            delete _colExpr;
        }
        _colExpr = parser.parseString( CompStatement );

        // Check to see if the expression was created.
        if( !_colExpr )
        {
            // Not created.
            totErrs++;
            PrintError( routine, "Could not interpret Component State on %s %s "
                "%s. Line: \"%s\".", getType(), _owner->getID(), _id, re_list );
            if ( _colExpr != NULL )
            {
                delete _colExpr;
                _colExpr = NULL;
            }
        }
        // Now check if it was created correctly.
        else if( _colExpr->verify() )
        {
           // Not created correctly.
            totErrs++;
            PrintError( routine, "Unable to verify constant values for \"%s\". "
                "(%s %s %s)", CompStatement, getType(), _owner->getID(), _id );
            delete _colExpr;
            _colExpr = NULL;
        }
        // Set the conversion factor that was, or could have been, used to
        // convert constants within the expression.  This conversion will be
        // appropriate also for column index values on the table.  This is not
        // done if there were problems creating the expression.
        else
        {
            _colConv = Component::_cfactor;
        }
    }

    // If non-INPUT_TS, then check to see if the user parameterized a TSINPUT
    // COLUMNS time series.
    if( _colVar != INDEX_TS &&  _colIndex )
    {
        PrintWarning( 1, routine, "COLVAR parameterized to not use the "
            "specified TSINDEX COLUMNS time series. (%s %s %s)", getType(),
            _owner->getID(), _id );
    }

    return totErrs;
}

//------------------------------------------------------------------------------
// Lookup :: key_INITIALTRANSFER - Handles the keyword INITIALTRANSFER and
//                                 related parameters.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     integer specifying success
// Calls:
//     atof()
//     PrintWarning()
//     getType()
//     Component::getID()
//     Method::getForecastDate1()
//     HourTS::setDataValue()
// Errors:
//     None
// Warnings:
//     Attempted transfer with a negative value.
// Debug:
//     None
//------------------------------------------------------------------------------
/**
Handles the keyword INITIALTRANSFER and related parameters.
@return int value signifying success
@param list character string list containing the fields from the line beginning
 with the keyword
@param nlist integer number of fields in the line
*/
int Lookup3::key_INITIALTRANSFER( char **list, int nlist )
{
    char routine[]="Lookup3::construct-INITIALTRANSFER";

    // Check for related value
    if( nlist < 2 )
    {
        // Value not provided, default to 0.
        _myValue = 0;
    }
    else
    {
        // Convert the value to internal units and place in _myValue
        _myValue = atof( list[1] ) * _tableConv;
    }

    // Check for negative value--TOCOMP reversed to FROMCOMP
    // will to be done at the end of Lookup::construct.

    return STATUS_SUCCESS;
}

//------------------------------------------------------------------------------
// Lookup :: key_INTERPOLATE - Handles the keyword INTERPOLATE and related
//                             parameters.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     integer specifying success
// Calls:
//     PrintError()
//     getType()
// Errors:
//     No user-input subkeywords.
//     Unrecognized user-input subkeywords.
// Warnings:
//     None
// Debug:
//     None
//------------------------------------------------------------------------------
/**
Handles the keyword INTERPOLATE and related parameters.
@return int value signifying success
@param list character string list containing the fields from the line beginning
 with the keyword
@param nlist integer number of fields in the line
*/
int Lookup3::key_INTERPOLATE( char **list, int nlist )
{
    char routine[]="Lookup3::construct-INTERPOLATE";

    // Check for secondary keyword.  These include ROWS, COLUMNS, ALL and BOTH.
    if ( nlist < 2 )
    {
        PrintError( routine, "Direction sub-keyword 'ROWS', 'COLUMNS', ALL' or "
            "'BOTH'  required immediately after %s method keyword %s.", 
            getType(), list[0] );
        return STATUS_FAILURE;
    }

    // The _mode is initialized in Method_initialize to NORMAL (probably 0).
    if ( !strcasecmp( list[1], "ROWS" ) )
    {
        _mode = INTERPOLATE_ROWS;
    }
    else if ( !strcasecmp( list[1], "COLUMNS" ) )
    {
        _mode = INTERPOLATE_COLS;
    }
    else if ( !strcasecmp( list[1], "ALL" ) ||
        !strcasecmp( list[1], "BOTH" ) )
    {
        _mode = INTERPOLATE_ALL;
    }
    else
    {
        PrintError( routine, "Direction sub-keyword 'ROWS', 'COLUMNS', ALL' or "
            "'BOTH'  required immediately after %s method keyword %s.", 
            getType(), list[0] );
        return STATUS_FAILURE;
    }
    return STATUS_SUCCESS;
}

//------------------------------------------------------------------------------
// Lookup :: key_ROWVAR - Handles the keyword ROWVAR and related parameters.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     integer specifying success
// Calls:
//     PrintError()
//     ExprParser::parseString()
//     getType()
//     Component::getID()
//     Expression::verify()
//     PrintWarning()
// Errors:
//     No user-input subkeywords.
//     Duplicate ROWVAR usage.
//     Unable to interpret user-input expression.
//     Unable to access user-input expression value.
// Warnings:
//     Parametered ROWVAR to not use an input time series previously specified
//     for that purpose.
// Debug:
//     None
//------------------------------------------------------------------------------
/**
Handles the keyword ROWVAR and related parameters.
@return int value signifying success
@param list character string list containing the fields from the line beginning
 with the keyword
@param nlist integer number of fields in the line
@param re_list character string containing the unparsed line beginning with the
 keyword
*/
int Lookup3::key_ROWVAR( char **list, int nlist, char *re_list )
{
    int totErrs = 0;
    char routine[]="Lookup3::construct-ROWVAR";

    if ( nlist < 2 )
    {
        PrintError( routine, "DATE, INPUT_TS or Component State "
            "sub-keyword required immediately after LOOKUP3 method "
            "keyword %s.", list[0] );
        return STATUS_FAILURE;
    }

    // Check to see if this is a duplicate parameterization
    if( _rowVar )
    {
        totErrs++;
        PrintError( routine, "'%s' is a duplicate usage of the "
            "'%s' keyword.", re_list, list[0] );
    }

    // Check the second field for secondary keyword(s).
    // These will be "DATE", "INPUT_TS" or some component state expression.
    if( !strcasecmp( list[1], "DATE" ) )
    {
        _rowVar = INDEX_DATE;
    }
    else if( !strcasecmp( list[1], "INPUT_TS" ) )
    {
        _rowVar = INDEX_TS;
        // Confirm that a TSINDEX ROWS time series was parameterized
        // near the end of _construct.
        
        // Handle the optional UNIT type for conversion of row indexes.
        if( nlist == 3 )
        {
            strncpy( _rowOrigUnits, list[2], 24 );
            _rowOrigUnits[ strlen(list[2]) ] = '\0';
        }
	// Otherwise, _rowOrigUnits remains as initialized (probably "\0").
    }
    else {
        // The input is now expected to be a component state expression.
        _rowVar = INDEX_COMPSTATE;
        char CompStatement[MAXC];
        strcpy( CompStatement, list[1] );

	// Just like in the Rules, allow complex states with + and - of
	// more than one.  If the user has more than one, s/he probably
	// used spaces.  If so, combine the remaining pieces of list
	// to create one bigger string containing the entire expression.
        if( nlist > 3 )
        {
            // Reconstruct the whole expression from list.
            int i;
            for( i=2; i<nlist; i++ )
            {
                // Add a space
                strcat( CompStatement, " " );
                // Add the next piece
                strcat( CompStatement, list[i] );
            }
        }

        // Tap into where the conditional expressions deal with their stuff.
        ExprParser parser;
        // There is a chance that _rowExpr already exists, delete it if it does.
        if( _rowExpr )
        {
            delete _rowExpr;
        }
        _rowExpr = parser.parseString( CompStatement );

        // Check to see if the expression was created.
        if( !_rowExpr )
        {
            // Not created.
            totErrs++;
            PrintError( routine, "Could not interpret Component State on %s %s "
                "%s. Line: \"%s\".", getType(), _owner->getID(), _id, re_list );
            if ( _rowExpr != NULL )
            {
                delete _rowExpr;
                _rowExpr = NULL;
            }
        }
        // Now check if it was created correctly.
        else if( _rowExpr->verify() )
        {
           // Not created correctly.
            totErrs++;
            PrintError( routine, "Unable to verify constant values for \"%s\".",
                CompStatement );
            delete _rowExpr;
            _rowExpr = NULL;
        }
        // Set the conversion factor that was, or could have been, used to
        // convert constants within the expression.  This conversion will be
        // appropriate also for column index values on the table.  This is not
        // done if there were problems creating the expression.
        else
        {
            _rowConv = Component::_cfactor;
        }
    }

    // If non-INPUT_TS, then check to see if the user parameterized a TSINPUT
    // ROWS time series.
    if( _rowVar != INDEX_TS &&  _rowIndex )
    {
        PrintWarning( 1, routine, "ROWVAR parameterized to not use the "
            "specified TSINDEX ROWS time series. (%s %s %s)", getType(),
            _owner->getID(), _id );
    }

    return totErrs;
}

//------------------------------------------------------------------------------
// Lookup :: key_TABLEVAR - Handles the keyword TABLEVAR and related parameters.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     integer specifying success
// Calls:
//     PrintError()
//     getType()
//     Component::getType()
//     Component::getID()
// Errors:
//     No user-input subkeywords.
//     Duplicate TABLEVAR usage.
//     Unrecognized user-input subkeywords.
//     Table type incompatible with owning component type.
//     Table type incompatible with previously parameterized observed time
//     series (4 different versions of this error).
// Warnings:
//     None
// Debug:
//     None
//------------------------------------------------------------------------------
/**
Handles the keyword TABLEVAR and related parameters.
@return int value signifying success
@param list character string list containing the fields from the line beginning
 with the keyword
@param nlist integer number of fields in the line
@param re_list character string containing the unparsed line beginning with the
 keyword
*/
int Lookup3::key_TABLEVAR( char **list, int nlist, char *re_list )
{
    int totErrs = 0;
    char routine[]="Lookup3::construct-TABLEVAR";
    if ( nlist < 2 )
    {
        PrintError( routine, "WITHDRAW(AL), RELEASE, DIVERSION or "
            "AUGMENTATION sub-keyword required immediately after "
            "LOOKUP3 method keyword %s.", list[0] );
        return STATUS_FAILURE;
    }

    // Check to see if this is a duplicate parameterization
    if( _tableVar )
    {
        totErrs++;
        PrintError( routine, "'%s' is a duplicate usage of the "
            "'%s' keyword.", re_list, list[0] );
    }

    // Determine what type of table this is, what variable it calculates.
    if( !strcasecmp( list[1], "RELEASE" ) )
    {
        _tableVar = TVAR_RELEASE;
        _group_id = RELEASE_METHOD;
    }
    else if( !strncasecmp( list[1], "WITHDRAW", 8 ) )
    {
        _tableVar = TVAR_WITHDRAWAL;
        _group_id = WITHD_METHOD;
    }
    else if( !strcasecmp( list[1], "DIVERSION" ) )
    {
        _tableVar = TVAR_DIVERSION;
        _group_id = DIVERS_METHOD;
    }
    else if( !strncasecmp( list[1], "AUGMENT", 7 ) )
    {
        _tableVar = TVAR_AUGMENTATION;
        if( _ownerType == CM_RESERVOIR )
        {
            _group_id = WITHD_METHOD;
        }
        else if( _ownerType == CM_NODE )
        {
            _group_id = DIVERS_METHOD;
        }
    }
    else {
        // If exescution reaches here, no valid keyword was found.
        // No sense in continuing.
        PrintError( routine, "WITHDRAW(AL), RELEASE, DIVERSION or "
            "AUGMENTATION sub-keyword required immediately after "
            "LOOKUP3 method keyword %s.", list[0] );
        return STATUS_FAILURE;
    }

    // Check to ensure that the tableVar matches the owning component type.
    // Reservoirs cannot have DIVERSION.
    // Nodes cannot have RELEASE or WITHDRAW.
    if( ( _ownerType == CM_RESERVOIR && _tableVar == TVAR_DIVERSION ) ||
        ( _ownerType == CM_NODE && 
            (_tableVar == TVAR_RELEASE || _tableVar == TVAR_WITHDRAWAL ) ) )
    {
        totErrs++;
        PrintError( routine, "'%s' not allowed on %s owned by %s %s.",
            re_list, getType(), _owner->getType(), _owner->getID() );
    }

    // Check to ensure that the tableVar is in agreement with any
    // TSINPUT observed value types.
    switch( _tableVar )
    {
        // _tableVar = TVAR_RELASE is incompatible with any TSINPUT except the
        // release.  Check for non-null observed time series variables.  If the
        // time series are defined later, they are checked elsewhere for
        // compatibility with tableVar also.
        case TVAR_RELEASE:
            if( _withdraw_obs || _augment_obs || _diversion_obs )
            {
                totErrs++;
                PrintError( routine, "%s %s not compatible with "
                    "TSINPUT OBSERVEDWITHDRAWAL, OBSERVEDAUGMENTATION "
                    "and / or OBSERVEDDIVERSION. (%s %s %s)",
                    list[0], list[1], getType(), _owner->getID(), _id );
            }
            break;
        // _tableVar = TVAR_WITHDRAWL is is incompatible with TSINPUT releases
        // and diversions.  Check for non-null observed time series variables.
        case TVAR_WITHDRAWAL:
            if( _release_obs || _diversion_obs )
            {
                totErrs++;
                PrintError( routine, "%s %s not compatible with "
                    "TSINPUT OBSERVEDRELEASE and / or "
                    "OBSERVEDDIVERSION. (%s %s %s)",
                    list[0], list[1], getType(), _owner->getID(), _id );
            }
            break;
        // _tableVar = TVAR_DIVERSION is incompatible with TSINPUT releases and
        // withdrawals.  Check for non-null observed time series variables.
        case TVAR_DIVERSION:
            if( _release_obs || _withdraw_obs )
            {
                totErrs++;
                PrintError( routine, "%s %s not compatible with "
                    "TSINPUT OBSERVEDRELEASE and / or "
                    "OBSERVEDWITHDRAWAL. (%s %s %s)",
                    list[0], list[1], getType(), _owner->getID(), _id );
            }
            break;
        // _tableVar = TVAR_AUGMENTATION is incompatible with TSINPUT releases.
        // Check for non-null observed time series variables.
        case TVAR_AUGMENTATION:
            if( _release_obs )
            {
                totErrs++;
                PrintError( routine, "%s %s not compatible with "
                    "TSINPUT OBSERVEDRELEASE. (%s %s %s)",
                    list[0], list[1], getType(), _owner->getID(), _id );
            }
            break;
        default:
            // There is no chance of getting here as _tableVar is defined above
            // to be one of these values.
            break;
    }

    return totErrs;
}

//------------------------------------------------------------------------------
// Lookup :: key_TOCOMP - Handles the keyword TOCOMP and related parameters.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     integer specifying success
// Calls:
//     PrintError()
//     getType()
//     Component::getID()
//     Component::findRoot()
//     Component::getComponentPtr()
//     Component::getSolutionNumber()
//     Method::getForecastDate1()
//     Method::getForecastDate2()
//     Method::getTimeInterval()
//     Component::setInflowTS()
//     HourTS::getIdentifier()
//     TSUtil::getTSDateFromString()
//     HourTS::setDate1()
//     HourTS::setDate2()
//     HourTS::setDataInterval()
//     HourTS::allocateDataSpace()
//     TSDate::addInterval()
//     HourTS::setDataValue()
//     HourTS::getIdentifier();
//     TSIdent::setAlias()
//     HourTS::setIdentifier()
//     Component::setInflowTS()
// Errors:
//     No user-input subkeywords.
//     Receiving component not found in component tree.
//     TOCOMP mode incompatibile with solution order.
//     Unrecognized user-input subkeywords.
//     Unable to link ToComp time series as inflow time series on receiving
//     component.
// Warnings:
//     Unable to allocate transfer time series space.
// Debug:
//     None
//------------------------------------------------------------------------------
/**
Handles the keyword TOCOMP and related parameters.
@return int value signifying success
@param list character string list containing the fields from the line beginning
 with the keyword
@param nlist integer number of fields in the line
*/
int Lookup3::key_TOCOMP( char **list, int nlist )
{
    char routine[]="Lookup3::construct-TOCOMP";
    int totErrs=0;

    if ( nlist < 3 )
    {
        // Insufficient information
        PrintError( routine, "Component ID and mode required "
            "immediately after keyword %s. (%s %s %s)", list[0],
            getType(), _owner->getID(), _id );
        return STATUS_FAILURE;
    }

    // Handle Component
    Component *root = _owner->findRoot();
    _receivingComp = (Component *) root->getComponentPtr( list[1] );
    if( _receivingComp == NULL )
    {
        PrintError( routine, "ToComp %s not found in Component tree. "
            "(%s %s %s)", list[1], getType(), _owner->getID(),_id );
        return STATUS_FAILURE;
    }

    // Handle transferMode
    if( !strcasecmp( list[2], "INSTANTANEOUS" ) )
    {
        // Check to ensure that this configuration is valid, based on order of
        // solution.
        int ownNum = _owner->getSolutionNumber();
        int CompNum = _receivingComp->getSolutionNumber();
        if ( ownNum > CompNum )
        {
            totErrs++;
            PrintError( routine, "%s transfer to Component %s is unacceptable "
                "due to solution order of components. (%s %s %s)", list[2], 
                list[1], getType(), _owner->getID(), _id );
        }
        _toCompMode = 0;
    }
    else if( strcasecmp( list[2], "NEXTSTEP" ) )
    {
        // The keyword is unrecognized.
        totErrs++;
        PrintError( routine, "Keyword \"%s\" unacceptable for transfer ToComp. "
            "(%s %s %s)", list[2], getType(), _owner->getID(),_id);
    }

    // Allocate and set up the _outflow TS...
    TSDate t;
    TSDate t1 = Method :: getForecastDate1();
    TSDate t2 = Method :: getForecastDate2();
    int timeInt = Method :: getTimeInterval();
    int timeMult = Method :: getTimeMult();

    _Comp_ts.setDate1( t1 );
    _Comp_ts.setDate2( t2 );
    _Comp_ts.setDataInterval( timeInt, timeMult );
    if( _Comp_ts.allocateDataSpace() )
    {
        PrintWarning( 1, routine, "Troubles allocating data space for transfer "
            "time series on %s.", _id );
        return( STATUS_FAILURE );
    }
    // Initialize all values to zero
    for( t=t1; t<=t2; t.addInterval( timeInt, timeMult ) )
    {
        _Comp_ts.setDataValue( t, 0.0 );
    }

    // Assign unique identifier to the timeseries, esp. the Alias
    char temp[MAXC];
    TSIdent id = _Comp_ts.getIdentifier();
    sprintf( temp, ">>SpecialTie<<Lookup3-%s_%sTo%s", _owner->getID(), _id,
        _receivingComp->getID() );
    id.setAlias( temp );
    _Comp_ts.setIdentifier( id );

    // Link the time series with the receiving component's inflows.
    if( _receivingComp->setInflowTS( &_Comp_ts) )
    {
        // If execution reaches here, this is big trouble, not a user error!
        PrintError( routine, "Troubles setting TOCOMP timeseries on %s. "
            "(%s %s %s)",_receivingComp->_id, getType(), _owner->getID(),_id );
        return STATUS_FAILURE;
    }
    
    // Increment some triggers on the owning and receiving components.
    _owner->_specialTieOut++; 
    _receivingComp->_specialTieIn++;

    return totErrs;
}

//------------------------------------------------------------------------------
// Lookup :: key_TSINDEX - Handles the keyword TSINDEX and related parameters.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     integer specifying success
// Calls:
//     PrintError()
//     getType()
//     Component::getID()
//     TSList::getTSFromList()
// Errors:
//     No (or insufficient number of) user-input subkeywords.
//     Duplicate usage of TSINDEX ROWS.
//     Inability to locate the user-specified time series in the RES-J time
//     series list.
//     Duplicate usage of TSINDEX COLUMNS.
//     Inability to locate the user-specified time series in the RES-J time
//     series list.
//     Unrecognized user-input subkeywords.
//     Inability to locate the user-specified time series in the RES-J time
//     series list.
// Warnings:
//     Unable to allocate transfer time series space.
// Debug:
//     None
//------------------------------------------------------------------------------
/**
Handles the keyword TSINDEX and related parameters.
@return int value signifying success
@param list character string list containing the fields from the line beginning
 with the keyword
@param nlist integer number of fields in the line
@param re_list character string containing the unparsed line beginning with the
 keyword
*/
int Lookup3::key_TSINDEX( char **list, int nlist, char *re_list )
{
    int totErrs = 0, j;
    char ts_id[MAXC] = "";
    char routine[]="Lookup3::construct-TSINDEX";

    // Follwing the keyword there should be a secondary keyword and a time
    // series identifier.
    // Acceptable keywords are ROWS and COLUMNS.

    if ( nlist < 3 )
    {
        PrintError( routine, "Sub-keyword ROWS or COLUMNS and timeseries "
            "alias required immediately after keyword %s. (%s %s %s)",
            list[0], getType(), _owner->getID(), _id );
        return STATUS_FAILURE;
    }

    // Read either full identifiers or alias.
    // This is code taken from SetWithdraw_construct.  Documentation does not
    // advertise usage of the full time series identifier, just the alias.
    strcpy( ts_id, list[2] );
    for( j = 3; j < nlist; j++ )
    {
        strcat( ts_id, " " );
        strcat( ts_id, list[j] );
    }

    // Determine whether the time series will be for rows or columns
    if( !strcasecmp( list[1], "ROWS" ) )
    {
        // Check to see if they have already parameterized TSINDEX ROWS
        if( _rowIndex )
        {
            totErrs++;
            PrintError( routine, "'%s' is a duplicate usage of the "
                "'%s %s' keywords. (%s %s %s)", re_list, list[0], list[1],
                getType(), _owner->getID(), _id );
        }

        // Match the input time series (from TIMESERIES section) with
        // the parameterized input time series.
        _rowIndex = (HourTS*)TSList::getTSFromList( ts_id );
        if( _rowIndex == NULL )
        {
            totErrs++;
            PrintError( routine, "Troubles setting index timeseries \"%s\". "
                "(%s %s %s)", list[2], getType(), _owner->getID(), _id );
        }
    }
    else if( !strcasecmp( list[1], "COLUMNS" ) )
    {
        // Check to see if they have already parameterized TSINDEX COLUMNS
        if( _colIndex )
        {
            totErrs++;
            PrintError( routine, "'%s' is a duplicate usage of the "
                "'%s %s' keywords. (%s %s %s)", re_list, list[0], list[1],
                getType(), _owner->getID(), _id );
        }

        // Match the input time series (from TIMESERIES section) with
        // the parameterized observed time series.
        _colIndex = (HourTS*)TSList::getTSFromList( ts_id );
        if( _colIndex == NULL )
        {
            totErrs++;
            PrintError( routine, "Troubles setting index timeseries \"%s\". "
                "(%s %s %s)", list[2], getType(), _owner->getID(), _id );
        }
    }
    else
    {
        // The keyword is not recognized.
        totErrs++;
        PrintError( routine, "Sub-keyword ROWS or COLUMNS and timeseries "
            "alias required immediately after keyword %s. (%s %s %s)",
            list[0], getType(), _owner->getID(), _id );

        // Even though the time series keyword failed, test for 
        // the alias name and the ability to set the time series.
        HourTS* testTS;
        testTS = (HourTS*)TSList::getTSFromList( ts_id );
        if( testTS == NULL )
        {
            totErrs++;
            PrintError( routine, "Troubles setting index timeseries \"%s\". "
                "(%s %s %s)", list[2], getType(), _owner->getID(), _id );
        }
    }
    return totErrs;
}

//------------------------------------------------------------------------------
// Lookup :: key_TSINPUT - Handles the keyword TSINPUT and related parameters.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     integer specifying success
// Calls:
//     PrintError()
//     getType()
//     Component::getID()
//     TSList::getTSFromList()
// Errors:
//     No (or insufficient number of) user-input subkeywords.
//     Unrecognized user-input subkeywords.
//     Inability to locate the user-specified time series in the RES-J time
//     series list.
//     The following 4 errors repeat for OBSERVEDRELEASE, OBSERVEDWITHDRAWAL,
//     OBSERVEDDIVERSION AND OBSERVEDAUGMENTATION keywords:
//       Duplicate usage of TSINPUT OBSERVEDRELEASE keywords.
//       Incompatibility of observed time series type with previously defined
//       observed time series type(s).
//       Incompatibility of observed time series type with owning component
//       type.
//       Inability to locate the user-specified time series in the RES-J time
//       series list.
//     Previously parameterized table type incompatible with observed time
//     series (4 different versions of this error).
// Warnings:
//     None
// Debug:
//     None
//------------------------------------------------------------------------------
/**
Handles the keyword TSINPUT and related parameters.
@return int value signifying success
@param list character string list containing the fields from the line beginning
 with the keyword
@param nlist integer number of fields in the line
@param re_list character string containing the unparsed line beginning with the
 keyword
*/
int Lookup3::key_TSINPUT( char **list, int nlist, char *re_list )
{
    int totErrs = 0, j;
    char ts_id[MAXC] = "";
    char routine[]="Lookup3::construct-TSINPUT";

    // Follwing the keyword there should be a secondary keyword and a time
    // series identifier.
    // Acceptable keywords are OBSERVEDWITHDRAW, OBSERVEDDIVERSION,
    //   OBSERVEDAUGMENTATION and OBSERVEDRELEASE.

    if ( nlist < 3 )
    {
        PrintError( routine, "Sub-keyword OBSERVEDWITHDRAW, OBSERVEDDIVERSION, "
            "OBSERVEDAUGMENTATION or OBSERVEDRELEASE and timeseries alias "
            "required immediately after keyword %s. (%s %s %s)",
            list[0], getType(), _owner->getID(), _id );
        return STATUS_FAILURE;
    }

    // Read either full identifiers or alias 
    // This is code taken from SetWithdraw_construct.  Documentation does not
    // advertise usage of the full time series identifier, just the alias.
    strcpy( ts_id, list[2] );
    for( j = 3; j < nlist; j++ )
    {
        strcat( ts_id, list[j] );
        strcat( ts_id, " " );
    }

    // Before traversing several tests, check for the required OBSERVED part.
    if( strncasecmp( list[1], "OBSERVED", 8 ) )
    {
        // The time series sub-keyword did not begin with "OBSERVED"
        PrintError( routine, "Sub-keyword OBSERVEDWITHDRAW, OBSERVEDDIVERSION, "
            "OBSERVEDAUGMENTATION or OBSERVEDRELEASE and timeseries alias "
            "required immediately after keyword %s. (%s %s %s)",
            list[0], getType(), _owner->getID(), _id );

        // Even though the time series sub-keyword failed, test
        // for the alias name and the ability to set the time series.
        HourTS* testTS;
        testTS = (HourTS*)TSList::getTSFromList( ts_id );
        if( testTS == NULL )
        {
            PrintError( routine, "Troubles setting observed timeseries \"%s\". "
                "(%s %s %s)", list[2], getType(), _owner->getID(), _id );
        }
        return STATUS_FAILURE;
    }

    // Determine what kind of OBSERVED time series it is.
    if( !strncasecmp( list[1], "OBSERVEDRELEASE", 15 ) )
    {
        // Check to see if they have already parameterized OBSERVEDRELEASE
        if( _release_obs )
        {
            totErrs++;
            PrintError( routine, "'%s' is a duplicate usage of the "
                "'%s %s' keywords. (%s %s %s)", re_list, list[0], list[1],
                getType(), _owner->getID(), _id );
        }

        // Check to see if an incompatible time series has been parameterized;
        //   releases are incompatible with other types.
        if( _withdraw_obs || _augment_obs || _diversion_obs )
        {
            totErrs++;
            PrintError( routine, "%s %s not compatible with other "
                "%s OBSERVEDWITHDRAWAL,  OBSERVEDAUGMENTATION and / or "
                "OBSERVEDDIVERSION. (%s %s %s)", list[0], list[1],
                list[0], getType(), _owner->getID(), _id );
        }

        // Check to see if owner is a RESERVOIR.  Release works only on
        // a Reservoir.
        if( _ownerType != CM_RESERVOIR)
        {
            // Owner is not a RESERVOIR.
            totErrs++;
            PrintError( routine, "%s %s compatible only with RESERVOIR "
                "components. (%s %s %s)", list[0], list[1], getType(),
                _owner->getID(), _id );
        }

        // Match the input time series (from TIMESERIES section) with
        // the parameterized observed time series.
        _release_obs = (HourTS*)TSList::getTSFromList( ts_id );
        if( _release_obs == NULL )
        {
            totErrs++;
            PrintError( routine, "Troubles setting observed timeseries \"%s\". "
                "(%s %s %s)", list[2], getType(), _owner->getID(), _id );
        }
    }
    else if( !strncasecmp( list[1], "OBSERVEDWITHDRAW", 16 ) )
    {
        // Check to see if they have already parameterized OBSERVEDWITHDRAW
        if( _withdraw_obs )
        {
            totErrs++;
            PrintError( routine, "'%s' is a duplicate usage of the "
                "'%s %s' keywords. (%s %s %s)", re_list, list[0], list[1],
                getType(), _owner->getID(), _id );
        }

        // Check to see if an incompatible time series has been parameterized;
        //   withdrawals are incompatible with release and diversion types.
        if( _release_obs || _diversion_obs )
        {
            totErrs++;
            PrintError( routine, "%s %s not compatible with other "
                "%s OBSERVEDRELEASE,  and / or OBSERVEDDIVERSION. (%s %s %s)",
                list[0], list[1], list[0], getType(), _owner->getID(),_id );
        }

        // Check to see if owner is a RESERVOIR.  Release works only on
        // on a Reservoir.
        if( _ownerType != CM_RESERVOIR)
        {
            // Owner is not a RESERVOIR.
            totErrs++;
            PrintError( routine, "%s %s compatible only on RESERVOIR "
                "components. (%s %s %s)", list[0], list[1], getType(),
                _owner->getID(), _id );
        }

        // Match the input time series (from TIMESERIES section) with
        // the parameterized observed time series.
        _withdraw_obs = (HourTS*)TSList::getTSFromList( ts_id );
        if( _withdraw_obs == NULL )
        {
            totErrs++;
            PrintError( routine, "Troubles setting observed timeseries \"%s\". "
                "(%s %s %s)", list[2], getType(), _owner->getID(), _id );
        }
    }
    else if( !strncasecmp( list[1], "OBSERVEDDIVERSION", 17 ) )
    {
        // Check to see if they have already parameterized OBSERVEDDIVERSION
        if( _diversion_obs )
        {
            totErrs++;
            PrintError( routine, "'%s' is a duplicate usage of the "
                "'%s %s' keywords. (%s %s %s)", re_list, list[0], list[1],
                getType(), _owner->getID(), _id );
        }

        // Check to see if an incompatible time series has been parameterized;
        //   withdrawals are incompatible with release and diversion types.
        if( _withdraw_obs || _release_obs )
        {
            totErrs++;
            PrintError( routine, "%s %s not compatible with other "
                "%s OBSERVEDRELEASE, and / or OBSERVEDWITHDRAWAL. (%s %s %s)",
                list[0], list[1], list[0], getType(), _owner->getID(), _id );
        }

        // Check to see if owner is a NODE.  Diversion works only on a
        // Node.
        if( _ownerType != CM_NODE)
        {
            // Owner is not on a NODE
            totErrs++;
            PrintError( routine, "%s %s compatible only on NODE "
                "components. (%s %s %s)", list[0], list[1], getType(),
                _owner->getID(), _id );
        }

        // Match the input time series (from TIMESERIES section) with
        // the parameterized observed time series.
        _diversion_obs = (HourTS*)TSList::getTSFromList( ts_id );
        if( _diversion_obs == NULL )
        {
            totErrs++;
            PrintError( routine, "Troubles setting observed timeseries \"%s\". "
                "(%s %s %s)", list[2], getType(), _owner->getID(), _id );
        }
    }
    else if( !strncasecmp( list[1], "OBSERVEDAUGMENT", 15 ) )
    {
        // Check to see if they have already parameterized OBSERVEDAUGMENT
        if( _augment_obs )
        {
            totErrs++;
            PrintError( routine, "'%s' is a duplicate usage of the "
                "'%s %s' keywords. (%s %s %s)", re_list, list[0], list[1],
                getType(), _owner->getID(), _id );
        }

        // Check to see if an incompatible time series has been parameterized;
        //   withdrawals are incompatible with release and diversion types.
        if( _release_obs )
        {
            totErrs++;
            PrintError( routine, "%s %s not compatible with other "
                "%s OBSERVEDRELEASE. (%s %s %s)", list[0], list[1],
                list[0], getType(), _owner->getID(),_id );
        }

        // Match the input time series (from TIMESERIES section) with
        // the parameterized observed time series.
        _augment_obs = (HourTS*)TSList::getTSFromList( ts_id );
        if( _augment_obs == NULL )
        {
            totErrs++;
            PrintError( routine, "Troubles setting observed timeseries \"%s\". "
                "(%s %s %s)", list[2], getType(), _owner->getID(), _id );
        }
    }
    else
    {
        totErrs++;
        PrintError( routine, "Sub-keyword OBSERVEDWITHDRAW, OBSERVEDDIVERSION, "
            "OBSERVEDAUGMENTATION or OBSERVEDRELEASE and timeseries alias "
            "required immediately after keyword %s. (%s %s %s)",
            list[0], getType(), _owner->getID(), _id );

        // Even though the time series keyword failed, test for 
        // the alias name and the ability to set the time series.
        HourTS* testTS;
        testTS = (HourTS*)TSList::getTSFromList( ts_id );
        if( testTS == NULL )
        {
            totErrs++;
            PrintError( routine, "Troubles setting observed timeseries \"%s\". "
                "(%s %s %s)", list[2], getType(), _owner->getID(), _id );
        }
    }

    // Check to ensure that the TSINPUT observed value type is in agreement with
    // any tableVar parameterization.
    switch( _tableVar )
    {
        // _tableVar = TVAR_RELASE is incompatible with any TSINPUT except the
        // release.  Check for non-null observed time series variables.  If the
        // time series are defined later, they are checked elsewhere for
        // compatibility with tableVar also.
        case TVAR_RELEASE:
            if( _withdraw_obs || _augment_obs || _diversion_obs )
            {
                totErrs++;
                PrintError( routine, "%s %s not compatible with "
                    "TSINPUT OBSERVEDWITHDRAWAL, OBSERVEDAUGMENTATION "
                    "and / or OBSERVEDDIVERSION. (%s %s %s)",
                    list[0], list[1], getType(), _owner->getID(), _id );
            }
            break;
        // _tableVar = TVAR_WITHDRAWL is is incompatible with TSINPUT releases
        // and diversions.  Check for non-null observed time series variables.
        case TVAR_WITHDRAWAL:
            if( _release_obs || _diversion_obs )
            {
                totErrs++;
                PrintError( routine, "%s %s not compatible with "
                    "TSINPUT OBSERVEDRELEASE and / or "
                    "OBSERVEDDIVERSION. (%s %s %s)",
                    list[0], list[1], getType(), _owner->getID(), _id );
            }
            break;
        // _tableVar = TVAR_DIVERSION is incompatible with TSINPUT releases and
        // withdrawals.  Check for non-null observed time series variables.
        case TVAR_DIVERSION:
            if( _release_obs || _withdraw_obs )
            {
                totErrs++;
                PrintError( routine, "%s %s not compatible with "
                    "TSINPUT OBSERVEDRELEASE and / or "
                    "OBSERVEDWITHDRAWAL. (%s %s %s)",
                    list[0], list[1], getType(), _owner->getID(), _id );
            }
            break;
        // _tableVar = TVAR_AUGMENTATION is incompatible with TSINPUT releases.
        // Check for non-null observed time series variables.
        case TVAR_AUGMENTATION:
            if( _release_obs )
            {
                totErrs++;
                PrintError( routine, "%s %s not compatible with "
                    "TSINPUT OBSERVEDWITHDRAWAL. (%s %s %s)",
                    list[0], list[1], getType(), _owner->getID(), _id );
            }
            break;
        default:
            // If execution reaches here, _tableVar is probably still set to
            // TVAR_UNDEFINED.  This is okay.  The TABLEVAR keyword has not yet
            // been parameterized.
            break;
    }
    return totErrs;
}

//------------------------------------------------------------------------------
// Lookup :: key_VALUES - Handles the keyword VALUES and related parameters.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     integer specifying success
// Calls:
//     GetSubStringList()
//     PrintError()
//     getType()
//     Component::getID()
//     FreeStringList()
// Errors:
//     Inability to extract the VALUES / ENDVALUES defined parameter section.
//     Attempt to proces VALUES keyword parameters section without previous
//     usage of COLVAR, ROWVAR, and TABLEVAR.
//     Unsuccessful creation of the Lookup3 table.
// Warnings:
//     None
// Debug:
//     None
//------------------------------------------------------------------------------
/**
Handles the keyword VALUES and related parameters including advancing the row
 counter.
@return int value signifying success
@param list character string list containing the fields from the line beginning
 with the keyword
@param nlist integer number of fields in the line
@param re_list character string list containing the rest of the user-input
 parameters, beginning with the line containing the keyword
@param nlist integer number of line withiin the original user-input file.
*/
int Lookup3::key_VALUES( char **list, int n_items, char **re_list, int *rowCount )
{
    int totErrs = 0, n_value;
    char **value_list = NULL;
    char routine[]="Lookup3::construct-VALUES";

    // Parse out the value list--everything between the VALUES and ENDVALUES
    // keywords.
    value_list = GetSubStringList( re_list, n_items, "VALUES", &n_value,
        RETURN_NO_KEYWORDS );
    if( value_list == NULL || n_value == 0 )
    {
        totErrs++;
        PrintError( routine, "Troubles forming sub_list beginning with %s. "
            "Does \"END%s\" exist? (%s %s %s)", list[0], list[0], getType(), 
            _owner->getID(), _id);
    }

    // Before proceeding, determine what the values represent.
    // Ensure user has defined column and row index and value meanings.
    else if( !_colVar || !_rowVar || !_tableVar )
    {
        totErrs++;
        PrintError( routine, "Cannot understand VALUES section without "
            "prior knowledge of 'COLUMNVAR', 'ROWVAR' and 'TABLEVAR'. "
            "(%s %s %s)", getType(), _owner->getID(), _id );
    }

    else if( buildTable( value_list, n_value ) )
    {
        // Table build failed.
        totErrs++;
        PrintError( routine, "Table not successfully read. (%s %s %s)",
           getType(), _owner->getID(), _id );
    }

    // Free the value list, if it exists.
    if ( value_list )
    {
        value_list = FreeStringList( value_list );
        // Increment the counter so _construct jumps over the list
        (*rowCount) += n_value + 1; 
    }

    return totErrs;
}

//------------------------------------------------------------------------------
// Lookup :: key_WEEKLYVARIATION - Handles the keyword WEEKLYVARIATION and
//                                 related parameters.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     integer specifying success
// Calls:
//     PrintError()
//     atof()
// Errors:
//     Erroneous number of weekly scaling values.
// Warnings:
//     None
// Debug:
//     None
//------------------------------------------------------------------------------
/**
Handles the keyword WEEKLYVARIATION and related parameters including advancing the row
 counter.
@return int value signifying success
@param list character string list containing the fields from the line beginning
 with the keyword
@param nlist integer number of fields in the line
*/
int Lookup3::key_WEEKLYVARIATION( char **list, int nlist )
{
    int j;
    char routine[]="Lookup3::construct-WEEKLYVARIATION";

    // Determine how many values should be given
    int requiredSize = ( 24 / _t_mult ) * 7;

    // Compare required number of values with the number provided.
    // Note: The "- 1" below removes the keyword included in the count.
    if( nlist -1  != requiredSize )
    {
        PrintError( routine, "Keyword %s must preceed %d scaling "
            "values instead of %d. (%s %s %s)", list[0], requiredSize, 
            nlist-1, _type, _owner->_id, _id );
        return STATUS_FAILURE; 
    }

    // Size the array.
    _inWeekScalars =  new double[ requiredSize ];

    // Populate the array.
    for( j = 0; j < requiredSize; j++ )
    {
        _inWeekScalars[j]=atof( list[j+1] );
        // Note: For efficiency, do not ensure that the user is inputting
        //       valid doubles.
    }

    return STATUS_SUCCESS;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source$";
 static char rcs_id2[] = "$Id$";}
/*  ===================================================  */

}
