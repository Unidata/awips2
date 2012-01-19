//------------------------------------------------------------------------------
// Matrix
//------------------------------------------------------------------------------
// Notes:    (1) Template class that can store any data type in a row/column
//               format.
//------------------------------------------------------------------------------
// History:
// 2006 Feb 06  James R. VanShaar, Riverside Technology, inc
//                                           Initial version based on RTi 
//                                           library code, rewritten and 
//                                           simplified for specific nwsrfs use.
//                   RTi library code contributed to by Daniel Weiler,
//                   Steven A. Malers and Michael Thiemann.  
//                   Last modification 2001 Jan 09.
//------------------------------------------------------------------------------

#include "Matrix.h"
#include "ResJ.h"
#include <math.h>

#include <stdio.h>

//------------------------------------------------------------------------------
// Matrix :: Matrix - Construct an empty Matrix
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     
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
Construct an empty matrix.
*/
Matrix :: Matrix()
{
    initialize();
}

//------------------------------------------------------------------------------
// Matrix :: ~Matrix - destructor
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     
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
Destroy instance.
*/
Matrix :: ~Matrix()
{
    if ( _data != NULL )
    {
        delete [] _data;
    }
    _data = NULL;
}

//------------------------------------------------------------------------------
// Matrix :: allocateDataSpace - Allocates space for indexing and data
//                                         arrays.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     
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
Allocate the data space for the memory.
@return 0 if successful, 1 if an error occurs.
*/
int Matrix :: allocateDataSpace()
{
    //char routine[] = "Matrix.allocateDataSpace";
    int n_values = 0, i;

    // Calculate the total number of pieces of double data to be allocated.
    n_values = _n_rows * _n_cols;

    if( _data != NULL )
    {
        delete [] _data;
    }

    _data = new double[ n_values ];

    if( _data == NULL )
    {
        return 1;
    }

    for( i = 0; i < n_values; i++ )
    {
        _data[i] = 0;
    }
    return 0;
}

//------------------------------------------------------------------------------
// Matrix :: getElement - Access the value defined by two indexes
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     double Matrix value
// Calls:
//     PrintWarning( int, char*, char* )
// Errors:
//     None
// Warnings:
//     Indexe(s) is(are) reference beyond the array length
// Debug:
//     None
//------------------------------------------------------------------------------
/**
Return a matrix element.
@param row Row (zero-index).
@param col Column (zero-index).
@return a matrix element.
*/
double Matrix :: getElement( int row, int col )
{
    char routine[] = "Matrix.getElement";
    int index=0;

    if( row < 0 || col < 0 || row >= _n_rows || col >= _n_cols )
    {
        PrintWarning( 1, routine, "Cannot get matrix element at "
            "position [%d][%d]  - out of range.", row, col );
        return( -999.0 );
    }
    if( _data == NULL )
    {
        return( -999.0 );
    } 

    // calculate the index in _data array that this row/column combo
    // corresponds to 
    index = row*_n_cols + col;

    return( _data[index] );
}

//------------------------------------------------------------------------------
// Matrix :: getNumRows - Return the number of rows in the Matrix
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     integer number of rows in the Matrix
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
Return the number of rows in the Matrix.
@return the number of rows in the Matrix.
*/
int Matrix :: getNumRows()
{    return( _n_rows );
}

//------------------------------------------------------------------------------
// Matrix :: getNumCols - Return the number of cols in the Matrix
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     integer number of cols in the Matrix
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
Return the number of columns in the Matrix.
@return the number of columns in the Matrix.
*/
int Matrix :: getNumCols()
{    return( _n_cols );
}

//------------------------------------------------------------------------------
// Matrix :: initialize - Initialize members
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     
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
Initialize - initializes private data members - matrix size is set to zero by
zero.
*/
void Matrix :: initialize()
{    _data = NULL;
    _n_rows = 0;
    _n_cols = 0;
}

//------------------------------------------------------------------------------
// Matrix :: setMatrix - Puts a single value into all data points of the matrix
//                       array
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     Integer defining successful construction of the Matrix data array
// Calls:
//     PrintWarning( int, char*, char* )
//     allocateDataSpace()
// Errors:
//     None
// Warnings:
//     Inability to allocate memory.
// Debug:
//     None
//------------------------------------------------------------------------------
/**
Fill the contents of the matrix.
@param rows Number of rows.
@param cols Number of columns.
@param value Value of all elements.
@return 1 if an error occurs, 0 if success.
*/
int Matrix :: setMatrix ( int rows, int cols, double value )
{
    char routine[] = "Matrix.setMatrix";
    int i, j, k = 0;

    if( rows < 1 || cols < 1 )
    {
        PrintWarning( 1, routine, "Cannot set matrix with the number "
                    "rows and/or columns < 1." );
        return 1;
    } 
    // Convert the 1-D array of values into a matrix, setting it in
    // row-major order
    _n_rows = rows;
    _n_cols = cols;

    // allocate some memory
    if( allocateDataSpace() )
    {
        PrintWarning( 1, routine, "Cannot allocate %d doubles.", 
                    rows*cols );
        return 1;
    }    

    for( i = 0; i < _n_rows; i++ )
    {
        for( j = 0; j < _n_cols; j++ )
    {
            _data[k] = value;
            k++;
        }
    }

    return 0;
}

//------------------------------------------------------------------------------
// Matrix :: setMatrix - Puts data into the matrix array
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     Integer defining successful construction of the Matrix data array
// Calls:
//     PrintWarning( int, char*, char* )
//     allocateDataSpace()
// Errors:
//     None
// Warnings:
//     Inability to allocate memory.
// Debug:
//     None
//------------------------------------------------------------------------------
/**
Fill the contents of the matrix.
@param rows Number of rows.
@param cols Number of columns.
@param values Value of all elements.
@return 1 if an error occurs, 0 if success.
*/
int Matrix :: setMatrix( int rows, int cols, double* values )
{
    char routine[] = "Matrix.setMatrix";
    int i, j, k = 0;

    if( rows < 1 || cols < 1 )
    {
        PrintWarning( 1, routine, "Cannot set matrix with the number rows "
            "and/or columns < 1." );
        return 1;
    } 
    if( values == NULL )
    {
        PrintWarning(1,routine, "Cannot set matrix with NULL data.");
        return 1;
    }

    // Convert the 1-D array of values into a matrix, setting it in
    // row-major order
    _n_rows = rows;
    _n_cols = cols;

    // allocate some memory
    if( allocateDataSpace() )
    {
        PrintWarning( 1, routine, "Cannot allocate %d double.", rows*cols );
        return 1;
    }

    for( i = 0; i < _n_rows; i++ )
    {
        for( j = 0; j < _n_cols; j++ )
    {
            _data[k] = values[k];
            k++;
        }
    }

    return 0;

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_system/RCS/Matrix.cxx,v $";
 static char rcs_id2[] = "$Id: Matrix.cxx,v 1.1 2006/10/26 15:25:47 hsu Exp $";}
/*  ===================================================  */

}

