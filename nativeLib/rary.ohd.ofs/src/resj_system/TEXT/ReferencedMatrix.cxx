//------------------------------------------------------------------------------
// Referenced Matrix class definition.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Notes:    (1) Template class that can store any data type in a row/column
//            format.
//------------------------------------------------------------------------------
// History:
// 2006 Feb 06  James R. VanShaar, Riverside Technology, inc
//                                           Initial version based on RTi 
//                                           library code, rewritten and 
//                                           simplified for specific nwsrfs use.
//                   RTi library code written by Michael Thiemann (2003 Jan 14).
//------------------------------------------------------------------------------

#include <stdlib.h>
#include "ReferencedMatrix.h"

//------------------------------------------------------------------------------
// ReferencedMatrix :: ReferencedMatrix - Construct an empty ReferencedMatrix
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
Construct an empty ReferencedMatrix.
*/
ReferencedMatrix :: ReferencedMatrix()
{
    initialize();
}

//------------------------------------------------------------------------------
// ReferencedMatrix :: ~ReferencedMatrix - destructor
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
ReferencedMatrix :: ~ReferencedMatrix()
{
    if ( _xValues != NULL )
    {
        delete [] _xValues;
    }
    _xValues = NULL;

    if ( _yValues != NULL )
    {
        delete [] _yValues;
    }
    _yValues = NULL;
}


//------------------------------------------------------------------------------
// ReferencedMatrix :: initialize - Initialize members
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
void ReferencedMatrix :: initialize()
{    
    _xValues = NULL;
    _yValues = NULL;
}

//------------------------------------------------------------------------------
// ReferencedMatrix :: getDataValue - References the Matrix to determine a value
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     
// Calls:
//     Matrix::getElement( int, int )
// Errors:
//     None
// Warnings:
//     None
// Debug:
//     None
//------------------------------------------------------------------------------
/**
Return a data value from the data matrix.
@param x double x Value
@param y double y Value
@param flag int  interpolation flag
@return a matrix element.
*/
double ReferencedMatrix :: getDataValue ( double x, double y, int flag)
{

    //get the lower x and y values
    int i, j;
    int lowerXi = _numX - 1;
    int lowerYi = _numY - 1;
    int upperXi = _numX - 1;
    int upperYi = _numY - 1;

    double lowX, lowY;

    for( i = 0; i < _numX; i++ )
    {
        if( x < _xValues[i] )
        {
            if( i == 0 )
            {
                lowerXi = 0;
                upperXi = 0;
            }
            else
            {
                lowerXi = i-1;
                upperXi = i;
            }
            break;
        } // else if x>
    }
    lowX = _xValues[lowerXi];

    for( j = 0; j < _numY; j++ )
    {
        if( y < _yValues[j] )
        {
            if( j == 0 )
            {
                lowerYi = 0;
                upperYi = 0;
            }
            else
            {
                lowerYi = j-1;
                upperYi = j;
            }
            break;
        }
    }
    lowY = _yValues[lowerYi];

    double lowerXlowerY = _dataValues.getElement( lowerXi, lowerYi );
    double value;

    switch ( flag )
    {
        // flag = INTERPOLATE_LOWER, no interpolation--handle with default
        case M_INTERPOLATE_X:
        {
            double upX;
            upX = _xValues[ upperXi ];
        
            double upperXlowerY;
            upperXlowerY= _dataValues.getElement( upperXi, lowerYi );
        
            //interpolate the X values for the lower Y's
            double interpXlowerY;
            if( upX != lowX )
            {
                interpXlowerY = lowerXlowerY + (x - lowX )/(upX - lowX) *
                    ( upperXlowerY - lowerXlowerY );
            }
            else
            {
                interpXlowerY = lowerXlowerY;
            }
            value = interpXlowerY;
            break;
        }
        case M_INTERPOLATE_Y:
        {
            double upY;
            upY = _yValues[ upperYi ]; 
        
            double lowerXupperY;
            lowerXupperY= _dataValues.getElement( lowerXi, upperYi );
        
            //interpolate the y values for the lower X's
            double interpYlowerX;
            if( upY != lowY )
            {
                interpYlowerX = lowerXlowerY + (y - lowY )/(upY - lowY) *
                    ( lowerXupperY - lowerXlowerY );
            }
            else
            {
                interpYlowerX = lowerXlowerY;
            }
            value = interpYlowerX;
            break;
        }
        case M_INTERPOLATE_ALL:
        {
            double upX, upY;
            upX = _xValues[ upperXi ];
            upY = _yValues[ upperYi ]; 
        
            double lowerXupperY, upperXlowerY, upperXupperY;
            lowerXupperY= _dataValues.getElement( lowerXi, upperYi );
            upperXlowerY= _dataValues.getElement( upperXi, lowerYi );
            upperXupperY= _dataValues.getElement( upperXi, upperYi );
        
            //interpolate the X values for the lower Y's
            //interpolate the X values for the upper Y's    
            double interpXlowerY, interpXupperY;
            if( upX != lowX )
            {
                interpXlowerY = lowerXlowerY + (x - lowX )/(upX - lowX) *
                    ( upperXlowerY - lowerXlowerY );
                interpXupperY = lowerXupperY + (x - lowX )/(upX - lowX) *
                ( upperXupperY - lowerXupperY );
            }
            else
            {
                interpXlowerY = lowerXlowerY;
                interpXupperY = lowerXupperY;
            }

            //interpolate the Y values
            double interpData;
            if( upY != lowY )
            {
                interpData= interpXlowerY +  (y - lowY )/(upY - lowY) *
                    ( interpXupperY - interpXlowerY );
            }
            else
            {
                interpData= interpXlowerY;
                
            }
            value = interpData;
            break;
        }
        default:
        {
            value = lowerXlowerY;
            break;
        }
    }

    return value;
}

//------------------------------------------------------------------------------
// ReferencedMatrix :: getDataValue - References the Matrix to determine a value
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     
// Calls:
//     Matrix::getElement( int, int )
// Errors:
//     None
// Warnings:
//     None
// Debug:
//     None
//------------------------------------------------------------------------------
/**
Return a data value from the data matrix and modifies two indexing integers.
@param x double x Value
@param y double y Value
@param flag int  interpolation flag
@param lowerXi int lower index of the x array
@param lowerYi int lower index of the y array
@return a matrix element.
*/
double ReferencedMatrix :: getDataValue ( double x, double y, int flag,
    int* xI, int* yI )
{

    //get the lower x and y values
    int i, j;
    int lowerXi = _numX - 1;
    int lowerYi = _numY - 1;
    int upperXi = _numX - 1;
    int upperYi = _numY - 1;

    double lowX, lowY;

    for( i = 0; i < _numX; i++ )
    {
        if( x < _xValues[i] )
        {
            if( i == 0 )
            {
                lowerXi = 0;
                upperXi = 0;
            }
            else
            {
                lowerXi = i-1;
                upperXi = i;
            }
            break;
        } // else if x>
    }
    lowX = _xValues[lowerXi];

    for( j = 0; j < _numY; j++ )
    {
        if( y < _yValues[j] )
        {
            if( j == 0 )
            {
                lowerYi = 0;
                upperYi = 0;
            }
            else
            {
                lowerYi = j-1;
                upperYi = j;
            }
            break;
        }
    }
    lowY = _yValues[lowerYi];

    double lowerXlowerY = _dataValues.getElement( lowerXi, lowerYi );
    double value;

    switch ( flag )
    {
        // flag = INTERPOLATE_LOWER, no interpolation--handle with default
        case M_INTERPOLATE_X:
        {
            double upX;
            upX = _xValues[ upperXi ];
        
            double upperXlowerY;
            upperXlowerY= _dataValues.getElement( upperXi, lowerYi );
        
            //interpolate the X values for the lower Y's
            double interpXlowerY;
            if( upX != lowX )
            {
                interpXlowerY = lowerXlowerY + (x - lowX )/(upX - lowX) *
                    ( upperXlowerY - lowerXlowerY );
            }
            else
            {
                interpXlowerY = lowerXlowerY;
            }
            value = interpXlowerY;
            break;
        }
        case M_INTERPOLATE_Y:
        {
            double upY;
            upY = _yValues[ upperYi ]; 
        
            double lowerXupperY;
            lowerXupperY= _dataValues.getElement( lowerXi, upperYi );
        
            //interpolate the y values for the lower X's
            double interpYlowerX;
            if( upY != lowY )
            {
                interpYlowerX = lowerXlowerY + (y - lowY )/(upY - lowY) *
                    ( lowerXupperY - lowerXlowerY );
            }
            else
            {
                interpYlowerX = lowerXlowerY;
            }
            value = interpYlowerX;
            break;
        }
        case M_INTERPOLATE_ALL:
        {
            double upX, upY;
            upX = _xValues[ upperXi ];
            upY = _yValues[ upperYi ]; 
        
            double lowerXupperY, upperXlowerY, upperXupperY;
            lowerXupperY= _dataValues.getElement( lowerXi, upperYi );
            upperXlowerY= _dataValues.getElement( upperXi, lowerYi );
            upperXupperY= _dataValues.getElement( upperXi, upperYi );
        
            //interpolate the X values for the lower Y's
            //interpolate the X values for the upper Y's    
            double interpXlowerY, interpXupperY;
            if( upX != lowX )
            {
                interpXlowerY = lowerXlowerY + (x - lowX )/(upX - lowX) *
                    ( upperXlowerY - lowerXlowerY );
                interpXupperY = lowerXupperY + (x - lowX )/(upX - lowX) *
                ( upperXupperY - lowerXupperY );
            }
            else
            {
                interpXlowerY = lowerXlowerY;
                interpXupperY = lowerXupperY;
            }

            //interpolate the Y values
            double interpData;
            if( upY != lowY )
            {
                interpData= interpXlowerY +  (y - lowY )/(upY - lowY) *
                    ( interpXupperY - interpXlowerY );
            }
            else
            {
                interpData= interpXlowerY;
                
            }
            value = interpData;
            break;
        }
        default:
        {
            value = lowerXlowerY;
            break;
        }
    }

    // Now set the indexes
    *xI = lowerXi;
    *yI = lowerYi;

    return value;
}

//------------------------------------------------------------------------------
// ReferencedMatrix :: getIndexInteger - Returns an index to the matrix indexes
//                                       corresponding to a value.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     Integer index in one of the ReferencedMatrix indexing arrays.
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
Return a data value from the data matrix.
@return integer index array index.
@param val double value to look up
@param flag int flag defining which array to use
*/
int ReferencedMatrix :: getIndexInteger ( double val, int flag )
{
    int lowerXi = _numX - 1;
    int lowerYi = _numY - 1;
    int index;

    // If flag = 1, search the _xValues array.
    if( flag )
    {
        int i;
        for( i = 0; i < _numX; i++ )
        {
            if( val < _xValues[i] )
            {
                if( i == 0 )
                {
                    lowerXi = 0;
                }
                else
                {
                    lowerXi = i-1;
                }
                break;
            }
        }
        index = lowerXi;
    }
    // Otherwise, search the _yValues array.
    else
    {
        int j;
        for( j = 0; j < _numY; j++ )
        {
            if( val < _yValues[j] )
            {
                if( j == 0 )
                {
                    lowerYi = 0;
                }
                else
                {
                    lowerYi = j-1;
                }
                break;
            }
        }
        index = lowerYi;
    }

    return index;
}

//------------------------------------------------------------------------------
// ReferencedMatrix :: populate - Populate the ReferencedMatrix arrays
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Return: 
//     int specifying success of population.
// Calls:
//     Matrix::setMatrix( int, int, double* )
// Errors:
//     None
// Warnings:
//     None
// Debug:
//     None
//------------------------------------------------------------------------------
/**
Return a data value from the data matrix.
@return integer index array index.
@param x double array for indexing x
@param xNum integer number of values in the x array
@param y double array for indexing y
@param yNum integer number of values in the y array
@param data double array containing the values in the matrix (table).
@param dNum integer number of values in the data array
*/
int ReferencedMatrix :: populate( 
    double *x, int xNum, double *y, int yNum, double *data, int dNum )
{

    int i;

    if( _xValues ) delete [] _xValues;
    if( _yValues ) delete [] _yValues;

    _numX = xNum;
    _numY = yNum;

    _xValues = new double[ _numX ];
    for( i = 0; i < _numX; i ++ ) {
        _xValues[i] = x[i];
    }

    _yValues = new double[ _numY ];
    for( i = 0; i < _numY; i ++ ) {
        _yValues[i] = y[i];
    }

    if( _dataValues.setMatrix(_numY, _numX , 0.0) ) {
        return 1;
    }

    return _dataValues.setMatrix( _numX, _numY, data );

/*  ==============  Statements containing RCS keywords:  */
{static char rcs_id1[] = "$Source: /fs/hseb/ob81/ohd/ofs/src/resj_system/RCS/ReferencedMatrix.cxx,v $";
 static char rcs_id2[] = "$Id: ReferencedMatrix.cxx,v 1.1 2006/10/26 15:30:32 hsu Exp $";}
/*  ===================================================  */

}
