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

#ifndef ReferencedMatrix_INCLUDED
#define ReferencedMatrix_INCLUDED

#include "Matrix.h"
//JRV #include "Object.h"

//JRV class ReferencedMatrix : public Object
class ReferencedMatrix
{
public:

    enum {
        M_INTERPOLATE_LOWER   = 0,
        M_INTERPOLATE_X       = 1,
        M_INTERPOLATE_Y       = 2,
        M_INTERPOLATE_ALL     = 3
    };
        // X is rows, Y is columns.  The numbering is consistent with other
        //  RES-J interpolation.

    // Output formats for toString()...
    ReferencedMatrix();
    ReferencedMatrix( const ReferencedMatrix& );
    ~ReferencedMatrix();

    ReferencedMatrix& operator= (const ReferencedMatrix&);

    double getDataValue ( double x, double y, int flag);    // Get the data
                                            // value according to the flag
                                            // which specifies an interpola-
                                            // tion or a block distribution.
    double getDataValue ( double x, double y, int flag, int* row, int* col);
            // Get the data, and pass back the related column and row.

    int getIndexInteger( double val, int flag );
            // flag = 0, look at _xValues array.
            // flag = 1, look at _yValues array.

    int populate(
        double *x, int xNum, double *y, int yNum, double *data, int dNum );
            // Creates the object
            //     x is an array of the row indexes
            //     xNum is the number of row indexes in the array
            //     y is an array of the column indexes
            //     yNum is the number of column indexes in the array
            //     data is an array of all the table values not including
            //         indexes
            //     dNum is the number of values in the data array

//JRV     int    parseFormattedStrings( const String &X,
//JRV         const String &Y, const String &Data);

    //from Object:
//JRV     Object* clone () { return NULL;};
//JRV     String    getClassName () const { return "ReferencedMatrix"; };
//JRV     String     toString () const;

private:
    Matrix _dataValues;
    double* _xValues;
    double* _yValues;
    int _numX, _numY;

    void initialize();
};


#endif
