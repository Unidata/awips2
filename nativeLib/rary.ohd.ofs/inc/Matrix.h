//------------------------------------------------------------------------------
// Matrix class definition.
//------------------------------------------------------------------------------
// Copyright:	See the COPYRIGHT file.
//------------------------------------------------------------------------------
// Notes:	(1) Template class that can store any data type in a row/column
//			format.
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

#ifndef Matrix_INCLUDED
#define Matrix_INCLUDED

//JRV #include "Object.h"
//JRV #include "String.h"

class StringList;

//JRV class Matrix : public Object
class Matrix
{
public:
	// Output formats for toString()...

	enum {
		FORMAT_ONE_STRING = 1,
		FORMAT_AS_ROWS = 2
	};

	Matrix();			// Default constructor.

	Matrix ( const Matrix& );	// Copy constructor.
	Matrix ( int row, int col, double value );
					//Constructor that sets the matrix

	// Template classes need no destructor, but we will use
	// one for consistency
	virtual ~Matrix();		// Destructor

//JRV 	virtual Matrix& operator= ( const Matrix& );
//JRV 					// Assignment operator.
	static Matrix* add ( Matrix*, Matrix* );	
	static Matrix* add ( Matrix*, double );	

//JRV 	Object* clone () const;
//JRV 	String	getClassName () const { return "Matrix"; };
	double 	getElement( int row, int col );
	double 	getElement( int col );
				// Gets the piece of data at _data[row][col]

	double*	get1DArray();	// Returns an array representing the row-major
				// 1-D equivalent of the Matrix 
	double** get2DArray();	// Returns a 2-d double array 

	int 	getNumCols();
	int	getNumRows();
	static Matrix*	ident( Matrix* );
	static Matrix*	invert( Matrix* );

	static void ludcmp ( double**, int, int*, double* );
	static void lubksb ( double**, int, int*, double* );
	
	static Matrix* mult( Matrix*, Matrix* );
	static Matrix* mult( Matrix*, double );
//JRV 	int	parse( const String& );
//JRV 	int	parseFormattedString( const String& );

	int	setElement( int row, int col, double value );
	int	setElement( int row, double value );
				// Sets _data[row][col] to value.

	int 	setMatrix( int row, int col, double** value );
				// Set _data to value

	int	setMatrix( int row, int col, double* value );
				// Break value up into rows and columns,
				// then set to _data.
	int	setMatrix( int row, int col, double value );
				// Break value up into rows and columns,
				//then set to value (used as an initialization).
	static Matrix* subtract( Matrix*, Matrix* );	

//JRV 	String 	toString () const;
//JRV 	String 	toString ( int format ) const;
//JRV 	String 	toString ( int format, const String &format_string ) const;

	static Matrix* transmult( Matrix*, Matrix* );
	static Matrix* transpose( Matrix* );
 	
protected:
	int 	allocateDataSpace();
				// allocates _n_rows by _n_cols of double 
				// data.
 
//JRV 	String	getFormattedMatrixString ( const String & ) const;
				// Returns a single string representation
				// of getRowStrings.
//JRV 	StringList*	getRowStrings( int*, int*, const String & ) const;
				// Returns a string list, with each element
				// of the list corresponding to a row in the 
				// matrix.


	void	initialize();
	int 	_n_rows,	// Number of rows in the matrix
		_n_cols;	// Number of columns in the matrix

	double	*_data;		// One dimensional structure that holds the
				// generic data. This can hold either vector
				// or matrix structures - a 1-D data structure
				// used b/c it is more efficient than its
				// 2-D counterpart.
	
};

#endif
