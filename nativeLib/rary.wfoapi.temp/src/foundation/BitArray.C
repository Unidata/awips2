// ----------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without
// technical support, and with no warranty, express or implied, as
// to its usefulness for any purpose.
//
// BitArray.C
// An array of bits.
//-----------------------------------------------------------------------------

#ifdef IDENT_C
static const char* const BitArray_C_Id =
"$Id: .BitArray.C__temp27950,v 1.3 2003/05/06 23:11:33 fluke Exp $";
#endif

// -- module ------------------------------------------------------------------
// The bit array is commonly used to store flags. The size of the bit array
// is specified upon creation and then remains fixed during
// the lifetime of the array (except if another array is assigned to it).
// The C language does not support bit arrays nor does it support array
// array bounds checking. This class overcomes these shortcomings.
//
// The bit array can be initialized in one step from an C array pointer.
// Individual bits, may be retrieved and stored.
//
// An invalid BitArray object represents an empty array without data.
//
// Bounds checking is performed on all indexing operations. Attempting to
// access an element outside the bounds results in a call to LogStream
// which will complain and then stumble on.
//
// Initialization of the array (through the constructor) is accomplished by
// passing a pointer to a block of contiguous memory. The memory should be
// large enough to hold nbits elements.
// This class cannot check for valid initialization data or pointer to
// memory that is of sufficient size.
//
// However, a function dataSize() returns the size of the memory needed
// in bytes.
//
// In the event that no pointer is passed to the initialization constructor,
// then the array is filled with bit values of zero.
//
// -- implementation ----------------------------------------------------------
// Bit Array is implemented as a one-dimensional contiguous block of memory.
// Indexing into the grid is accomplished by determining the bit number in
// the array, accessing the byte, and extracting/inserting the bit.
//-----------------------------------------------------------------------------
#include "BitArray.H"
#include "LogStream.H"
#include <stdlib.h>
#include <string.h>

// -- public ------------------------------------------------------------------
// BitArray::BitArray()
// Constructor for creating an array of bits.
// Default constructor creates an invalid grid.
//-----------------------------------------------------------------------------
BitArray::BitArray()
    {
    setInvalid();
    }

// -- public ------------------------------------------------------------------
// BitArray::BitArray()
// Constructor for creating an array of bits.
// size specifies the size of the grid. data is a pointer to
// initialization data or zero (NULL) for no data.
// -- implementation ----------------------------------------------------------
// Uses setupNewArray() and initData() to allocate the memory for the
// new array and to initialize the memory.
//-----------------------------------------------------------------------------
BitArray::BitArray(unsigned int size, const byte *data)
    {
    if (size == 0)
        {
        setInvalid();
        }
    else
        {
        setupNewArray(size);
        initData(data);
        }
    }

// -- public ------------------------------------------------------------------
// BitArray::BitArray()
// Copy constructor for BitArray.
// -- implementation ----------------------------------------------------------
// Uses copyArray() to copy the specified array into this object.
//-----------------------------------------------------------------------------
BitArray::BitArray(const BitArray& old)
    {
    setInvalid();
    copyArray(old);
    }

// -- public ------------------------------------------------------------------
// BitArray::~BitArray()
// Destructor for BitArray.
// -- implementation ----------------------------------------------------------
// Simply deallocates the byte array.
//-----------------------------------------------------------------------------
BitArray::~BitArray()
    {
    delete[] _data;
    }

// -- public ------------------------------------------------------------------
// BitArray::operator==()
// Equality operator for BitArray.  Returns true if two arrays are equal.
// Invalid grids are never equal to anything including other invalid grids.
// -- implementation ----------------------------------------------------------
// Must be the same size and have the same data to be equal.
//-----------------------------------------------------------------------------
bool BitArray::operator==(const BitArray& rhs) const
    {
    if (_size == rhs._size && _arraySize
      && !memcmp(_data, rhs._data, _arraySize))
        return true;

    return false;
    }

// -- public ------------------------------------------------------------------
// BitArray::operator!=()
// Inequality operator for BitArray.  Returns true if two arrays are not
// equal. Returns true for invalid arrays.
// -- implementation ----------------------------------------------------------
// Uses operator==(BitArray).
//-----------------------------------------------------------------------------
bool BitArray::operator!=(const BitArray& rhs) const
    {
    return !operator==(rhs);
    }

// -- public ------------------------------------------------------------------
// BitArray::operator=()
// Assignment operator for BitArray.
// -- implementation ----------------------------------------------------------
// Uses copyArray() to copy the specified array over this object.  Checks
// for the case of a=a.  If the array is the same dimensions as the
// current array, then a high-efficiency memcpy is performed which simply
// copies over the data without free/new being performed.
//-----------------------------------------------------------------------------
BitArray& BitArray::operator=(const BitArray& rhs)
    {
    if (this == &rhs)
        return *this;  // same array - do nothing

    // high-efficiency case where sizes are identical
    if (rhs._size == _size)
        memcpy(_data, rhs._data, _arraySize);
    else
        copyArray(rhs);

    return *this;
    }

// -- public ------------------------------------------------------------------
// BitArray::operator|=()
// Logical 'OR' operator for BitArray.
// -- implementation ----------------------------------------------------------
// Sizes must be identical for this operation to work.  Each byte of the
// internal data structure is logical "OR"'d to the passed in data
// structure.
//-----------------------------------------------------------------------------
BitArray& BitArray::operator|=(const BitArray& rhs)
    {
    if (this == &rhs)
        return *this;  // same array - do nothing

    // sizes must be identical
    if (rhs._size == _size && _data)
        {
        for (unsigned int i = 0; i < _arraySize; i++)
            _data[i] |= rhs._data[i];
        }
    else
        logBug << "BitArray OR operator on different sizes -- IGNORED"
          << std::endl;

    return *this;
    }

// -- public ------------------------------------------------------------------
// BitArray::negate()
// Logical '!' operator for BitArray.
// -- implementation ----------------------------------------------------------
// Each bit in this BitArray is negated.
//-----------------------------------------------------------------------------
void BitArray::negate()
    {
    for (unsigned int i = 0; i < _arraySize; i++)
        _data[i] = ~_data[i];
    }

// -- public ------------------------------------------------------------------
// BitArray::clear()
// Logical clear operator for BitArray
// -- implementation ----------------------------------------------------------
// Each bit in this BitArray is cleared.
//-----------------------------------------------------------------------------
void BitArray::clear()
    {
    if (_data)
        memset(_data, '\0', _arraySize);
    }

// -- public ------------------------------------------------------------------
// BitArray::operator&=()
// Logical 'AND' operator for BitArray.
// -- implementation ----------------------------------------------------------
// Sizes must be identical for this operation to work.  Each byte of the
// internal data structure is logical "AND"'d to the passed in data
// structure.
//-----------------------------------------------------------------------------
BitArray& BitArray::operator&=(const BitArray& rhs)
    {
    if (this == &rhs)
        return *this;  // same grid - do nothing

    // sizes must be identical
    if (rhs._size == _size && _data)
        {
        for (unsigned int i = 0; i < _arraySize; i++)
            _data[i] &= rhs._data[i];
        }
    else
        logBug << "BitArray AND operator on different sizes -- IGNORED"
          << std::endl;

    return *this;
    }

// -- public ------------------------------------------------------------------
// BitArray::operator^=()
// Logical 'XOR' operator for BitArray.
// -- implementation ----------------------------------------------------------
// Sizes must be identical for this operation to work.  Each byte of the
// internal data structure is logical "XOR"'d to the passed in data
// structure.
//-----------------------------------------------------------------------------
BitArray& BitArray::operator^=(const BitArray& rhs)
    {
    if (this == &rhs)
        return *this;  // same grid - do nothing

    // sizes must be identical
    if (rhs._size == _size && _data)
        {
        for (unsigned int i = 0; i < _arraySize; i++)
            _data[i] ^= rhs._data[i];
        }
    else
        logBug << "BitArray XOR operator on different sizes -- IGNORED"
          << std::endl;

    return *this;
    }

// -- public ------------------------------------------------------------------
// BitArray::clear()
// Clears the specified bit. If the position is
// out-of-bounds or if this is an invalid array, then no bits are changed.
// -- implementation ----------------------------------------------------------
// Uses validLocation() to obtain the array index and mask for which the
// individual bit may be accessed.
//-----------------------------------------------------------------------------
void BitArray::clear(unsigned int position)
    {
    unsigned int word;
    byte mask;

    // determine the proper bit
    if (!validLocation(position, word, mask)) // sets word and mask
        return;  // problem -- stumble on

    _data[word] &= ~mask;    // clear the bit
    }

// -- private -----------------------------------------------------------------
// BitArray::setInvalid()
// Sets the array to a known initial state.  The initial state is an invalid
// array containing no data.
// -- implementation ----------------------------------------------------------
// The invalid array state is indicated by the size being 0 and the
// memory pointer _data being zero.
//-----------------------------------------------------------------------------
void BitArray::setInvalid()
    {
    _size = 0;
    _data = 0;
    _arraySize = 0;
    }

// -- private -----------------------------------------------------------------
// BitArray::copyArray()
// Copies data from the specified array to this array.
// -- implementation ----------------------------------------------------------
// The current array is deallocated using cleanupMemory().  The routines
// setupNewArray() and initData() are called to create a copy of the
// specified array.
//-----------------------------------------------------------------------------
void BitArray::copyArray(const BitArray& source)
    {
    delete [] _data;
    if (!source.isValid())
        setInvalid();
    else
        {
        setupNewArray(source.size());
        initData(source.dataPtr());
        }
    }

// -- private -----------------------------------------------------------------
// BitArray::setupNewArray()
// Creates memory for new array and sets the private data members
// appropriately.  size is the new size of the array.
// -- implementation ----------------------------------------------------------
// The memory for the array is contiguous.
//-----------------------------------------------------------------------------
void BitArray::setupNewArray(unsigned int size)
    {
    _size = size;
    calcArraySize();  // set the _arraySize variable
    if (_size == 0)
        {
        _data = 0;
        return;
        }

    _data = new byte [_arraySize];
    }

// -- private -----------------------------------------------------------------
// BitArray::initData()
// Fills the array with initialization data or zeros. data is a pointer to
// the initialization data.  If zero, then the array is initialized
// with zeros.
// -- implementation ----------------------------------------------------------
// This routine can be called for the invalid array, for which it does nothing.
//-----------------------------------------------------------------------------
void BitArray::initData(const byte *data)
    {
    if (!isValid())
        return;  // no array -- this is an invalid array

    // fill data with zero bytes if no data passed
    if (!data)
        memset(_data, '\0', _arraySize);
    else
        memcpy(_data, data, _arraySize);
    }


// -- private -----------------------------------------------------------------
// BitArray::outOfRange()
// Error handler for index out of range for current array.  Error
// handler for invalid array accesses.
// -- implementation ----------------------------------------------------------
// Logs the appropriate error, "array index out of range" or "invalid array
// access".
//-----------------------------------------------------------------------------
void BitArray::outOfRange(unsigned int index) const
    {
    if(_size != 0)
        logBug << "array index out of range, user-specified="
          << index << " maximum is" << _size - 1 << std::endl;
    else
        logBug << "access to invalid array" << std::endl;
    }

// -- public ------------------------------------------------------------------
// BitArray::calcArraySize()
// Sets up the size of array in bytes pointed to by arrayPtr().  Sets the
// value _arraySize.
//-----------------------------------------------------------------------------
void BitArray::calcArraySize()
    {
    if (!isValid())
        {
        _arraySize = 0;
        return;
        }
    if (_size % BITSPERBYTE == 0)
        _arraySize = _size/BITSPERBYTE;
    else
        _arraySize = (_size/BITSPERBYTE)+1; // a few bits in the last byte
    }

// -- global ------------------------------------------------------------------
// operator<<()
// Output operator for BitArray.
// -- implementation ----------------------------------------------------------
// The dimensions of the array and each element in the array is output.
//-----------------------------------------------------------------------------
std::ostream& operator<<(std::ostream& o, const BitArray& array)
    {
    if(!array.isValid())
        return o << "[invalid array]";

    o << "size=[" << array.size() << "] ";
    for (unsigned int i = 0; i < array.size(); i++)
        o << array.get(i);
    return o;
    }
