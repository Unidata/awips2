// ----------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without
// technical support, and with no warranty, express or implied, as
// to its usefulness for any purpose.
//
// BitArray.H
// Array of bit values.
//-----------------------------------------------------------------------------
#ifndef _BitArray_H
#define _BitArray_H

#ifdef IDENT_H
static const char* const BitArray_H_Id =
"$Id: .BitArray.H__temp27950,v 1.2 2003/05/06 23:11:33 fluke Exp $";
#endif

#include "commonDefs.h"
#include <iostream>
#include <values.h>

class BitArray
    {
    public:

        // constructors and destructors
        BitArray();
        BitArray(unsigned int size, const byte *data = 0);
        BitArray(const BitArray& old); // copy constructor
        ~BitArray();

        bool operator==(const BitArray& rhs) const;
        bool operator!=(const BitArray& rhs) const;

        // operators
        BitArray& operator=(const BitArray& rhs);
        BitArray& operator|=(const BitArray& rhs);
        BitArray& operator&=(const BitArray& rhs);
        BitArray& operator^=(const BitArray& rhs);
        void negate(); // negates the bits in the entire array
        void clear(); // clears all bits in the entire array

        // extracting and setting individual components
        bool get(unsigned int pos) const;
        void set(unsigned int pos);
        void clear(unsigned int pos);

        // extraction of contiguous memory array
        const byte *dataPtr() const;    // returns pointer to data
        unsigned int dataSize() const;  // returns size of data in bytes

        // size information
        unsigned int size() const;

        // valid array check
        bool isValid() const; // returns true if real data

    protected:
        unsigned int _size;     // size of array (0=null array)
        byte *_data;  // block of memory containing array
        unsigned int _arraySize; // number of bytes in array

    private:
        void calcArraySize();  // calculates the array size
        void setInvalid(); // sets private data to indicate invalid array
        bool validLocation(unsigned int location, unsigned int& word,
          byte& mask) const;
        void copyArray(const BitArray& source); // copies to this object
        void setupNewArray(unsigned int size); // sets up memory for array
        void initData(const byte *data);  // copies data into new array
        bool checkRange(unsigned int size) const;  // bounds-checking
        void outOfRange(unsigned int size) const;  // called if out-of-bounds

    };

// operator <<
std::ostream& operator<<(std::ostream& o, const BitArray& array);

// -- public ------------------------------------------------------------------
// BitArray::size()
// Returns the size of the array or 0 if this is an invalid array.
//-----------------------------------------------------------------------------
inline unsigned int BitArray::size() const
    {
    return _size;
    }

// -- public ------------------------------------------------------------------
// BitArray::isValid()
// Returns true if this is a valid array (containing data).
// -- implementation ----------------------------------------------------------
// An invalid array is defined as having no data, i.e., its size is 0.
//-----------------------------------------------------------------------------
inline bool BitArray::isValid() const
    {
    return !(_size==0);
    }

// -- private -----------------------------------------------------------------
// BitArray::checkRange()
// Checks whether specified location is within range and returns
// true if the location is valid. Returns false if the location is
// bad or if it is an invalid array.
//-----------------------------------------------------------------------------
inline bool BitArray::checkRange(unsigned int size) const
    {
    if(size >= _size)
        return false;
    return true;
    }

// -- public ------------------------------------------------------------------
// BitArray::dataPtr()
// Returns a pointer to a contiguous block of memory containing the
// array.  This routine is useful when a C array pointer is required.
//-----------------------------------------------------------------------------
inline const byte *BitArray::dataPtr() const
    {
    return _data;
    }

// -- public ------------------------------------------------------------------
// BitArray::dataSize()
// Returns the size of the data in bytes.
//-----------------------------------------------------------------------------
inline unsigned int BitArray::dataSize() const
    {
    return _arraySize;
    }

// -- private -----------------------------------------------------------------
// BitArray::validLocation()
// Figures out bit location in array given the location.  Returns
// through the calling argument list the byte number and a mask that can
// be used to access the array.  Returns true if valid location, false
// if not valid location.
// -- implementation ----------------------------------------------------------
// If the passed location is out-of-range, this routine calls the
// error handler, and returns 0 for the word and mask.
//-----------------------------------------------------------------------------
inline bool BitArray::validLocation(unsigned int location,
  unsigned int& word, byte& mask) const
    {
    if (!checkRange(location))
        {
        outOfRange(location);
        word = 0;
        mask = 0;
        return false;
        }

    word = (location / BITSPERBYTE);
    unsigned int bitNumber = (location % BITSPERBYTE);
    mask = 1 << bitNumber;
    return true;
    }


// -- public ------------------------------------------------------------------
// BitArray::get()
// Returns true if the specified bit is set, false if not set. Returns false
// if the position is out-of-range or if this is an invalid array.
// -- implementation ----------------------------------------------------------
// Uses validLocation() to obtain the array index and mask for which the
// individual bit may be accessed.
//-----------------------------------------------------------------------------
inline bool BitArray::get(unsigned int location) const
    {
    unsigned int word;
    byte mask;

    // calculate the bit location
    if (!validLocation(location, word, mask)) // returns data in word, mask
        return false;

    if (_data[word] & mask)
        return true;

    return false;
    }

// -- public ------------------------------------------------------------------
// BitArray::set()
// Sets the specified bit. If the location is
// out-of-bounds or if this is an invalid array, then no bits are changed.
// -- implementation ----------------------------------------------------------
// Uses validLocation() to obtain the array index and mask for which the
// individual bit may be accessed.
//-----------------------------------------------------------------------------
inline void BitArray::set(unsigned int position)
    {
    unsigned int word;
    byte mask;

    // determine the proper bit
    if (!validLocation(position, word, mask)) // sets word and mask
        return;  // problem -- stumble on

    _data[word] |= mask;    // set the bit
    }

#endif
