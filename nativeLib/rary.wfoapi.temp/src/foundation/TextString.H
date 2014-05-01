// ----------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without
// technical support, and with no warranty, express or implied, as
// to its usefulness for any purpose.
//
// TextString.H
// Text String Class
//
// Modifications:
// 02/14/02 David Friedman   Made TextString(const char *, unsigned int) public
//                           and more efficient.
//                           Added TextString::assign. (DR 10321)
// 03/15/02 David Friedman   Validate substring lengths in ::left and ::mid
//-----------------------------------------------------------------------------
#ifndef TextString_H
#define TextString_H

#include "RCObject.H"
#include "RCPtr.H"
#include "utilityFuncs.H"
#include <iostream>
#include <sstream>
#include <string.h>

class TextString
    {
    public:
        // search direction for found()
        enum SearchDir { SEARCH_FWD, SEARCH_BCK };

        TextString();
        TextString(const char *ivalue);
        TextString(char *ivalue);
        TextString(const char *ivalue, unsigned int length);

        TextString& operator=(const char rhs);

	void assign(const char *new_value, unsigned int new_length);

        // getting the guaranteed initialized empty string
        static const TextString& emptyString();

        // part (char* & length) access
        unsigned int length() const;
        const char *stringPtr() const;

        // concatenation
        TextString& operator+=(const TextString& rhs);
        TextString& operator+=(const char *rhs);
        TextString& operator+=(const char c);
        TextString operator+(const TextString& rhs) const;
        friend TextString operator+(const char lhs, const TextString& rhs);
        friend TextString operator+(const char *lhs, const TextString& rhs);
        TextString operator+(const char rhs) const;

        // comparision operators
        bool operator==(const TextString& rhs) const;
        bool operator==(const char rhs) const;
        bool operator!=(const TextString& rhs) const;
        bool operator!=(const char rhs) const;
        bool operator>=(const TextString& rhs) const;
        bool operator<=(const TextString& rhs) const;
        bool operator<(const TextString& rhs) const;
        bool operator>(const TextString& rhs) const;

        int compare(const TextString& rhs) const;

        // finding a character, character set, or substring
        bool found(const TextString& searchString,
          unsigned int& foundPos, unsigned int startPos = 0,
          SearchDir dir=SEARCH_FWD) const;
        bool found(const char searchChar, unsigned int& foundPos,
          unsigned int startPos = 0, SearchDir dir=SEARCH_FWD) const;

        // extracting substrings
        TextString left(unsigned int length) const;
        TextString mid(unsigned int startPos, unsigned int subLength) const;
        TextString right(unsigned int length) const;

        // character ("indexing") access
        char operator[](int index) const;
        char& operator[](int index);

        // modification
        void insertBefore(unsigned int pos, const TextString& substring);
        void insertBefore(unsigned int pos, const char c);
        void insertAfter(unsigned int pos, const TextString& substring);
        void insertAfter(unsigned int pos, const char c);
        void remove(unsigned int startPos, unsigned int subLength);
        void toUpper(void);  // changes all alpha chars to uppercase
        void toLower(void);  // changes all alpha chars to lowercase
        void removeTrailSpcs(void);  // removes all trailing spaces
        void removeLeadingSpcs(void);  // removes all leading spaces

        std::ostream &printOn(std::ostream &os) const;

    private:
        // class representing string values
        struct StringValue: public RCObject
            {
                char *data;
                unsigned int length;
                unsigned int plength;

                StringValue(const char *initValue, bool overallocate=false)
                    { init(initValue, overallocate); }
                StringValue(const StringValue& rhs)
                    { init(rhs.data, false); }
                StringValue(const char *init_value, unsigned init_length)
                    {
                        plength = length = init_length;
                        data = new char[init_length + 1];
                        strncpy(data, init_value, init_length);
                        data[init_length] = '\0';
                    }
                void init(const char *initValue,
                  bool overallocate);
                void initOver(const char *initValue);
                virtual ~StringValue() FORCE_INLINE;
            };

        RCPtr<StringValue> value;

        char& getDummyCharPtr(unsigned int position);
        unsigned int checkIndex(unsigned int index) const;
        const char *strrstr(const char *str, const char *sstr) const;
        static RCPtr<StringValue> initSV(const char *v)
            {
            if (v == NULL || !strcmp(v, ""))
                return emptyString().value;
            return new StringValue(v);
            }
        static RCPtr<StringValue> initSV(const char *v, unsigned int length)
            {
	    if (v == NULL)
		return emptyString().value;
            return new StringValue(v, length);
            }
        void logit(int index) const;

        void appendAndCopy(const TextString &rhs, bool overallocate)
            {
            char *buf = (char *)alloca(length() + rhs.length() + 1);
            memmove(buf, value->data, value->length);
            memmove(buf + value->length, rhs.value->data,
              rhs.value->length + 1);
            value = new StringValue(buf, overallocate);
            }

        void appendAndCopy(char c, bool overallocate)
            {
            char *buf = (char *)alloca(length() + 2);
            strcpy(buf, this->value->data);
            buf[length()] = c;
            buf[length() + 1] = '\0';
            value = new StringValue(buf, overallocate);
            }

        void insertAndCopy(unsigned int pos, char c, bool overallocate);
        void insertAndCopy(unsigned int pos, const TextString &substring,
          bool overallocate);
    };

inline TextString::StringValue::~StringValue()
    {
    delete [] data;
    }

inline void TextString::StringValue::initOver(const char *initValue)
    {
    // must shrink/grow by at least this absolute,
    const int TS_THRESHOLD = 10;
    // and by at least this proportion
    const float TS_ALPHA = 0.10;
    length = strlen(initValue);
    plength = max((unsigned)(length * (1 + TS_ALPHA)),
      length + TS_THRESHOLD);
    data = new char[plength + 1];
    strcpy(data, initValue);
    }

inline void TextString::StringValue::init(const char *initValue,
  bool overallocate)
    {
    if (!overallocate)
        {
        plength = length = strlen(initValue);
        data = new char[length + 1];
        strcpy(data, initValue);
        }
    else
        initOver(initValue);
    }

inline TextString::TextString() : value(emptyString().value)
    {
    }

inline TextString::TextString(const char *ivalue) : value(initSV(ivalue))
    {
    }

inline TextString::TextString(char *ivalue) : value(initSV(ivalue))
    {
    }

// -- public ------------------------------------------------------------------
// TextString::assign()
//
// Assign a new value to the TextString from a character array and a maximum
// length.
//
// -- implementation ----------------------------------------------------------
//-----------------------------------------------------------------------------
inline void TextString::assign(const char *new_value, unsigned int new_length)
    {
	value = initSV(new_value, new_length);
    }

// -- public ------------------------------------------------------------------
// TextString::length()
//
// Returns the length of this TextString.
//
// -- implementation ----------------------------------------------------------
//-----------------------------------------------------------------------------
inline unsigned int TextString::length() const
    {
    return this->value->length;
    }

// -- private -----------------------------------------------------------------
// TextString::checkIndex()
// Routine to validate the index.  If invalid, then a message is issued
// and a "safe" index to use is returned.
// -- implementation ----------------------------------------------------------
// If the index is invalid, then the index returned is the _logSize since
// that always points to the final NULL terminating character.
// The character should always be NULL.
//-----------------------------------------------------------------------------
inline unsigned int TextString::checkIndex(unsigned int index) const
    {
    if (index >= length())
        {
        logit(index);
        return length();
        }

    return index;
    }

// -- public -----------------------------------------------------------------
// TextString::printOn()
//
// Prints this TextString on an ostream.
//
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
inline std::ostream &TextString::printOn(std::ostream &os) const
    {
    return os << this->value->data;
    }

// -- global -----------------------------------------------------------------
// ::operator<<()
//
// Prints a TextString on an ostream.
//
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
inline std::ostream& operator<<(std::ostream& os, const TextString& string)
    {
    return string.printOn(os);
    }


// -- public ------------------------------------------------------------------
// TextString::compare()
//
// Compares this TextString with the supplied TextString.  Returns an integer
// less than 0 of this is lexicographically less than rhs, 0 if they are
// equal and greater than 0 if this is greater than rhs.
//
// -- implementation ----------------------------------------------------------
// Since in most cases strings will be != on the first character,
// avoid the call to strcmp() unless they are ==.
//-----------------------------------------------------------------------------
inline int TextString::compare(const TextString& rhs) const
    {
    if (*this->value->data != *rhs.value->data)
        return *this->value->data - *rhs.value->data;

    return strcmp(this->value->data, rhs.value->data);
    }

// -- public ------------------------------------------------------------------
// TextString::operator==()
// Equality operator. Returns true if equal.
// -- implementation ----------------------------------------------------------
//-----------------------------------------------------------------------------
inline bool TextString::operator==(const char rhs) const
    {
    char *buf = (char *)alloca(2);
    buf[0] = rhs;
    buf[1] = '\0';
    return !compare(TextString(buf));
    }

// -- public ------------------------------------------------------------------
// TextString::operator==()
// Equality operator. Returns true if equal.
// -- implementation ----------------------------------------------------------
//-----------------------------------------------------------------------------
inline bool TextString::operator==(const TextString& rhs) const
    {
    if (&this->value == &rhs.value) // compare not needed
        return true;

    return !compare(rhs);
    }

// -- global -----------------------------------------------------------------
// ::operator==()
//
// Compares a TextString and a char.
//
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
inline bool operator==(const char lhs, const TextString& rhs)
    {
    return rhs == lhs;
    }

// -- global -----------------------------------------------------------------
// ::operator!=()
//
// Compares a TextString and a char.
//
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
inline bool operator!=(const char lhs, const TextString& rhs)
    {
    return !(rhs == lhs);
    }

// -- global -----------------------------------------------------------------
// ::operator==()
//
// Compares A TextString and a char *.
//
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
inline bool operator==(const char *lhs, const TextString& rhs)
    {
    return rhs == lhs;
    }

// -- global -----------------------------------------------------------------
// ::operator!=()
//
// Compares A TextString and a char *.
//
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
inline bool operator!=(const char *lhs, const TextString& rhs)
    {
    return !(rhs == lhs);
    }

// -- public -----------------------------------------------------------------
// TextString::operator<()
//
// Compares two TextStrings.
//
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
inline bool TextString::operator<(const TextString& rhs) const
    {
    return compare(rhs) < 0;
    }

// -- public -----------------------------------------------------------------
// TextString::operator>()
//
// Compares two TextStrings.
//
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
inline bool TextString::operator>(const TextString& rhs) const
    {
    return compare(rhs) > 0;
    }

// -- public -----------------------------------------------------------------
// TextString::insertAfter()
//
// Inserts the specified subString after the specified position.  If the
// specified position is out-of-bounds, then this routine functions as
// an append a TextString operation.
//
// -- implementation ---------------------------------------------------------
// Implemented in terms of insertBefore(pos, TextString).
// ---------------------------------------------------------------------------
inline void TextString::insertAfter(unsigned int pos,
  const TextString& substring)
    {
    insertBefore(++pos, substring);
    }

// -- public ------------------------------------------------------------------
// TextString::left()
// Returns a TextString consisting of the leftmost characters in
// this string.  The number of characters desired is specified.  If there
// aren't enough characters in this string to extract, then this routine
// returns the entire string.
// -- implementation ----------------------------------------------------------
// Uses the constructor TextString(const char *, length).
//-----------------------------------------------------------------------------
inline TextString TextString::left(unsigned int length) const
    {
    return TextString(this->value->data, min(this->length(), length));// use the private constructor
    }

// -- public ------------------------------------------------------------------
// TextString::mid()
// Returns a TextString which contains characters starting at the specified
// position and the specified length.  If there aren't enough characters
// to extract, then this routine will take as many as possible and will
// function like the right() routine.  If the starting position is
// out-of-bounds, then this routine returns an empty string.
// -- implementation ----------------------------------------------------------
// Uses the constructor TextString(const char *, length).
//-----------------------------------------------------------------------------
inline TextString TextString::mid(unsigned int startPos,
  unsigned int subLength) const
    {
    unsigned int checked_index = checkIndex(startPos);
    return TextString(&this->value->data[checked_index],
	min(length() - checked_index, subLength));
    }

// -- public -----------------------------------------------------------------
// TextString::operator>=()
//
// Compares two TextStrings.
//
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
inline bool TextString::operator>=(const TextString& rhs) const
    {
    return !(*this < rhs);
    }

// -- public -----------------------------------------------------------------
// TextString::operator<=()
//
// Compares two TextStrings.
//
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
inline bool TextString::operator<=(const TextString& rhs) const
    {
    return !(*this > rhs);
    }

// -- public -----------------------------------------------------------------
// TextString::operator!=()
//
// Compares two TextStrings.
//
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
inline bool TextString::operator!=(const TextString& rhs) const
    {
    return !(*this == rhs);
    }

// -- public -----------------------------------------------------------------
// TextString::operator!=()
//
// Compares a TextString and a char.
//
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
inline bool TextString::operator!=(const char rhs) const
    {
    return !(*this == rhs);
    }

// -- public ------------------------------------------------------------------
// TextString::operator[]()
// Returns a reference to the specified character in the TextString. If the
// position is out-of-bounds, then this routine returns the reference to the
// NULL terminating character, which if changed could result in some
// problems.
// -- implementation ----------------------------------------------------------
// This is inlined for high performance.
//-----------------------------------------------------------------------------
inline char TextString::operator[](int index) const
    {
    return this->value->data[checkIndex(index)];
    }

// -- public ------------------------------------------------------------------
// TextString::insertAfter()
// Inserts the specified character after the specified position.  If the
// specified position is out-of-bounds, then this routine functions as
// an append a character operation.
// -- implementation ----------------------------------------------------------
// Implemented in terms of insertBefore(pos, char).
//-----------------------------------------------------------------------------
inline void TextString::insertAfter(unsigned int pos, const char c)
    {
    insertBefore(++pos, c);
    }

// -- public ------------------------------------------------------------------
// TextString::operator+=()
// Append a single character to this TextString.
// -- implementation ----------------------------------------------------------
//-----------------------------------------------------------------------------
inline TextString& TextString::operator+=(const TextString& rhs)
    {
    if (length() + rhs.length() <= value->plength)
        {
        if (value->isShared())
            appendAndCopy(rhs, true);
        else
            {
            memmove(value->data + value->length, rhs.value->data,
              rhs.value->length + 1);
            value->length += rhs.length();
            }
        }
    else
        appendAndCopy(rhs, true);

    return *this;
    }

// -- public ------------------------------------------------------------------
// TextString::operator+=()
// This is to avoid a constructor.  It makes TextString
// slightly faster than STL strings on my test platform.
// -- implementation ----------------------------------------------------------
//-----------------------------------------------------------------------------
inline TextString& TextString::operator+=(const char *rhs)
    {
    if (rhs == NULL)
        return *this;
    int rlen = strlen(rhs);
    if (length() + rlen <= value->plength)
        {
        if (value->isShared())
            appendAndCopy(rhs, true);
        else
            {
            memmove(value->data + value->length, rhs,
              rlen + 1);
            value->length += rlen;
            }
        }
    else
        appendAndCopy(rhs, true);

    return *this;
    }

// -- public ------------------------------------------------------------------
// TextString::operator+=()
// Append a single character to this TextString.
// -- implementation ----------------------------------------------------------
//-----------------------------------------------------------------------------
inline TextString& TextString::operator+=(const char c)
    {
    if (length() + 1 <= value->plength)
        {
        if (value->isShared())
            appendAndCopy(c, true);
        else
            {
            value->data[length()] = c;
            value->data[length() + 1] = '\0';
            value->length++;
            }
        }
    else
        appendAndCopy(c, true);

    return *this;
    }

// -- public ------------------------------------------------------------------
// TextString::operator+()
// Adds two TextStrings together and returns the result as a new TextString.
// -- implementation ----------------------------------------------------------
// Uses operator+=() on a new TextString.
//-----------------------------------------------------------------------------
inline TextString TextString::operator+(const char rhs) const
    {
    TextString rval(*this);
    return rval += rhs;
    }

// -- public ------------------------------------------------------------------
// TextString::operator+()
// Adds two TextStrings together and returns the result as a new TextString.
// -- implementation ----------------------------------------------------------
// Uses operator+=() on a new TextString.
//-----------------------------------------------------------------------------
inline TextString operator+(const char *lhs, const TextString& rhs)
    {
    TextString rval(lhs);
    return rval += rhs;
    }

// -- public ------------------------------------------------------------------
// TextString::operator+()
// Adds two TextStrings together and returns the result as a new TextString.
// -- implementation ----------------------------------------------------------
// Uses operator+=() on a new TextString.
//-----------------------------------------------------------------------------
inline TextString operator+(const char lhs, const TextString& rhs)
    {
    char buf[2];
    buf[0] = lhs;
    buf[1] = '\0';
    TextString rval(buf);
    return rval += rhs;
    }

// -- public ------------------------------------------------------------------
// TextString::operator+()
// Adds two TextStrings together and returns the result as a new TextString.
// -- implementation ----------------------------------------------------------
// Uses operator+=() on a new TextString.
//-----------------------------------------------------------------------------
inline TextString TextString::operator+(const TextString& rhs) const
    {
    TextString rval(*this);
    rval += rhs;
    return rval;
    }

// -- public ------------------------------------------------------------------
// TextString::stringPtr()
// Returns a pointer to the character data for use with traditional C
// or const char * requirements.  The memory pointed to by this pointer
// MUST NOT BE MODIFIED!
// -- implementation ----------------------------------------------------------
//-----------------------------------------------------------------------------
inline const char *TextString::stringPtr() const
    {
    TextString *tmp = (TextString *)this;
    return tmp->value->data;
    }

// -- private -----------------------------------------------------------------
// TextString::getDummyCharPtr()
// Returns a reference to a dummy character that is safe for the user to
// write/read into/from.  It is used with the [] operator.  The position
// argument is used for the error logging.
// -- implementation ----------------------------------------------------------
// Calls checkIndex() just to log the problem of the bounds.
//-----------------------------------------------------------------------------
inline char& TextString::getDummyCharPtr(unsigned int position)
    {
    static char dummyChar;
    dummyChar = '\0'; // always init to null in case this is a read operation
    checkIndex(position);  // this will call the out-of-bounds logger
    return dummyChar;
    }

// -- global ------------------------------------------------------------------
// ::toString()
// Utility template function which can be used to convert any class instance
// into a TextString.  The instance of the supplied class must supply an
// std::ostream insertion operator.
// -- implementation ----------------------------------------------------------
// Use ostrstream to produce the string.
//-----------------------------------------------------------------------------
template <class T>
inline static TextString toString(const T &t)
    {
    std::ostringstream os;
    os << t << std::ends;
    return TextString(os.str().c_str());
    }

#endif
