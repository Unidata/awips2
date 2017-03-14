// ----------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without
// technical support, and with no warranty, express or implied, as
// to its usefulness for any purpose.
//
// TextString.C
// TextString class
//
// Modifications:
// 02/14/02 David Friedman   Made TextString(const char *, unsigned int) public
//                           and more efficient. (DR 10321)
//
// 04/11/02 Gerry Murray (FSL)  Fixed a bug in TextString::removeTrailSpcs().
//                              Method should set its length to 0 if it is
//                              called with a string of all spaces.  Instead,
//                              it was setting its length to 1.
//
//-----------------------------------------------------------------------------
#ifdef IDENT_C
static const char* const TextString_C_Id =
"$Id: .TextString.C__temp27950,v 1.9 2003/05/06 23:11:52 fluke Exp $";
#endif

// -- module ------------------------------------------------------------------
// The TextString class is designed to hold a string of characters which
// are NULL-terminated.  The class takes care of memory allocation
// and deallocation.  Routines are provided to add strings together, append
// characters and strings to this string, index through the array using
// the familiar [] notation, inserting and removing characters from the
// middle of the string, and obtaining sub strings.
//
// Rules:
// 1. Never insert a NULL character into the string using any of the
//    possible operations to do so or undefined results may occur
//
// 2. Bounds checking is performed on all operations.  This generally results
//    in logBug being generated when indexing positions are specified
//    but the code will stumble on.  In particular, bounds errors from
//    the operator[] will result in no operation occurring.  Other bounds
//    errors are adjusted.  For example, asking for a subString starting
//    at position 10 in a string that only has 4 characters results in
//    a string with 0 characters.  Inserting characters before position 10
//    in a string that only has 4 characters results in appending characters
//    to the original string.  But attempting an operation with the []
//    operator for position 10 when the string only has 5 characters results
//    in no change to the string.
//
// 3. References returned from operator[] remain valid until a non-const
//    member function (other than operator[]) is called.
//
// 4. The character positions begin at 0 and go through length()-1.  For
//    an empty string with no characters in it, a position of 0 is invalid.
//
//
// -- implementation ----------------------------------------------------------
// The class makes heavy use of memory(3) routines, such as memcpy.  It
// also uses many of the string(3) routines.
//-----------------------------------------------------------------------------
#include "TextString.H"
#include "commonDefs.h"
#include "LogStream.H"
#include <ctype.h>

// -- public ------------------------------------------------------------------
// TextString::operator=()
// Assignment operator.  Returns a reference to this TextString.
// -- implementation ----------------------------------------------------------
//-----------------------------------------------------------------------------
TextString& TextString::operator=(const char rhs)
    {
    static char buf[] = " ";
    buf[0] = rhs;
    value = new StringValue(buf);
    return *this;
    }

// -- public ------------------------------------------------------------------
// TextString::removeTrailSpcs()
// Removes all trailing spaces.
// -- implementation ----------------------------------------------------------
//-----------------------------------------------------------------------------
void TextString::removeTrailSpcs(void)
    {
    if (value->isShared())
        value = new StringValue(value->data);

    int i = (int)(length()) - 1;
    while (i >= 0 && value->data[i] == ' ')
        i--;
    value->data[i+1] = '\0';
    value->length = strlen(value->data);
    }

// -- public ------------------------------------------------------------------
// TextString::removeLeadingSpcs()
// Removes all leading spaces.
// -- implementation ----------------------------------------------------------
//-----------------------------------------------------------------------------
void TextString::removeLeadingSpcs(void)
    {
    int i = 0;  // pos and number to delete
    while (value->data[i] == ' ')
        i++;
    if (i == 0)
        return;

    if (value->isShared())
        value = new StringValue(value->data);

    int j = i;
    while (value->data[j] != '\0')
        {
        value->data[j-i] = value->data[j];
        j++;
        }
    value->data[j-i] = '\0';
    value->length = strlen(value->data);
    }

// -- public ------------------------------------------------------------------
// TextString::toUpper()
// Converts all alpha chars to uppercase.
// -- implementation ----------------------------------------------------------
// Calls ::toupper on each char.
//-----------------------------------------------------------------------------
void TextString::toUpper(void)
    {
    if (value->isShared())
        value = new StringValue(value->data);

    for (unsigned int i = 0 ; i < length() ; i++)
        value->data[i] = toupper(value->data[i]);
    }

// -- public ------------------------------------------------------------------
// TextString::toLower()
// Converts all alpha chars to lowercase.
// -- implementation ----------------------------------------------------------
// Calls ::tolower on each char.
//-----------------------------------------------------------------------------
void TextString::toLower(void)
    {
    if (value->isShared())
        value = new StringValue(value->data);

    for (unsigned int i = 0 ; i < length() ; i++)
        value->data[i] = tolower(value->data[i]);
    }

// -- public ------------------------------------------------------------------
// TextString::remove()
// Remove the specified number of characters starting at the specified
// location from this TextString. If the starting position is out-of-bounds,
// then no operation occurs.  If the specified number of characters to
// remove is greater than the number of characters than can be removed,
// then only the number of characters than can be removed will be
// removed.
// -- implementation ----------------------------------------------------------
// Uses memmove() due to overlapping input/output buffers.
//-----------------------------------------------------------------------------
void TextString::remove(unsigned int startPos, unsigned int subLength)
    {
    if (value->isShared())
        value = new StringValue(value->data);

    startPos = checkIndex(startPos);  // bounds-check
    subLength = min(subLength, length() - startPos);
    if (subLength == 0)
        return;
    memmove(&value->data[startPos], &value->data[startPos+subLength],
      length() - startPos - subLength + 1);
    value->length = strlen(value->data);
    }

// -- private -----------------------------------------------------------------
// TextString::insertAndCopy()
//
// Utility function which does an 'insertBefore' and a copy.
//
// -- implementation ----------------------------------------------------------
//-----------------------------------------------------------------------------
inline void TextString::insertAndCopy(unsigned int pos, char c,
  bool overallocate)
    {
    char *buf = (char *)alloca(length() + 2);
    strcpy(buf, value->data);
    pos = checkIndex(pos);  // bounds check
    unsigned int prevSize = length();
    // move the characters in the existing string to the right
    memmove(&buf[pos+1], &buf[pos], prevSize - pos + 1);
    // copy in the character
    buf[pos] = c;
    value = new StringValue(buf, overallocate);
    }

// -- private -----------------------------------------------------------------
// TextString::insertAndCopy()
//
// Utility function which does an 'insertBefore' and a copy.
//
// -- implementation ----------------------------------------------------------
//-----------------------------------------------------------------------------
inline void TextString::insertAndCopy(unsigned int pos,
  const TextString &substring, bool overallocate)
    {
    char *buf = (char *)alloca(length() + substring.length() + 1);
    strcpy(buf, value->data);
    pos = checkIndex(pos);  // bounds check
    unsigned int prevSize = length();
    // move the characters in the existing string to the right
    // including the null terminator
    memmove(&buf[pos+substring.length()], &buf[pos], prevSize-pos+1);
    // copy in the middle part
    memcpy(&buf[pos], substring.value->data, substring.length());
    value = new StringValue(buf, overallocate);
    }

// -- public ------------------------------------------------------------------
// TextString::insertBefore()
// Inserts the specified character before the specified position.  If the
// specified position is out-of-bounds, then this routine functions as
// an append a character operation.
// -- implementation ----------------------------------------------------------
//-----------------------------------------------------------------------------
void TextString::insertBefore(unsigned int pos, const char c)
    {
    if (length() + 1 <= value->plength)
        {
        if (value->isShared())
            insertAndCopy(pos, c, true);
        else
            {
            pos = checkIndex(pos);
            unsigned int prevSize = length();
            memmove(&value->data[pos+1], &value->data[pos],
              prevSize - pos + 1);
            value->data[pos] = c;
            value->length++;
            }
        }
    else
        insertAndCopy(pos, c, true);
    }

// -- public ------------------------------------------------------------------
// TextString::insertBefore()
// Inserts the specified subString before the specified position.  If the
// specified position is out-of-bounds, then this functions as a append
// operation.
// -- implementation ----------------------------------------------------------
// Bounds-checks the index, calls setLength() to ensure the correct amount
// of memory is present, uses memmove() to move the trailing set of
// characters out of the way, uses memcpy() to copy in the inserted portion.
//-----------------------------------------------------------------------------
void TextString::insertBefore(unsigned int pos, const TextString& substring)
    {
    if (length() + substring.length() <= value->plength)
        {
        if (value->isShared())
            insertAndCopy(pos, substring, true);
        else
            {
            pos = checkIndex(pos);  // bounds check
            unsigned int prevSize = length();
            // move the characters in the existing string to the right
            // including the null terminator
            memmove(&value->data[pos+substring.length()],
              &value->data[pos], prevSize-pos+1);
            // copy in the middle part
            memcpy(&value->data[pos],
              substring.value->data, substring.length());
            value->length = prevSize + substring.length();
            }
        }
    else
        insertAndCopy(pos, substring, true);
    }

// -- public ------------------------------------------------------------------
// TextString::right()
// Returns a TextString consisting of the rightmost characters in this
// string.  The number of characters desired is specified.  If too many
// characters are specified such that they exceed the length of the
// existing string, then this routine returns the entire existing string.
// -- implementation ----------------------------------------------------------
// Uses the constructor TextString(const char *, length).
//-----------------------------------------------------------------------------
TextString TextString::right(unsigned int len) const
    {
    if (len >= length())
        return *this;   // return a copy of this TextString
    else
        return TextString(&value->data[length() - len], len);
    }

// -- public -----------------------------------------------------------------
// TextString::TextString()
// Constructor taking a character pointer and a size.  Constructs
// a TextString containing just those characters.  If a NULL is found
// with the length specified, then this routine truncates the string at
// that point.
// -- implementation ----------------------------------------------------------
//-----------------------------------------------------------------------------
TextString::TextString(const char *ivalue, unsigned int length) :
    value(initSV(ivalue, length))
    {
        
    }

// -- private -----------------------------------------------------------------
// TextString::strrstr()
//
// Returns a pointer to the last occurrence of string sstr in string str,
// or a NULL pointer if sstr does not occur in the string.  If sstr points
// to a string of zero length, strrstr() returns str.
//
// -- implementation ----------------------------------------------------------
// This code only necessary for non-HP platforms.  strrstr()
// is not POSIX.
//-----------------------------------------------------------------------------
const char *TextString::strrstr(const char *str, const char *sstr) const
    {
    int l1 = strlen(str);
    int l2 = strlen(sstr);

    if (l2 > l1)
        return NULL;

    char *ptr = (char *)str + (l1 - l2);

    do
        {
        if (!strncmp(ptr, sstr, l2))
            return ptr;
        } while (--ptr != str);

    return NULL;
    }

// -- public ------------------------------------------------------------------
// TextString::found()
// Returns true and returns the found position in foundPos if the
// search string was found in this string starting at the specified
// start position and searches performed in the specified direction.
// If the string is not found, then the value returned in foundPos
// is undefined.
// -- implementation ----------------------------------------------------------
// Uses strstr() and strrstr().
//-----------------------------------------------------------------------------
bool TextString::found(const TextString& searchString,
  unsigned int& foundPos, unsigned int startPos,
  SearchDir dir) const
    {
    const char *ptr;

    // bounds check the startPos
    if (!length() && !startPos)  // special case - empty string and 0 start
        return false;

    startPos = checkIndex(startPos);

    if (dir == SEARCH_FWD)
        {
        ptr = strstr(&value->data[startPos], searchString.value->data);
        if (ptr)
            {
            foundPos = (unsigned int)(ptr - value->data); // calculate position
            return true;
            }
        }

    else if (dir == SEARCH_BCK) // search backwards
        {
        TextString tempString = left(startPos+1);
        ptr = strrstr(tempString.value->data, searchString.value->data);
        if (ptr)
            {
            foundPos = (unsigned int)(ptr - tempString.value->data);
            return true;
            }
        }
    else
        logBug << "Unknown SearchDir specified" << std::endl;

    return false;
    }

// -- public ------------------------------------------------------------------
// TextString::found()
// Returns true and returns the found position in foundPos if the
// search character was found in this string starting at the specified
// start position and searches performed in the specified direction.
// If the character is not found, then the value returned in foundPos
// is undefined.
// -- implementation ----------------------------------------------------------
// Although this routine could be coded to convert the search character
// into a search string and then call found(searchString...), that would
// not be terribly efficient.  Therefore the author has decided to
// duplicate some code for efficiency.
//-----------------------------------------------------------------------------
bool TextString::found(const char searchChar, unsigned int& foundPos,
  unsigned int startPos, SearchDir dir) const
    {
    char *ptr;

    // bounds check the startPos
    if (!length() && !startPos)  // special case - empty string and 0 start
        return false;
    startPos = checkIndex(startPos);

    if (dir == SEARCH_FWD)
        {
        ptr = strchr(&value->data[startPos], searchChar);
        if (ptr)
            {
            foundPos = (unsigned int)(ptr - value->data); // calculate position
            return true;
            }
        }

    else if (dir == SEARCH_BCK) // search backwards
        {
        TextString tempString = left(startPos+1);
        ptr = strrchr(tempString.value->data, searchChar);
        if (ptr)
            {
            foundPos = (unsigned int)(ptr - tempString.value->data);
            return true;
            }
        }

    else
        logBug << "Unknown SearchDir specified" << std::endl;

    return false;
    }

// -- public ------------------------------------------------------------------
// TextString::emptyString()
//
// Returns an empty TextString.
//
// -- implementation ----------------------------------------------------------
// Returns a reference to a function static object.  Returning a reference
// to a file static object can cause race conditions when static constructors
// are used.  For example LogStream uses a static TextString in a file static
// object of its own.  So what happens if the LogStream constructor runs before
// the TextString constructor?  Using a function static TextString eliminates
// this trouble.
//-----------------------------------------------------------------------------
const TextString& TextString::emptyString()
    {
    static const TextString emptyStr("", 0);
    return emptyStr;
    }

// -- public ------------------------------------------------------------------
// TextString::operator[]()
// Returns a reference to the specified character in the TextString. If the
// position is out-of-bounds, then this routine returns the reference to the
// NULL terminating character, which if changed could result in some
// problems.
// -- implementation ----------------------------------------------------------
//-----------------------------------------------------------------------------
char& TextString::operator[](int index)
    {
    if (index >= (int)length())
        return getDummyCharPtr(index);

    if (value->isShared())
        {
        value = new StringValue(value->data);
        }

    value->markUnshareable();

    return value->data[index];
    }

// -- private -----------------------------------------------------------------
// TextString::logit()
// We want checkIndex() to be inline.  But, LogStream and TextString both
// include each other.  So, we move the message part of checkIndex() here
// where it is ok to include LogStream.H
// -- implementation ----------------------------------------------------------
//-----------------------------------------------------------------------------
void TextString::logit(int index) const
    {
    logBug << "index out of range in TextString::checkIndex() "
      << " stringSize=" << length() << " user-index=" << index << std::endl;
    }
