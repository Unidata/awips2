// ---------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without technical
// support, and with no warranty, express or implied, as to its usefulness for
// any purpose.
//
// StringTokenizer.C
// Handles simple linear tokenization of a String.
//
// Author: romberg
// ---------------------------------------------------------------------------

#ifdef IDENT_C
static const char* const StringTokenizer_C_Id =
  "$Id: .StringTokenizer.C__temp27950,v 1.2 2003/05/06 23:11:51 fluke Exp $";
#endif

// -- module -----------------------------------------------------------------
//
// StringTokenizer is a class that controls simple linear tokenization of
// a String. The set of delimiters, which defaults to common whitespace
// characters, may be specified at creation time or on a per-token basis.
//
// This StringTokenizer class was inspired by the Java StringTokenizer
// class.
//
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
#include "LogStream.H"
#include "StringTokenizer.H"
#include <string.h>

// -- public -----------------------------------------------------------------
// StringTokenizer::StringTokenizer()
//
// Constructor for a StringTokenizer.  The string to be broken into tokens is
// supplied along with a string containing delimiter characters.  If the
// parameter returnTokens is true then substrings composed of delimiters will
// be returned as tokens.
//
// -- implementation ---------------------------------------------------------
// Save the parameters in private data.
// ---------------------------------------------------------------------------
StringTokenizer::StringTokenizer(const TextString &str,
  const TextString &delim, bool returnTokens) :
  _str(str), _delim(delim), _returnTokens(returnTokens)
    {
    }

// -- public -----------------------------------------------------------------
// StringTokenizer::StringTokenizer()
//
// Constructor for a StringTokenizer.  The string to be broken into tokens is
// supplied.  The delimiter string defaults to common whitespace characters
// " /t/n/r" and delimiters will not be returned as tokens.
//
// -- implementation ---------------------------------------------------------
// Store the string in private data.  Set the delimiter string to " /t/n/r",
// and set _returnTokens to false.
// ---------------------------------------------------------------------------
StringTokenizer::StringTokenizer(const TextString &str) :
  _str(str), _delim(" \t\n\r"), _returnTokens(false)
    {
    }

// -- public -----------------------------------------------------------------
// StringTokenizer::nextToken()
//
// Returns the next token.  If there are no more tokens an empty string will
// be returned.  So, one will need to use another method (such
// as hasMoreTokens()) to determine if all of the tokens have been returned.
//
// -- implementation ---------------------------------------------------------
// Find the next token by calling findNextToken().  Extract the token and then
// remove it from _str.  Finally, return the token.
// ---------------------------------------------------------------------------
TextString StringTokenizer::nextToken(void)
    {
    unsigned int start = 0, end = 0;
    TextString rval;

    if (findNextToken(0, start, end))
        {
        rval = _str.mid(start, end - start + 1);
        _str.remove(0, end + 1);
        }

    return rval;
    }

// -- public -----------------------------------------------------------------
// StringTokenizer::countTokens()
//
// Returns the number of tokens contained in this StringTokenizer using the
// current delimiter set.
//
// -- implementation ---------------------------------------------------------
// Call findNextToken() until it returns false.  The number of times it
// returns true will be the number of tokens that are left.
// ---------------------------------------------------------------------------
int StringTokenizer::countTokens(void) const
    {
    int rval = 0;
    unsigned int index = 0, start = 0, end = 0;

    while (findNextToken(index, start, end))
        {
        rval++;
        index = end + 1;
        }

    return rval;
    }

// -- public -----------------------------------------------------------------
// StringTokenizer::printOn()
//
// Prints a StringTokenizer on an ostream.
//
// -- implementation ---------------------------------------------------------
// Dump _str, _delim, and _returnTokens.
// ---------------------------------------------------------------------------
std::ostream &StringTokenizer::printOn(std::ostream &os) const
    {
    return os << "StringTokenizer(str = \"" << _str << "\""
      << ", _delim = \"" << _delim << "\""
      << ", returnTokens = " << _returnTokens << ")";
    }

// -- private ----------------------------------------------------------------
// StringTokenizer::findNextToken()
//
// A utility function which will locate the start and end positions of the
// next token.  The starting position (for the search) is supplied.  If a
// token is found this function will return true and tokenStart and tokenEnd
// will be set to the start and end of the next token.  If no token was found
// then this function returns false.
//
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
bool StringTokenizer::findNextToken(unsigned int startPos,
  unsigned int &tokenStart, unsigned int &tokenEnd) const
    {
    if (startPos >= _str.length())
        return false;

    tokenStart = tokenEnd = startPos;

    if (isaDelim(_str[startPos]) && _returnTokens)
        {
        tokenEnd = nextChange(startPos);

        if (tokenEnd > _str.length() - 1)
            tokenEnd = _str.length() - 1;
        else if (tokenEnd < 0)
            tokenEnd = 0;
        else
            tokenEnd--;

        return true;
        }

    // Either no token at begining or we are not returning them
    // So, strip off the delimiters.
    while (tokenStart < _str.length() && isaDelim(_str[tokenStart]))
        tokenStart++;

    // Check for a string of all delimiters.
    if (tokenStart >= _str.length())
        return false;

    tokenEnd = nextChange(tokenStart);
    if (tokenEnd > _str.length() - 1)
        tokenEnd = _str.length() - 1;
    else
        tokenEnd--;
    return true;
    }

// -- private ----------------------------------------------------------------
// StringTokenizer::nextChange()
//
// Returns the index of the next change in the string being tokenized.  The
// starting position is specified.  If the starting position is on a delimiter
// then this will return the location of the next non delimiter.  If the
// starting position is not a delimiter then this function returns the start
// of the next delimiter.
//
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
int StringTokenizer::nextChange(unsigned int startPos) const
    {
    if (startPos >= _str.length())
        return -1;

    if (isaDelim(_str[startPos]))
        while (startPos < _str.length() && isaDelim(_str[startPos]))
            startPos++;
    else
        while (startPos < _str.length() && !isaDelim(_str[startPos]))
            startPos++;

//    if (startPos >= _str.length())
//        startPos = _str.length() - 1;

    return startPos;
    }

// -- public -----------------------------------------------------------------
// StringTokenizer::isaDelim()
//
// Returns true if the supplied character is a delimiter.
//
// -- implementation ---------------------------------------------------------
// Use strchr() and _delim.
// ---------------------------------------------------------------------------
bool StringTokenizer::isaDelim(char c) const
    {
    return strchr(_delim.stringPtr(), c) != NULL;
    }
