// ---------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without technical
// support, and with no warranty, express or implied, as to its usefulness for
// any purpose.
//
// StringTokenizer.H
// Handles simple linear tokenization of a String.
//
// Author: romberg
// ---------------------------------------------------------------------------

#ifndef StringTokenizer_H
#define StringTokenizer_H

#ifdef IDENT_H
static const char* const StringTokenizer_H_Id =
  "$Id: .StringTokenizer.H__temp27950,v 1.2 2003/05/06 23:11:51 fluke Exp $";
#endif

#include "TextString.H"

class StringTokenizer
    {
    public:
        StringTokenizer(const TextString &str, const TextString &delim,
          bool returnTokens = false);
        StringTokenizer(const TextString &str);

        // Use default copy constructor and assignment operator.

        bool hasMoreTokens(void) const;
        TextString nextToken(void);
        TextString nextToken(const TextString &delim);
        int countTokens(void) const;
        const TextString &delimiter(void) const;

        std::ostream &printOn(std::ostream &os) const;

    private:
        TextString _str;
        TextString _delim;
        bool _returnTokens;

        bool findNextToken(unsigned int startPos, unsigned int &tokenStart,
          unsigned int &tokenEnd) const;
        bool isaDelim(char c) const;
        int nextChange(unsigned int startPos) const;
    };

// -- global -----------------------------------------------------------------
// operator<<()
//
// Insertion operator for an ostream.
//
// -- implementation ---------------------------------------------------------
// Use StringTokenizer::printOn().
// ---------------------------------------------------------------------------
inline std::ostream &operator<<(std::ostream &os, const StringTokenizer &st)
    {
    return st.printOn(os);
    }

// -- public -----------------------------------------------------------------
// StringTokenizer::hasMoreTokens()
//
// Returns true if more tokens exist in this StringTokenizer.
//
// -- implementation ---------------------------------------------------------
// Return the results of findNextToken().
// ---------------------------------------------------------------------------
inline bool StringTokenizer::hasMoreTokens(void) const
    {
    unsigned int start = 0, end = 0;
    return findNextToken(0, start, end);
    }

// -- public -----------------------------------------------------------------
// StringTokenizer::nextToken()
//
// Changes the delimiter string to the one supplied and then returns the
// next token.  The delimiter string will remain the default after this
// call.
//
// -- implementation ---------------------------------------------------------
// Store the delimiter string and then call nextToken().
// ---------------------------------------------------------------------------
inline TextString StringTokenizer::nextToken(const TextString &delim)
    {
    _delim = delim;
    return nextToken();
    }

// -- public -----------------------------------------------------------------
// StringTokenizer::delimiter()
//
// Returns the delimiter string used by this StringTokenizer.
//
// -- implementation ---------------------------------------------------------
// Returns _delim.
// ---------------------------------------------------------------------------
inline const TextString &StringTokenizer::delimiter(void) const
    {
    return _delim;
    }

#endif
