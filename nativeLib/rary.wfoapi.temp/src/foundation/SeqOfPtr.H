// ---------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without technical
// support, and with no warranty, express or implied, as to its usefulness for
// any purpose.
//
// SeqOfPtr.H
// General-purpose class template for sequences of pointers.
//
// Author: Tom Cargill
// ---------------------------------------------------------------------------

// -- module -----------------------------------------------------------------
// The SeqOfPtr template class allows sequences of any type of pointers.
// Refer to SeqOf.C for further information about the use of this class.
//
// Semantics of SeqOfPtr<T*> are the same as SeqOf<T*>.
//
// SeqOfPtr<P> addresses naive code generation for SeqOf<T>,
// where T is a pointer to object. Use SeqOfPtr<T*> rather than SeqOf<T*>.
//
// -- implementation ---------------------------------------------------------
// This class is mostly inline. The pointers are cast to and from void*
// which allows it to use a SeqOf<void*> as the private data. This class
// exists to prevent code bloat.  Refer to SeqOf.C for more details.
// ---------------------------------------------------------------------------

#ifndef _SeqOfPtr_H
#define _SeqOfPtr_H

#ifdef IDENT_TEMPLATE_H
static const char* const SeqOfPtr_H_Id =
"$Id: .SeqOfPtr.H__temp27950,v 1.2 2003/05/06 23:11:49 fluke Exp $";
#endif

#include "SeqOf.H"
#include "LogStream.H"

template <class P>  // P is T*, where T is not member nor function
class SeqOfPtr
    {
    public:
        SeqOfPtr() {}
        SeqOfPtr(const P &p) : seqV((void*)p) {}
        SeqOfPtr(const P *vec, int len) : seqV((void**)vec, len) {}
        virtual ~SeqOfPtr() {}

        // compiler generated copy ctor and op=

        bool operator==(const SeqOfPtr &rhs) const
            { return seqV == rhs.seqV;}
        bool operator!=(const SeqOfPtr &rhs) const
            { return seqV != rhs.seqV; }

        int length() const { return seqV.length(); }
        void setLength(int len) { seqV.setLength(len); }
        SeqOfPtr operator+(const SeqOfPtr &rhs) const
            { return seqV+rhs.seqV; }      // use private ctor
        void append(const SeqOfPtr &rhs) { seqV.append(rhs.seqV); }
        void append(const P &p) { seqV.append((void*) p); }

        P &operator[](int index) { return (P&)(seqV[index]); }
        const P &operator[](int index) const
            { return (const P&)(seqV[index]); }

        SeqOfPtr extract(int lo, int hi) const
            { return seqV.extract(lo,hi); }    // use private ctor
        void remove(int lo, int hi) { seqV.remove(lo, hi); }
        void replace(int lo, int hi, const SeqOfPtr &sp)
            { seqV.replace(lo, hi, sp.seqV); }
        void replace(int index, const SeqOfPtr &sp)
            { replace(index, index, sp); }
        void prepend(const SeqOfPtr &sp)
            { seqV.prepend(sp.seqV); }
        void insertBefore(int index, const SeqOfPtr &sp)
            { seqV.insertBefore(index, sp.seqV); }
        void insertAfter(int index, const SeqOfPtr &sp)
            { seqV.insertAfter(index, sp.seqV); }

        // If p is found in this SeqOf then this function returns its index.
        int find(const P &p, int startPos = 0) const
            { return seqV.find((void *)p, startPos); }

        // finds the entries in this list that aren't in the other list
        SeqOfPtr notInList(const SeqOfPtr<P>& otherList) const;

        SeqOfPtr in(const SeqOfPtr<P>& otherList) const;

        // deprecated
        const P *dataPtr() const
            { return (const P*) seqV.dataPtr(); }

        std::ostream &printOn(std::ostream &os) const;

    private:
        SeqOf<void*> seqV;
        SeqOfPtr(const SeqOf<void*> &sv) : seqV(sv) {} // for op+ and extract
    };

//
// Inline function definitions
//
template <class P>
inline std::ostream &operator<<(std::ostream &os, const SeqOfPtr<P> &seq)
    {
    return seq.printOn(os);
    }

// -- private -----------------------------------------------------------------
// SeqOfPtr<P>::printOn()
//
// Prints a SeqOfPtr<P> on an ostream.
//
// -- implementation ---------------------------------------------------------
// The class represented by the pointer must have an operator<<() defined.
// ---------------------------------------------------------------------------
template <class P>
std::ostream &SeqOfPtr<P>::printOn(std::ostream &o) const
    {
    o << "[";
    for (int i = 0; i < length(); ++i )
        {
        P pointer = (*this)[i];
        o << " " << *pointer;
        }
    return o << " ]";
    }

// -- public -----------------------------------------------------------------
// template <class P>
// SeqOf<P>::notInList()
//
// Returns a list of entries that are in this list, but are not in the
// other list. (See more description in SeqOf<T>::notInList().)
// ---------------------------------------------------------------------------
template <class P>
SeqOfPtr<P> SeqOfPtr<P>::notInList(const SeqOfPtr<P>& otherList) const
    {
    SeqOfPtr<P> rval;  // to be returned

    // check each entry in this list for a match in otherList
    // if no match then add it to the return list.
    for (int i = 0 ; i < length() ; i++)
        if (otherList.find((*this)[i]) == -1)
            rval.append((*this)[i]);

    return rval;
    }


// -- public -----------------------------------------------------------------
// template <class P>
// SeqOf<P>::in()
//
// Returns a list of entries that are in this sequence and also in the
// given sequence.
// ---------------------------------------------------------------------------
template <class P>
SeqOfPtr<P> SeqOfPtr<P>::in(const SeqOfPtr<P>& otherList) const
    {
    SeqOfPtr<P> rval;  // to be returned

    // check each entry in this list
    // for a match in otherList
    // if match then add it to the return list.
    for (int i = 0 ; i < length() ; i++)
        if (otherList.find((*this)[i]) != -1)
            rval.append((*this)[i]);

    return rval;
    }

#endif
