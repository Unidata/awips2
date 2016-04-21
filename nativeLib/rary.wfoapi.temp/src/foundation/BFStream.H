//---------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without technical
// support, and with no warranty, express or implied, as to its usefulness for
// any purpose.
//
// BFStream.H
// Binary file stream translator class.
//
// Author: Brian Woods & Vivian Chao.
// ---------------------------------------------------------------------------

#ifndef BFStream_H
#define BFStream_H

#ifdef IDENT_H
static const char* const BFStream_H_Id = "$id$";
#endif

#include <iostream>
#include <fstream>
#include "TextString.H"


// Holds the common information for all classes
class BFStreamBase : public fstream
    {
    public:
        enum BFStream_Modes { binary = 0200 };

        BFStreamBase(void) {};
        BFStreamBase(const char* name, int mode, int prot)
            : fstream(name, mode, prot) { _mode = mode; }
        BFStreamBase(int fd, int mode)
            : fstream(fd) { _mode = mode; }
        BFStreamBase(int fd, char*  p, int l, int mode)
            : fstream(fd, p, l ) { _mode = mode; }

        // Attach and open
        virtual void open(const char* name, int mode,int prot)
            { _mode = mode; fstream::open( name, mode, prot ); }
        virtual void attach(int fd, int mode)
            { _mode = mode; fstream::attach( fd ); }

        // Get the current mode
        int mode( void ) { return _mode; }

        // Get a reference to a fstream for regular io
        fstream& fs(void) { return *((fstream*)this); }
    private: 
        int _mode;
    };

// Input binary file stream
class BIFStream : virtual public BFStreamBase
    {
    public:
        BIFStream(void) {};
        BIFStream(const char* name, int mode = ios::in | BFStreamBase::binary,
                  int prot = filebuf::openprot)
            : BFStreamBase(name, mode, prot) { }
        BIFStream(int fd, int mode = ios::in | BFStreamBase::binary)
            : BFStreamBase(fd, mode) { }
        BIFStream(int fd, char*  p, int l, int mode = ios::in |
                  BFStreamBase::binary)
            : BFStreamBase(fd, p, l, mode) { }

        // Attach and open
        virtual void open(const char* name, int mode = ios::in |
                          BFStreamBase::binary, int prot = filebuf::openprot)
            { BFStreamBase::open( name, mode, prot ); }
        virtual void attach(int fd, int mode = ios::in | BFStreamBase::binary)
            { BFStreamBase::attach( fd, mode ); }
    };

// Output binary file stream
class BOFStream : virtual public BFStreamBase
    {
    public: 
        BOFStream(void) {};
        BOFStream(const char* name, int mode=ios::out | BFStreamBase::binary,
                  int prot = filebuf::openprot)
            : BFStreamBase(name, mode, prot) {}
        BOFStream(int fd, int mode = ios::out | BFStreamBase::binary)
            : BFStreamBase(fd, mode) {}
        BOFStream(int fd, char*  p, int l, int mode = ios::in |
                  BFStreamBase::binary)
            : BFStreamBase(fd, p, l, mode) {}

        // Attrach and open
        virtual void open(const char* name, int mode = ios::out |
                          BFStreamBase::binary, int prot = filebuf::openprot)
            { BFStreamBase::open( name, mode, prot ); }

        virtual void attach(int fd, int mode = ios::out | BFStreamBase::binary)
            { BFStreamBase::attach( fd, mode ); }
    };

// Input/output binary file stream
class BFStream : public BIFStream,
                 public BOFStream
    {
    public: 
        BFStream(void) {};
        BFStream(const char* name,  int mode = ios::in | ios::out |
                 BFStreamBase::binary, int prot = filebuf::openprot) :
            BFStreamBase(name, mode, prot), BIFStream(), BOFStream() {}
        BFStream(int fd, int mode = ios::in | ios::out |
                 BFStreamBase::binary) :
            BFStreamBase(fd, mode), BIFStream(), BOFStream() {}
        BFStream(int fd, char*  p, int l, int mode = ios::in | ios::out |
                 BFStreamBase::binary) :
            BFStreamBase(fd, p, l, mode), BIFStream(), BOFStream() {}

        // Attrach and open
        virtual void open(const char* name, int mode = ios::in | ios::out |
                          BFStreamBase::binary, int prot = filebuf::openprot)
            { BFStreamBase::open( name, mode, prot ); }

        virtual void attach(int fd, int mode = ios::in | ios::out |
                            BFStreamBase::binary)
            { BFStreamBase::attach( fd, mode ); }
    };

// All operators for types are defined here
// Extractors
// Integral types
BIFStream& operator>>(BIFStream& in, char *string);
BIFStream& operator>>(BIFStream& in, unsigned char *string);
inline BIFStream& operator>>(BIFStream& in, char &c)
    { in.fs().get( c ); return in; }
BIFStream& operator>>(BIFStream& in, short &s);
BIFStream& operator>>(BIFStream& in, unsigned short &s);
BIFStream& operator>>(BIFStream& in, int &i);
BIFStream& operator>>(BIFStream& in, unsigned int &i);
BIFStream& operator>>(BIFStream& in, long &l);
BIFStream& operator>>(BIFStream& in, unsigned long &l);
BIFStream& operator>>(BIFStream& in, float &f);
BIFStream& operator>>(BIFStream& in, double &d);

// User defined types
BIFStream& operator>>(BIFStream& in, TextString& string);


// Inserters
// Integral types
BOFStream& operator<<(BOFStream& out, char * string);
BOFStream& operator<<(BOFStream& out, unsigned char * string);
inline BOFStream& operator<<(BOFStream& out, char c)
    { out.fs().put( c ); return out; }
BOFStream& operator<<(BOFStream& out, short s);
BOFStream& operator<<(BOFStream& out, unsigned short s);
BOFStream& operator<<(BOFStream& out, int i);
BOFStream& operator<<(BOFStream& out, unsigned int i);
BOFStream& operator<<(BOFStream& out, long l);
BOFStream& operator<<(BOFStream& out, unsigned long l);
BOFStream& operator<<(BOFStream& out, float f);
BOFStream& operator<<(BOFStream& out, double d);

// User defined types
BOFStream& operator<<(BOFStream& out, const TextString& string);


#endif

