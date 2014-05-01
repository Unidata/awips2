// ---------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without technical
// support, and with no warranty, express or implied, as to its usefulness for
// any purpose.
//
// FDostream.H
// Declaration of an output stream class that uses an output stream buffer
// class that writes to file descriptor.
//
// Author: Jim Fluke (Modified from Josuttis)
// ---------------------------------------------------------------------------
#ifndef _FDostream_H
#define _FDostream_H

#ifdef IDENT_H
static const char* const FDostream_H_Id =
"$Id: .FDostream.H__temp10625,v 1.1 2003/05/20 23:04:00 fluke Exp $";
#endif

#include <iostream>
#if __GNUC__ == 3
#include <streambuf>
#else
#include <streambuf.h>
typedef int			int_type;
#endif
#include <cstdio>

// First the stream buffer class
class FDoutbuf: public std::streambuf
    {
    // Implemented in FDoutbuf.C    
    public:
        FDoutbuf(void) : _fd(-1) {}
        FDoutbuf(int fd) : _fd(fd) {}

        void attach(int fd) { _fd = fd; }

        int fd(void) const { return _fd; }

    protected:
        //static const FDOUTBUF_BUF_SIZE = 256;
        //char _fdOutBufBuffer[FDOUTBUF_BUF_SIZE];
        int _fd;

        virtual int_type overflow (int_type c);
        virtual std::streamsize xsputn(const char* s, std::streamsize num);

        //int flushBuffer (void);

    };

class FDostream: public std::ostream
    {
    public:
        FDostream (void)
          : std::ostream(&_theFileDescriptorStreambuf) {}
        FDostream (int fd)
          : std::ostream(&_theFileDescriptorStreambuf)
            { _theFileDescriptorStreambuf.attach(fd); }
            
        void attach(int fd) { _theFileDescriptorStreambuf.attach(fd); }

        int fd(void) const { return _theFileDescriptorStreambuf.fd(); }

		void open(const char* filePath, const std::ios::openmode mode);

		void close(void);

    protected:
        FDoutbuf _theFileDescriptorStreambuf;
    };

#endif
