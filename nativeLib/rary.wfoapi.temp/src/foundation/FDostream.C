// ---------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without technical
// support, and with no warranty, express or implied, as to its usefulness for
// any purpose.
//
// FDostream.C
// Implementation of an output stream class that uses an output stream buffer
// class that writes to file descriptor.
//
// Most of this is implementation of the stream buffer.
//
// Author: Jim Fluke (Modified from Josuttis)
// ---------------------------------------------------------------------------
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <iostream>
#include <errno.h>
#include <string.h>   // Pre ANSI C++ string handling
#include "FDostream.H"

#ifdef IDENT_C
static const char* const FDostream_C_Id =
"$Id: .FDostream.C__temp10625,v 1.1 2003/05/20 23:03:58 fluke Exp $";
#endif

// -- protected --------------------------------------------------------------
// FDoutbuf::overflow()
//
// Write one character
//
// -- implementation ---------------------------------------------------------
// Function signature should use int_type for both ints, but it doesn't work!
// ---------------------------------------------------------------------------
//int_type FDoutbuf::overflow(int_type c)
int FDoutbuf::overflow(int c)
    {
    if (_fd == -1) return EOF;
    if (c != EOF)
        {
        char z = c;
        if (write(_fd, &z, 1) != 1)
            return EOF;
        }
    return c;
    }

// -- protected --------------------------------------------------------------
// FDoutbuf::overflow()
//
// Write multiple characters
//
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
std::streamsize FDoutbuf::xsputn(const char* s, std::streamsize num)
    {
    if (_fd == -1) return EOF;
    return write(_fd, s, num);
    }

// -- protected --------------------------------------------------------------
// FDostream::open()
//
// Open a file and use the resulting file descriptor.
//
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
void FDostream::open(const char* filePath, const std::ios::openmode mode) 
	{
	// output stream - only use the output flags
	// assumes ios::trunc and ios::app are not both set
	int flags = O_WRONLY | O_CREAT;
	if (mode & std::ios::trunc)
		flags |= O_TRUNC;
	if (mode & std::ios::app)
		flags |= O_APPEND;

	// The permissions will be filtered through umask
	int fd = ::open(filePath, flags, 0666);
	if (fd == -1)
		{
		setstate(std::ios::badbit);
		std::cerr << "::open() failed: " << strerror(errno) << std::endl;
		}
	else
    	_theFileDescriptorStreambuf.attach(fd);
	}

// -- protected --------------------------------------------------------------
// FDostream::open()
//
// Open a file and use the resulting file descriptor.
//
// -- implementation ---------------------------------------------------------
// ---------------------------------------------------------------------------
void FDostream::close(void)
	{
	if (::close(_theFileDescriptorStreambuf.fd()) == -1)
		{
		setstate(std::ios::badbit);
		std::cerr << "::close() failed: " << strerror(errno) << std::endl;
		}
	setstate(std::ios::failbit);
	}
