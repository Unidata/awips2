// ---------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without technical
// support, and with no warranty, express or implied, as to its usefulness for
// any purpose.
//
// fdostreamTest.C
// Test driver for fdostream.
//
// Author: Jim Fluke (Modified from Josuttis)
// ---------------------------------------------------------------------------

#include <iostream>
#include "FDostream.H"

int main()
    {
	bool result = true;

    FDostream out(1); // stream with buffer writing to fd 1 (stdout).

	if (out)
    	out << "31 hexadecimal: " << std::hex << 31 << std::endl;
	else
		result = false;

    FDostream out2; // Not initialized with a fd yet.
    FDoutbuf* fdBufPtr = dynamic_cast<FDoutbuf*>(out2.rdbuf());
    if (fdBufPtr)
        {
        out << "Successfull dynamic_cast" << std::endl;
        fdBufPtr->attach(1);
        out2 << "42 hexadecimal: " << std::hex << 42 << std::endl;
        }
	else
		result = false;

    FDostream out3; // Not initialized with a fd yet.
	out3.open("/blather/foobar/testFile", std::ios::out);
	if (!out3)
		out << "Could not open /blather/foobar/testFile. This was expected.\n";

	out3.clear();
	out3.open("fdostreamTest_testFile", std::ios::out);
	if (!out3)
		{
		out << "Could not open fdostreamTest_testFile\n";
		result = false;
		}
	else
		out3 << "This should be written to fdostreamTest_testFile\n";

	if (result)
		std::cout << "All test completed successfully.\n";
	else
		std::cout << "Some tests failed.\n";
    }
