// ---------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without technical
// support, and with no warranty, express or implied, as to its usefulness for
// any purpose.
//
// swapBytesTest.C
//
// Test driver for the SwapBytes class.
//
// Author:  Gerry Murray
// ---------------------------------------------------------------------------

#include "SwapBytes.H"
#include "LogStream.H"
#include "commonDefs.h"

int main (int, char **)
    {
    ByteOrderFormat foreignFormat = SwapBytes::hostByteOrder()
    == LittleEndian ? BigEndian : LittleEndian;
    
    // Translate a short from native to little endian, little endian
    // to big endian, big endian to native.  We should get the same
    // value that we started from.
    short startS = -47;
    short foreignS = SwapBytes::nativeTo(foreignFormat, startS);
    short finishS = SwapBytes::nativeFrom(foreignFormat, foreignS);
    if (startS == finishS)
        logEvent << "Short test was a success!" << std::endl;
    else
        logProblem << "Short test failed.  Bummer!" << std::endl
                   << "Start: " << startS << " Finish: " << finishS << std::endl;

    // Translate a unsigned short from native to little endian, little endian
    // to big endian, big endian to native.  We should get the same
    // value that we started from.
    unsigned short startUS = 47;
    unsigned short foreignUS = SwapBytes::nativeTo(foreignFormat, startUS);
    unsigned short finishUS =
    SwapBytes::nativeFrom(foreignFormat, foreignUS);
    if (startUS == finishUS)
        logEvent << "Unsigned short test was a success!" << std::endl;
    else
        logProblem << "Unsigned short test failed.  Bummer!" << std::endl
                   << "Start: " << startUS << " Finish: " << finishUS << std::endl;

    // Translate an int from native to little endian, little endian
    // to big endian, big endian to native.  We should get the same
    // value that we started from.
    int startI = -47;
    int foreignI = SwapBytes::nativeTo(foreignFormat, startI);
    int finishI = SwapBytes::nativeFrom(foreignFormat, foreignI);
    if (startI == finishI)
        logEvent << "Int test was a success!" << std::endl;
    else
        logProblem << "Int test failed.  Bummer!" << std::endl
                   << "Start: " << startI << " Finish: " << finishI << std::endl;

    // Translate a unsigned int from native to little endian, little endian
    // to big endian, big endian to native.  We should get the same
    // value that we started from.
    unsigned int startUI = 47;
    unsigned int foreignUI = SwapBytes::nativeTo(foreignFormat, startUI);
    unsigned int finishUI= SwapBytes::nativeFrom(foreignFormat, foreignUI);
    if (startUI == finishUI)
        logEvent << "Unsigned int test was a success!" << std::endl;
    else
        logProblem << "Unsigned int test failed.  Bummer!" << std::endl
                   << "Start: " << startUI << " Finish: " << finishUI << std::endl;
    
    // Translate a long from native to little endian, little endian
    // to big endian, big endian to native.  We should get the same
    // value that we started from.
    long startL = -47;
    long foreignL = SwapBytes::nativeTo(foreignFormat, startL);
    long finishL = SwapBytes::nativeFrom(foreignFormat, foreignL);
    if (startL == finishL)
        logEvent << "Long test was a success!" << std::endl;
    else
        logProblem << "Long test failed.  Bummer!" << std::endl
                   << "Start: " << startL << " Finish: " << finishL << std::endl;

    // Translate a unsigned long from native to little endian, little endian
    // to big endian, big endian to native.  We should get the same
    // value that we started from.
    unsigned long startUL = 47;
    unsigned long foreignUL = SwapBytes::nativeTo(foreignFormat, startUL);
    unsigned long finishUL= SwapBytes::nativeFrom(foreignFormat, foreignUL);
    if (startUL == finishUL)
        logEvent << "Unsigned long test was a success!" << std::endl;
    else
        logProblem << "Unsigned long test failed.  Bummer!" << std::endl
                   << "Start: " << startUL << " Finish: " << finishUL << std::endl;

    // Translate a float from native to little endian, little endian
    // to big endian, big endian to native.  We should get the same
    // value that we started from.
    float startF = 128.6035;
    FloatByteBuf foreignFloat = SwapBytes::nativeTo (foreignFormat, startF);
    float finishF = SwapBytes::nativeFrom (foreignFormat, foreignFloat);
    if (startF == finishF)
        logEvent << "Float test was a success!" << std::endl;
    else
        logProblem << "Float test failed.  Bummer!" << std::endl
                   << "Start: " << startF << " Finish: " << finishF << std::endl;

    // Translate a double from native to little endian, little endian
    // to big endian, big endian to native.  We should get the same
    // value that we started from.
    double startD = 128.6035;
    DoubleByteBuf foreignFloat2 = SwapBytes::nativeTo (foreignFormat, startD);
    double finishD = SwapBytes::nativeFrom (foreignFormat,
        foreignFloat2);
    if (startD == finishD)
        logEvent << "Double test was a success!" << std::endl;
    else
        logProblem << "Double test failed.  Bummer!" << std::endl
                   << "Start: " << startD << " Finish: " << finishD << std::endl;

    return 0;
    }
