// ---------------------------------------------------------------------------
// This software is in the public domain, furnished "as is", without technical
// support, and with no warranty, express or implied, as to its usefulness for
// any purpose.
//
// SwapBytes.H
//
// Free functions that convert byte order of primitive data types.
// Supports three types of formats: Little Endian, Big Endian, and the
// format that it is native to a machine (little or big endian.)
//
// Authors: Odonnell, Romberg, Murray
// ---------------------------------------------------------------------------
#ifndef _SwapBytes_H
#define _SwapBytes_H

#include "commonDefs.h"

// Would be better if it was LITTLE_ENDIAN and BIG_ENDIAN but there is a name
// conflict with STL.
enum ByteOrderFormat {LittleEndian, BigEndian};

// Used for holding floating point values that are not in a native format.
struct FloatByteBuf
    {
    byte buf[sizeof(float)];
    };
struct DoubleByteBuf
    {
    byte buf[sizeof(double)];
    };

class SwapBytes
    {
    public:
        // Interface for all types except for float and double.
        template <class T>
        static T nativeFrom(ByteOrderFormat srcFormat, const T &srcValue);
        template <class T>
        static T nativeTo(ByteOrderFormat returnFormat, const T &nativeVal);

        // Float and Double interface.  Values that are not in native
        // format can not be stored in float and double variables
        // because if the non-native value is NaN, assignment is only
        // guaranteed to preserve the NaN bytes.
        static FloatByteBuf nativeTo(ByteOrderFormat returnFormat,
            const float &nativeVal);
        static DoubleByteBuf nativeTo(ByteOrderFormat returnFormat,
            const double &nativeVal);
        static float nativeFrom(ByteOrderFormat returnFormat,
            FloatByteBuf foreignBuf);
        static double nativeFrom(ByteOrderFormat returnFormat,
            DoubleByteBuf foreignBuf);
                
        // Query of the endian format of this host.  Will return either
        // LittleEndian or BigEndian
        static ByteOrderFormat hostByteOrder (void);

    private:
        // Declared private to prevent client from calling these.
        static float nativeFrom(ByteOrderFormat returnFormat, const float &);
        static double nativeFrom(ByteOrderFormat returnFormat, const double &);
        
        // Conversion methods for primitive types.  
        static short swap(short w);
        static unsigned short swap(unsigned short w)
            {return (unsigned short)swap ((short)w);}
        static int swap(int dw);
        static unsigned int swap(unsigned int dw)
            {return (unsigned int)swap ((int)dw);}
        static long swap(long dw)  {return (long)swap ((int)dw);}
        static unsigned long swap(unsigned long dw)
            {return (unsigned long)swap ((long)dw);}
    };

// -- public ---------------------------------------------------------------
// ByteOrderFormat SwapBytes<T>::hostByteOrder (void)
//
// Returns the an enumerated value describing the native byte order for this
// host.  Currently, only two values are supported, BigEndian and LittleEndian
//
// --------------------------------------------------------------------------
inline ByteOrderFormat SwapBytes::hostByteOrder (void)
    {
    static union {
        unsigned long  _whole;
        unsigned short _half[2];
        } endianTest;
    endianTest._whole = 1;
    return endianTest._half[1] ? BigEndian : LittleEndian;
    }

// -- public ---------------------------------------------------------------
// template <class T>
// T SwapBytes::nativeFrom (ByteOrderFormat srcFormat, const T &srcValue) 
//
// Converts and returns the specified value in the specified byte
// order format to the byte order format that the host hardware supports.
//
// Important.  This method should not be called for float or double types!
//
// srcFormat:  An enumerated value describing the byte order format that the
//             srcValue is currently in.
//
// srcValue:  A value of a primitive data type which will be converted to
//            the byte order format that is supported by the native hardware.
//
// ---------------------------------------------------------------------------
template <class T>
inline T SwapBytes::nativeFrom(ByteOrderFormat srcFormat, const T &srcValue)
   {
   return hostByteOrder() == srcFormat ? srcValue : swap(srcValue);
   }

// -- public ----------------------------------------------------------------
// T SwapBytes::nativeTo (ByteOrderFormat returnFormat,
// const T& nativeValue) 
//
// Converts and returns the value in the byte order format that the
// host hardware supports to some other byte order format,
//
// Important.  This method should not be called for float or double types!
//
// returnFormat:  An enumerated value describing the byte order format that the
//                return value will be in.
//
// nativeValue:  A value of a primitive data type which is in the byte order
//               format of the host hardware.  It will be converted to the
//               format specified in "returnFormat".
//
// ---------------------------------------------------------------------------
template <class T>
inline T SwapBytes::nativeTo(ByteOrderFormat returnFormat,
    const T &nativeVal)
   {
   return hostByteOrder() == returnFormat ? nativeVal : swap(nativeVal);
   }

// -- public ----------------------------------------------------------------
// FloatByteBuf SwapBytes::nativeTo (ByteOrderFormat returnFormat,
//       float nativeValue) 
//
// Converts the single precision floating point value in the byte order
// format that the host hardware supports to a sequence of bytes that
// represent a single precision floating point value in a foreign byte
// order format.  This sequence of bytes is the return value.  Once
// swapped, the return value is no longer a valid double on the native
// platform, and can not be treated as such.  That's why we return it
// in a byte buffer to discourage math, and even assignment.
//
// returnFormat:  An enumerated value describing the byte order format that the
//                return value will be in.
//
// nativeValue:  A single precision floating point value in the byte order
//               format of the host hardware.  It will be converted to the
//               format specified in "returnFormat".
//
// ---------------------------------------------------------------------------
inline FloatByteBuf SwapBytes::nativeTo(ByteOrderFormat returnFormat,
    const float &nativeVal)
   {
   // No need to swap.  The target format is the same as our host format.
   if (hostByteOrder() == returnFormat)
       {
       FloatByteBuf *retBuf = (FloatByteBuf*)&nativeVal;
       return *retBuf;
       }

   // Swap Away.
   union {
        short s[2];
        float f;
        } floatUnion;
   floatUnion.f = nativeVal;
   short tmp = swap(floatUnion.s[0]);
   floatUnion.s[0] = swap(floatUnion.s[1]);
   floatUnion.s[1] = tmp;
   FloatByteBuf *retBuf = (FloatByteBuf *) &floatUnion;
   return *retBuf;
   }

// -- public ----------------------------------------------------------------
// DoubleByteBuf SwapBytes::nativeTo (ByteOrderFormat returnFormat,
// const double *nativeValue) 
//
// Converts the double precision floating point value in the byte order
// format that the host hardware supports to a sequence of bytes that
// represent a double precision floating point value in a foreign byte
// order format.  This sequence of bytes is the return value.  Once
// swapped, the return value is no longer a valid float on the native
// platform, and can not be treated as such.  That's why we return it
// in a byte buffer to discourage math, and even assignment.
//
// returnFormat:  An enumerated value describing the byte order format that the
//                return value will be in.
//
// nativeValue:  A double precision floating point value in the byte order
//               format of the host hardware.  It will be converted to the
//               format specified in "returnFormat".
//
// ---------------------------------------------------------------------------
inline DoubleByteBuf SwapBytes::nativeTo(ByteOrderFormat returnFormat,
    const double &nativeVal)
   {
   // No need to swap.  The target format is the same as our host format.
   if (hostByteOrder() == returnFormat)
       {
       DoubleByteBuf *retBuf = (DoubleByteBuf *)&nativeVal;
       return *retBuf;
       }

   // Swap Away.
   union {
        long l[2];
        double f;
        } doubleUnion;
   doubleUnion.f = nativeVal;
   long tmp = swap(doubleUnion.l[0]);
   doubleUnion.l[0] = swap(doubleUnion.l[1]);
   doubleUnion.l[1] = tmp;
   DoubleByteBuf *retBuf = (DoubleByteBuf*) &doubleUnion;
   return *retBuf;
   }

// -- public ---------------------------------------------------------------
// float SwapBytes::nativeFrom(ByteOrderFormat srcFormat,
//      FloatByteBuf foreignBytes)
//
// Converts a sequence of bytes that represent a single precision
// floating point value in the byte order of some foreign platform and
// returns a single precision floating point value in the byte order
// format that the host hardware supports.
//
// srcFormat:  An enumerated value describing the byte order format that the
//             srcValue is currently in.
//
// srcValue:  A struct containing a sequence of bytes representing a single
//            precision floating value from a foreign platform.
//
// ---------------------------------------------------------------------------
inline float SwapBytes::nativeFrom(ByteOrderFormat srcFormat,
    FloatByteBuf foreignBytes)
    {
    // No need to swap.  The value is already in the same byte
    // order as our host.
    if (hostByteOrder() == srcFormat)
        {
        float *retVal = (float*) &foreignBytes;
        return *retVal;
        }

    // Swap Away.
    union FloatUnion {
        short s[2];
        float f;
        } *floatUnion = (FloatUnion *)&foreignBytes;
    short tmp = swap(floatUnion->s[0]);
    floatUnion->s[0] = swap(floatUnion->s[1]);
    floatUnion->s[1] = tmp;
    return floatUnion->f;
    }

// -- public ---------------------------------------------------------------
// double SwapBytes::nativeFrom(ByteOrderFormat srcFormat,
//      DoubleByteBuf foreignBytes)
//
// Converts a sequence of bytes that represent a double precision
// floating point value in the byte order of some foreign platform and
// returns a double precision floating point value in the byte order
// format that the host hardware supports.
//
// srcFormat:  An enumerated value describing the byte order format that the
//             srcValue is currently in.
//
// srcValue:  A struct containing a sequence of bytes representing a double
//            precision floating value from a foreign platform.
//
// ---------------------------------------------------------------------------
inline double SwapBytes::nativeFrom(ByteOrderFormat srcFormat,
    DoubleByteBuf foreignBytes)
    {
    // No need to swap.  The value is already in the same byte
    // order as our host.
    if (hostByteOrder() == srcFormat)
        {
        double *retVal = (double*) &foreignBytes;
        return *retVal;
        }

    // Swap Away.
    union DoubleUnion {
        long l[2];
        double f;
        } *doubleUnion = (DoubleUnion *)&foreignBytes;
    long tmp = swap(doubleUnion->l[0]);
    doubleUnion->l[0] = swap(doubleUnion->l[1]);
    doubleUnion->l[1] = tmp;
    return doubleUnion->f;
    }

// -- private -------------------------------------------------------------
// short SwapBytes::swap (short shortVal)
//
// Convert and returns the specified short value with its byte order swapped.
// The value 0x1234 would be returned as 0x3412.
//
// shortVal:  Two byte signed integer that will be swapped.
//
// --------------------------------------------------------------------------
inline short SwapBytes::swap(short shortVal)
    {
    return ((shortVal & 0xFF00) >> 0x08) | ((shortVal & 0x00FF) << 0x08);
    }

// -- private -------------------------------------------------------------
// int SwapBytes::swap (int intVal)
//
// Convert and returns the specified int value with its byte order swapped.
// The value 0x12345678 would be stored as 0x78563412.
//
// intVal:  Four byte signed integer that will be swapped.
//
// --------------------------------------------------------------------------
inline int SwapBytes::swap(int intVal)
    {
    register int tmp = (intVal & 0x000000FF);
    tmp = ((intVal & 0x0000FF00) >> 0x08) | (tmp << 0x08);
    tmp = ((intVal & 0x00FF0000) >> 0x10) | (tmp << 0x08);
    return ((intVal & 0xFF000000) >> 0x18) | (tmp << 0x08);
    }

#endif
