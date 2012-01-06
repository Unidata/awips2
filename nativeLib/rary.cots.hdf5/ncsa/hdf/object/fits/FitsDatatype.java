/*****************************************************************************
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of the HDF Java Products distribution.                  *
 * The full copyright notice, including terms governing use, modification,   *
 * and redistribution, is contained in the files COPYING and Copyright.html. *
 * COPYING can be found at the root of the source code distribution tree.    *
 * Or, see http://hdfgroup.org/products/hdf-java/doc/Copyright.html.         *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 ****************************************************************************/

package ncsa.hdf.object.fits;

import ncsa.hdf.object.*;
import nom.tam.fits.*;

/**
 * Datatype encapsulates information of a datatype.
 * Information includes the class, size, endian of a datatype.
 * <p>
 * @version 1.1 9/4/2007
 * @author Peter X. Cao
 */
public class FitsDatatype extends Datatype
{
	public static final long serialVersionUID = HObject.serialVersionUID;

    private int nativeType;

    /**
     * Create an Datatype with specified class, size, byte order and sign.
     * The following list a few example of how to create a Datatype.
     * <OL>
     * <LI>to create unsigned native integer<br>
     * FitsDatatype type = new H5Dataype(CLASS_INTEGER, NATIVE, NATIVE, SIGN_NONE);
     * <LI>to create 16-bit signed integer with big endian<br>
     * FitsDatatype type = new H5Dataype(CLASS_INTEGER, 2, ORDER_BE, NATIVE);
     * <LI>to create native float<br>
     * FitsDatatype type = new H5Dataype(CLASS_FLOAT, NATIVE, NATIVE, -1);
     * <LI>to create 64-bit double<br>
     * FitsDatatype type = new H5Dataype(CLASS_FLOAT, 8, NATIVE, -1);
     * </OL>
     * <p>
     * @param tclass the class of the datatype.
     * @param tsize the size of the datatype in bytes.
     * @param torder the order of the datatype.
     * @param tsign the sign of the datatype.
     */
    public FitsDatatype(int tclass, int tsize, int torder, int tsign) {
        super(tclass, tsize, torder, tsign);
    }

    /**
     * Create a Datatype with a given fits native datatype.
     * <p>
     * @param theType the fits native datatype.
     */
    public FitsDatatype(int theType) {
        super(-1);
        nativeType = theType;
        fromNative(0);
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.DataFormat#hasAttribute()
     */
    public boolean hasAttribute () { return false; }

    /**
     * Allocate an one-dimensional array of byte, short, int, long, float, double,
     * or String to store data retrieved from an fits file based on the given
     * fits datatype and dimension sizes.
     * <p>
     * @param dtype the fits datatype.
     * @param size the total size of the array.
     * @return the array object if successful and null otherwise.
     */
    public static Object allocateArray(int dtype, int size) throws OutOfMemoryError
    {
        Object data = null;

        if (size <= 0 ) {
            return null;
        }

        switch (dtype) {
            case BasicHDU.BITPIX_BYTE:
                data = new byte[size];
                break;
            case BasicHDU.BITPIX_SHORT:
                data = new short[size];
                break;
            case BasicHDU.BITPIX_INT:
                data = new int[size];
                break;
            case BasicHDU.BITPIX_LONG:
                data = new long[size];
                break;
            case BasicHDU.BITPIX_FLOAT:
                data = new float[size];
                break;
            case BasicHDU.BITPIX_DOUBLE:
                data = new double[size];
                break;
        }

        return data;
    }

    /**
     * Translate fits datatype identifier into FitsDatatype.
     */
    public void fromNative() {
        fromNative(nativeType);
    }

    /**
     * Translate fits datatype identifier into FitsDatatype.
     * <p>
     * @param nativeID the fits native datatype.
     */
    public void fromNative(int dtype)
    {
        switch (dtype) {
            case BasicHDU.BITPIX_BYTE:
                datatypeClass = CLASS_INTEGER;
                datatypeSize = 1;
                break;
            case BasicHDU.BITPIX_SHORT:
                datatypeClass = CLASS_INTEGER;
                datatypeSize = 2;
                break;
            case BasicHDU.BITPIX_INT:
                datatypeClass = CLASS_INTEGER;
                datatypeSize = 4;
                break;
            case BasicHDU.BITPIX_LONG:
                datatypeClass = CLASS_INTEGER;
                datatypeSize = 8;
                break;
            case BasicHDU.BITPIX_FLOAT:
                datatypeClass = CLASS_FLOAT;
                datatypeSize = 4;
                break;
            case BasicHDU.BITPIX_DOUBLE:
                datatypeClass = CLASS_FLOAT;
                datatypeSize = 8;
                break;
        }
    }

    // implementing Datatype
    public String getDatatypeDescription() {
        String description = "Unknown data type.";

        switch (nativeType) {
            case BasicHDU.BITPIX_BYTE:
                description = "8-bit integer";
                break;
            case BasicHDU.BITPIX_SHORT:
                description = "16-bit integer";
                break;
            case BasicHDU.BITPIX_INT:
                description = "32-bit integer";
                break;
            case BasicHDU.BITPIX_LONG:
                description = "64-bit integer";
                break;
            case BasicHDU.BITPIX_FLOAT:
                description = "32-bit float";
                break;
            case BasicHDU.BITPIX_DOUBLE:
                description = "64-bit float";
                break;
            default:
                if (datatypeClass==Datatype.CLASS_STRING) {
                    description = "String";
                } else if (datatypeClass==Datatype.CLASS_CHAR) {
                    description = "Char";
                } else if (datatypeClass==Datatype.CLASS_INTEGER) {
                    description = "Integer";
                } else if (datatypeClass==Datatype.CLASS_FLOAT) {
                    description = "Float";
                }
                break;
        }

        return description;
    }

    // implementing Datatype
    public boolean isUnsigned() {
        return false;
    }

    // implementing Datatype
    public int toNative() {
        if (datatypeClass == CLASS_INTEGER) {
            if (datatypeSize == 1) {
                nativeType = BasicHDU.BITPIX_BYTE;
            } else if (datatypeSize == 2) {
                nativeType = BasicHDU.BITPIX_SHORT;
            } else if (datatypeSize == 4) {
                nativeType = BasicHDU.BITPIX_INT;
            } else if (datatypeSize == 8) {
                nativeType = BasicHDU.BITPIX_LONG;
            }
        } else if (datatypeClass == CLASS_FLOAT) {
            if (datatypeSize == 4) {
                nativeType = BasicHDU.BITPIX_FLOAT;
            } else if (datatypeSize == 8) {
                nativeType = BasicHDU.BITPIX_DOUBLE;
            }
        }

        return nativeType;
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.Datatype#close(int)
     */
    public void close(int id) {;}    
}
