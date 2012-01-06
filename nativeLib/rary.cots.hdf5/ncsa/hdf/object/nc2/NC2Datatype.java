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

package ncsa.hdf.object.nc2;

import ucar.ma2.DataType;
import ncsa.hdf.object.Datatype;
import ncsa.hdf.object.HObject;



/**
 * Datatype encapsulates information of a datatype.
 * Information includes the class, size, endian of a datatype.
 * <p>
 * @version 1.1 9/4/2007
 * @author Peter X. Cao
 */
public class NC2Datatype extends Datatype
{
	public static final long serialVersionUID = HObject.serialVersionUID;

    DataType nativeType = null;

    /**
     * Create an Datatype with specified class, size, byte order and sign.
     * The following list a few example of how to create a Datatype.
     * <OL>
     * <LI>to create unsigned native integer<br>
     * NC2Datatype type = new H5Dataype(CLASS_INTEGER, NATIVE, NATIVE, SIGN_NONE);
     * <LI>to create 16-bit signed integer with big endian<br>
     * NC2Datatype type = new H5Dataype(CLASS_INTEGER, 2, ORDER_BE, NATIVE);
     * <LI>to create native float<br>
     * NC2Datatype type = new H5Dataype(CLASS_FLOAT, NATIVE, NATIVE, -1);
     * <LI>to create 64-bit double<br>
     * NC2Datatype type = new H5Dataype(CLASS_FLOAT, 8, NATIVE, -1);
     * </OL>
     * <p>
     * @param tclass the class of the datatype.
     * @param tsize the size of the datatype in bytes.
     * @param torder the order of the datatype.
     * @param tsign the sign of the datatype.
     */
    public NC2Datatype(int tclass, int tsize, int torder, int tsign) {
        super(tclass, tsize, torder, tsign);
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.DataFormat#hasAttribute()
     */
    public boolean hasAttribute () { return false; }

    /**
     * Create a Datatype with a given Netcdf native datatype.
     * <p>
     * @param theType the netcdf native datatype.
     */
    public NC2Datatype(DataType theType) {
        super(-1);
        nativeType = theType;
        fromNative(0);
    }

    /**
     * Allocate an one-dimensional array of byte, short, int, long, float, double,
     * or String to store data retrieved from an Netcdf file based on the given
     * Netcdf datatype and dimension sizes.
     * <p>
     * @param dtype the netdcdf datatype.
     * @param size the total size of the array.
     * @return the array object if successful and null otherwise.
     */
    public static Object allocateArray(DataType dtype, int size) throws OutOfMemoryError
    {
        Object data = null;

        if ((size <= 0) || (dtype == null)) {
            return null;
        }

        if (dtype.equals(DataType.BYTE)) {
            data = new byte[size];
        } else if (dtype.equals(DataType.SHORT)) {
            data = new short[size];
        } else if (dtype.equals(DataType.INT)) {
            data = new int[size];
        } else if (dtype.equals(DataType.LONG)) {
            data = new long[size];
        } else if (dtype.equals(DataType.FLOAT)) {
            data = new float[size];
        } else if (dtype.equals(DataType.DOUBLE)) {
            data = new double[size];
        } else if (dtype.equals(DataType.STRING)) {
            data = new String[size];
        }

        return data;
    }

    /**
     * Translate Netcdf datatype identifier into NC2Datatype.
     * <p>
     * @param nativeID the netcdf native datatype.
     */
    public void fromNative(int tid)
    {
        if (nativeType == null) {
            return;
        }

        datatypeOrder = NATIVE;
        if (nativeType.equals(DataType.BYTE)) {
            datatypeClass = CLASS_INTEGER;
            datatypeSize = 1;
        } else if (nativeType.equals(DataType.SHORT)) {
            datatypeClass = CLASS_INTEGER;
            datatypeSize = 2;
        } else if (nativeType.equals(DataType.INT)) {
            datatypeClass = CLASS_INTEGER;
            datatypeSize = 4;
        } else if (nativeType.equals(DataType.LONG)) {
            datatypeClass = CLASS_INTEGER;
            datatypeSize = 8;
        } else if (nativeType.equals(DataType.FLOAT)) {
            datatypeClass = CLASS_FLOAT;
            datatypeSize = 4;
        } else if (nativeType.equals(DataType.DOUBLE)) {
            datatypeClass = CLASS_FLOAT;
            datatypeSize = 8;
        } else if (nativeType.equals(DataType.STRING)) {
            datatypeClass = CLASS_STRING;
            datatypeSize = 80; // default length. need to figure out the actual length
        }
    }

    // implementing Datatype
    public String getDatatypeDescription() {
        if (nativeType == null) {
            return "Unknown data type.";
        }

        return nativeType.toString();
    }

    // implementing Datatype
    public boolean isUnsigned() {
        return false;
    }

    // implementing Datatype
    public int toNative() {
        if (datatypeClass == CLASS_INTEGER) {
            if (datatypeSize == 1) {
                nativeType = DataType.BYTE;
            } else if (datatypeSize == 2) {
                nativeType = DataType.SHORT;
            } else if (datatypeSize == 4) {
                nativeType = DataType.INT;
            } else if (datatypeSize == 8) {
                nativeType = DataType.LONG;
            }
        } else if (datatypeClass == CLASS_FLOAT) {
            if (datatypeSize == 4) {
                nativeType = DataType.FLOAT;
            } else if (datatypeSize == 8) {
                nativeType = DataType.DOUBLE;
            }
        } else if (datatypeClass == CLASS_STRING) {
            nativeType = DataType.STRING;
        }

        return -1;
    }
    
    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.Datatype#close(int)
     */
    public void close(int id) {;}    
    
}
