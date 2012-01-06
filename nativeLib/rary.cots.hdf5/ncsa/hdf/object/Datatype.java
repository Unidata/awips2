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

package ncsa.hdf.object;

import java.util.List;

/**
 * Datatype is an abstract class that defines datatype characteristics and APIs for a data type.
 * <p>
 * A datatype has four basic characteristics: class, size, byte order and sign.
 * These charactertics are defeined in the 
 * <a href="http://hdfgroup.org/HDF5/doc/UG/index.html">HDF5 User's Guide</a>.
 * <p>
 * These charactertics apply to all the sub-classes. The sub-classes may have different 
 * ways to describe a datatype. We here define the <strong> native datatype</strong> to 
 * the datatype used by the sub-class. For example, H5Datatype uses a datatype identifier
 * (hid_t) to specify a datatype. NC2Datatype uses ucar.nc2.DataType object to describe
 * its dataype. "Native" here is different from the "native" definition in the HDF5 library.
 * <p>
 * Two functions, toNative() and fromNative(), are defined to convert the general charactertics
 * to/form the native datatype. Sub-classes must implement these functions so that the conversion 
 * will be done correctly.
 * <p>
 * @version 1.1 9/4/2007
 * @author Peter X. Cao
 */
public abstract class Datatype extends HObject
{
    /** 
     * The default definition for datatype size, order, and sign. 
     */
    public static final int NATIVE = -1;

    /**
     * See {@link <a href="http://hdfgroup.org/HDF5/doc/UG/index.html">HDF5 User's Guide</a>}
     */
    public static final int CLASS_NO_CLASS         = -1;

    /**
     * See {@link <a href="http://hdfgroup.org/HDF5/doc/UG/index.html">HDF5 User's Guide</a>}
     */
    public static final int CLASS_INTEGER          = 0;

    /**
     * See {@link <a href="http://hdfgroup.org/HDF5/doc/UG/index.html">HDF5 User's Guide</a>}
     */
    public static final int CLASS_FLOAT            = 1;

    /**
     * See {@link <a href="http://hdfgroup.org/HDF5/doc/UG/index.html">HDF5 User's Guide</a>}
     */
    public static final int CLASS_CHAR             = 2;

    /**
     * See {@link <a href="http://hdfgroup.org/HDF5/doc/UG/index.html">HDF5 User's Guide</a>}
     */
    public static final int CLASS_STRING           = 3;

    /**
     * See {@link <a href="http://hdfgroup.org/HDF5/doc/UG/index.html">HDF5 User's Guide</a>}
     */
    public static final int CLASS_BITFIELD         = 4;

    /**
     * See {@link <a href="http://hdfgroup.org/HDF5/doc/UG/index.html">HDF5 User's Guide</a>}
     */
    public static final int CLASS_OPAQUE           = 5;

    /**
     * See {@link <a href="http://hdfgroup.org/HDF5/doc/UG/index.html">HDF5 User's Guide</a>}
     */
    public static final int CLASS_COMPOUND         = 6;

    /**
     * See {@link <a href="http://hdfgroup.org/HDF5/doc/UG/index.html">HDF5 User's Guide</a>}
     */
    public static final int CLASS_REFERENCE        = 7;

    /**
     * See {@link <a href="http://hdfgroup.org/HDF5/doc/UG/index.html">HDF5 User's Guide</a>}
     */
    public static final int CLASS_ENUM             = 8;

    /**
     * See {@link <a href="http://hdfgroup.org/HDF5/doc/UG/index.html">HDF5 User's Guide</a>}
     */
    public static final int CLASS_VLEN             = 9;

    /**
     * See {@link <a href="http://hdfgroup.org/HDF5/doc/UG/index.html">HDF5 User's Guide</a>}
     */
    public static final int CLASS_ARRAY            = 10;

    /**
     * See {@link <a href="http://hdfgroup.org/HDF5/doc/UG/index.html">HDF5 User's Guide</a>}
     */
    public static final int ORDER_LE         = 0;

    /**
     * See {@link <a href="http://hdfgroup.org/HDF5/doc/UG/index.html">HDF5 User's Guide</a>}
     */
    public static final int ORDER_BE         = 1;

    /**
     * See {@link <a href="http://hdfgroup.org/HDF5/doc/UG/index.html">HDF5 User's Guide</a>}
     */
    public static final int ORDER_VAX        = 2;

    /**
     * See {@link <a href="http://hdfgroup.org/HDF5/doc/UG/index.html">HDF5 User's Guide</a>}
     */
    public static final int ORDER_NONE       = 3;

    // sign
    /**
     * See {@link <a href="http://hdfgroup.org/HDF5/doc/UG/index.html">HDF5 User's Guide</a>}
     */
    public static final int SIGN_NONE         = 0;

    /**
     * See {@link <a href="http://hdfgroup.org/HDF5/doc/UG/index.html">HDF5 User's Guide</a>}
     */
    public static final int SIGN_2            = 1;

    /**
     * See {@link <a href="http://hdfgroup.org/HDF5/doc/UG/index.html">HDF5 User's Guide</a>}
     */
    public static final int NSGN              = 2;
    
    /**
     * The class of the datatype.
     */
    protected int datatypeClass;

    /**
     * The size (in bytes)  of the datatype.
     */
    protected int datatypeSize;

    /**
     * The byte order of the datatype.
     * Valid values are ORDER_LE, ORDER_BE, and ORDER_VAX.
     */
    protected int datatypeOrder;

    /**
     * The sign of the datatype.
     */
    protected int datatypeSign;

    /**
     * The (name, value) pairs of enum members
     */
    protected String enumMembers;

    /**
     * The base datatype of every element of the array (for CLASS_ARRAY datatype).
     */
    protected Datatype baseType;
    
    /*
     * The dimension of the ARRAY element. For ARRAY datatype only
     */
    protected int[] dims;

    /**
     * Contructs a named datatype with a given file, name and path.
     * <p>
     * @param theFile the HDF file.
     * @param name the name of the datatype, e.g "12-bit Integer".
     * @param path the full group path of the datatype, e.g. "/datatypes/".
     */
    public Datatype(
        FileFormat theFile,
        String name,
        String path)
    {
        this (theFile, name, path, null);
    }
    
    /**
     * @deprecated  Not for public use in the future.<br>
     * Using {@link #Datatype(FileFormat, String, String)}
     */
    public Datatype(
        FileFormat theFile,
        String name,
        String path,
        long[] oid)
    {
        super (theFile, name, path, oid);
    }


    /**
     * Constructs a Datatype with specified class, size, byte order and sign.
     * <p>
     * The following is a list of a few example of H5Datatype.
     * <OL>
     * <LI>to create unsigned native integer<br>
     * H5Datatype type = new H5Dataype(CLASS_INTEGER, NATIVE, NATIVE, SIGN_NONE);
     * <LI>to create 16-bit signed integer with big endian<br>
     * H5Datatype type = new H5Dataype(CLASS_INTEGER, 2, ORDER_BE, NATIVE);
     * <LI>to create native float<br>
     * H5Datatype type = new H5Dataype(CLASS_FLOAT, NATIVE, NATIVE, -1);
     * <LI>to create 64-bit double<br>
     * H5Datatype type = new H5Dataype(CLASS_FLOAT, 8, NATIVE, -1);
     * </OL>
     * @param tclass the class of the datatype, e.g. CLASS_INTEGER, CLASS_FLOAT and etc.
     * @param tsize the size of the datatype in bytes, e.g. for a 32-bit integer, the size is 4.
     * @param torder the byte order of the datatype. Valid values are ORDER_LE, ORDER_BE, ORDER_VAX and ORDER_NONE
     * @param tsign the sign of the datatype. Valid values are SIGN_NONE, SIGN_2 and MSGN
     */
    public Datatype(int tclass, int tsize, int torder, int tsign)
    {
        datatypeClass = tclass;
        datatypeSize = tsize;
        datatypeOrder = torder;
        datatypeSign = tsign;
        enumMembers = null;
        baseType = null;
        dims = null;
    }

    /**
     * Constructs a Datatype with a given native datatype identifier.
     * <p>
     * For example, if the datatype identifier is a 32-bit unsigned integer created
     * from HDF5,
     * <pre>
     * int tid = H5.H5Tcopy( HDF5Constants.H5T_NATIVE_UNINT32);
     * Datatype dtype = new Datatype(tid);
     * </pre>
     * will construct a datatype equivalent to
     * new Datatype(CLASS_INTEGER, 4, NATIVE, SIGN_NONE);
     * <p>
     * @see #fromNative(int nativeID)
     * @param type the native datatype identifier.
     */
    public Datatype(int type)
    {
        this(CLASS_NO_CLASS, NATIVE, NATIVE, NATIVE);
    }

    /**
     * Returns the class of the datatype.
     * Valid values are: 
     * <ul>
        <li>CLASS_NO_CLASS
        <li>CLASS_INTEGER
        <li>CLASS_FLOAT
        <li>CLASS_CHAR
        <li>CLASS_STRING
        <li>CLASS_BITFIELD
        <li>CLASS_OPAQUE
        <li>CLASS_COMPOUND
        <li>CLASS_REFERENCE
        <li>CLASS_ENUM
        <li>CLASS_VLEN
        <li>CLASS_ARRAY
     * </ul>
     * 
     * @return the class of the datatype.
     */
    public int getDatatypeClass()
    {
        return datatypeClass;
    }

    /**
     * Returns the size of the datatype in bytes.
     * For example, for a 32-bit integer, the size is 4 (bytes).
     * 
     * @return the size of the datatype.
     */
    public int getDatatypeSize()
    {
        return datatypeSize;
    }

    /**
     * Returns the byte order of the datatype.
     * Valid values are
     * <ul>
     *   <li> ORDER_LE
     *   <li> ORDER_BE
     *   <li> ORDER_VAX
     *   <li> ORDER_NONE
     * </ul>
     * 
     * @return the byte order of the datatype.
     */
    public int getDatatypeOrder()
    {
        return datatypeOrder;
    }

    /**
     * Returns the sign (SIGN_NONE, SIGN_2 or NSGN) of an integer datatype.
     *
     * @return the sign of the datatype.
     */
    public int getDatatypeSign()
    {
        return datatypeSign;
    }
    
    /**
     * Returns the datatype of array element for ARRAY datatype.
     * <p>
     * For example, a dataset set of ARRAY of inteter, The datatype
     * of the dataset is ARRAY. The datatype of the base type is integer.
     *
     * @return the the datatype of array element for ARRAY datatype.
     */
    public Datatype getBasetype()
    {
        return baseType;
    }

    /**
     * Sets the (name, value) pairs of enum members for enum datatype.
     * <p>For Example,
     * <dl>
     *     <dt>setEnumMembers("lowTemp=-40, highTemp=90")</dt>
     *         <dd>sets the value of enum member lowTemp to -40 and highTemp to 90.</dd>
     *     <dt>setEnumMembers("lowTemp, highTemp")</dt>
     *         <dd>sets enum members to defaults, i.e. lowTemp=0 and highTemp=1</dd>
     *     <dt>setEnumMembers("lowTemp=10, highTemp")</dt>
     *         <dd>sets enum member lowTemp to 10 and highTemp to 11.</dd>
     * </dl>
     * @param enumStr the (name, value) pairs of enum members
     */
    public final void setEnumMembers(String enumStr) { enumMembers = enumStr; }

    /**
     * Returns the "name=value" pairs of enum members for enum datatype.
     * <p>For Example,
     * <dl>
     *     <dt>setEnumMembers("lowTemp=-40, highTemp=90")</dt>
     *         <dd>sets the value of enum member lowTemp to -40 and highTemp to 90.</dd>
     *     <dt>setEnumMembers("lowTemp, highTemp")</dt>
     *         <dd>sets enum members to defaults, i.e. lowTemp=0 and highTemp=1</dd>
     *     <dt>setEnumMembers("lowTemp=10, highTemp")</dt>
     *         <dd>sets enum member lowTemp to 10 and highTemp to 11.</dd>
     * </dl>
     * 
     * @return enumStr the (name, value) pairs of enum members
     */
    public final String getEnumMembers() { return enumMembers; }

    /**
     * Converts the datatype object to a native datatype.
     *
     * Subclasses must implement it so that this datatype will be converted accordingly.
     *  Use close() to close the native identifier; otherwise, the datatype will be left open.
     * <p>
     * For example, a HDF5 datatype created from<br>
     * <pre>
     * H5Dataype dtype = new H5Datatype(CLASS_INTEGER, 4, NATIVE, SIGN_NONE);
     * int tid = dtype.toNative();
     * </pre>
     * There "tid" will be the HDF5 datatype id of a 32-bit unsigned integer,
     * which is equivalent to
     * <pre>
     * int tid = H5.H5Tcopy( HDF5Constants.H5T_NATIVE_UNINT32);
     * </pre>
     *
     * @return the identifier of the native datatype.
     */
    public abstract int toNative();

    /**
     * Set datatype characteristics (class, size, byte order and sign) from a given datatye identifier.
     * <p>
     * Sub-classes must implement it so that this datatype will be converted accordingly.
     * <p>
     * For example, if the type identifier is a 32-bit unsigned integer created
     * from HDF5,
     * <pre>
     * H5Datatype dtype = new H5Datatype();
     * dtype.fromNative(HDF5Constants.H5T_NATIVE_UNINT32);
     * </pre>
     * Where dtype is equivalent to <br>
     * new H5Datatype(CLASS_INTEGER, 4, NATIVE, SIGN_NONE);
     * <p>
     * @param nativeID the datatype identifier.
     */
    public abstract void fromNative(int nativeID);

    /**
     *  Returns a short text description of this datatype.
     *  
     *  @return a short text description of this datatype
     */
    public String getDatatypeDescription()
    {
        String description = "Unknown";

        switch (datatypeClass)
        {
            case CLASS_INTEGER:
                if (datatypeSign == SIGN_NONE) {
                    description = String.valueOf(datatypeSize*8) + "-bit unsigned integer";
                } else {
                    description = String.valueOf(datatypeSize*8) + "-bit integer";
                }
                break;
            case CLASS_FLOAT:
                description = String.valueOf(datatypeSize*8) + "-bit floating-point";
                break;
            case CLASS_STRING:
                description = "String";
                break;
            case CLASS_REFERENCE:
                description = "Object reference";
                break;
            case CLASS_BITFIELD:
                description = "Bitfield";
                break;
            case CLASS_ENUM:
                description = "enum";
                break;
            case CLASS_ARRAY:
                description = "Array";
                break;
            case CLASS_COMPOUND:
                description = "Compound ";
                break;
            case CLASS_VLEN:
                description = "Variable-length";
                break;
            default:
                description = "Unknown";
                break;
        }

        return description;
    }

    /**
     *  Checks if this datatype is an unsigned integer.
     *  
     *  @return true if the datatype is an unsigned integer; otherwise, returns false.
     */
    public abstract boolean isUnsigned();

    /**
     * Opens access to this named datatype.
     * Sub-clases must replace this default implementation. For example, in H5Datatype,
     * open() function H5.H5Topen(loc_id, name) to get the datatype identifier. 
     * 
     * @return the datatype identifier if successful; otherwise returns negative value.
     */
    public int open() { return -1; }

    /** 
     * Closes a datatype identifier.
     * <p> 
     * Sub-clases must replace this default implementation.
     * 
     * @param id the datatype identifier to close.
     */
    public abstract void close(int id);
    
    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.DataFormat#getMetadata()
     */
    public List getMetadata() throws Exception { return null; }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.DataFormat#writeMetadata(java.lang.Object)
     */
    public void writeMetadata(Object info) throws Exception {;}

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.DataFormat#removeMetadata(java.lang.Object)
     */
    public void removeMetadata(Object info) throws Exception {;}
}
