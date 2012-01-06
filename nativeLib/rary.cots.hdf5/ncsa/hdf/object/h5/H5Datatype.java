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

package ncsa.hdf.object.h5;

import ncsa.hdf.hdf5lib.*;
import ncsa.hdf.hdf5lib.exceptions.*;
import ncsa.hdf.object.*;

import java.lang.reflect.Array;
import java.util.*;


/**
 * This class defines HDF5 data type characteristics and APIs for a data type.
 * <p>
 * This class provides several methods to convert an HDF5 dataype identifier to
 * a dataype object, and vice versa. A dataype object is described by four basic 
 * fields: datatype class, size, byte order, and sign, while an HDF5 dataype is 
 * presented by a datetype identifier. 
 * <p>
 * @version 1.1 9/4/2007
 * @author Peter X. Cao
 */
public class H5Datatype extends Datatype
{
    /**
     * @see ncsa.hdf.object.HObject#serialVersionUID
     */
    public static final long serialVersionUID = HObject.serialVersionUID;

    /**
     * The list of attributes of this data object.
     */
    private List attributeList;


    /** Flag to indicate if this datatype is a named datatype */
    private boolean isNamed=false;

    private int nAttributes = -1;

    private boolean isVLEN = false;
    
    private String description = null;

    /**
     * Constrcuts an named HDF5 data type object for a given file, dataset name and group path.
     * <p>
     * The datatype object represents an existing named datatype in file. For example, 
     * new H5Datatype(file, "dtype1", "/g0") constructs a datatype object that corresponds to
     * the dataset,"dset1", at group "/g0".
     * <p>
     * @param theFile the file that contains the dataset.
     * @param name the name of the dataset such as "dset1".
     * @param path the group path to the dataset such as "/g0/".
     */
    public H5Datatype(
            FileFormat theFile,
            String name,
            String path)
    {
        super (theFile, name, path, null);
    }

    /**
     * @deprecated  Not for public use in the future. <br>
     * Using {@link #H5Datatype(FileFormat, String, String)}
     */
    @Deprecated
    public H5Datatype(
            FileFormat theFile,
            String name,
            String path,
            long[] oid)
    {
        super (theFile, name, path, oid);

        if ((oid == null) && (theFile != null)) {
            // retrieve the object ID
            try {
                byte[] ref_buf = H5.H5Rcreate(theFile.getFID(), this.getFullName(), HDF5Constants.H5R_OBJECT, -1);
                this.oid = new long[1];
                this.oid[0] = HDFNativeData.byteToLong(ref_buf, 0);
            } catch (Exception ex) {}
        }
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
    public H5Datatype(int tclass, int tsize, int torder, int tsign)
    {
        super(tclass, tsize, torder, tsign);
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
     * @param nativeID the native datatype identifier.
     */
    public H5Datatype(int nativeID)
    {
        super(nativeID);

        description = getDatatypeDescription(nativeID);
        fromNative(nativeID);
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.DataFormat#hasAttribute()
     */
    public boolean hasAttribute () 
    { 
        if (nAttributes < 0) {
            int tid = -1;
            try
            {
                tid = H5.H5Topen(getFID(), getPath()+getName());
                fromNative(tid);
                nAttributes = H5.H5Aget_num_attrs(tid);
                isNamed = true;
            } catch (Exception ex) { nAttributes = 0;} 
            finally {
                try {H5.H5Tclose(tid);} catch (Exception ex){}
            }
        }

        return (nAttributes>0);
    }

    /**
     * Converts values in an Enumeration Datatype to names.
     * <p>
     * This method searches the identified enumeration datatype for 
     * the values appearing in <code>inValues</code> and returns the
     * names corresponding to those values.  If a given value is not
     * found in the enumeration datatype, the name corresponding to that
     * value will be set to <code>null</code> in the string array that is
     * returned.
     * <p>
     * If the method fails in general, null will be returned instead
     * of a String array.   An empty <code>inValues</code> parameter,
     * an <code>outNames</code> array with a different number of entries 
     * than the <code>inValues</code> array, or an invalid <code>tid</code>
     * would all cause general failure.
     *
     * @param tid The identifier of the enumeration datatype.
     * @param inValues The array of enumerations values to be converted.
     * @param outNames The array of names to be populated.  If null, the array 
     *         will be created.  If <code>outNames</code> is not null, the 
     *         number of entries must be the same as the number of values in 
     *         <code>inValues</code>.  
     * @return The string array of names if successful; 
     *         otherwise return null.
     * @throws HDF5Exception If there is an error at the HDF5 library level.
     *
     */
    public static final String[] convertEnumValueToName(
            int tid, Object inValues, String[] outNames) throws HDF5Exception
            {
        int inSize = 0;

        if ( (inValues == null) || 
                ( (inSize = Array.getLength(inValues)) <=0) || 
                ( (outNames != null) && (inSize != Array.getLength(outNames))) ) {
            return null;
        }

        int nMembers = H5.H5Tget_nmembers(tid);
        if (nMembers <=0 ) {
            return null;
        }

        if (outNames == null) {
            outNames = new String[inSize];
        } else {
            // set values in existing array to null in case no match found
            for ( int i = 0; i < inSize; i++ ) {
                outNames[i] = null;
            }
        }

        String[] names = new String[nMembers];
        int[] values = new int[nMembers];
        int[] theValue = {0};

        // Loop through the enumeration datatype and extract the names and
        // values.
        for (int i=0; i<nMembers; i++) {
            names[i] = H5.H5Tget_member_name(tid, i);
            H5.H5Tget_member_value(tid, i, theValue);
            values[i] = theValue[0];
        }

        int val = -1;

        // Look for matches
        for (int i=0; i<inSize; i++) {
            val = Array.getInt(inValues, i);
            for (int j=0; j<nMembers; j++) {
                if (val == values[j]) {
                    outNames[i] = names[j];
                    break;
                }
            }
        } 

        return outNames;
            }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.Datatype#fromNative(int)
     */
    @Override
    public void fromNative(int tid)
    {
        int tclass=-1, tsize=-1;
        boolean isChar=false, isUchar=false;

        try { 
            tclass = H5.H5Tget_class(tid);
            tsize = H5.H5Tget_size(tid);
            isVLEN = (tclass == HDF5Constants.H5T_VLEN);
        } catch (Exception ex) { datatypeClass = CLASS_NO_CLASS; };

        try { 
            isUchar = H5.H5Tequal(tid, HDF5Constants.H5T_NATIVE_UCHAR);
            isChar = (H5.H5Tequal(tid, HDF5Constants.H5T_NATIVE_CHAR) || isUchar);
        } catch (Exception ex) {};

        if (tclass == HDF5Constants.H5T_ARRAY) {
            int tmptid = -1;
            datatypeClass = CLASS_ARRAY;
            try {
                int ndims = H5.H5Tget_array_ndims(tid);
                dims = new int[ndims];
                H5.H5Tget_array_dims(tid, dims, null);
                tmptid = H5.H5Tget_super(tid);
                baseType = new H5Datatype(tmptid);
            } catch (Exception ex) {}
            finally {
                try {H5.H5Tclose(tmptid); } catch (Exception ex) {}
            }
        }
        else if (isChar) {
            datatypeClass = CLASS_CHAR;
            if (isUchar)
                datatypeSign = SIGN_NONE;
        } 
        else if (tclass == HDF5Constants.H5T_INTEGER)
        {
            datatypeClass = CLASS_INTEGER;
            try {
                int tsign = H5.H5Tget_sign(tid);
                if (tsign == HDF5Constants.H5T_SGN_NONE) {
                    datatypeSign = SIGN_NONE;
                }
            } catch (Exception ex) {}
        }
        else if (tclass == HDF5Constants.H5T_FLOAT) {
            datatypeClass = CLASS_FLOAT;
        } else if (tclass == HDF5Constants.H5T_STRING) {
            try { isVLEN = H5.H5Tis_variable_str(tid); } catch (Exception ex) {}

            datatypeClass = CLASS_STRING;
        } else if (tclass == HDF5Constants.H5T_REFERENCE) {
            datatypeClass = CLASS_REFERENCE;
        } else if (tclass == HDF5Constants.H5T_ENUM)
        {
            datatypeClass = CLASS_ENUM;
            try {
                int nMember = H5.H5Tget_nmembers(tid);
                String name = null;
                int[] val = new int[1];
                String enumStr = "";
                for (int i=0; i<nMember; i++)
                {
                    name = H5.H5Tget_member_name(tid, i);
                    H5.H5Tget_member_value(tid, i, val);
                    enumStr += name+"="+val[0]+",";
                }
                enumMembers = enumStr;;
            } catch (Exception ex) {}
        }

        if (isVLEN)
            datatypeSize = -1;
        else
            datatypeSize = tsize;

        datatypeOrder = NATIVE;
    }

    /**
     * @deprecated  Not for public use in the future.<br>
     * Using {@link ncsa.hdf.hdf5lib.H5#H5Tget_native_type(int)}
     * <p>
     * Return the HDF5 memory datatype identifier based on the HDF5 datatype identifier on disk
     * <p>
     * @param tid the datatype identifieron disk.
     * @return the memory datatype identifier if successful, and negative otherwise.
     */
    @Deprecated
    public static int toNative(int tid)
    {
        // data type information
        int native_type=-1;

        try {
            native_type = H5.H5Tget_native_type(tid);
        } catch (Exception ex) {}

        try {
            if (H5.H5Tis_variable_str(tid))
                H5.H5Tset_size (native_type, HDF5Constants.H5T_VARIABLE);
        } catch (Exception ex) {}

        return native_type;
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.Datatype#toNative()
     */
    @Override
    public int toNative()
    {
        int tid=-1, tmptid=-1;

        if (isNamed) {
            try {tid = H5.H5Topen(getFID(), getPath()+getName());}
            catch (Exception ex) {;}
        }

        if (tid >=0 ) {
            return tid;
        }

        // figure the datatype
        try {
            switch (datatypeClass)
            {
            case CLASS_ARRAY:
                try {
                    tmptid = baseType.toNative();
                    tid = H5.H5Tarray_create(tmptid, dims.length, dims, null);
                } finally { close(tmptid); }
                break;
            case CLASS_INTEGER:
            case CLASS_ENUM:
                if (datatypeSize == 1) {
                    tid = H5.H5Tcopy(HDF5Constants.H5T_NATIVE_INT8);
                } else if (datatypeSize == 2) {
                    tid = H5.H5Tcopy(HDF5Constants.H5T_NATIVE_INT16);
                } else if (datatypeSize == 4) {
                    tid = H5.H5Tcopy(HDF5Constants.H5T_NATIVE_INT32);
                } else if (datatypeSize == 8) {
                    tid = H5.H5Tcopy(HDF5Constants.H5T_NATIVE_INT64);
                } else {
                    tid = H5.H5Tcopy(HDF5Constants.H5T_NATIVE_INT);
                }

                if (datatypeOrder == Datatype.ORDER_BE) {
                    H5.H5Tset_order(tid, HDF5Constants.H5T_ORDER_BE);
                } else if (datatypeOrder == Datatype.ORDER_LE) {
                    H5.H5Tset_order(tid, HDF5Constants.H5T_ORDER_LE);
                }

                if (datatypeSign == Datatype.SIGN_NONE) {
                    H5.H5Tset_sign(tid, HDF5Constants.H5T_SGN_NONE);
                }
                break;
            case CLASS_FLOAT:
                if (datatypeSize == 8) {
                    tid = H5.H5Tcopy(HDF5Constants.H5T_NATIVE_DOUBLE);
                } else {
                    tid = H5.H5Tcopy(HDF5Constants.H5T_NATIVE_FLOAT);
                }

                if (datatypeOrder == Datatype.ORDER_BE) {
                    H5.H5Tset_order(tid, HDF5Constants.H5T_ORDER_BE);
                } else if (datatypeOrder == Datatype.ORDER_LE) {
                    H5.H5Tset_order(tid, HDF5Constants.H5T_ORDER_LE);
                }
                break;
            case CLASS_CHAR:
                if (datatypeSign == Datatype.SIGN_NONE) {
                    tid = H5.H5Tcopy(HDF5Constants.H5T_NATIVE_UCHAR);
                } else {
                    tid = H5.H5Tcopy(HDF5Constants.H5T_NATIVE_CHAR);
                }
                break;
            case CLASS_STRING:
                tid = H5.H5Tcopy(HDF5Constants.H5T_C_S1);
                if (isVLEN)
                    H5.H5Tset_size (tid, HDF5Constants.H5T_VARIABLE);
                else
                    H5.H5Tset_size(tid, datatypeSize);

                H5.H5Tset_strpad(tid, HDF5Constants.H5T_STR_NULLPAD);
                break;
            case CLASS_REFERENCE:
                if (datatypeSize > H5.H5Tget_size(HDF5Constants.H5T_STD_REF_OBJ)) {
                    tid = H5.H5Tcopy(HDF5Constants.H5T_STD_REF_DSETREG);
                } else {
                    tid = H5.H5Tcopy(HDF5Constants.H5T_STD_REF_OBJ);
                }
                break;
            } // switch (tclass)
        } catch (Exception ex) { tid = -1;}

        // set up enum members
        if (datatypeClass == CLASS_ENUM) {
            int ptid = tid;
            try {
                tid = H5.H5Tenum_create(ptid);
                String memstr, memname;
                int memval=0, idx;
                StringTokenizer token;

                // using "0" and "1" as default
                if (enumMembers == null) {
                    token = new StringTokenizer("0,1", ",");
                } else {
                    token = new StringTokenizer(enumMembers, ",");
                }

                while (token.hasMoreTokens()) {
                    memstr = token.nextToken();

                    if (memstr != null) {
                        memstr = memstr.trim();
                    }

                    if ((memstr==null) || (memstr.length()<1)) {
                        continue;
                    }

                    idx = memstr.indexOf('=');
                    if (idx>0) {
                        memname = memstr.substring(0, idx);
                        memval = Integer.parseInt(memstr.substring(idx+1));
                    } else {
                        memname = memstr;
                        memval++;
                    }
                    H5.H5Tenum_insert(tid, memname, memval);
                }
            } catch (Exception ex) { tid = -1;}

            try { H5.H5Tclose(ptid); } catch (Exception ex) {}
        } // if (datatypeClass == CLASS_ENUM) {

        return tid;
    }

    /**
     * Allocates an one-dimensional array of byte, short, int, long, float, double,
     * or String to store data in memory.
     * 
     * For example,
     * <pre>
     *     int tid = H5.H5Tcopy( HDF5Constants.H5T_NATIVE_INT32);
     *     int[] data  =(int[])allocateArray(tid, 100);
     * </pre>
     * returns a 32-bit integer array of size 100.
     * 
     * @param tid the datatype id.
     * @param size the total number of data points of the array.
     * @return the array object if successful; otherwise, return null.
     */
    public static Object allocateArray(int tid, int size) throws OutOfMemoryError
    {
        Object data = null;
        boolean isVL = false;
        boolean is_variable_str = false;
        boolean is_reg_ref = false;

        if (size < 0) {
            return null;
        }

        // Scalar members have dimensionality zero, i.e. size =0
        // what can we do about it, set the size to 1
        if (size == 0) {
            size = 1;
        }

        // data type information
        int tclass=-1, tsize=-1, tsign=-1;

        try
        {
            tclass = H5.H5Tget_class(tid);
            tsize = H5.H5Tget_size(tid);
            tsign = H5.H5Tget_sign(tid);
        } catch (Exception ex) {}

        try { is_variable_str = H5.H5Tis_variable_str(tid); } catch (Exception ex) {}
        try { isVL = (tclass==HDF5Constants.H5T_VLEN); } catch (Exception ex) {}
        try { is_reg_ref = H5.H5Tequal(tid, HDF5Constants.H5T_STD_REF_DSETREG); } catch (Exception ex) {}

        if (is_variable_str || isVL || is_reg_ref)
        {
            data = new String[size];
            for (int i=0; i<size; i++) {
                ((String[])data)[i] = "";
            }
        }
        else if (tclass == HDF5Constants.H5T_INTEGER)
        {
            if (tsize == 1) {
                data = new byte[size];
            } else if (tsize == 2) {
                data = new short[size];
            } else if (tsize == 4) {
                data = new int[size];
            } else if (tsize == 8) {
                data = new long[size];
            }
        }
        else if (tclass == HDF5Constants.H5T_ENUM) {
            // can be any integer
            // data = new int[size];
            int superTid = -1;
            try { 
                superTid = H5.H5Tget_super(tid);
                data =  allocateArray ( superTid, size); }
            catch (Exception ex) {}
            finally {
                try { H5.H5Tclose(superTid); } catch (Exception ex) {}
            }
        }
        else if (tclass == HDF5Constants.H5T_FLOAT)
        {
            if (tsize == 4) {
                data = new float[size];
            } else if (tsize == 8) {
                data = new double[size];
            }
        }
        else if ( (tclass == HDF5Constants.H5T_STRING) ||
                (tclass == HDF5Constants.H5T_REFERENCE) ||
                (tclass == HDF5Constants.H5T_BITFIELD))
        {
            data = new byte[size*tsize];
        }
        else if (tclass == HDF5Constants.H5T_ARRAY)
        {
            // use the base datatype to define the array
            int superTid = -1;
            try {
                int mn = H5.H5Tget_array_ndims(tid);
                int[] marray = new int[mn];

                H5.H5Tget_array_dims(tid, marray, null);
                int asize = 1;
                for (int j=0; j<mn; j++)
                {
                    asize *= marray[j];
                }

                superTid = H5.H5Tget_super(tid);
                data =  allocateArray (superTid, size*asize);
            } catch (Exception ex) {}
            finally {
                try { H5.H5Tclose(superTid); } catch (Exception ex) {}
            }
        }
        else {
            data = null;
        }

        return data;
    }

    /**
     *  Returns the size (in bytes) of a given datatype identifier.
     *  <p>
     *  It basically just calls H5Tget_size(tid).
     *
     *  @param tid  The datatype identifier.
     *  @return The size of the datatype in bytes.
     *  
     *  @see ncsa.hdf.hdf5lib.H5#H5Tget_size(int)
     */
    public static final int getDatatypeSize(int tid)
    {
        // data type information
        int tsize=-1;

        try {
            tsize = H5.H5Tget_size(tid);
        } catch (Exception ex) {tsize = -1; }

        return tsize;
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.Datatype#getDatatypeDescription()
     */
    @Override
    public String getDatatypeDescription()
    {
        if (description == null) {
            int tid = toNative();
            description = getDatatypeDescription(tid);
            close(tid);           
        }

        return description;
    }

    /**
     *  Returns a short description of a given datatype ID.
     *  
     * @param tid the HDF5 datatype identifier
     * @return a string describing the data type.
     */
    public static final String getDatatypeDescription(int tid)
    {
        String description = "Unknown";

        // data type information
        int tclass=-1, tsize=-1, tsign=-1, torder=-1;

        try
        {
            tclass = H5.H5Tget_class(tid);
            tsize = H5.H5Tget_size(tid);
            tsign = H5.H5Tget_sign(tid);
        } catch (Exception ex) {;}

        if (tclass == HDF5Constants.H5T_INTEGER)
        {
            if (tsize == 1)
            {
                try {
                    if (H5.H5Tequal(tid, HDF5Constants.H5T_NATIVE_UCHAR)) {
                        description = "8-bit unsigned character";
                    } else if (H5.H5Tequal(tid, HDF5Constants.H5T_NATIVE_CHAR)) {
                        description = "8-bit character";
                    } else if (tsign == HDF5Constants.H5T_SGN_NONE) {
                        description = "8-bit unsigned integer";
                    } else {
                        description = "8-bit integer";
                    }
                } catch (Exception ex) { description = "Unknown"; }
            }
            else if (tsize == 2)
            {
                if (tsign == HDF5Constants.H5T_SGN_NONE) {
                    description = "16-bit unsigned integer";
                } else {
                    description = "16-bit integer";
                }
            }
            else if (tsize == 4)
            {
                if (tsign == HDF5Constants.H5T_SGN_NONE) {
                    description = "32-bit unsigned integer";
                } else {
                    description = "32-bit integer";
                }
            }
            else if (tsize == 8)
            {
                if (tsign == HDF5Constants.H5T_SGN_NONE) {
                    description = "64-bit unsigned integer";
                } else {
                    description = "64-bit integer";
                }
            }
        }
        else if (tclass == HDF5Constants.H5T_FLOAT)
        {
            if (tsize == 4)
            {
                description = "32-bit floating-point";
            }
            else if (tsize == 8)
            {
                description = "64-bit floating-point";
            }
        }
        else if (tclass == HDF5Constants.H5T_STRING)
        {
            try {
                if ( H5.H5Tis_variable_str(tid )) {
                    description = "String, length = variable";
                } else {
                    description = "String, length = "+H5.H5Tget_size(tid);
                }
            }
            catch (Exception ex)
            {
                description = "String";
            }
        }
        else if (tclass == HDF5Constants.H5T_REFERENCE)
        {
            boolean is_reg_ref = false;
            try {is_reg_ref=H5.H5Tequal(tid, HDF5Constants.H5T_STD_REF_DSETREG);}
            catch (Exception ex) {}

            if (is_reg_ref) {
                description = "Dataset region reference";
            } else {
                description = "Object reference";
            }
        }
        else if (tclass == HDF5Constants.H5T_BITFIELD)
        {
            description = "Bitfield";
        }
        else if (tclass == HDF5Constants.H5T_ENUM)
        {
            description = "enum";
            String enames = " ( ";
            int[] evalue= {0};
            try {
                int n = H5.H5Tget_nmembers(tid );
                for (int i=0; i<n; i++)
                {
                    H5.H5Tget_member_value(tid, i, evalue);
                    enames += H5.H5Tget_member_name(tid, i);
                    enames += "="+evalue[0]+"  ";
                }
                enames += ")";
                description += enames;
            } catch (Exception ex) {}

        }
        else if (tclass == HDF5Constants.H5T_ARRAY)
        {
            description = "Array of ";
            // use the base datatype to define the array
            int tmptid = -1;
            try {
                tmptid = H5.H5Tget_super(tid);
                description += getDatatypeDescription(tmptid);
                int ndims = H5.H5Tget_array_ndims(tid);
                int adims[] = new int[ndims];
                try { H5.H5Tget_array_dims(tid, adims, null);}
                catch (Exception ex) {}
                description += " ("+adims[0];
                for (int j=1; j<ndims; j++)
                    description += "x"+adims[j];
                description += ")";
            } catch (Exception ex) {}
            finally {
                try {H5.H5Tclose(tmptid); } catch (Exception ex) {}
            }
        }
        else if (tclass == HDF5Constants.H5T_COMPOUND)
        {
            description = "Compound ";
            try {
                description += "{";
                int n = H5.H5Tget_nmembers(tid );
                int mtid = 0;
                try { H5.H5Tclose(mtid); } catch (Exception ex2) {;}
                for (int i=0; i<n; i++)
                {
                    mtid = H5.H5Tget_member_type(tid, i);
                    description += getDatatypeDescription(mtid) +", ";
                    try { H5.H5Tclose(mtid); } catch (Exception ex2) {;}
                }
                description += "}";
            } catch (Exception ex) {;}
        }
        else if (tclass == HDF5Constants.H5T_VLEN)
        {
            int tmptid = -1;
            try { 
                tmptid = H5.H5Tget_super(tid);
                description = "Variable-length of " +getDatatypeDescription(tmptid);
            }
            catch (Exception ex) {description = "Variable-length";}
            finally {
                try {H5.H5Tclose(tmptid); } catch (Exception ex) {}
            }
        } else if (tclass == HDF5Constants.H5T_OPAQUE) {
            description = "Opaque";
        } else {
            description = "Unknown";
        }

        return description;
    }


    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.Datatype#isUnsigned()
     */
    @Override
    public boolean isUnsigned()
    {
        return (datatypeSign == SIGN_NONE);
    }

    /**
     *  Checks if a datatype specified by the identifier is an unsigned integer.
     *  <p>
     *  @param datatype  the datatype ID to be checked.
     *  
     *  @return true is the datatype is an unsigned integer; otherwise returns false.
     */
    public static final boolean isUnsigned(int datatype)
    {
        boolean unsigned = false;;

        try
        {
            int tsign = H5.H5Tget_sign(datatype);
            if (tsign == HDF5Constants.H5T_SGN_NONE) {
                unsigned = true;
            }
        } catch (Exception ex) {
            unsigned = false;
        }

        return unsigned;
    }

    /**
     * Opens access to a named datatype.
     * <p>
     * It calls H5.H5Topen(loc, name). 
     * 
     * @return the datatype identifier if successful; otherwise returns negative value.
     * 
     * @see ncsa.hdf.hdf5lib.H5#H5Topen(int, String)
     */
    @Override
    public int open()
    {
        int tid = -1;

        try
        {
            tid = H5.H5Topen(getFID(), getPath()+getName());
        } catch (HDF5Exception ex)
        {
            tid = -1;
        }

        return tid;
    }

    /**
     * Closes a datatype identifier.
     * <p>
     * It calls H5.H5close(tid).
     * 
     * @param tid the datatype ID to close
     */
    @Override
    public void close(int tid)    {
        try { H5.H5Tclose(tid); }
        catch (HDF5Exception ex) {;}
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.Datatype#getMetadata()
     */
    @Override
    public List getMetadata() throws HDF5Exception
    {
        // load attributes first
        if (attributeList == null)
        {
            int tid = open();

            try {
                attributeList = H5File.getAttribute(tid);
            } catch (Exception ex) {}
            finally  {
                close(tid);
            }
        } // if (attributeList == null)

        return attributeList;
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.Datatype#writeMetadata(java.lang.Object)
     */
    @Override
    public void writeMetadata(Object info) throws Exception
    {
        // only attribute metadata is supported.
        if (!(info instanceof Attribute)) {
            return;
        }

        boolean attrExisted = false;
        Attribute attr = (Attribute)info;
        String name = attr.getName();

        if (attributeList == null) {
            this.getMetadata();
        } else {
            attrExisted = attributeList.contains(attr);
        }

        getFileFormat().writeAttribute(this, attr, attrExisted);

        // add the new attribute into attribute list
        if (!attrExisted) {
            attributeList.add(attr);
            nAttributes = attributeList.size();
        }
    }

    /*
     * (non-Javadoc)
     * @see ncsa.hdf.object.Datatype#removeMetadata(java.lang.Object)
     */
    @Override
    public void removeMetadata(Object info) throws HDF5Exception
    {
        // only attribute metadata is supported.
        if (!(info instanceof Attribute)) {
            return;
        }

        Attribute attr = (Attribute)info;
        int tid = open();
        try {
            H5.H5Adelete(tid, attr.getName());
            List attrList = getMetadata();
            attrList.remove(attr);
            nAttributes = attributeList.size();
        } finally {
            close(tid);
        }
    }
}
