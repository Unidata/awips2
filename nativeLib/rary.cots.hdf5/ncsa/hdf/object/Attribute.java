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

 import java.lang.reflect.*;


/**
 * An attribute is a (name, value) pair of metadata attached to a primary
 * data object such as dataset, group or named datatype.
 * <p>
 * Like a dataset, an attribute has a name, datatype and dataspace. 
 * 
 * <p>
 * For more details on attibutes, see
 * {@link <a href="http://hdfgroup.org/HDF5/doc/UG/index.html">HDF5 User's Guide</a>}
 *<p>
 *
 * The following code is an example of an attribute with 1D integer array of two elements.
 * <pre>
 * // Example of creatinge a new attribute
 *
 * // The name of the new attribute
 * String name = "Data range";
 *
 * // Creating an unsigned 1-byte integer datatype
 * Datatype type = new Datatype(Datatype.CLASS_INTEGER, // class
 *                              1,                      // size in bytes
 *                              Datatype.ORDER_LE,      // byte order
 *                              Datatype.SIGN_NONE);    // signed or unsigned
 *
 * // 1-D array of size two
 * long[] dims = {2};
 *
 * // The value of the attribute
 * int[] value = {0, 255};
 *
 * // Create a new attribute
 * Attribute dataRange = new Attribute(name, type, dims);
 *
 * // Set the attribute value
 * dataRange.setValue(value);
 *
 * // See FileFormat.writeAttribute() for how to attach an attribute to an object, 
 * @see ncsa.hdf.object.FileFormat#writeAttribute(HObject, Attribute, boolean)
 * </pre>
 *
 * @see ncsa.hdf.object.Datatype
 *
 * @version 1.1 9/4/2007
 * @author Peter X. Cao
 */
public class Attribute implements Metadata
{
    /**
     * @see ncsa.hdf.object.HObject#serialVersionUID
     */
	public static final long serialVersionUID = HObject.serialVersionUID;

    /** The name of the attribute. */
    private final String name;

    /** The datatype of the attribute. */
    private final Datatype type;

    /** The rank of the data value of the attribute. */
    private int rank;

    /** The dimension sizes of the attribute. */
    private long[] dims;

    /** The value of the attribute. */
    private Object value;

    /** Flag to indicate if the datatype is an unsigned integer. */
    private boolean isUnsigned;

    /**
     * Create an attribute with specified name, data type and dimension sizes.
     * 
     * For scalar attribute, the dimension size can be either an array of size one
     * or null, and the rank can be either 1 or zero. Attribute is a general class
     * and is independent of file format, e.g., the implementation of attribute
     * applies to both HDF4 and HDF5.
     * <p>
     * The following example creates a string attribute with the name "CLASS" and value "IMAGE".
     * <pre>
        long[] attrDims = {1};
        String attrName = "CLASS";
        String[] classValue = {"IMAGE"};
        Datatype attrType = new H5Datatype(Datatype.CLASS_STRING, classValue[0].length()+1, -1, -1);
        Attribute attr = new Attribute(attrName, attrType, attrDims);
        attr.setValue(classValue);
     * </pre>
     * 
     * @param attrName the name of the attribute.
     * @param attrType the datatype of the attribute.
     * @param attrDims the dimension sizes of the attribute, null for scalar attribute
     *
     * @see ncsa.hdf.object.Datatype
     */
    public Attribute(String attrName, Datatype attrType, long[] attrDims)
    {
        this(attrName, attrType, attrDims, null);        
    }
    
    /**
     * Create an attribute with specific name and value.
     * 
     * For scalar attribute, the dimension size can be either an array of size one
     * or null, and the rank can be either 1 or zero. Attribute is a general class
     * and is independent of file format, e.g., the implementation of attribute
     * applies to both HDF4 and HDF5.
     * <p>
     * The following example creates a string attribute with the name "CLASS" and value "IMAGE".
     * <pre>
        long[] attrDims = {1};
        String attrName = "CLASS";
        String[] classValue = {"IMAGE"};
        Datatype attrType = new H5Datatype(Datatype.CLASS_STRING, classValue[0].length()+1, -1, -1);
        Attribute attr = new Attribute(attrName, attrType, attrDims, classValue);
     * </pre>
     * 
     * @param attrName the name of the attribute.
     * @param attrType the datatype of the attribute.
     * @param attrDims the dimension sizes of the attribute, null for scalar attribute
     * @param attrValue the value of the attribute, null if no value
     *
     * @see ncsa.hdf.object.Datatype
     */
    public Attribute(String attrName, Datatype attrType, long[] attrDims, Object attrValue)
    {
        name = attrName;
        type = attrType;
        dims = attrDims;
        value = null;
        rank = 0;

        if (dims != null) {
            rank = dims.length;
        }

        if (attrValue != null) {
            value = attrValue;
        }
        
        isUnsigned = (type.getDatatypeSign()==Datatype.SIGN_NONE);
    }
    

    /**
     * Returns the value of the attribute.
     * For atomic datatype, this will be an 1D array of integers, floats and strings.
     * For compound datatype, it will be an 1D array of strings with field members
     * separated by comma. For example, "{0, 10.5}, {255, 20.0}, {512, 30.0}" is a cmpound 
     * attribute of {int, float} of three data points.  
     * 
     * @return the value of the attribute, or null if failed to retrieve data from file.
     */
    public Object getValue()
    {
        return value;
    }

    /**
     * Sets the value of the attribute.
     * It returns null if failed to retrieve the name from file.
     *
     * @param theValue The value of the attribute to set
     */
    public void setValue(Object theValue)
    {
        value = theValue;
    }

    /**
     * Returns the name of the attribute.
     * 
     * @return the name of the attribute.
     */
    public String getName()
    {
        return name;
    }

    /**
     * Returns the rank (number of dimensions) of the attribute.
     * It returns a negative number if failed to retrieve the dimension 
     * information from file.
     * 
     * @return the number of dimensions of the attribute.
     */
    public int getRank()
    {
        return rank;
    }

    /**
     * Returns the dimension sizes of the data value of the attribute.
     * It returns null if failed to retrieve the dimension information from file.
     * 
     * @return the dimension sizes of the attribute. 
     */
    public long[] getDataDims()
    {
        return dims;
    }

    /**
     * Returns the datatype of the attribute.
     * It returns null if failed to retrieve the datatype information from file.
     * 
     * @return the datatype of the attribute.
     */
    public Datatype getType()
    {
        return type;
    }

    /**
     * Checks if the data type of this attribute is an unsigned integer.
     * 
     * @return true if the data type of the attribute is an unsigned integer; otherwise returns false.
     */
    public boolean isUnsigned()
    {
        return isUnsigned;
    }

    /**
     * Return the name of the attribute.
     * @see toString(String delimiter)
     */
    public String toString() {
        return name;
    }

    /**
     * Returns a string representation of the data value of the attribute.
     * For example, "0, 255". 
     * <p>
     * For compound datatype, it will be an 1D array of strings with field members
     * separated by comma. For example, "{0, 10.5}, {255, 20.0}, {512, 30.0}" is a cmpound 
     * attribute of {int, float} of three data points.
     * <p>
     * @param delimiter The delimiter to separate individual data point. 
     *        It can be comma, semicolon, tab or space. 
     *        For example, to String(",") will separate data by comma.
     *        
     * @return the string representation of the data values.
     */
    public String toString(String delimiter)
    {
        if (value == null) {
            return null;
        }

        Class valClass = value.getClass();

        if (!valClass.isArray()) {
            return value.toString();
        }

        // attribute value is an array
        StringBuffer sb = new StringBuffer();
        int n = Array.getLength(value);
        if (isUnsigned)
        {
            String cname = valClass.getName();
            char dname = cname.charAt(cname.lastIndexOf("[")+1);

            switch (dname)
            {
                case 'B':
                    byte[] barray = (byte[])value;
                    short sValue = barray[0];
                    if (sValue < 0) {
                        sValue += 256;
                    }
                    sb.append(sValue);
                    for (int i=1; i<n; i++)
                    {
                        sb.append(delimiter);
                        sValue = barray[i];
                        if (sValue < 0) {
                            sValue += 256;
                        }
                        sb.append(sValue);
                    }
                    break;
                case 'S':
                    short[] sarray = (short[])value;
                    int iValue = sarray[0];
                    if (iValue < 0) {
                        iValue += 65536;
                    }
                    sb.append(iValue);
                    for (int i=1; i<n; i++)
                    {
                        sb.append(delimiter);
                        iValue = sarray[i];
                        if (iValue < 0) {
                            iValue += 65536;
                        }
                        sb.append(iValue);
                    }
                    break;
                case 'I':
                    int[] iarray = (int[])value;
                    long lValue = iarray[0];
                    if (lValue < 0) {
                        lValue += 4294967296L;
                    }
                    sb.append(lValue);
                    for (int i=1; i<n; i++)
                    {
                        sb.append(delimiter);
                        lValue = iarray[i];
                        if (lValue < 0) {
                            lValue += 4294967296L;
                        }
                        sb.append(lValue);
                    }
                    break;
                default:
                    sb.append(Array.get(value, 0));
                    for (int i=1; i<n; i++)
                    {
                        sb.append(delimiter);
                        sb.append(Array.get(value, i));
                    }
                    break;
            }
        }
        else
        {
            sb.append(Array.get(value, 0));
            for (int i=1; i<n; i++)
            {
                sb.append(delimiter);
                sb.append(Array.get(value, i));
            }
        }

        return sb.toString();
    }
}
