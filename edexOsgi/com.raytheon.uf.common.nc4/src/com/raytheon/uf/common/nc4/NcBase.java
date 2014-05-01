/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.common.nc4;

import java.util.Arrays;

import org.apache.commons.lang.ArrayUtils;

import edu.mit.ll.netcdf.LLNetcdfAttrJNI;
import edu.mit.ll.netcdf.LLNetcdfException;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 7, 2013            bclement     Initial creation
 *
 * </pre>
 *
 * @author bclement
 * @version 1.0	
 */
public abstract class NcBase {
    
    protected int fileId;

    protected int varId;

    protected final LLNetcdfAttrJNI attrs = new LLNetcdfAttrJNI();

    protected void init(int fileId, int varId) {
        this.fileId = fileId;
        this.varId = varId;
    }
   
    /**
     * Add attribute. This method can only be called when file is in define mode
     * before {@link Netcdf#endFileDefinition()} is called.
     * 
     * @param name
     *            name of attribute
     * @param value
     *            value of attribute
     * @throws NetcdfException
     */
    public void putBytesAttribute(String name, byte[] value)
            throws NetcdfException {
        try {
            attrs.writeByteArrayAttr(fileId, varId, name, value);
        } catch (LLNetcdfException e) {
            throw new NetcdfException(e);
        }
    }

    /**
     * Add attribute. This method can only be called when file is in define mode
     * before {@link Netcdf#endFileDefinition()} is called.
     * 
     * @param name
     *            name of attribute
     * @param value
     *            value of attribute
     * @throws NetcdfException
     */
    public void putStringAttribute(String name, String value)
            throws NetcdfException {
        try {
            attrs.writeStringAttr(fileId, varId, name, value);
        } catch (LLNetcdfException e) {
            throw new NetcdfException(e);
        }
    }

    /**
     * Add attribute. This method can only be called when file is in define mode
     * before {@link Netcdf#endFileDefinition()} is called.
     * 
     * @param name
     *            name of attribute
     * @param value
     *            value of attribute
     * @throws NetcdfException
     */
    public void putStringsAttribute(String name, String[] value)
            throws NetcdfException {
        try {
            attrs.writeStringArrayAttr(fileId, varId, name, value.length, value);
        } catch (LLNetcdfException e) {
            throw new NetcdfException(e);
        }
    }

    /**
     * Add attribute. This method can only be called when file is in define mode
     * before {@link Netcdf#endFileDefinition()} is called.
     * 
     * @param name
     *            name of attribute
     * @param value
     *            value of attribute
     * @throws NetcdfException
     */
    public void putFloatsAttribute(String name, float[] value)
            throws NetcdfException {
        try {
            attrs.writeFloatArrayAttr(fileId, varId, name, value);
        } catch (LLNetcdfException e) {
            throw new NetcdfException(e);
        }
    }

    /**
     * Add attribute. This method can only be called when file is in define mode
     * before {@link Netcdf#endFileDefinition()} is called.
     * 
     * @param name
     *            name of attribute
     * @param value
     *            value of attribute
     * @throws NetcdfException
     */
    public void putDoublesAttribute(String name, double[] value)
            throws NetcdfException {
        try {
            attrs.writeDoubleArrayAttr(fileId, varId, name, value);
        } catch (LLNetcdfException e) {
            throw new NetcdfException(e);
        }
    }

    /**
     * Add attribute. This method can only be called when file is in define mode
     * before {@link Netcdf#endFileDefinition()} is called.
     * 
     * @param name
     *            name of attribute
     * @param value
     *            value of attribute
     * @throws NetcdfException
     */
    public void putShortsAttribute(String name, short[] value)
            throws NetcdfException {
        try {
            attrs.writeShortArrayAttr(fileId, varId, name, value);
        } catch (LLNetcdfException e) {
            throw new NetcdfException(e);
        }
    }

    /**
     * Add attribute. This method can only be called when file is in define mode
     * before {@link Netcdf#endFileDefinition()} is called.
     * 
     * @param name
     *            name of attribute
     * @param value
     *            value of attribute
     * @throws NetcdfException
     */
    public void putIntsAttribute(String name, int[] value)
            throws NetcdfException {
        try {
            attrs.writeIntegerArrayAttr(fileId, varId, name, value);
        } catch (LLNetcdfException e) {
            throw new NetcdfException(e);
        }
    }

    /**
     * Add attribute. This method can only be called when file is in define mode
     * before {@link Netcdf#endFileDefinition()} is called.
     * 
     * @param name
     * @param value
     * @throws NetcdfException
     */
    public void putNumberAttribute(String name, Number[] value)
            throws NetcdfException {
        if (value.length < 1) {
            throw new NetcdfException("Unable to write empty attribute array");
        }
        Class<?> c = value[0].getClass();
        if (c.equals(Byte.class)) {
            Byte[] tmp = Arrays.copyOf(value, value.length, Byte[].class);
            putBytesAttribute(name, ArrayUtils.toPrimitive((tmp)));
        } else if (c.equals(Short.class)) {
            Short[] tmp = Arrays.copyOf(value, value.length, Short[].class);
            putShortsAttribute(name, ArrayUtils.toPrimitive((tmp)));
        } else if (c.equals(Integer.class)) {
            Integer[] tmp = Arrays.copyOf(value, value.length, Integer[].class);
            putIntsAttribute(name, ArrayUtils.toPrimitive((tmp)));
        } else if (c.equals(Float.class)) {
            Float[] tmp = Arrays.copyOf(value, value.length, Float[].class);
            putFloatsAttribute(name, ArrayUtils.toPrimitive((tmp)));
        } else if (c.equals(Double.class)) {
            Double[] tmp = Arrays.copyOf(value, value.length, Double[].class);
            putDoublesAttribute(name, ArrayUtils.toPrimitive((tmp)));
        } else {
            throw new NetcdfException("Unable to write attribute of type: " + c);
        }
    }

    /**
     * @return the fileId
     */
    public int getFileId() {
        return fileId;
    }

    /**
     * @return the varId
     */
    public int getVarId() {
        return varId;
    }

}
