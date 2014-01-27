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

import edu.mit.ll.netcdf.LLNetcdfException;
import edu.mit.ll.netcdf.LLNetcdfVarJNI;

/**
 * Wrapper for NetCDF variable native library
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 5, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public abstract class NcVariable extends NcBase {

    protected final String name;

    protected final NcDimension[] dims;

    protected final int dataType;

    protected final LLNetcdfVarJNI vars = new LLNetcdfVarJNI();

    /**
     * @param fileId
     * @param name
     * @param dataType
     * @param dims
     * @throws NetcdfException
     */
    public NcVariable(int fileId, String name, int dataType, NcDimension[] dims)
            throws NetcdfException {
        this.name = name;
        this.dims = dims;
        this.dataType = dataType;
        int[] dimIds = new int[dims.length];
        for (int i = 0; i < dims.length; ++i) {
            dimIds[i] = dims[i].getDimId();
        }
        try {
            int varId = vars.defineVar(fileId, name, dataType, dims.length,
                    dimIds);
            init(fileId, varId);
        } catch (LLNetcdfException e) {
            throw new NetcdfException(e);
        }
    }

    public static class ByteVariable extends NcVariable {

        /**
         * @param fileId
         * @param name
         * @param dataType
         * @param dims
         * @throws NetcdfException
         */
        public ByteVariable(int fileId, String name, NcDimension[] dims)
                throws NetcdfException {
            super(fileId, name, NcConstants.NC_BYTE, dims);
        }

        /**
         * Write data to variable. This method can only be called when file is
         * in data mode after {@link Netcdf#endFileDefinition()} is called.
         * 
         * @param startIndex
         *            starting indexes for each dimension
         * @param shape
         *            length of each dimension in value
         * @param value
         *            data to be written
         * @throws NetcdfException
         */
        public void putVar(int[] startIndex, int[] shape, byte[] value)
                throws NetcdfException {
            try {
                vars.writeByteArrayVar(fileId, varId, startIndex, shape, value);
            } catch (LLNetcdfException e) {
                throw new NetcdfException(e);
            }
        }

    }

    public static class ShortVariable extends NcVariable {

        /**
         * @param fileId
         * @param name
         * @param dataType
         * @param dims
         * @throws NetcdfException
         */
        public ShortVariable(int fileId, String name, NcDimension[] dims)
                throws NetcdfException {
            super(fileId, name, NcConstants.NC_SHORT, dims);
        }

        /**
         * Write data to variable. This method can only be called when file is
         * in data mode after {@link Netcdf#endFileDefinition()} is called.
         * 
         * @param startIndex
         *            starting indexes for each dimension
         * @param shape
         *            length of each dimension in value
         * @param value
         *            data to be written
         * @throws NetcdfException
         */
        public void putVar(int[] startIndex, int[] shape, short[] value)
                throws NetcdfException {
            try {
                vars.writeShortArrayVar(fileId, varId, startIndex, shape, value);
            } catch (LLNetcdfException e) {
                throw new NetcdfException(e);
            }
        }

    }

    public static class IntVariable extends NcVariable {

        /**
         * @param fileId
         * @param name
         * @param dataType
         * @param dims
         * @throws NetcdfException
         */
        public IntVariable(int fileId, String name, NcDimension[] dims)
                throws NetcdfException {
            super(fileId, name, NcConstants.NC_INT, dims);
        }

        /**
         * Write data to variable. This method can only be called when file is
         * in data mode after {@link Netcdf#endFileDefinition()} is called.
         * 
         * @param startIndex
         *            starting indexes for each dimension
         * @param shape
         *            length of each dimension in value
         * @param value
         *            data to be written
         * @throws NetcdfException
         */
        public void putVar(int[] startIndex, int[] shape, int[] value)
                throws NetcdfException {
            try {
                vars.writeIntegerArrayVar(fileId, varId, startIndex, shape,
                        value);
            } catch (LLNetcdfException e) {
                throw new NetcdfException(e);
            }
        }

    }

    public static class FloatVariable extends NcVariable {

        /**
         * @param fileId
         * @param name
         * @param dataType
         * @param dims
         * @throws NetcdfException
         */
        public FloatVariable(int fileId, String name, NcDimension[] dims)
                throws NetcdfException {
            super(fileId, name, NcConstants.NC_FLOAT, dims);
        }

        /**
         * Write data to variable. This method can only be called when file is
         * in data mode after {@link Netcdf#endFileDefinition()} is called.
         * 
         * @param startIndex
         *            starting indexes for each dimension
         * @param shape
         *            length of each dimension in value
         * @param value
         *            data to be written
         * @throws NetcdfException
         */
        public void putVar(int[] startIndex, int[] shape, float[] value)
                throws NetcdfException {
            try {
                vars.writeFloatArrayVar(fileId, varId, startIndex, shape, value);
            } catch (LLNetcdfException e) {
                throw new NetcdfException(e);
            }
        }

    }

    public static class DoubleVariable extends NcVariable {

        /**
         * @param fileId
         * @param name
         * @param dataType
         * @param dims
         * @throws NetcdfException
         */
        public DoubleVariable(int fileId, String name, NcDimension[] dims)
                throws NetcdfException {
            super(fileId, name, NcConstants.NC_DOUBLE, dims);
        }

        /**
         * Write data to variable. This method can only be called when file is
         * in data mode after {@link Netcdf#endFileDefinition()} is called.
         * 
         * @param startIndex
         *            starting indexes for each dimension
         * @param shape
         *            length of each dimension in value
         * @param value
         *            data to be written
         * @throws NetcdfException
         */
        public void putVar(int[] startIndex, int[] shape, double[] value)
                throws NetcdfException {
            try {
                vars.writeDoubleArrayVar(fileId, varId, startIndex, shape,
                        value);
            } catch (LLNetcdfException e) {
                throw new NetcdfException(e);
            }
        }

    }

    /**
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * @return the dims
     */
    public NcDimension[] getDims() {
        return dims;
    }

    /**
     * @return the dataType
     */
    public int getDataType() {
        return dataType;
    }

}
