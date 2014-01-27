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

import edu.mit.ll.netcdf.LLNetcdfDimJNI;
import edu.mit.ll.netcdf.LLNetcdfException;
import edu.mit.ll.netcdf.LLNetcdfVarJNI;

/**
 * Wrapper to NetCDF Dimension native library calls
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
public abstract class NcDimension extends NcBase {

    protected final int dimId;

    protected final String name;

    protected final int len;
    
    protected final int dataType;

    protected final LLNetcdfDimJNI dims = new LLNetcdfDimJNI();

    protected final LLNetcdfVarJNI vars = new LLNetcdfVarJNI();

    /**
     * @param fileId
     * @param name
     * @param len
     * @param dataType
     * @throws NetcdfException
     */
    public NcDimension(int fileId, String name, int len, int dataType)
            throws NetcdfException {
        this.name = name;
        this.len = len;
        this.dataType = dataType;
        try {
            this.dimId = dims.defineDim(fileId, name, len);
            int dimVar = vars.defineVar(fileId, name, dataType, 1,
                    new int[] { dimId });
            init(fileId, dimVar);
        } catch (LLNetcdfException e) {
            throw new NetcdfException(e);
        }
    }

    public static class ShortDimension extends NcDimension {

        /**
         * @param fileId
         * @param name
         * @param len
         * @throws NetcdfException
         */
        public ShortDimension(int fileId, String name, int len)
                throws NetcdfException {
            super(fileId, name, len, NcConstants.NC_SHORT);
        }

        /**
         * Write dimension axis values. This method can only be called when file
         * is in data mode after {@link Netcdf#endFileDefinition()} is called.
         * 
         * @param values
         * @throws NetcdfException
         */
        public void putDim(short[] values) throws NetcdfException {
            try {
                vars.writeShortArrayVar(fileId, varId, new int[] { 0 },
                        new int[] { values.length }, values);
            } catch (LLNetcdfException e) {
                throw new NetcdfException(e);
            }
        }
    }

    public static class IntDimension extends NcDimension {

        /**
         * @param fileId
         * @param name
         * @param len
         * @throws NetcdfException
         */
        public IntDimension(int fileId, String name, int len)
                throws NetcdfException {
            super(fileId, name, len, NcConstants.NC_INT);
        }

        /**
         * Write dimension axis values. This method can only be called when file
         * is in data mode after {@link Netcdf#endFileDefinition()} is called.
         * 
         * @param values
         * @throws NetcdfException
         */
        public void putDim(int[] values) throws NetcdfException {
            try {
                vars.writeIntegerArrayVar(fileId, varId, new int[] { 0 },
                        new int[] { values.length }, values);
            } catch (LLNetcdfException e) {
                throw new NetcdfException(e);
            }
        }
    }

    public static class FloatDimension extends NcDimension {

        /**
         * @param fileId
         * @param name
         * @param len
         * @throws NetcdfException
         */
        public FloatDimension(int fileId, String name, int len)
                throws NetcdfException {
            super(fileId, name, len, NcConstants.NC_FLOAT);
        }

        /**
         * Write dimension axis values. This method can only be called when file
         * is in data mode after {@link Netcdf#endFileDefinition()} is called.
         * 
         * @param values
         * @throws NetcdfException
         */
        public void putDim(float[] values) throws NetcdfException {
            try {
                vars.writeFloatArrayVar(fileId, varId, new int[] { 0 },
                        new int[] { values.length }, values);
            } catch (LLNetcdfException e) {
                throw new NetcdfException(e);
            }
        }
    }

    public static class DoubleDimension extends NcDimension {

        /**
         * @param fileId
         * @param name
         * @param len
         * @throws NetcdfException
         */
        public DoubleDimension(int fileId, String name, int len)
                throws NetcdfException {
            super(fileId, name, len, NcConstants.NC_DOUBLE);
        }

        /**
         * Write dimension axis values. This method can only be called when file
         * is in data mode after {@link Netcdf#endFileDefinition()} is called.
         * 
         * @param values
         * @throws NetcdfException
         */
        public void putDim(double[] values) throws NetcdfException {
            try {
                vars.writeDoubleArrayVar(fileId, varId, new int[] { 0 },
                        new int[] { values.length }, values);
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
     * @return the len
     */
    public int getLen() {
        return len;
    }


    /**
     * @return the dimId
     */
    public int getDimId() {
        return dimId;
    }

    /**
     * @return the dataType
     */
    public int getDataType() {
        return dataType;
    }

}
