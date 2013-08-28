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
import edu.mit.ll.netcdf.LLNetcdfJNI;

/**
 * Wrapper to NetCDF 4 native library.
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
public class Netcdf extends NcBase {

    protected final LLNetcdfJNI ncfile = new LLNetcdfJNI();
    
    /**
     * create mode for NetCDF 4 using classic model
     */
    public static final int NETCDF4_CLASSIC_MODE = NcConstants.NC_CLASSIC_MODEL
            | NcConstants.NC_NETCDF4;


    /**
     * @param path
     *            absolute path of file
     * @throws NetcdfException
     */
    public Netcdf(String path) throws NetcdfException {
        this(path, NcConstants.NC_CLOBBER);
    }
    
    /**
     * @param path
     *            absolute path of file
     * @param mode
     *            create mode passed to nc_open
     * @throws NetcdfException
     */
    public Netcdf(String path, int mode) throws NetcdfException {
        this(path, mode, NcConstants.NC_SIZEHINT_DEFAULT,
                NcConstants.NC_SIZEHINT_DEFAULT);
    }

    /**
     * @param path
     *            absolute path of file
     * @param mode
     *            create mode passed to nc__open
     * @param initialSize
     *            initial size hint passed to nc__open
     * @param chunkSizeHint
     *            buffer size hint passed to nc__open
     * @throws NetcdfException
     */
    public Netcdf(String path, int mode, int initialSize, int chunkSizeHint)
            throws NetcdfException {
        try {
            int fileId = ncfile.createFile(path, mode, initialSize,
                    chunkSizeHint);
            init(fileId, NcConstants.NC_GLOBAL);
        } catch (LLNetcdfException e) {
            throw new NetcdfException(e);
        }
    }

    /**
     * Define a NetCDF dimension in file. The return value can be used to
     * populate the dimension axis values after {@link #endFileDefinition()} is
     * called
     * 
     * @param <T>
     *            data type specific {@link NcDimension} sub class
     * @param name
     *            name of dimension
     * @param len
     *            length of dimension
     * @param dimClass
     *            class object for {@link NcDimension} sub class
     * @return datatype specific {@link NcDimension} object
     * @throws NetcdfException
     */
    public <T extends NcDimension> T defineDim(String name, int len,
            Class<T> dimClass) throws NetcdfException {
        try {
            return NcFactory.createDim(fileId, name, len, dimClass);
        } catch (Exception e) {
            throw new NetcdfException("Unable to create dimension", e);
        }
    }

    /**
     * Define a NetCDF variable in file. The return value can be used to
     * populate the variable values after {@link #endFileDefinition()} is called
     * 
     * @param <T>
     *            data type specific {@link NcVariable} sub class
     * @param name
     *            name of variable
     * @param dims
     *            array of dimensions for this variable in proper order
     * @param varClass
     *            class object for {@link NcVariable} sub class
     * @return datatype specific {@link NcVariable} object
     * @throws NetcdfException
     */
    public <T extends NcVariable> T defineVar(String name, NcDimension[] dims,
            Class<T> varClass) throws NetcdfException {
        try {
            return NcFactory.createVar(fileId, name, dims, varClass);
        } catch (Exception e) {
            throw new NetcdfException("Unable to create variable", e);
        }
    }

    /**
     * End definition stage of file creation and start data writing stage. After
     * this method is called, return values of
     * {@link #defineDim(String, int, Class)} and
     * {@link #defineVar(String, NcDimension[], Class)} can be used to write
     * data to file.
     * 
     * @throws NetcdfException
     */
    public void endFileDefinition() throws NetcdfException {
        try {
            ncfile.endFileDefinition(fileId);
        } catch (LLNetcdfException e) {
            throw new NetcdfException(e);
        }
    }

    /**
     * Called after all data has been written to file.
     * 
     * @throws NetcdfException
     */
    public void close() throws NetcdfException {
        try {
            ncfile.closeFile(fileId);
        } catch (LLNetcdfException e) {
            throw new NetcdfException(e);
        }
    }

    /**
     * @return the ncid
     */
    public int getFileId() {
        return fileId;
    }

}
