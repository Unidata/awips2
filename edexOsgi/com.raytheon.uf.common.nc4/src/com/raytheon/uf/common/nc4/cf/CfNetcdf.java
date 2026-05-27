/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.common.nc4.cf;

import com.raytheon.uf.common.nc4.NcDimension;
import com.raytheon.uf.common.nc4.Netcdf;
import com.raytheon.uf.common.nc4.NetcdfException;

/**
 * Climate and Forecast conventions wrapper for netcdf
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
public class CfNetcdf extends Netcdf {

    /**
     * @param path
     * @throws NetcdfException
     */
    public CfNetcdf(String path) throws NetcdfException {
        super(path);
        addDefaultAttributes();
    }

    /**
     * @param path
     * @param mode
     * @throws NetcdfException
     */
    public CfNetcdf(String path, int mode) throws NetcdfException {
        super(path, mode);
        addDefaultAttributes();
    }

    /**
     * @param path
     * @param mode
     * @param initialSize
     * @param chunkSizeHint
     * @throws NetcdfException
     */
    public CfNetcdf(String path, int mode, int initialSize, int chunkSizeHint)
            throws NetcdfException {
        super(path, mode, initialSize, chunkSizeHint);
        addDefaultAttributes();
    }

    /**
     * Add default global attributes for CF file
     * 
     * @throws NetcdfException
     */
    private void addDefaultAttributes() throws NetcdfException {
        putStringAttribute(CfConstants.CONVENTIONS_ATTR,
                CfConstants.CONVENTIONS1_6);
    }

    /**
     * Define the X dimension for file. If the X/Y axes are in Long/Lat the X
     * dimension is longitude. The return value can be used to populate the
     * dimension axis values after {@link #endFileDefinition()} is called
     * 
     * @param <T>
     *            data type specific {@link NcDimension} sub class
     * @param name
     *            name of dimension
     * @param longName
     *            long name of dimension
     * @param len
     *            length of dimension
     * @param units
     *            units that this axis is in
     * @param dimClass
     *            class object for {@link NcDimension} sub class
     * @return
     * @throws NetcdfException
     */
    public <T extends NcDimension> T defineXDim(String name, String longName,
            int len, String units, Class<T> dimClass) throws NetcdfException {
        T rval = super.defineDim(name, len, dimClass);
        setDimAttrs(rval, longName, units, CfConstants.X_AXIS);
        return rval;
    }

    /**
     * Define the Y dimension for file. If the X/Y axes are in Long/Lat the Y
     * dimension is latitude. The return value can be used to populate the
     * dimension axis values after {@link #endFileDefinition()} is called
     * 
     * @param <T>
     *            data type specific {@link NcDimension} sub class
     * @param name
     *            name of dimension
     * @param longName
     *            long name of dimension
     * @param len
     *            length of dimension
     * @param units
     *            units that this axis is in
     * @param dimClass
     *            class object for {@link NcDimension} sub class
     * @return
     * @throws NetcdfException
     */
    public <T extends NcDimension> T defineYDim(String name, String longName,
            int len, String units, Class<T> dimClass) throws NetcdfException {
        T rval = super.defineDim(name, len, dimClass);
        setDimAttrs(rval, longName, units, CfConstants.Y_AXIS);
        return rval;
    }

    /**
     * Define the Z dimension for file. This is the horizontal axis. The return
     * value can be used to populate the dimension axis values after
     * {@link #endFileDefinition()} is called
     * 
     * @param <T>
     *            data type specific {@link NcDimension} sub class
     * @param name
     *            name of dimension
     * @param longName
     *            long name of dimension
     * @param len
     *            length of dimension
     * @param units
     *            units that this axis is in
     * @param upIsPositive
     *            true if increasing values on axis indicate an increase in
     *            horizontal distance
     * @param dimClass
     *            class object for {@link NcDimension} sub class
     * @return
     * @throws NetcdfException
     */
    public <T extends NcDimension> T defineZDim(String name, String longName,
            int len, String units, boolean upIsPositive, Class<T> dimClass)
            throws NetcdfException {
        T rval = super.defineDim(name, len, dimClass);
        setDimAttrs(rval, longName, units, CfConstants.Z_AXIS);
        String posVal = upIsPositive ? CfConstants.UP_POS : CfConstants.DWN_POS;
        rval.putStringAttribute(CfConstants.POS_ATTR, posVal);
        return rval;
    }

    /**
     * Define the T dimension for file. This is the temporal dimension. The
     * return value can be used to populate the dimension axis values after
     * {@link #endFileDefinition()} is called
     * 
     * @param <T>
     *            data type specific {@link NcDimension} sub class
     * @param name
     *            name of dimension
     * @param stdName
     *            standard name of dimension
     * @param len
     *            length of dimension
     * @param units
     *            units that this axis is in
     * @param dimClass
     *            class object for {@link NcDimension} sub class
     * @return
     * @throws NetcdfException
     */
    public <T extends NcDimension> T defineTimeDim(String name, String stdName,
            int len, String units, Class<T> dimClass) throws NetcdfException {
        T rval = super.defineDim(name, len, dimClass);
        setDimAttrs(rval, stdName, units, CfConstants.T_AXIS);
        rval.putStringAttribute(CfConstants.CAL_ATTR, CfConstants.GREG_CALENDAR);
        return rval;
    }

    /**
     * Set default dimension attributes
     * 
     * @param dim
     * @param stdName
     * @param units
     * @param axis
     * @throws NetcdfException
     */
    private void setDimAttrs(NcDimension dim, String stdName, String units,
            String axis) throws NetcdfException {
        dim.putStringAttribute(CfConstants.LONG_NAME_ATTR, stdName);
        dim.putStringAttribute(CfConstants.STANDARD_NAME_ATTR, stdName);
        dim.putStringAttribute(CfConstants.UNITS_ATTR, units);
        dim.putStringAttribute(CfConstants.AXIS_ATTR, axis);
    }
}
