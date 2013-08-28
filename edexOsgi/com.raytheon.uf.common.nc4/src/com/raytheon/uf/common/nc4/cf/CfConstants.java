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

/**
 * CF Constants as described for NetCDF library 1.6
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
public class CfConstants {

    /**
     * Global attribute {@link #CONVENTIONS_ATTR} value for version 1.6
     */
    public static final String CONVENTIONS1_6 = "CF-1.6";

    /**
     * Global attribute for conventions version
     */
    public static final String CONVENTIONS_ATTR = "Conventions";

    /**
     * Variable attribute for standard name
     */
    public static final String STANDARD_NAME_ATTR = "standard_name";

    /**
     * Variable attribute for long name
     */
    public static final String LONG_NAME_ATTR = "long_name";

    /**
     * Variable attribute for units
     */
    public static final String UNITS_ATTR = "units";

    /**
     * Variable attribute for cell methods
     */
    public static final String CELL_METHODS_ATTR = "cell_methods";

    /**
     * Dimension attribute for axis label (x, y, z...)
     */
    public static final String AXIS_ATTR = "axis";

    /**
     * Attribute value for {@link #AXIS_ATTR} for x axis
     */
    public static final String X_AXIS = "X";

    /**
     * Attribute value for {@link #AXIS_ATTR} for y axis
     */
    public static final String Y_AXIS = "Y";

    /**
     * Attribute value for {@link #AXIS_ATTR} for vertical axis
     */
    public static final String Z_AXIS = "Z";

    /**
     * Attribute value for {@link #AXIS_ATTR} for time axis
     */
    public static final String T_AXIS = "T";

    /**
     * Variable attribute for missing values
     */
    public static final String MISSING_VAL_ATTR = "missing_value";

    /**
     * {@link #UNITS_ATTR} value for latitude dimension
     */
    public static final String LAT_UNITS = "degrees_north";

    /**
     * {@link #UNITS_ATTR} value for longitude dimension
     */
    public static final String LON_UNITS = "degrees_east";

    /**
     * Vertical dimension attribute to denote which direction is positive
     */
    public static final String POS_ATTR = "positive";

    /**
     * {@link #POS_ATTR} value indicating that up is positive
     */
    public static final String UP_POS = "up";

    /**
     * {@link #POS_ATTR} value indicating that down is positive
     */
    public static final String DWN_POS = "down";

    /**
     * Calendar attribute for Time Dimensions
     */
    public static final String CAL_ATTR = "calendar";

    /**
     * Default value for {@link #CAL_ATTR}
     */
    public static final String GREG_CALENDAR = "gregorian";

    /**
     * {@link #UNITS_ATTR} value for time dimension that uses UNIX time
     */
    public static final String UNIX_TIME_UNITS = "seconds since 1970-1-1 0:0:0";

    /**
     * attribute for coverage id
     */
    public static final String COVERAGE_ID_ATTR = "coverage_id";

    /**
     * Grid mapping name attribute
     */
    public static final String GRID_MAP_NAME_ATTR = "grid_mapping_name";

    /**
     * standard parallel attribute used in grid mapping variable
     */
    public static final String STD_PARALLEL_ATTR = "standard_parallel";

    /**
     * central meridian longitude attribute used in grid mapping variable
     */
    public static final String CENTRAL_MERID_LON_ATTR = "longitude_of_central_meridian";

    /**
     * projection origin latitude attribute used in grid mapping variable
     */
    public static final String PROJ_ORIGIN_LAT_ATTR = "latitude_of_projection_origin";

    /**
     * attribute for list of variables that contain geographic coordinates
     */
    public static final String COORDS_ATTR = "coordinates";

    /**
     * Standard name value for projected x dimension
     */
    public static final String X_STD_NAME = "projection_x_coordinate";

    /**
     * Standard name value for projected y dimension
     */
    public static final String Y_STD_NAME = "projection_y_coordinate";

}
