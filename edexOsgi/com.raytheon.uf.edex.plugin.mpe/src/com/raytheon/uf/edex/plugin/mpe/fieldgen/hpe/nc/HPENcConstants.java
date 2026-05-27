/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe.nc;

/**
 * Constants used for HPE Field Gen NetCDF generation.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 19, 2016 5631       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class HPENcConstants {

    public static final class DimensionConstants {

        public static final String DIM_HRAPX = "hrapx";

        public static final String DIM_HRAPY = "hrapy";

        public static final String DIM_LATLONG = "latlong";

        public static final long DIM_LATLONG_VALUE = 4L;

        public static final String DIM_DATES = "dates";

        public static final long DIM_DATES_VALUE = 11L;

        protected DimensionConstants() {
        }
    }

    public static final class VariableConstants {

        /*
         * Names of variables.
         */

        public static final String LAT = "lat";

        public static final String LON = "lon";

        public static final String TRUE_LAT = "true_lat";

        public static final String TRUE_LON = "true_lon";

        public static final String TIMEOFDATA = "timeofdata";

        public static final String TIMEOFCREATION = "timeofcreation";

        public static final String HRAP_XOR = "hrap_xor";

        public static final String HRAP_YOR = "hrap_yor";

        /*
         * Attributes.
         */

        public static final String LONG_NAME = "long_name";

        public static final String UNITS = "units";

        public static final String GRID = "grid";

        public static final String RESOLUTION = "resolution";

        public static final String DATEOFDATA = "dateofdata";

        public static final String DATEOFCREATION = "dateofcreation";

        public static final String SOURCE = "source";

        public static final String COMMENTS = "comments";

        public static final String ORDER = "order";

        /*
         * Attribute values.
         */

        public static final String COMMENTS_PROCESS_VALUE = "preliminary data...subject to change";

        public static final String COMMENTS_HRAP_XOR_VALUE = "offset in x direction of hrap grid";

        public static final String COMMENTS_HRAP_YOR_VALUE = "offset in y direction of hrap grid";

        public static final String LAT_ORDER = "bottom_left,bottom_right,top_right,top_left";

        public static final String LON_ORDER = "bottom_left,bottom_right,top_right,top_left";

        /*
         * Possible values based on the process id.
         */

        public static final String DHR_PROC_NAME = "preciprate";

        public static final String DHR_LONG_NAME_VALUE = "precipitation rate";

        public static final String DHR_UNITS_VALUE = "mm/hr";

        public static final String DHR_GRID_LABEL_VALUE = "1/4 hrap grid";

        public static final String DHR_RES_LABEL_VALUE = "1km*1km";

        public static final String DSP_PROC_NAME = "stormtotalprecip";

        public static final String DSP_LONG_NAME_VALUE = "storm total precipitation";

        public static final String DSP_UNITS_VALUE = "mm";

        public static final String DSP_GRID_LABEL_VALUE = "1/4 hrap grid";

        public static final String DSP_RES_LABEL_VALUE = "1km*1km";

        public static final String OTHER_PROC_NAME = "amountofprecip";

        public static final String OTHER_LONG_NAME_VALUE = "hourly precipitation";

        public static final String OTHER_UNITS_VALUE = "hundredths of mm";

        public static final String OTHER_GRID_LABEL_VALUE = "hrap_grid=1/40th lfm grid";

        public static final String OTHER_RES_LABEL_VALUE = "4km*4km";

        protected VariableConstants() {
        }
    }

    public static final class AppsDefaults {

        public static final String ST3_NETCDF_LOC = "st3_netcdf_loc";

        public static final String ST3_NETCDF_SWLAT = "st3_netcdf_swlat";

        public static final String ST3_NETCDF_SWLON = "st3_netcdf_swlon";

        public static final String ST3_NETCDF_SELAT = "st3_netcdf_selat";

        public static final String ST3_NETCDF_SELON = "st3_netcdf_selon";

        public static final String ST3_NETCDF_NELAT = "st3_netcdf_nelat";

        public static final String ST3_NETCDF_NELON = "st3_netcdf_nelon";

        public static final String ST3_NETCDF_NWLAT = "st3_netcdf_nwlat";

        public static final String ST3_NETCDF_NWLON = "st3_netcdf_nwlon";

        protected AppsDefaults() {
        }
    }

    protected HPENcConstants() {
    }
}