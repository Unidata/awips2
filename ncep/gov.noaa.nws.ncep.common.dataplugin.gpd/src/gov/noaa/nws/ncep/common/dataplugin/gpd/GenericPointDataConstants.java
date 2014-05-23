/**
 * This code has unlimited rights, and is provided "as is" by the National Centers 
 * for Environmental Prediction, without warranty of any kind, either expressed or implied, 
 * including but not limited to the implied warranties of merchantability and/or fitness 
 * for a particular purpose.
 * 
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 05/30/2013				Chin J. Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin J. Chen
 * @version 1.0
 */
package gov.noaa.nws.ncep.common.dataplugin.gpd;

public class GenericPointDataConstants {
	public static final String HDF5_NUM_LEVEL = "NumProfileLevel";
    public static final String HDF5_STN_ID = "StationId";
    public static final String HDF5_LEVEL_VALUE = "LevelValue";
    public static final String HDF5_PDV_CURIDX="pointDataView.curIdx";
    public static final String HDF5_PDV_ID="id";
    public static final String MAX_LEVELS = "maxLevels";
    
    public static final int MANDATORY_DATASET_NUM = 3;
    public static final int MAX_STNID_STRING_SIZE = 16;
    // db filed defined in GenericPointDataRecord
    public static final String DB_PROD_NAME= "productInfo.name";
    public static final String DB_MASTER_LEVEL_NAME= "productInfo.masterLevel.name";
    public static final String DB_REF_TIME= "dataTime.refTime";
    public static final String DB_FORECAST_TIME= "dataTime.fcstTime";
    public static final String DB_RANGESTART_TIME= "dataTime.validPeriod.start";
    public static final String DB_UTILITY_FLAGS= "dataTime.utilityFlags";
    public static final String DB_PRODUCT_VERSION = "productVersion";
    public static final String DB_STN_CATALOGTYPE = "location.catalogType";
    public static final String DB_SLAT = "slat";
    public static final String DB_SLON = "slon";
    
    // GEMPAK sounding data table definition names string
    public static final String SND_PARM = "SNPARM"; //unique string in sounding data table, used as table type identifier
    public static final int SND_PARM_PER_LINE = 8; // defined bases on Gempak Fortran coding
    public static final String SND_STN_PARM = "STNPRM";
    public static final String SND_STN_ID = "STID";
    public static final String SND_STN_NUM = "STNM";
    public static final String SND_STN_LAT = "SLAT";
    public static final String SND_STN_LON = "SLON";
    public static final String SND_STN_ELEVATION= "SELV";
    public static final String SND_STN_IM= "STIM";
    public static final String SND_REFTIME= "TIME"; // time format like this, 130724/0000  
    public static final int SND_HDR_PARM_PER_LINE = 13; //based on Gempak definition
    // GEMPAK surface data table definition names string
    public static final String SFC_PARM = "PARM";
    public static final int SFC_PARM_PER_LINE = 6; // defined bases on Gempak Fortran coding
    public static final String SFC_STN = "STN";	
    public static final String SFC_REFTIME = "YYMMDD/HHMM";//unique string in surface data table, used as table type identifier
    public static final String SFC_LAT = "SLAT";
    public static final String SFC_LON = "SLON";
    public static final int SFC_HDR_PARM_PER_LINE = 14; //based on Gempak definition
    
    public static final float GPD_INVALID_FLOAT_VALUE = -9999.0f;
    
    public static final String GEMPAK_TEMP = "TMPC";
    public static final String GEMPAK_DEWPT = "DWPT";
    public static final String GEMPAK_HEIGHT = "HGHT";
    public static final String GEMPAK_WIND_DIR = "DRCT";
    public static final String GEMPAK_WIND_SPEED = "SKNT"; //in knot
    public static final String GEMPAK_PRESSURE = "PRES"; //in millibars
    public static final String GEMPAK_OMEGA = "";
}
