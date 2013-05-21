package gov.noaa.nws.ncep.common.dataplugin.geomag.util;


import gov.noaa.nws.ncep.common.dataplugin.geomag.exception.GeoMagException;
import gov.noaa.nws.ncep.common.dataplugin.geomag.table.GeoMagStation;
import gov.noaa.nws.ncep.common.dataplugin.geomag.table.GeoMagStationTableReader;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;

/**
 * Load the geoMagStations xml file.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/2013      975        sgurung     Initial creation
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */

public class GeoMagStationLookup {

    /** The logger */
    protected transient Log logger = LogFactory.getLog(getClass());

    /** The singleton instance of GeoMagStationLookup **/
    private static GeoMagStationLookup instance;

    /** A map of the stations. The key is the station code of the station */
    private Map<String, GeoMagStation> stnsByCode;

    public static synchronized GeoMagStationLookup getInstance() {
        if (instance == null) {
            // System.out.println("in reloading geoMagStations.xml ....");
            instance = new GeoMagStationLookup();
        }
        return instance;
    }

    /**
     * If file has been modified, then reload it again
     * 
     * @return
     * @return
     */
    public static void ReloadInstance() {
        instance = null;
    }

    private GeoMagStationLookup() {
        stnsByCode = new HashMap<String, GeoMagStation>();
        try {
            initStationList();
        } catch (GeoMagException e) {
            logger.error("Unable to initialize geomag stations list!", e);
        }
    }

    public GeoMagStation getStationByCode(String stnCode) {
        return stnsByCode.get(stnCode);
    }

    public Map<String, GeoMagStation> getStationsByCodeMap() {
        return stnsByCode;
    }

    private void initStationList() throws GeoMagException {
    	IPathManager pathMgr = PathManagerFactory.getPathManager();
		
		LocalizationContext commonStaticBase = pathMgr.getContext(
                LocalizationContext.LocalizationType.COMMON_STATIC,
                LocalizationContext.LocalizationLevel.BASE);
		
		/*LocalizationContext commonStaticSite = pathMgr.getContext(
	            LocalizationContext.LocalizationType.COMMON_STATIC,
	            LocalizationContext.LocalizationLevel.SITE);*/

		String path = "";
        //String sitePath = "";
        try {
            path = pathMgr.getFile(commonStaticBase, 
            		"ncep" + File.separator + "geomag" + File.separator + "geoMagStations.xml")
            		.getCanonicalPath();
            //sitePath = pathMgr.getFile(commonStaticSite, NcPathConstants.GEOMAG_STNS_TBL).getCanonicalPath();
        } catch (IOException e) {
        	logger.error("Error reading geomag stations table. ", e);
        }
		

        File stnsFile = new File(path);
        //File siteStnsFile = new File(sitePath);
        GeoMagStationTableReader geoMagStationsTbl = null;
        try {
            if (stnsFile.exists()) {
            	geoMagStationsTbl = new GeoMagStationTableReader(stnsFile.getPath());
            } 
            // if site version exists, use it instead
            /*if (siteStnsFile.exists()) {
            	geoMagStationsTbl = new GeoMagStationTableReader(stnsFile.getPath());
            }*/
            
            List<GeoMagStation> list = (geoMagStationsTbl!=null)?geoMagStationsTbl.getStationList():new ArrayList<GeoMagStation>();
            
            for(GeoMagStation station : list){	
            	stnsByCode.put(station.getStationCode(), station);
            } 		        
        } catch (Exception e) {
            throw new GeoMagException("Unable to unmarshal ncep geomag stations file");
        }
       
    }

}
