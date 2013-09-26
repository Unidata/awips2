package gov.noaa.nws.ncep.common.dataplugin.geomag.util;

import gov.noaa.nws.ncep.common.dataplugin.geomag.exception.GeoMagException;
import gov.noaa.nws.ncep.common.dataplugin.geomag.table.KStationCoeffTableReader;
import gov.noaa.nws.ncep.common.dataplugin.geomag.table.KStationCoefficient;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;

/*
 * The KStationCoefficient table Lookup.
 * 
 * <pre>
 * SOFTWARE HISTORY
 *                   
 * ate          Ticket#     Engineer   Description
 * -----------  ----------  ---------- --------------------------
 * 05/14/2013   #989        qzhou      Initial Creation
 * </pre>
 * 
 * @author qzhou
 * @version 1
 */

public class KStationCoefficientLookup {

	/** The logger */
    protected transient Log logger = LogFactory.getLog(getClass());

    /** The singleton instance of GeoMagStationLookup **/
    private static KStationCoefficientLookup instance;

    /** A map of the stations. The key is the station code of the station */
    private Map<String, KStationCoefficient> coeff;
    
    public static synchronized KStationCoefficientLookup getInstance() {
        if (instance == null) {
            
            try {
				instance = new KStationCoefficientLookup();
			} catch (JAXBException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
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

    private KStationCoefficientLookup() throws JAXBException {
    	coeff = new HashMap<String, KStationCoefficient>();
        try {
            initStationList();
        } catch (GeoMagException e) {
            logger.error("Unable to initialize K stations list!", e);
		}
    }
   
    public KStationCoefficient getStationByCode(String stnCode) {
        return coeff.get(stnCode);
    }

    public Map<String, KStationCoefficient> getStationsByCodeMap() {
        return coeff;
    }

    private void initStationList() throws GeoMagException, JAXBException {
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
            		"ncep" + File.separator + "geomag" + File.separator + "kStandardLookup.xml")
            		.getCanonicalPath();
            //sitePath = pathMgr.getFile(commonStaticSite, NcPathConstants.GEOMAG_STNS_TBL).getCanonicalPath();
        } catch (IOException e) {
        	logger.error("Error reading K stations coefficient table. ", e);
        }
        
        File stnsFile = new File(path);
        
        //File siteStnsFile = new File(sitePath);
        KStationCoeffTableReader kStationCoeffTbl = null;
        
        if (stnsFile.exists()) {
        	kStationCoeffTbl = new KStationCoeffTableReader(stnsFile.getPath());
        } 
        // if site version exists, use it instead
        /*if (siteStnsFile.exists()) {
        	geoMagStationsTbl = new GeoMagStationTableReader(stnsFile.getPath());
        }*/
        
        List<KStationCoefficient> list = (kStationCoeffTbl!=null)? kStationCoeffTbl.getStationList() : new ArrayList<KStationCoefficient>();
        
        for(KStationCoefficient station : list){
        	coeff.put(station.getStationCode(), station);
        	//System.out.println("****look2 "+station.getStationCode());
        } 		        
    }
}
