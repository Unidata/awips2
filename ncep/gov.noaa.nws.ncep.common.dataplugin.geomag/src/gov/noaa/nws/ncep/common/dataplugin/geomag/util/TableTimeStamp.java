package gov.noaa.nws.ncep.common.dataplugin.geomag.util;

import gov.noaa.nws.ncep.common.dataplugin.geomag.exception.GeoMagException;
import java.io.File;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;

/**
 *  TableTimeStamp - A Java class to update geoMagStations.xml.
 *
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/2013  	975		   S. Gurung   Initial creation
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */

public class TableTimeStamp {

    private static long geoMagStationsTimeStamp;

    public TableTimeStamp() {
    }

    public static synchronized void updateXmlTables() throws GeoMagException {

        long geoMagStationFileTime = 0;

        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext commonStaticBase = pathMgr.getContext(
                LocalizationContext.LocalizationType.COMMON_STATIC,
                LocalizationContext.LocalizationLevel.BASE);

        /* check geoMagStations.xml file */
        String path = "";

        try {
        	path = pathMgr.getFile(commonStaticBase,
            		"ncep" + File.separator + "geomag" + File.separator + "geoMagStations.xml")
            		.getCanonicalPath();

        } catch (Exception e) {
            throw new GeoMagException(
                    "Unable to unmarshal geoMagStations.xml file");
        }

        File stnsTable = new File(path);
        try {
            if (stnsTable.exists()) {
                geoMagStationFileTime = stnsTable.lastModified();
                if (geoMagStationFileTime != getGeoMagStationsTimeStamp()) {
                    System.out
                            .println("geoMagStations.xml has been modified or the first time, so load it ...");
                    GeoMagStationLookup.ReloadInstance();
                    setGeoMagStationsTimeStamp(geoMagStationFileTime);
                }
            }
        } catch (Exception e) {
            throw new GeoMagException("Unable to read geoMagStations.xml file");
        }

    }

    public static long getGeoMagStationsTimeStamp() {
        return geoMagStationsTimeStamp;
    }

    public static void setGeoMagStationsTimeStamp(long geoMagStationsTimeStamp) {
        TableTimeStamp.geoMagStationsTimeStamp = geoMagStationsTimeStamp;
    }

}
