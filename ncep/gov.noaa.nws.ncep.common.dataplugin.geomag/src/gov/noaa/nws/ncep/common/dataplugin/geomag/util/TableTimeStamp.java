package gov.noaa.nws.ncep.common.dataplugin.geomag.util;

import gov.noaa.nws.ncep.common.dataplugin.geomag.exception.GeoMagException;

import java.io.File;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;

/**
 * TableTimeStamp - A Java class to update geoMagStations.xml.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/2013      975        S. Gurung   Initial creation
 * 06/2014      R4078      S. Gurung   Added support for site level geoMagStations.xml file
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

        LocalizationContext commonStaticSite = pathMgr.getContext(
                LocalizationContext.LocalizationType.COMMON_STATIC,
                LocalizationContext.LocalizationLevel.SITE);

        /* check geoMagStations.xml file */
        String basePath = "";
        String sitePath = "";

        try {
            basePath = pathMgr.getFile(
                    commonStaticBase,
                    "ncep" + File.separator + "geomag" + File.separator
                            + "geoMagStations.xml").getCanonicalPath();

            sitePath = pathMgr.getFile(
                    commonStaticSite,
                    "ncep" + File.separator + "geomag" + File.separator
                            + "geoMagStations.xml").getCanonicalPath();

        } catch (Exception e) {
            throw new GeoMagException(
                    "Unable to unmarshal geoMagStations.xml file");
        }

        File stnsTableBase = new File(basePath);
        File stnsTableSite = new File(sitePath);
        try {
            if (stnsTableSite.exists()) {
                geoMagStationFileTime = stnsTableSite.lastModified();
                if (geoMagStationFileTime != getGeoMagStationsTimeStamp()) {
                    System.out
                            .println("Site level geoMagStations.xml has been modified or the first time, so load it ...");
                    GeoMagStationLookup.ReloadInstance();
                    setGeoMagStationsTimeStamp(geoMagStationFileTime);
                }
            } else {
                if (stnsTableBase.exists()) {
                    geoMagStationFileTime = stnsTableBase.lastModified();
                    if (geoMagStationFileTime != getGeoMagStationsTimeStamp()) {
                        System.out
                                .println("Base level geoMagStations.xml has been modified or the first time, so load it ...");
                        GeoMagStationLookup.ReloadInstance();
                        setGeoMagStationsTimeStamp(geoMagStationFileTime);
                    }
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
