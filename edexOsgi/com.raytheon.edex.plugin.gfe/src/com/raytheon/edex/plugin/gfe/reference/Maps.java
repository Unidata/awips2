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
package com.raytheon.edex.plugin.gfe.reference;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.geotools.factory.CommonFactoryFinder;
import org.geotools.factory.GeoTools;
import org.opengis.filter.Filter;
import org.opengis.filter.FilterFactory2;

import com.raytheon.edex.plugin.gfe.reference.ShapeFile.IEditAreaNamer;
import com.raytheon.edex.plugin.gfe.reference.ShapeFile.IMapBackgroundFilter;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterTask;

/**
 * Defines what edit areas are to be generated based on shapefiles
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket//		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Apr 10, 2008				randerso	Initial creation
 * Jun 25, 2008     #1210   randerso    Modified to get directories from UtilityContext
 * Jul 10, 2009      #2590  njensen      Added support for multiple sites
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class Maps {
    public static enum GetMode {
        FILES_CHANGED, UNCONDITIONAL
    }

    private static final List<String> ALU_OFFSHORE_ZONES = Arrays
            .asList(new String[] { "PKZ410" });

    private static final List<String> AER_OFFSHORE_ZONES = Arrays
            .asList(new String[] { "PKZ350" });

    private static final List<String> ALU_MARINE_ZONES = Arrays
            .asList(new String[] { "PKZ150", "PKZ155", "PKZ160", "PKZ165",
                    "PKZ170", "PKZ171", "PKZ172", "PKZ175", "PKZ176", "PKZ179",
                    "PKZ180", "PKZ185" });

    private static final List<String> AER_MARINE_ZONES = Arrays
            .asList(new String[] { "PKZ120", "PKZ121", "PKZ125", "PKZ126",
                    "PKZ127", "PKZ128", "PKZ129", "PKZ130", "PKZ132", "PKZ136",
                    "PKZ137", "PKZ138", "PKZ140", "PKZ141" });

    private static final List<String> ALU_ZONES = Arrays.asList(new String[] {
            "AKZ151", "AKZ155", "AKZ161", "AKZ181", "AKZ185", "AKZ187",
            "AKZ191", "AKZ195" });

    private static final List<String> AER_ZONES = Arrays.asList(new String[] {
            "AKZ101", "AKZ111", "AKZ121", "AKZ125", "AKZ131", "AKZ135",
            "AKZ141", "AKZ145", "AKZ171" });

    private static final Log theLogger = LogFactory.getLog(Maps.class);

    // -------------------------------------------------------------
    // Functions for determining name of edit areas
    // -------------------------------------------------------------
    // FIPS codes

    private static class fips implements IEditAreaNamer {
        @Override
        public String getEditAreaName(Map<String, String> atts) {
            // make sure FIPS attribute exists and of proper length
            // make sure STATE attribute exists and of proper length
            String fips = atts.get("FIPS");
            String state = atts.get("STATE");
            if (fips != null && fips.length() == 5 && state != null
                    && state.length() == 2) {
                // last 3 digits from FIPS code
                ;
                String s = state + "C" + fips.substring(fips.length() - 3); // assemble
                // COC013
                return s;
            } else {
                return ""; // for no FIPS in shapefile
            }
        }
    }

    // Public Zones
    private static class cwazones implements IEditAreaNamer {
        @Override
        public String getEditAreaName(Map<String, String> atts) {
            String zone = atts.get("ZONE");
            String state = atts.get("STATE");
            if (zone != null && zone.length() == 3 && state != null
                    && state.length() == 2) {
                return state + "Z" + zone; // assemble COZ023
            } else {
                return ""; // bad attributes
            }
        }
    }

    // Fire Wx Zones
    private static class fwxzones implements IEditAreaNamer {
        @Override
        public String getEditAreaName(Map<String, String> atts) {
            String zone = atts.get("ZONE");
            String state = atts.get("STATE");
            if (zone != null && zone.length() == 3 && state != null
                    && state.length() == 2) {
                return state + "Z" + zone; // assemble COZ023
            } else {
                return "";
            }
        }
    }

    // Marine Zones
    private static class marineZ implements IEditAreaNamer {
        @Override
        public String getEditAreaName(Map<String, String> atts) {
            String id = atts.get("ID");
            if (id != null && id.length() == 6) {
                return id;
            } else {
                return "";
            }
        }
    }

    // Offshore Marine Zones
    private static class offshoreZ implements IEditAreaNamer {
        @Override
        public String getEditAreaName(Map<String, String> atts) {
            String id = atts.get("ID");
            if (id != null && id.length() == 6) {
                return id;
            } else {
                return "";
            }
        }
    }

    // ---------------------------------------------------------------------
    // Map Background Filters
    // ---------------------------------------------------------------------
    // filter for public zones.
    private static class publicZoneFilter implements IMapBackgroundFilter {
        private cwazones cwazones = new cwazones();

        private String cwaStr;

        protected publicZoneFilter(String cwa) {
            cwaStr = cwa;
        }

        @Override
        public boolean filter(Map<String, String> atts) {
            // this CWA (all but AFC site)
            if (cwaStr.equals(atts.get("CWA"))) {
                return true;

                // AFC data - separate out AER/ALU data
            } else if ("AFC".equals(atts.get("CWA"))) {
                String id = cwazones.getEditAreaName(atts);
                if ("AER".equals(cwaStr)) {
                    return AER_ZONES.contains(id);

                } else if ("ALU".equals(cwaStr)) {
                    return ALU_ZONES.contains(id);

                } else if ("AICE".equals(cwaStr)) {
                    return true;
                }
            }

            return false;
        }
    }

    // filter for fire weather zones.
    private static class firewxZoneFilter implements IMapBackgroundFilter {
        private fwxzones fwxzones = new fwxzones();

        private String cwaStr;

        protected firewxZoneFilter(String cwa) {
            cwaStr = cwa;
        }

        @Override
        public boolean filter(Map<String, String> atts) {
            // this CWA (all but AFC site)
            if (cwaStr.equals(atts.get("CWA"))) {
                return true;

                // AFC data - separate out AER/ALU data
            } else if ("AFC".equals(atts.get("CWA"))) {
                String id = fwxzones.getEditAreaName(atts);
                if ("AER".equals(cwaStr)) {
                    return AER_ZONES.contains(id);

                } else if ("ALU".equals(cwaStr)) {
                    return ALU_ZONES.contains(id);

                } else if ("AICE".equals(cwaStr)) {
                    return true;
                }
            }
            return false;
        }
    }

    // filter for marine zones.
    private static class marineZoneFilter implements IMapBackgroundFilter {
        private marineZ marineZ = new marineZ();

        private String cwaStr;

        protected marineZoneFilter(String cwa) {
            cwaStr = cwa;
        }

        @Override
        public boolean filter(Map<String, String> atts) {
            // this CWA (all but AFC site)
            if (cwaStr.equals(atts.get("WFO"))) {
                return true;

                // AFC data - separate out AER/ALU data
            } else if ("AFC".equals(atts.get("WFO"))) {
                String id = marineZ.getEditAreaName(atts);
                if ("AER".equals(cwaStr)) {
                    return AER_MARINE_ZONES.contains(id);
                } else if ("ALU".equals(cwaStr)) {
                    return ALU_MARINE_ZONES.contains(id);
                } else if ("AICE".equals(cwaStr)) {
                    return true;
                }
            }
            return false;
        }
    }

    // filter for offshore marine zones.
    private static class offshoreZoneFilter implements IMapBackgroundFilter {
        private offshoreZ offshoreZ = new offshoreZ();

        private String cwaStr;

        protected offshoreZoneFilter(String cwa) {
            cwaStr = cwa;
        }

        @Override
        public boolean filter(Map<String, String> atts) {
            // this CWA (all but AFC site)
            if (cwaStr.equals(atts.get("WFO"))) {
                return true;

                // AFC data - separate out AER/ALU data
            } else if ("AFC".equals(atts.get("WFO"))) {
                String id = offshoreZ.getEditAreaName(atts);
                if ("AER".equals(cwaStr)) {
                    return AER_OFFSHORE_ZONES.contains(id);
                } else if ("ALU".equals(cwaStr)) {
                    return ALU_OFFSHORE_ZONES.contains(id);
                } else if ("AICE".equals(cwaStr)) {
                    return true;
                }
            }
            return false;
        }
    }

    // ---------------------------------------------------------------------
    // Map Background Definitions
    // ---------------------------------------------------------------------
    public static ShapeFile[] getMaps(String cwaStr, GetMode mode) {
        FilterFactory2 ff = CommonFactoryFinder.getFilterFactory2(GeoTools
                .getDefaultHints());

        String shapeFileDir = "";

        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext edexStaticBase = pathMgr.getContext(
                LocalizationContext.LocalizationType.EDEX_STATIC,
                LocalizationContext.LocalizationLevel.BASE);

        try {
            shapeFileDir = pathMgr.getFile(edexStaticBase, "shapefiles")
                    .getCanonicalPath() + File.separator;
        } catch (IOException e) {
            theLogger.error(e);
        }

        ShapeFile[] maps = null;
        if (GetMode.UNCONDITIONAL == mode
                || filesHaveChanges(cwaStr, shapeFileDir)) {

            // CWA Counties
            String p1 = cwaStr + "*";
            String p2 = "???" + p1;
            Filter countyFilter = ff.or(ff.like(ff.property("CWA"), p1),
                    ff.like(ff.property("CWA"), p2));
            ShapeFile CWAcounties = new ShapeFile(shapeFileDir + "County");
            CWAcounties.setAttributeFilter(countyFilter);
            CWAcounties.setDisplayName("Counties_" + cwaStr);
            CWAcounties.setEditAreaName(new String[] { "STATE", "COUNTYNAME" });
            CWAcounties.setGroupName("Counties");

            // FIPS for my counties - only include first WFO indicated in CWA
            // field
            Filter fipsFilter = ff.like(ff.property("CWA"), p1);
            ShapeFile FIPS = new ShapeFile(shapeFileDir + "County");
            FIPS.setDisplayName("FIPS_" + cwaStr);
            FIPS.setAttributeFilter(fipsFilter);
            FIPS.setEditAreaName(new fips());
            FIPS.setGroupName("FIPS_" + cwaStr);

            // Unfiltered Counties
            ShapeFile Counties = new ShapeFile(shapeFileDir + "County");
            Counties.setDisplayName("Counties");
            Counties.setEditAreaName(new fips());
            Counties.setGroupName("FIPS");

            // CWA Zones
            ShapeFile CWAzones = new ShapeFile(shapeFileDir + "Zone");
            CWAzones.setMapBackgroundFilter(new publicZoneFilter(cwaStr));
            CWAzones.setDisplayName("Zones_" + cwaStr);
            CWAzones.setEditAreaName(new cwazones());
            CWAzones.setGroupName("Zones_" + cwaStr);

            // Unfiltered Zones
            ShapeFile Zones = new ShapeFile(shapeFileDir + "Zone");
            Zones.setDisplayName("Zones");
            Zones.setEditAreaName(new cwazones());
            Zones.setGroupName("Zones");

            // Fire Wx Zones
            ShapeFile FWCWAzones = new ShapeFile(shapeFileDir + "FireWxZones");
            FWCWAzones.setMapBackgroundFilter(new firewxZoneFilter(cwaStr));
            FWCWAzones.setDisplayName("FireWxZones_" + cwaStr);
            FWCWAzones.setEditAreaName(new fwxzones());
            FWCWAzones.setGroupName("FireWxZones_" + cwaStr);

            // Unfiltered Fire Wx Zones
            ShapeFile FWZones = new ShapeFile(shapeFileDir + "FireWxZones");
            FWZones.setDisplayName("FireWxZones");
            FWZones.setEditAreaName(new fwxzones());
            FWZones.setGroupName("FireWxZones");

            // CWA for just this CWA
            Filter cwaFilter = ff
                    .equals(ff.property("CWA"), ff.literal(cwaStr));
            ShapeFile cwa = new ShapeFile(shapeFileDir + "CWA");
            cwa.setAttributeFilter(cwaFilter);
            cwa.setDisplayName("CWA");

            // CWAs for all
            ShapeFile cwas = new ShapeFile(shapeFileDir + "CWA");
            cwas.setDisplayName("CWA_all");
            cwas.setEditAreaName("WFO");
            cwas.setGroupName("WFOs");

            // ISC areas for all
            ShapeFile isc = new ShapeFile(shapeFileDir + "ISC");
            isc.setDisplayName("ISC_all");
            isc.setEditAreaName(new String[] { "ISC", "WFO" });
            isc.setGroupName("ISC");

            // Fire Wx AOR for all
            ShapeFile fwaor = new ShapeFile(shapeFileDir + "FireWxAOR");
            fwaor.setDisplayName("FireWxAOR");
            fwaor.setEditAreaName(new String[] { "FireWxAOR", "CWA" });
            fwaor.setGroupName("FireWxAOR");

            // Marine Zones for CWA
            ShapeFile CWAmzones = new ShapeFile(shapeFileDir + "MarineZones");
            CWAmzones.setMapBackgroundFilter(new marineZoneFilter(cwaStr));
            CWAmzones.setDisplayName("Marine_Zones_" + cwaStr);
            CWAmzones.setEditAreaName(new marineZ());
            CWAmzones.setGroupName("MZones_" + cwaStr);

            // Marine Zones (unfiltered)
            ShapeFile Mzones = new ShapeFile(shapeFileDir + "MarineZones");
            Mzones.setDisplayName("Marine_Zones");
            Mzones.setEditAreaName(new marineZ());
            Mzones.setGroupName("MZones");

            // States (unfiltered)
            ShapeFile States = new ShapeFile(shapeFileDir + "States");
            States.setDisplayName("States");
            States.setEditAreaName("NAME");
            States.setGroupName("States");

            // River Basins - unfiltered
            ShapeFile Basins = new ShapeFile(shapeFileDir + "Basins");
            Basins.setDisplayName("Basins");

            // RFC maps
            ShapeFile rfc = new ShapeFile(shapeFileDir + "RFC");
            rfc.setDisplayName("RFC");
            rfc.setEditAreaName(new String[] { "ISC", "SITE_ID" });
            rfc.setGroupName("ISC");

            // Lakes - unfiltered
            ShapeFile lakes = new ShapeFile(shapeFileDir + "Lake");
            lakes.setDisplayName("Lakes");

            // Offshore Marine Zones - unfiltered
            ShapeFile offshore = new ShapeFile(shapeFileDir + "Offshore");
            offshore.setDisplayName("Offshore_Marine_Zones");
            offshore.setEditAreaName(new offshoreZ());
            offshore.setGroupName("OffShoreMZones");

            // Offshore Marine Zones - filtered by CWA
            ShapeFile offshoreCWA = new ShapeFile(shapeFileDir + "Offshore");
            offshoreCWA.setMapBackgroundFilter(new offshoreZoneFilter(cwaStr));
            offshoreCWA.setDisplayName("Offshore_Marine_Zones_" + cwaStr);
            offshoreCWA.setEditAreaName(new offshoreZ());
            offshoreCWA.setGroupName("OffShoreMZones_" + cwaStr);

            // High Sea Marine Zones - unfiltered
            ShapeFile hsmz = new ShapeFile(shapeFileDir + "HighSea");
            hsmz.setDisplayName("High_Sea_Marine_Zones");

            // High Sea Marine Zones - filtered by CWA
            Filter wfoFilter = ff
                    .equals(ff.property("WFO"), ff.literal(cwaStr));
            ShapeFile CWAhsmz = new ShapeFile(shapeFileDir + "HighSea");
            CWAhsmz.setAttributeFilter(wfoFilter);
            CWAhsmz.setDisplayName("High_Sea_Marine_Zones_" + cwaStr);

            // Interstates
            Filter interstateFilter = ff.equals(ff.property("ADMN_CLASS"),
                    ff.literal("Interstate"));
            ShapeFile interstates = new ShapeFile(shapeFileDir + "Interstate");
            interstates.setAttributeFilter(interstateFilter);
            interstates.setDisplayName("Interstates");

            // Low-resolution highways
            ShapeFile highways = new ShapeFile(shapeFileDir + "Highway");
            highways.setDisplayName("Interstates_and_US_Highways");

            // Cities, filtered
            Filter populationFilter = ff.greater(ff.property("POP_1990"),
                    ff.literal(50000f));
            ShapeFile cities = new ShapeFile(shapeFileDir + "City");
            cities.setAttributeFilter(populationFilter);
            cities.setDisplayName("Cities");

            ShapeFile canada = new ShapeFile(shapeFileDir + "Canada");
            canada.setDisplayName("Canada");

            Filter countryFilter = ff.equals(ff.property("GMI_CNTRY"),
                    ff.literal("USA"));
            ShapeFile world = new ShapeFile(shapeFileDir + "World");
            world.setDisplayName("Countries");
            world.setAttributeFilter(countryFilter);

            ShapeFile railroads = new ShapeFile(shapeFileDir + "Railroad");
            railroads.setDisplayName("RailRoads");

            ShapeFile airports = new ShapeFile(shapeFileDir + "FAA"
                    + File.separator + "Airport");
            airports.setDisplayName("Airports");

            maps = new ShapeFile[] { CWAcounties, Counties, FIPS, CWAzones,
                    Zones, cwa, cwas, CWAmzones, Mzones, States, Basins, rfc,
                    lakes, hsmz, CWAhsmz, interstates, highways, cities,
                    FWZones, FWCWAzones, isc, canada, world, railroads,
                    offshore, offshoreCWA, fwaor, airports };
        }
        return maps;
    }

    private static boolean filesHaveChanges(String cwa, String shapeFileDir) {
        boolean filesHaveChanges = false;
        File dir = new File(shapeFileDir);
        if (dir.exists() && dir.isDirectory()) {
            ClusterTask task = ClusterLockUtils.lookupLock(
                    MapManager.EDIT_AREA_GEN_TASK, cwa);
            filesHaveChanges = FileUtil.hasBeenModifiedSince(dir,
                    task.getLastExecution(), true);
        }

        return filesHaveChanges;
    }

}
