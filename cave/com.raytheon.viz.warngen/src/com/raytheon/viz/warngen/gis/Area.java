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
package com.raytheon.viz.warngen.gis;

import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.Validate;

import com.raytheon.uf.common.dataplugin.warning.config.AreaSourceConfiguration;
import com.raytheon.uf.common.dataplugin.warning.config.AreaSourceConfiguration.AreaType;
import com.raytheon.uf.common.dataplugin.warning.config.GeospatialConfiguration;
import com.raytheon.uf.common.dataplugin.warning.config.WarngenConfiguration;
import com.raytheon.uf.common.dataplugin.warning.gis.GeospatialData;
import com.raytheon.uf.common.dataplugin.warning.portions.GisUtil;
import com.raytheon.uf.common.dataplugin.warning.portions.PortionsUtil;
import com.raytheon.uf.common.dataplugin.warning.util.CountyUserData;
import com.raytheon.uf.common.dataplugin.warning.util.FileUtil;
import com.raytheon.uf.common.dataplugin.warning.util.GeometryUtil;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.geospatial.ISpatialQuery.SearchMode;
import com.raytheon.uf.common.geospatial.SpatialException;
import com.raytheon.uf.common.geospatial.SpatialQueryFactory;
import com.raytheon.uf.common.geospatial.SpatialQueryResult;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.warngen.gui.WarngenLayer;
import com.raytheon.viz.warngen.util.Abbreviation;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.prep.PreparedGeometry;

/**
 * Area
 * 
 * Finds areas affected by area warnings
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Nov 15, 2007 #601        chammack    Initial Creation.
 *    Mar 28, 2012 #14691      Qinglu lin  Created AffectedAreas' partOfParentRegion based on 
 *                                         FE_AREA stored in GeospatialData's attributes map, 
 *                                         instead of calculating them.
 *    Apr 11, 2012 #14691      Qinglu lin  Extra code were added to handle marine warnings as
 *                                         MarineZones shapefiles have no FE_AREA.
 *    Apr 13, 2012 #14691      Qinglu lin  Added code for two more fe_area: er and nr.
 *    May  4, 2012 #14887      Qinglu lin  Changed 0.25 to 0.60 for DEFAULT_PORTION_TOLERANCE; 
 *                                         added code to pass a Envelope calculatePortion().
 *    Nov  9, 2012 DR 15430    D. Friedman Extracted method converFeAreaToPartList.
 *    Apr 29, 2013  1955       jsanchez    Ignored comparing the geometry's user data when finding intersected areas.
 *    May  2, 2013  1963       jsanchez    Updated method to determine partOfArea.
 *    Aug 19, 2013  2177       jsanchez    Used portionsUtil to calculate area portion descriptions.
 *    Dec  4, 2013  2604       jsanchez    Refactored GisUtil and PortionsUtil.
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class Area {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(Area.class);

    /**
     * If an area greater than this percentage of the area is covered, no
     * direction is included
     */
    public static final double DEFAULT_PORTION_TOLERANCE = 0.60;

    private PortionsUtil portionsUtil;

    public Area(PortionsUtil portionsUtil) {
        this.portionsUtil = portionsUtil;
    }

    public AffectedAreas[] findAffectedAreas(WarngenConfiguration config,
            Geometry polygon, Geometry warningArea, String localizedSite)
            throws VizException {

        // --- Begin argument checking ---
        Validate.notNull(config.getGeospatialConfig().getAreaSource(),
                "Area source must be provided for findAffectedAreas to operate");
        Validate.notNull(polygon, "Area geometry must not be null");

        // Get spatial query result for entries in our area from existing data;
        List<Geometry> geoms = new ArrayList<Geometry>();
        GeometryUtil.buildGeometryList(geoms, warningArea);

        GeospatialConfiguration geospatialConfig = config.getGeospatialConfig();
        AreaSourceConfiguration areaConfig = config.getHatchedAreaSource();

        return findAffectedAreas(areaConfig, geospatialConfig, polygon,
                localizedSite, geoms);
    }

    private AffectedAreas[] findAffectedAreas(
            AreaSourceConfiguration areaConfig,
            GeospatialConfiguration geospatialConfig, Geometry polygon,
            String localizedSite, List<Geometry> geoms) throws VizException {
        String areaSource = areaConfig.getAreaSource();
        String areaField = areaConfig.getAreaField();
        String fipsField = areaConfig.getFipsField();
        String areaNotationField = areaConfig.getAreaNotationField();
        String pointField = areaConfig.getPointField();
        String pointSource = geospatialConfig.getPointSource();
        Map<String, RequestConstraint> pointFilter = areaConfig
                .getPointFilter();
        String parentAreaField = areaConfig.getParentAreaField();
        String timezonePathcastField = geospatialConfig.getTimezoneField();
        ArrayList<String> fields = new ArrayList<String>();
        /* fields is not used in querying to the database */
        if (areaConfig.getSortBy() != null) {
            for (String field : areaConfig.getSortBy()) {
                fields.add(field);
            }
        }

        Map<String, GeospatialData> countyMap = new HashMap<String, GeospatialData>();
        for (Geometry g : geoms) {
            CountyUserData data = (CountyUserData) g.getUserData();
            if (data != null) {
                String gid = GeometryUtil.getPrefix(data);
                if (countyMap.containsKey(gid) == false) {
                    countyMap.put(gid, data.entry);
                }
            }
        }

        // Query for points within polygon
        SpatialQueryResult[] ptFeatures = null;
        if (pointField != null) {
            try {
                ptFeatures = SpatialQueryFactory.create().query(pointSource,
                        new String[] { pointField }, polygon, pointFilter,
                        SearchMode.INTERSECTS);
            } catch (SpatialException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }

        Abbreviation abbreviation = null;

        if (areaConfig.getAreaNotationTranslationFile() != null) {
            try {
                abbreviation = new Abbreviation(FileUtil.getFile(
                        areaConfig.getAreaNotationTranslationFile(),
                        localizedSite));
            } catch (FileNotFoundException e) {
                statusHandler.handle(Priority.ERROR, "Unable to find "
                        + areaConfig.getAreaNotationTranslationFile() + "", e);
            }
        }

        List<String> uniqueFips = new ArrayList<String>();
        List<AffectedAreas> areas = new ArrayList<AffectedAreas>();
        for (GeospatialData regionFeature : countyMap.values()) {
            Geometry regionGeom = regionFeature.geometry;
            PreparedGeometry preparedRegionGeom = regionFeature.prepGeom;
            AffectedAreas area = new AffectedAreas();
            area.name = regionFeature.attributes.get(areaField).toString();
            area.fips = regionFeature.attributes.get(fipsField).toString();
            area.stateabbr = regionFeature.attributes.get(areaNotationField)
                    .toString();
            area.size = regionGeom.getArea();

            Object tzData = regionFeature.attributes.get(timezonePathcastField);

            if (tzData != null) {
                area.timezone = String.valueOf(tzData);
            } else {
                area.timezone = "P";
            }

            if (abbreviation != null) {
                area.areaNotation = abbreviation.translate(String
                        .valueOf(regionFeature.attributes
                                .get(areaNotationField)));
                area.areasNotation = abbreviation.translatePlural(String
                        .valueOf(regionFeature.attributes
                                .get(areaNotationField)));
            }
            String gid = String.valueOf(regionFeature.attributes
                    .get(WarngenLayer.GID));
            List<Geometry> intersections = new ArrayList<Geometry>();
            for (Geometry g : geoms) {
                if (GeometryUtil.getPrefix(g.getUserData()).equalsIgnoreCase(
                        gid)) {
                    intersections.add(g);
                }
            }
            Geometry intersection = regionGeom.getFactory()
                    .createGeometryCollection(
                            intersections.toArray(new Geometry[intersections
                                    .size()]));
            double areaIntersection = intersection.getArea();

            double tolerCheck = regionGeom.getArea()
                    * DEFAULT_PORTION_TOLERANCE;
            if (areaIntersection < tolerCheck) {
                try {
                    String entityID = area.stateabbr + areaSource.charAt(0)
                            + area.fips.substring(2);
                    area.partOfArea = GisUtil.asStringList(portionsUtil
                            .getPortions(entityID, regionGeom, intersection,
                                    true));
                } catch (Exception e) {
                    statusHandler.error("Unable to calculate part of area for "
                            + area.name, e);
                }
            }

            // Search the parent region
            GeospatialData parentRegion = regionFeature.parent;
            if (parentRegion != null) {
                area.parentRegion = String.valueOf(parentRegion.attributes
                        .get(parentAreaField));
                String feArea = (String) regionFeature.attributes
                        .get("FE_AREA");
                area.partOfParentRegion = converFeAreaToPartList(feArea);
            }

            // Search against point matches
            if (ptFeatures != null) {
                List<String> pointList = new ArrayList<String>();
                for (SpatialQueryResult ptRslt : ptFeatures) {
                    if (preparedRegionGeom.contains(ptRslt.geometry)) {
                        pointList.add(String.valueOf(ptRslt.attributes
                                .get(pointField)));
                    }
                }

                area.points = pointList.toArray(new String[pointList.size()]);
            }
            if (uniqueFips.contains(area.fips) == false) {
                uniqueFips.add(area.fips);
                areas.add(area);
            }
        }

        // Perform Sort
        if (fields.size() > 0) {
            AffectedAreasComparator comparator = new AffectedAreasComparator(
                    fields);
            Collections.sort(areas, comparator);
        }
        return areas.toArray(new AffectedAreas[areas.size()]);
    }

    /**
     * Determines the affected areas that intersect the warnArea. This method
     * should be used if the intersected areas are of a different area source
     * compared to the hatched area source. Otherwise, the information in the
     * warnArea can just be re-used in the template. If the area source of the
     * intersect and the hatched are the same, then the configuration and
     * template files are configured inefficiently.
     * 
     * @param config
     * @param warnPolygon
     * @param warnArea
     * @param localizedSite
     * @param warngenLayer
     * @return
     * @throws VizException
     */
    public Map<String, Object> findInsectingAreas(WarngenConfiguration config,
            Geometry warnPolygon, Geometry warnArea, String localizedSite,
            WarngenLayer warngenLayer) throws VizException {
        Map<String, Object> areasMap = new HashMap<String, Object>();

        String hatchedAreaSource = config.getHatchedAreaSource()
                .getAreaSource();
        for (AreaSourceConfiguration asc : config.getAreaSources()) {
            if (asc.getType() == AreaType.INTERSECT) {
                List<Geometry> geoms = new ArrayList<Geometry>();
                for (GeospatialData f : warngenLayer.getGeodataFeatures(
                        asc.getAreaSource(), localizedSite)) {
                    boolean ignoreUserData = asc.getAreaSource().equals(
                            hatchedAreaSource) == false;
                    Geometry intersect = GeometryUtil.intersection(warnArea,
                            f.prepGeom, ignoreUserData);
                    if (intersect.isEmpty() == false) {
                        geoms.add(intersect);
                    }
                }

                AffectedAreas[] affectedAreas = findAffectedAreas(asc,
                        config.getGeospatialConfig(), warnPolygon,
                        localizedSite, geoms);

                areasMap.put(asc.getVariable(), affectedAreas);
            }
        }

        return areasMap;

    }

    public static List<String> converFeAreaToPartList(String feArea) {
        final List<String> partList = new ArrayList<String>();
        if (feArea == null) {
            // Marine warnings
            partList.add("");
        } else {
            if (feArea.equals("pa"))
                partList.add("PA");
            else if (feArea.equals("mi"))
                partList.add("MI");
            else if (feArea.equals("pd"))
                partList.add("PD");
            else if (feArea.equals("up"))
                partList.add("UP");
            else if (feArea.equals("bb"))
                partList.add("BB");
            else if (feArea.equals("er"))
                partList.add("ER");
            else if (feArea.equals("eu"))
                partList.add("EU");
            else if (feArea.equals("sr"))
                partList.add("SR");
            else if (feArea.equals("nr"))
                partList.add("NR");
            else if (feArea.equals("wu"))
                partList.add("WU");
            else if (feArea.equals("ds"))
                partList.add("DS");
            else if (feArea.equals("ne"))
                partList.add("NE");
            else if (feArea.equals("nw"))
                partList.add("NW");
            else if (feArea.equals("se"))
                partList.add("SE");
            else if (feArea.equals("sw"))
                partList.add("SW");
            else {
                for (int i = 0; i < feArea.length(); i++) {
                    char c = feArea.charAt(i);
                    switch (c) {
                    case 'c':
                        partList.add("CENTRAL");
                        break;
                    case 'w':
                        partList.add("WEST");
                        break;
                    case 'n':
                        partList.add("NORTH");
                        break;
                    case 'e':
                        partList.add("EAST");
                        break;
                    case 's':
                        partList.add("SOUTH");
                        break;
                    default:
                        break;
                    }
                }
            }
        }
        return partList;
    }

}
