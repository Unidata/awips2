package com.raytheon.viz.warngen.config;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.measure.converter.UnitConverter;

import com.raytheon.uf.common.dataplugin.warning.config.PathcastConfiguration;
import com.raytheon.uf.common.dataplugin.warning.config.PointSourceConfiguration;
import com.raytheon.uf.common.dataplugin.warning.portions.GisUtil;
import com.raytheon.uf.common.dataplugin.warning.portions.GisUtil.Direction;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.geospatial.SpatialQueryResult;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.warngen.PreferenceUtil;
import com.raytheon.viz.warngen.gis.Area;
import com.raytheon.viz.warngen.gis.ClosestPoint;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.prep.PreparedGeometry;
import com.vividsolutions.jts.geom.prep.PreparedGeometryFactory;

/**
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 25, 2012 #15425     Qinglu Lin   Updated createClosestPoint().
 * Feb 13, 2012  1605      jsanchez     Calculated the point based on lat,lon values.
 * Mar 25, 2013  1810      jsanchez     Allowed other values to be accepted as a true value for useDirs.
 * Mar 25, 2013  1605      jsanchez     Set ClosestPoint's prepGeom.
 * Apr 24, 2013  1944      jsanchez     Updated calculateLocationPortion visibility to public.
 * May  2, 2013  1963      jsanchez     Referenced calculatePortion from GisUtil if intersection less than DEFAULT_PORTION_TOLERANCE.
 * Sep 13, 2013  DR 16601  D. Friedman  Fix from jsanchez: Allow cities outside the CWA.
 * Dec  4, 2013  2604      jsanchez     Refactored GisUtil.
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class DbAreaSourceDataAdaptor extends AbstractDbSourceDataAdaptor {

    private static final String useDirectionField = "usedirs";

    private static final String suppressedDirectionsField = "supdirs";

    private static final String cwaField = "cwa";

    private static final String longitude = "lon";

    private static final String latitude = "lat";

    public DbAreaSourceDataAdaptor(PathcastConfiguration pathcastConfiguration,
            UnitConverter distanceToMeters, Geometry searchArea,
            String localizedSite) throws VizException {
        super(pathcastConfiguration, distanceToMeters, searchArea,
                localizedSite);
    }

    public DbAreaSourceDataAdaptor(
            PointSourceConfiguration pointSourceConfiguration,
            Geometry searchArea, String localizedSite) throws VizException {
        super(pointSourceConfiguration, searchArea, localizedSite);
    }

    /**
     * 
     */
    @Override
    protected Set<String> createSpatialQueryField(String pointField,
            String[] sortBy) {
        Set<String> ptFields = new HashSet<String>();
        ptFields.add(pointField);
        ptFields.add(useDirectionField);
        ptFields.add(suppressedDirectionsField);
        ptFields.add(longitude);
        ptFields.add(latitude);

        List<String> fields = null;
        if (sortBy != null && sortBy.length > 0) {
            fields = Arrays.asList(sortBy);
        } else {
            fields = new ArrayList<String>(0);
        }

        // Sort fields don't exist in the db.
        for (String field : fields) {
            if (undatabasedSortableFields.contains(field.toUpperCase()) == false) {
                ptFields.add(field.toUpperCase());
            }
        }

        return ptFields;
    }

    /**
     * Creates a closest point object.
     */
    @Override
    protected ClosestPoint createClosestPoint(String pointField,
            Set<String> ptFields, SpatialQueryResult ptRslt) {
        Map<String, Object> attributes = ptRslt.attributes;

        String name = String.valueOf(attributes.get(pointField));
        Coordinate point = getPoint(attributes);
        int population = getPopulation(ptFields, attributes);
        int warngenlev = getWangenlev(ptFields, attributes);
        List<String> partOfArea = getPartOfArea(ptFields, attributes,
                ptRslt.geometry);
        int gid = getGid(ptFields, attributes);
        ClosestPoint cp = new ClosestPoint(name, point, population, warngenlev,
                partOfArea, gid);

        // Used to determine if a storm location is within an urban bound area
        if (useDirections(attributes.get(useDirectionField))) {
            cp.setPrepGeom(PreparedGeometryFactory.prepare(ptRslt.geometry));
        }
        return cp;
    }

    /**
     * Converts DB value (i.e. 1, t, true) to a boolean true
     * 
     * @param useDirectionValue
     * @return
     */
    private boolean useDirections(Object useDirectionValue) {
        String userDir = String.valueOf(useDirectionValue).toLowerCase();
        return Boolean.valueOf(userDir) || userDir.equals("t")
                || userDir.equals("1");
    }

    /**
     * Processes the filter to set the localized site.
     */
    @Override
    protected Map<String, RequestConstraint> processFilterSubstitution(
            Map<String, RequestConstraint> filter) {
        if (filter != null) {
            // Process substitutes for filter
            for (RequestConstraint rc : filter.values()) {
                rc.setConstraintValue(PreferenceUtil.substitute(
                        rc.getConstraintValue(), localizedSite));
            }
        }

        if (filter == null) {
            filter = new HashMap<String, RequestConstraint>();
        }

        return filter;
    }

    /**
     * Determines the part of area impacted if the userDirectionField is set to
     * true. This method only takes into account areas within the warning
     * polygon.
     * 
     * @param ptFields
     * @param attributes
     * @param geom
     * @return
     */
    private List<String> getPartOfArea(Set<String> ptFields,
            Map<String, Object> attributes, Geometry geom) {
        List<String> partOfArea = null;
        if (useDirections(attributes.get(useDirectionField))) {
            PreparedGeometry prepGeom = PreparedGeometryFactory.prepare(geom);
            if (prepGeom.intersects(searchArea) && !prepGeom.within(searchArea)) {
                Geometry intersection = searchArea.intersection(geom);

                double areaIntersection = intersection.getArea();
                double tolerCheck = geom.getArea()
                        * Area.DEFAULT_PORTION_TOLERANCE;
                if (areaIntersection < tolerCheck) {
                    partOfArea = GisUtil.asStringList(GisUtil.calculatePortion(
                            geom, intersection, false, false));
                }

                if ((partOfArea != null)
                        && (attributes.get(suppressedDirectionsField) != null)) {
                    String suppressedDirections = String.valueOf(
                            attributes.get(suppressedDirectionsField))
                            .toLowerCase();
                    // supdirs can be 'nse', for example
                    // TODO create an enum constructor for Directions
                    for (int i = 0; i < suppressedDirections.length(); i++) {
                        switch (suppressedDirections.charAt(i)) {
                        case 'n':
                            partOfArea.remove(Direction.NORTH.toString());
                            break;
                        case 's':
                            partOfArea.remove(Direction.SOUTH.toString());
                            break;
                        case 'e':
                            partOfArea.remove(Direction.EAST.toString());
                            break;
                        case 'w':
                            partOfArea.remove(Direction.WEST.toString());
                            break;
                        }
                    }
                }
            }
        }

        if (partOfArea != null && !partOfArea.isEmpty()) {
            return partOfArea;
        }

        return null;
    }

    /**
     * Returns a Coordinate based on the lat,lon values in the attributes.
     * 
     * @param attributes
     * @return
     */
    private Coordinate getPoint(Map<String, Object> attributes) {
        double lat = Double.valueOf(String.valueOf(attributes.get(latitude)));
        double lon = Double.valueOf(String.valueOf(attributes.get(longitude)));

        return new Coordinate(lon, lat);
    }
}
