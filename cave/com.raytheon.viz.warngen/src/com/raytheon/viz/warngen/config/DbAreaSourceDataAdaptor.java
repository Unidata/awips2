package com.raytheon.viz.warngen.config;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.geotools.referencing.GeodeticCalculator;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.geospatial.SpatialQueryResult;
import com.raytheon.viz.warngen.PreferenceUtil;
import com.raytheon.viz.warngen.gis.ClosestPoint;
import com.raytheon.viz.warngen.gis.GisUtil;
import com.raytheon.viz.warngen.gis.GisUtil.Direction;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;

/**
 * 
 * @author jsanchez
 * 
 */
public class DbAreaSourceDataAdaptor extends AbstractDbSourceDataAdaptor {

    private static final String useDirectionField = "usedirs";

    private static final String suppressedDirectionsField = "supdirs";

    private static final String cwaField = "cwa";

    private GeodeticCalculator gc = new GeodeticCalculator();

    /**
     * 
     */
    @Override
    protected Set<String> createSpatialQueryField() {
        Set<String> ptFields = new HashSet<String>();
        ptFields.add(pointConfig.getPointField());
        ptFields.add(useDirectionField);
        ptFields.add(suppressedDirectionsField);

        List<String> fields = new ArrayList<String>();
        if (pointConfig.getSortBy() != null) {
            fields = Arrays.asList(pointConfig.getSortBy());
        }

        for (String field : fields) {
            if (sortFields.contains(field.toLowerCase()) == false) {
                ptFields.add(field.toLowerCase());
            }
        }

        return ptFields;
    }

    /**
     * 
     */
    @Override
    protected ClosestPoint createClosestPoint(Set<String> ptFields,
            SpatialQueryResult ptRslt) {
        Map<String, Object> attributes = ptRslt.attributes;

        String name = String
                .valueOf(attributes.get(pointConfig.getPointField()));
        Coordinate point = ptRslt.geometry.getCoordinate();
        int population = getPopulation(ptFields, attributes);
        int warngenlev = getWangenlev(ptFields, attributes);
        List<String> partOfArea = getPartOfArea(ptFields, attributes,
                ptRslt.geometry);

        return new ClosestPoint(name, point, population, warngenlev, partOfArea);
    }

    /**
     * 
     * @param ptFields
     * @param attributes
     * @param geom
     * @return
     */
    private List<String> getPartOfArea(Set<String> ptFields,
            Map<String, Object> attributes, Geometry geom) {
        List<String> partOfArea = null;

        boolean userDirections = Boolean.valueOf(String.valueOf(attributes
                .get(useDirectionField)));
        if (userDirections) {
            Geometry intersection = searchArea.intersection(geom);
            partOfArea = GisUtil.asStringList(GisUtil.calculatePortion(geom,
                    intersection, gc, ""));

            if (attributes.get(suppressedDirectionsField) != null) {
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
                    case 'c':
                        partOfArea.remove(Direction.CENTRAL.toString());
                        break;
                    }
                }

            }
        }

        return partOfArea;
    }

    @Override
    protected Map<String, RequestConstraint> processFilterSubstitution() {
        Map<String, RequestConstraint> filter = pointConfig.getFilter();
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

        filter.put(cwaField, new RequestConstraint(localizedSite));

        return filter;
    }
}
