package com.raytheon.viz.warngen.config;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.measure.converter.UnitConverter;

import org.apache.commons.lang.StringUtils;
import org.geotools.geometry.jts.JTS;
import org.geotools.referencing.GeodeticCalculator;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.dataplugin.warning.config.PathcastConfiguration;
import com.raytheon.uf.common.dataplugin.warning.config.PointSourceConfiguration;
import com.raytheon.uf.common.dataplugin.warning.config.WarngenConfiguration;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.geospatial.ISpatialQuery.SearchMode;
import com.raytheon.uf.common.geospatial.SpatialException;
import com.raytheon.uf.common.geospatial.SpatialQueryFactory;
import com.raytheon.uf.common.geospatial.SpatialQueryResult;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.maps.rsc.DbMapQueryFactory;
import com.raytheon.viz.core.map.GeoUtil;
import com.raytheon.viz.warngen.gis.ClosestPoint;
import com.raytheon.viz.warngen.gis.ClosestPointComparator;
import com.raytheon.viz.warngen.util.AdjustAngle;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Point;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * pre-history
 * Sep 25, 2012 #15425     Qinglu Lin   Added getGid().
 * Oct 17, 2012            jsanchez     Added pathcast algorithm.
 * Feb 12, 2013  1600      jsanchez     Used adjustAngle method from AbstractStormTrackResource.
 * Mar  5, 2013  1600      jsanchez     Used AdjustAngle instead of AbstractStormTrackResource to handle angle adjusting.
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
abstract public class AbstractDbSourceDataAdaptor {

    private static final String transformedKey = "com.raytheon.transformed";

    private static final String GEOM_FIELD = "the_geom";

    protected Set<String> undatabasedSortableFields = new HashSet<String>(
            Arrays.asList(new String[] {
                    ClosestPointComparator.Sort.DISTANCE.toString(),
                    ClosestPointComparator.Sort.AREA.toString(),
                    ClosestPointComparator.Sort.PARENTAREA.toString() }));

    protected GeodeticCalculator gc = new GeodeticCalculator();

    protected Geometry searchArea;

    protected String localizedSite;

    protected SpatialQueryResult[] ptFeatures;

    protected Map<String, RequestConstraint> filter;

    protected Set<String> ptFields;

    protected String[] sortBy;

    abstract protected Set<String> createSpatialQueryField(String pointField,
            String[] sortBy);

    abstract protected ClosestPoint createClosestPoint(String pointField,
            Set<String> ptFields, SpatialQueryResult ptRslt);

    abstract protected Map<String, RequestConstraint> processFilterSubstitution(
            Map<String, RequestConstraint> filter);

    public AbstractDbSourceDataAdaptor(
            PathcastConfiguration pathcastConfiguration,
            UnitConverter distanceToMeters, Geometry searchArea,
            String localizedSite) throws VizException {
        this.localizedSite = localizedSite;
        this.sortBy = pathcastConfiguration.getSortBy();
        this.searchArea = searchArea;
        this.ptFields = createSpatialQueryField(
                pathcastConfiguration.getPointField(), sortBy);
        this.filter = processFilterSubstitution(pathcastConfiguration
                .getFilter());
        this.ptFeatures = spatialQuery(pathcastConfiguration.getPointSource(),
                null);
    }

    public AbstractDbSourceDataAdaptor(
            PointSourceConfiguration pointSourceConfiguration,
            Geometry searchArea, String localizedSite) throws VizException {
        this.localizedSite = localizedSite;
        this.sortBy = pointSourceConfiguration.getSortBy();
        this.searchArea = searchArea;
        this.ptFields = createSpatialQueryField(
                pointSourceConfiguration.getPointField(), sortBy);
        this.filter = processFilterSubstitution(pointSourceConfiguration
                .getFilter());
        this.ptFeatures = spatialQuery(
                pointSourceConfiguration.getPointSource(), null);
    }

    /**
     * Queries the maps database depending on the point source set in the
     * config.
     * 
     * @param pointSource
     * @param decimationTolerance
     * @return
     * @throws VizException
     */
    private SpatialQueryResult[] spatialQuery(String pointSource,
            Double decimationTolerance) throws VizException {

        SpatialQueryResult[] ptFeatures = null;
        long t0 = System.currentTimeMillis();
        try {
            if (decimationTolerance != null && decimationTolerance > 0) {
                // find available tolerances
                List<Double> results = DbMapQueryFactory.getMapQuery(
                        "mapdata." + pointSource.toLowerCase(), GEOM_FIELD)
                        .getLevels();
                Collections.sort(results, Collections.reverseOrder());

                boolean found = false;
                for (Double val : results) {
                    if (val <= decimationTolerance) {
                        decimationTolerance = val;
                        found = true;
                        break;
                    }
                }

                if (!found) {
                    decimationTolerance = null;
                }
            }

            if (decimationTolerance != null) {
                DecimalFormat df = new DecimalFormat("0.######");
                String suffix = "_"
                        + StringUtils.replaceChars(
                                df.format(decimationTolerance), '.', '_');
                ptFeatures = SpatialQueryFactory.create().query(pointSource,
                        GEOM_FIELD + suffix,
                        ptFields.toArray(new String[ptFields.size()]),
                        searchArea, filter, SearchMode.INTERSECTS);
            } else {
                ptFeatures = SpatialQueryFactory.create().query(pointSource,
                        ptFields.toArray(new String[ptFields.size()]),
                        searchArea, filter, SearchMode.INTERSECTS);
            }
            System.out.println("Retrieve location data for '" + pointSource
                    + "' = " + (System.currentTimeMillis() - t0));
        } catch (SpatialException e) {
            throw new VizException("Error querying " + pointSource + " table: "
                    + e.getLocalizedMessage(), e);
        }

        return ptFeatures;
    }

    /**
     * Returns the data of the points/areas relative to the searchArea
     * 
     * @param config
     * @param pointConfig
     * @param localizedSite
     * @return
     * @throws VizException
     */
    public Collection<ClosestPoint> getData(WarngenConfiguration config,
            PointSourceConfiguration pointConfig, String localizedSite)
            throws VizException {
        List<ClosestPoint> points = null;

        String pointField = pointConfig.getPointField();
        if (ptFeatures != null) {
            points = new ArrayList<ClosestPoint>(ptFeatures.length);
        } else {
            points = new ArrayList<ClosestPoint>(0);
        }

        for (SpatialQueryResult ptRslt : ptFeatures) {
            if (ptRslt != null && ptRslt.geometry != null) {
                Object nameObj = ptRslt.attributes.get(pointConfig
                        .getPointField());
                if (nameObj != null) {
                    ClosestPoint cp = createClosestPoint(pointField, ptFields,
                            ptRslt);
                    cp.setGid(getGid(ptFields, ptRslt.attributes));
                    points.add(cp);
                }
            }
        }

        return points;
    }

    /**
     * Returns a list of implacted points/areas that are relative to the
     * centroid.
     * 
     * @param pcGeom
     * @param centroid
     * @param areaFeatures
     * @param pcArea
     * @param pcParentArea
     * @return
     * @throws Exception
     */
    public List<ClosestPoint> getPathcastData(
            PathcastConfiguration pathcastConfiguration,
            UnitConverter distanceToMeters, MathTransform latLonToLocal,
            Geometry pcGeom, Point centroid, SpatialQueryResult[] areaFeatures,
            String pcArea, String pcParentArea) throws Exception {
        String pointField = pathcastConfiguration.getPointField();
        String areaField = pathcastConfiguration.getAreaField();
        String parentAreaField = pathcastConfiguration.getParentAreaField();
        double thresholdInMeters = distanceToMeters
                .convert(pathcastConfiguration.getDistanceThreshold());

        if (latLonToLocal != null) {
            for (SpatialQueryResult rslt : ptFeatures) {
                rslt.attributes.put(transformedKey,
                        JTS.transform(rslt.geometry, latLonToLocal));
            }
        }

        Geometry localPCGeom = null;
        if (pcGeom != null) {
            localPCGeom = JTS.transform(pcGeom, latLonToLocal);
        }

        // Find closest points
        List<ClosestPoint> points = new ArrayList<ClosestPoint>(
                ptFeatures.length);
        for (SpatialQueryResult pointRslt : ptFeatures) {
            Geometry localPt = (Geometry) pointRslt.attributes
                    .get(transformedKey);
            double minDist = Double.MAX_VALUE;
            Coordinate closestCoord = null;
            if (localPCGeom != null) {
                Coordinate[] localPts = localPCGeom.getCoordinates();
                Coordinate[] latLonPts = pcGeom.getCoordinates();
                for (int i = 0; i < localPts.length; ++i) {
                    Coordinate loc = localPts[i];
                    double distance = loc.distance(localPt.getCoordinate());
                    if (distance <= thresholdInMeters && distance < minDist) {
                        minDist = distance;
                        closestCoord = latLonPts[i];
                    }
                }
            } else {
                closestCoord = centroid.getCoordinate();
                minDist = 0;
            }

            if (closestCoord != null) {
                boolean found = false;
                String area = null;
                String parentArea = null;
                for (SpatialQueryResult areaRslt : areaFeatures) {
                    if (areaRslt.geometry.contains(pointRslt.geometry)) {
                        area = String.valueOf(areaRslt.attributes
                                .get(areaField));
                        parentArea = String.valueOf(areaRslt.attributes
                                .get(parentAreaField));
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    area = pcArea;
                    parentArea = pcParentArea;
                }

                ClosestPoint cp = createClosestPoint(pointField, pointRslt,
                        minDist, closestCoord, area, parentArea,
                        distanceToMeters.inverse());
                points.add(cp);
            }
        }

        List<String> fields = null;
        if (pathcastConfiguration.getSortBy() != null) {
            fields = Arrays.asList(pathcastConfiguration.getSortBy());
        } else {
            fields = new ArrayList<String>(0);
        }

        if (!fields.isEmpty()) {
            // Sort the points based on sortBy fields
            Collections.sort(points, new ClosestPointComparator(fields));
        }

        return points;
    }

    /**
     * Creates a closestPoint setting the distance, azimuth, etc. Used for
     * pathcast calculations
     * 
     * @param pointRslt
     * @param minDist
     * @param closestCoord
     * @param area
     * @param parentArea
     * @return
     */
    private ClosestPoint createClosestPoint(String pointField,
            SpatialQueryResult pointRslt, double minDist,
            Coordinate closestCoord, String area, String parentArea,
            UnitConverter metersToDistance) {

        ClosestPoint cp = createClosestPoint(pointField, ptFields, pointRslt);
        cp.setDistance(minDist);
        cp.setRoundedDistance((int) metersToDistance.convert(minDist));
        gc.setStartingGeographicPoint(cp.getPoint().x, cp.getPoint().y);
        gc.setDestinationGeographicPoint(closestCoord.x, closestCoord.y);
        cp.setAzimuth(gc.getAzimuth());
        cp.setOppositeAzimuth(AdjustAngle.to360Degrees(cp.getAzimuth() + 180));
        cp.setRoundedAzimuth(GeoUtil.roundAzimuth(cp.getAzimuth()));
        cp.setOppositeRoundedAzimuth(AdjustAngle.to360Degrees(cp
                .getRoundedAzimuth() + 180));
        cp.setArea(area);
        cp.setParentArea(parentArea);
        cp.setGid(getGid(ptFields, pointRslt.attributes));

        return cp;
    }

    /**
     * Retrieves the population from the attributes.
     * 
     * @param ptFields
     * @param attributes
     * @return
     */
    protected int getPopulation(Set<String> ptFields,
            Map<String, Object> attributes) {
        int population = 0;

        if (ptFields.contains(String
                .valueOf(ClosestPointComparator.Sort.POPULATION))) {
            try {
                population = Integer
                        .valueOf(String.valueOf(attributes.get(String
                                .valueOf(ClosestPointComparator.Sort.POPULATION))));
            } catch (Exception e) {
                // Ignore
            }
        }

        return population;
    }

    /**
     * Retrieves the warngenlev from the attributes.
     * 
     * @param ptFields
     * @param attributes
     * @return
     */
    protected int getWangenlev(Set<String> ptFields,
            Map<String, Object> attributes) {
        int warngenlev = 3;

        if (ptFields.contains(String
                .valueOf(ClosestPointComparator.Sort.WARNGENLEV))) {
            try {
                warngenlev = Integer
                        .valueOf(String.valueOf(attributes.get(String
                                .valueOf(ClosestPointComparator.Sort.WARNGENLEV))));
            } catch (Exception e) {
                // Ignore
            }
        }

        return warngenlev;
    }

    /**
     * Returns the gid.
     * 
     * @param ptFields
     * @param attributes
     * @return
     */
    protected int getGid(Set<String> ptFields, Map<String, Object> attributes) {
        int gid = 0;

        if (ptFields.contains(String.valueOf(ClosestPointComparator.Sort.GID))) {
            try {
                gid = Integer.valueOf(String.valueOf(attributes.get(String
                        .valueOf(ClosestPointComparator.Sort.GID))));
            } catch (Exception e) {
                // Ignore
            }
        }

        return gid;
    }

}
