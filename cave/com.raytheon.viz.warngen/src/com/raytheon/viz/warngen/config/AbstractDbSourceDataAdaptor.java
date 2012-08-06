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

import org.apache.commons.lang.StringUtils;

import com.raytheon.uf.common.dataplugin.warning.config.PointSourceConfiguration;
import com.raytheon.uf.common.dataplugin.warning.config.WarngenConfiguration;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.geospatial.ISpatialQuery.SearchMode;
import com.raytheon.uf.common.geospatial.SpatialException;
import com.raytheon.uf.common.geospatial.SpatialQueryFactory;
import com.raytheon.uf.common.geospatial.SpatialQueryResult;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.maps.rsc.DbMapQueryFactory;
import com.raytheon.viz.warngen.gis.ClosestPoint;
import com.vividsolutions.jts.geom.Geometry;

abstract public class AbstractDbSourceDataAdaptor {

    protected Set<String> sortFields = new HashSet<String>(
            Arrays.asList(new String[] { "distance", "area", "parentArea" }));

    protected PointSourceConfiguration pointConfig;

    protected Geometry searchArea;

    protected String localizedSite;

    abstract protected Set<String> createSpatialQueryField();

    abstract protected ClosestPoint createClosestPoint(Set<String> ptFields,
            SpatialQueryResult ptRslt);

    abstract protected Map<String, RequestConstraint> processFilterSubstitution();

    public Collection<ClosestPoint> getData(WarngenConfiguration config,
            PointSourceConfiguration pointConfig, Geometry searchArea,
            String localizedSite) throws VizException {
        this.pointConfig = pointConfig;
        this.searchArea = searchArea;
        this.localizedSite = localizedSite;

        Map<String, RequestConstraint> filter = processFilterSubstitution();
        Set<String> ptFields = createSpatialQueryField();
        List<ClosestPoint> points = null;

        try {
            long t0 = System.currentTimeMillis();
            SpatialQueryResult[] ptFeatures = null;
            Double decimationTolerance = pointConfig
                    .getGeometryDecimationTolerance();
            String field = "the_geom";

            if (decimationTolerance != null && decimationTolerance > 0) {
                // find available tolerances
                List<Double> results = DbMapQueryFactory
                        .getMapQuery(
                                "mapdata."
                                        + pointConfig.getPointSource()
                                                .toLowerCase(), field)
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
                ptFeatures = SpatialQueryFactory.create().query(
                        pointConfig.getPointSource(), field + suffix,
                        ptFields.toArray(new String[ptFields.size()]),
                        searchArea, filter, SearchMode.INTERSECTS);
            } else {
                ptFeatures = SpatialQueryFactory.create().query(
                        pointConfig.getPointSource(),
                        ptFields.toArray(new String[ptFields.size()]),
                        searchArea, filter, SearchMode.INTERSECTS);
            }

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
                        ClosestPoint cp = createClosestPoint(ptFields, ptRslt);
                        points.add(cp);
                    }
                }
            }
            System.out.println("Retrieve location data for '"
                    + pointConfig.getVariable() + "' = "
                    + (System.currentTimeMillis() - t0));
        } catch (SpatialException e) {
            throw new VizException("Error querying "
                    + pointConfig.getPointSource() + " table: "
                    + e.getLocalizedMessage(), e);
        }

        return points;
    }

    protected int getPopulation(Set<String> ptFields,
            Map<String, Object> attributes) {
        int population = 0;

        if (ptFields.contains("population")) {
            try {
                population = Integer.valueOf(String.valueOf(attributes
                        .get("population")));
            } catch (Exception e) {
                // Ignore
            }
        }

        return population;
    }

    protected int getWangenlev(Set<String> ptFields,
            Map<String, Object> attributes) {
        int warngenlev = 0;

        if (ptFields.contains("warngenlev")) {
            try {
                warngenlev = Integer.valueOf(String.valueOf(attributes
                        .get("warngenlev")));
            } catch (Exception e) {
                // Ignore
            }
        }

        return warngenlev;
    }

}
