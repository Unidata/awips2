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
package com.raytheon.viz.warngen.config;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.warning.config.PointSourceConfiguration;
import com.raytheon.uf.common.dataplugin.warning.config.WarngenConfiguration;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.geospatial.ISpatialQuery.SearchMode;
import com.raytheon.uf.common.geospatial.SpatialException;
import com.raytheon.uf.common.geospatial.SpatialQueryFactory;
import com.raytheon.uf.common.geospatial.SpatialQueryResult;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.warngen.PreferenceUtil;
import com.raytheon.viz.warngen.gis.ClosestPoint;
import com.vividsolutions.jts.geom.Geometry;

/**
 * PointSource data adaptor for data retrieved from a Database.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 26, 2011            bgonzale     Initial creation
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

public class DbPointSourceDataAdaptor implements IPointSourceDataAdaptor {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.warngen.config.IPointSourceDataAdaptor#findClosestPoints
     * (
     * com.raytheon.uf.common.dataplugin.warning.config.PointSourceConfiguration
     * , java.lang.String)
     */
    @Override
    public Collection<ClosestPoint> getData(WarngenConfiguration warngenConfig,
            PointSourceConfiguration pointConfig, Geometry searchArea,
            String localizedSite) throws VizException {
        String pointSource = pointConfig.getPointSource();
        String pointField = pointConfig.getPointField();

        Map<String, RequestConstraint> filter = pointConfig.getFilter();
        if (filter != null) {
            // Process substitutes for filter
            for (RequestConstraint rc : filter.values()) {
                rc.setConstraintValue(PreferenceUtil.substitute(
                        rc.getConstraintValue(), localizedSite));
            }
        }

        List<String> fields = pointConfig.getSortBy() != null ? Arrays
                .asList(pointConfig.getSortBy()) : new ArrayList<String>();

        Set<String> ptFields = new HashSet<String>();
        ptFields.add(pointField);
        for (String field : fields) {
            if (!field.equalsIgnoreCase("distance")
                    && !field.equalsIgnoreCase("area")
                    && !field.equalsIgnoreCase("parentArea")) {
                ptFields.add(field.toLowerCase());
            }
        }

        List<ClosestPoint> points = new ArrayList<ClosestPoint>();

        try {
            SpatialQueryResult[] ptFeatures = SpatialQueryFactory.create()
                    .query(pointSource,
                            ptFields.toArray(new String[ptFields.size()]),
                            searchArea, filter, SearchMode.INTERSECTS);
            for (SpatialQueryResult ptRslt : ptFeatures) {
                if (ptRslt != null && ptRslt.geometry != null) {
                    Object nameObj = ptRslt.attributes.get(pointField);
                    if (nameObj != null) {
                        int population = 0;
                        int warngenlev = 0;
                        ClosestPoint cp = new ClosestPoint();

                        if (ptFields.contains("population")) {
                            try {
                                population = Integer.valueOf(String
                                        .valueOf(ptRslt.attributes
                                                .get("population")));
                            } catch (Exception e) {
                                // Ignore
                            }
                        }
                        if (ptFields.contains("warngenlev")) {
                            try {
                                warngenlev = Integer.valueOf(String
                                        .valueOf(ptRslt.attributes
                                                .get("warngenlev")));
                            } catch (Exception e) {
                                // Ignore
                            }
                        }

                        points.add(new ClosestPoint(nameObj.toString(),
                                ptRslt.geometry.getCoordinate(), population,
                                warngenlev));
                    }
                }
            }
        } catch (SpatialException e) {
            throw new VizException("Error querying " + pointSource + " table: "
                    + e.getLocalizedMessage(), e);
        }

        return points;
    }
}
