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
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.measure.converter.UnitConverter;

import com.raytheon.uf.common.dataplugin.warning.config.PathcastConfiguration;
import com.raytheon.uf.common.dataplugin.warning.config.PointSourceConfiguration;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.geospatial.SpatialQueryResult;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.warngen.PreferenceUtil;
import com.raytheon.viz.warngen.gis.ClosestPoint;
import com.vividsolutions.jts.geom.Coordinate;
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
 * Sep 25, 2012 #15425     Qinglu Lin   Updated createClosestPoint().
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

public class DbPointSourceDataAdaptor extends AbstractDbSourceDataAdaptor {

    public DbPointSourceDataAdaptor(
            PathcastConfiguration pathcastConfiguration,
            UnitConverter distanceToMeters, Geometry searchArea,
            String localizedSite) throws VizException {
        super(pathcastConfiguration, distanceToMeters, searchArea,
                localizedSite);
    }

    public DbPointSourceDataAdaptor(
            PointSourceConfiguration pointSourceConfiguration,
            Geometry searchArea, String localizedSite) throws VizException {
        super(pointSourceConfiguration, searchArea, localizedSite);
    }

    @Override
    protected Set<String> createSpatialQueryField(String pointField,
            String[] sortBy) {
        Set<String> ptFields = new HashSet<String>();
        ptFields.add(pointField);

        List<String> fields = new ArrayList<String>();
        if (sortBy != null) {
            fields = Arrays.asList(sortBy);
        }

        // Sort fields don't exist in the db.
        for (String field : fields) {
            if (undatabasedSortableFields.contains(field.toUpperCase()) == false) {
                ptFields.add(field.toUpperCase());
            }
        }

        return ptFields;
    }

    @Override
    protected ClosestPoint createClosestPoint(String pointField,
            Set<String> ptFields, SpatialQueryResult ptRslt) {
        Map<String, Object> attributes = ptRslt.attributes;

        String name = String.valueOf(attributes.get(pointField));
        Coordinate point = ptRslt.geometry.getCoordinate();
        int population = getPopulation(ptFields, attributes);
        int warngenlev = getWangenlev(ptFields, attributes);
        int gid = getGid(ptFields, attributes);

        return new ClosestPoint(name, point, population, warngenlev, null, gid);
    }

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

        return filter;
    }
}
