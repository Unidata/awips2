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

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.StringUtils;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.geospatial.SpatialQueryResult;
import com.raytheon.uf.viz.core.maps.rsc.DbMapQueryFactory;
import com.raytheon.viz.warngen.PreferenceUtil;
import com.raytheon.viz.warngen.gis.ClosestPoint;
import com.vividsolutions.jts.geom.Coordinate;

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

public class DbPointSourceDataAdaptor extends AbstractDbSourceDataAdaptor {

    @Override
    protected Set<String> createSpatialQueryField() {
        Set<String> ptFields = new HashSet<String>();
        ptFields.add(pointConfig.getPointField());

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

    @Override
    protected ClosestPoint createClosestPoint(Set<String> ptFields,
            SpatialQueryResult ptRslt) {
        Map<String, Object> attributes = ptRslt.attributes;

        String name = String
                .valueOf(attributes.get(pointConfig.getPointField()));
        Coordinate point = ptRslt.geometry.getCoordinate();
        int population = getPopulation(ptFields, attributes);
        int warngenlev = getWangenlev(ptFields, attributes);

        return new ClosestPoint(name, point, population, warngenlev, null);
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

        return filter;
    }
}
