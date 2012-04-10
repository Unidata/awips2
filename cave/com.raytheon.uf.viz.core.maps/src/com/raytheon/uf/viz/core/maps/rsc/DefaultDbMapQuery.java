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
package com.raytheon.uf.viz.core.maps.rsc;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.maps.rsc.DbMapQueryFactory.DbMapQuery;
import com.vividsolutions.jts.geom.Envelope;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 9, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class DefaultDbMapQuery implements DbMapQuery {

    protected static final String MAPS = "maps";

    protected final String table;

    protected final String geomField;

    protected DefaultDbMapQuery(String table, String geomField) {
        this.table = table;
        this.geomField = geomField;
    }

    @Override
    public QueryResult queryWithinEnvelope(Envelope env, List<String> columns,
            List<String> additionalConstraints) throws VizException {
        // add the geospatial constraint
        if (env != null) {
            // copy before modifying
            if (additionalConstraints == null) {
                additionalConstraints = new ArrayList<String>();
            } else {
                additionalConstraints = new ArrayList<String>(
                        additionalConstraints);
            }
            // geospatial constraint will be first
            additionalConstraints.add(0, String.format(
                    "%s && ST_SetSrid('BOX3D(%f %f, %f %f)'::box3d,4326)",
                    geomField, env.getMinX(), env.getMinY(), env.getMaxX(),
                    env.getMaxY()));
        }

        StringBuilder query = new StringBuilder("SELECT ");
        if (columns != null && !columns.isEmpty()) {
            Iterator<String> iter = columns.iterator();
            query.append(iter.next());
            while (iter.hasNext()) {
                query.append(", ");
                query.append(iter.next());
            }
        }

        query.append(" FROM ");
        query.append(table);

        // add any additional constraints
        if (additionalConstraints != null && !additionalConstraints.isEmpty()) {
            query.append(" WHERE ");
            Iterator<String> iter = additionalConstraints.iterator();
            query.append(iter.next());
            while (iter.hasNext()) {
                query.append(" AND ");
                query.append(iter.next());
            }
        }

        query.append(';');

        return DirectDbQuery.executeMappedQuery(query.toString(), MAPS,
                QueryLanguage.SQL);
    }

    @Override
    public List<String> getColumnNamesWithoutGeometries() throws VizException {
        List<String> labelFields = new ArrayList<String>();
        int p = table.indexOf('.');
        String schema = table.substring(0, p);
        String table = this.table.substring(p + 1);

        StringBuilder query = new StringBuilder(
                "SELECT column_name FROM information_schema.columns WHERE table_schema = '");
        query.append(schema);
        query.append("' AND table_name='");
        query.append(table);
        query.append("' ");
        query.append("AND udt_name != 'geometry' ");
        query.append("ORDER BY ordinal_position;");
        List<Object[]> results = DirectDbQuery.executeQuery(query.toString(),
                MAPS, QueryLanguage.SQL);

        for (Object[] obj : results) {
            labelFields.add(obj[0].toString());
        }
        return labelFields;
    }

    @Override
    public String getGeometryType() throws VizException {
        int p = table.indexOf('.');
        String schema = table.substring(0, p);
        String table = this.table.substring(p + 1);
        StringBuilder query = new StringBuilder(
                "SELECT type FROM geometry_columns WHERE f_table_schema='");
        query.append(schema);
        query.append("' AND f_table_name='");
        query.append(table);
        query.append("' LIMIT 1;");
        List<Object[]> results = DirectDbQuery.executeQuery(query.toString(),
                MAPS, QueryLanguage.SQL);

        return (String) results.get(0)[0];
    }

    @Override
    public List<Double> getLevels() throws VizException {
        int p = table.indexOf('.');
        String schema = table.substring(0, p);
        String table = this.table.substring(p + 1);
        StringBuilder query = new StringBuilder(
                "SELECT f_geometry_column FROM public.geometry_columns WHERE f_table_schema='");
        query.append(schema);
        query.append("' AND f_table_name='");
        query.append(table);
        query.append("' AND f_geometry_column LIKE '");
        query.append(geomField);
        query.append("_%';");
        List<Object[]> results = DirectDbQuery.executeQuery(query.toString(),
                MAPS, QueryLanguage.SQL);

        List<Double> levels = new ArrayList<Double>(results.size());
        for (Object[] objs : results) {
            String s = (String) objs[0];
            s = s.replace(geomField + "_", "");
            s = s.replace('_', '.');
            levels.add(Double.parseDouble(s));
        }
        return levels;
    }
}
