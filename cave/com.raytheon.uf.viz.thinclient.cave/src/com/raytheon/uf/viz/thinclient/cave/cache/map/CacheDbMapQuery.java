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
package com.raytheon.uf.viz.thinclient.cave.cache.map;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.maps.rsc.DefaultDbMapQuery;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.index.strtree.STRtree;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKBReader;

/**
 * DbMapQuery implementation using client side cache
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 9, 2011            bsteffen     Initial creation
 * Apr 9, 2014   #2997    randerso     Added queryWithinGeometry
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class CacheDbMapQuery extends DefaultDbMapQuery {

    private static final String GID = "gid";

    private static final String GEOM_ENV = "geom_env";

    // Used for spatial lookups, returns a list of indexes into the data
    private STRtree tree;

    // all database columns for every row, this is the primary cached data.
    private QueryResult data = null;

    // a specific list of indexes which are returned for any given constraint
    private Map<String, List<Integer>> constraint2indices = new HashMap<String, List<Integer>>();

    // The column names available without any geometry columns.
    private List<String> columnNames = null;

    // the geometry type
    private String geometryType = null;;

    // all levels for a the geometry
    private List<Double> levels = null;

    private Object columnNamesLock = new Object();

    private Object geometryTypeLock = new Object();

    private Object levelsLock = new Object();

    protected CacheDbMapQuery(String table, String geomField) {
        super(table, geomField);
    }

    protected CacheDbMapQuery(CacheDbMapQuerySerializeable s)
            throws VizException {
        super(s.getTable(), s.getGeomField());
        geometryType = s.getGeometryType();
        columnNames = s.getColumnNames();
        levels = s.getLevels();
        constraint2indices = s.getConstraint2indices();
        data = s.getData();
        if (data != null) {
            createTree();
        }
    }

    private QueryResult queryWithinEnvelope(Envelope env, List<String> columns,
            List<String> additionalConstraints) throws VizException {
        fillCache(geomField, columns);
        List<?> items = tree.query(env);
        List<Integer> validGIDs = null;
        if (additionalConstraints != null) {
            for (String constraint : additionalConstraints) {
                items.retainAll(getIndices(constraint));
            }
        }
        List<QueryResultRow> rows = new ArrayList<QueryResultRow>();
        for (int i = 0; i < items.size(); i++) {
            int rowNumber = (Integer) items.get(i);
            if ((validGIDs != null)
                    && !validGIDs.contains(data.getRowColumnValue(rowNumber,
                            GID))) {
                continue;
            }
            Object[] columnValues = new Object[columns.size()];
            for (int j = 0; j < columns.size(); j++) {
                columnValues[j] = data.getRowColumnValue(rowNumber,
                        columns.get(j));
            }
            rows.add(new QueryResultRow(columnValues));
        }
        Map<String, Integer> columnNames = new HashMap<String, Integer>();
        for (int i = 0; i < columns.size(); i++) {
            columnNames.put(doAs(columns.get(i)), i);
        }
        return new QueryResult(columnNames, rows.toArray(new QueryResultRow[0]));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.maps.rsc.DefaultDbMapQuery#queryWithinGeometry
     * (com.vividsolutions.jts.geom.Geometry, java.util.List, java.util.List)
     */
    @Override
    public QueryResult queryWithinGeometry(Geometry geom, List<String> columns,
            List<String> additionalConstraints) throws VizException {
        return queryWithinEnvelope(geom.getEnvelopeInternal(), columns,
                additionalConstraints);
    }

    private List<Integer> getIndices(String constraint) throws VizException {
        synchronized (constraint2indices) {
            List<Integer> indices = constraint2indices.get(constraint);
            if (indices != null) {
                return indices;
            }
            StringBuilder query = new StringBuilder("SELECT ");
            query.append(GID);
            query.append(" FROM ");
            query.append(table);
            query.append(" WHERE ");
            query.append(constraint);
            query.append(";");

            List<Object[]> results = DirectDbQuery.executeQuery(
                    query.toString(), MAPS, QueryLanguage.SQL);
            ArrayList<Integer> gids = new ArrayList<Integer>(results.size());
            for (Object[] result : results) {
                gids.add((Integer) result[0]);
            }
            indices = new ArrayList<Integer>(gids.size());
            for (int i = 0; i < data.getResultCount(); ++i) {
                int gid = (Integer) data.getRowColumnValue(i, "gid");
                if (gids.contains(gid)) {
                    indices.add(i);
                }
            }
            constraint2indices.put(constraint, indices);
            return indices;
        }
    }

    private QueryResult getColumns(List<String> columns) throws VizException {
        StringBuilder query = new StringBuilder("SELECT ");
        Iterator<String> iter = columns.iterator();
        query.append(iter.next());
        while (iter.hasNext()) {
            query.append(", ");
            query.append(iter.next());
        }
        query.append(" from ");
        query.append(table);
        query.append(";");
        QueryResult result = DirectDbQuery.executeMappedQuery(query.toString(),
                MAPS, QueryLanguage.SQL);
        for (String column : columns) {
            String as = doAs(column);
            if (as.equals(column)) {
                continue;
            }
            if (GEOM_ENV.equals(as)) {
                continue;
            }
            int index = result.getColumnNames().remove(doAs(column));
            result.getColumnNames().put(column, index);
        }
        return result;
    }

    private synchronized void fillCache(String geomField, List<String> columns)
            throws VizException {
        columns = new ArrayList<String>(columns);
        if (data != null) {
            for (String column : data.getColumnNames().keySet()) {
                columns.remove(column);
            }
        }
        if (columns.isEmpty()) {
            return;
        }
        // we correlate data based off gid so make sure it is in there.
        if (!columns.contains("gid")) {
            columns.add("gid");
        }
        if (tree == null) {
            // request the envelope
            String envColumn = "AsBinary(ST_Envelope(" + geomField + ")) as "
                    + GEOM_ENV;
            columns.add(envColumn);
            QueryResult result = getColumns(columns);
            columns.remove(envColumn);
            data = result;
            createTree();
        } else {
            QueryResult result = getColumns(columns);
            // gid should already be in the result set.
            columns.remove("gid");
            Map<Integer, QueryResultRow> gid2row = new HashMap<Integer, QueryResultRow>();
            for (int i = 0; i < result.getResultCount(); ++i) {
                int gid = (Integer) result.getRowColumnValue(i, "gid");
                gid2row.put(gid, result.getRows()[i]);
            }
            int index = data.getColumnNames().size();
            for (String column : columns) {
                data.getColumnNames().put(column, index++);
            }
            for (int i = 0; i < data.getResultCount(); ++i) {
                int gid = (Integer) data.getRowColumnValue(i, "gid");
                QueryResultRow existRow = data.getRows()[i];
                QueryResultRow resultRow = gid2row.get(gid);
                Object[] columnValues = existRow.getColumnValues();
                columnValues = Arrays.copyOf(columnValues, columnValues.length
                        + columns.size());
                for (String column : columns) {
                    int resultIndex = result.getColumnNames().get(column);
                    int newIndex = data.getColumnNames().get(column);
                    columnValues[newIndex] = resultRow.getColumn(resultIndex);
                }
                existRow.setColumnValues(columnValues);
            }
        }
    }

    private void createTree() throws VizException {
        WKBReader wkbReader = new WKBReader();
        tree = new STRtree();
        try {
            for (int i = 0; i < data.getResultCount(); ++i) {
                Object obj = data.getRowColumnValue(i, GEOM_ENV);
                if (obj instanceof byte[]) {
                    byte[] wkb = (byte[]) obj;
                    Geometry g = wkbReader.read(wkb);
                    Envelope e = g.getEnvelopeInternal();
                    tree.insert(e, i);
                } else {
                    throw new VizException("Error populating GID Cache.");
                }
            }
        } catch (ParseException e) {
            throw new VizException(e);
        }
    }

    private String doAs(String string) {
        String[] split = string.split("\\s[aA][sS]\\s");
        return split[split.length - 1].trim().toLowerCase();
    }

    @Override
    public List<String> getColumnNamesWithoutGeometries() throws VizException {
        synchronized (columnNamesLock) {
            if (columnNames == null) {
                columnNames = super.getColumnNamesWithoutGeometries();
            }
            return new ArrayList<String>(columnNames);
        }
    }

    @Override
    public String getGeometryType() throws VizException {
        synchronized (geometryTypeLock) {
            if (geometryType == null) {
                geometryType = super.getGeometryType();
            }
            return geometryType;
        }
    }

    @Override
    public List<Double> getLevels() throws VizException {
        synchronized (levelsLock) {
            if (levels == null) {
                levels = super.getLevels();
            }
            return levels;
        }

    }

    protected CacheDbMapQuerySerializeable getSerializeable() {
        CacheDbMapQuerySerializeable s = new CacheDbMapQuerySerializeable();
        s.setTable(table);
        s.setGeomField(geomField);
        s.setGeometryType(geometryType);
        s.setColumnNames(columnNames);
        s.setLevels(levels);
        s.setConstraint2indices(constraint2indices);
        s.setData(data);
        return s;
    }

    protected String getTable() {
        return table;
    }

    protected String getGeomField() {
        return geomField;
    }

}
