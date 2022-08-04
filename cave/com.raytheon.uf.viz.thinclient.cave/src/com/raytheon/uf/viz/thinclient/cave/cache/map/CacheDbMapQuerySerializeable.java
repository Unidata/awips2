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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

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

@DynamicSerialize
public class CacheDbMapQuerySerializeable {

    @DynamicSerialize
    public static class ConstraintAndIndices {
        @DynamicSerializeElement
        private String constraint;

        @DynamicSerializeElement
        private List<Integer> indices;

        public String getConstraint() {
            return constraint;
        }

        public void setConstraint(String constraint) {
            this.constraint = constraint;
        }

        public List<Integer> getIndices() {
            return indices;
        }

        public void setIndices(List<Integer> indices) {
            this.indices = indices;
        }

    }

    @DynamicSerializeElement
    private String table;

    @DynamicSerializeElement
    private String geomField;

    @DynamicSerializeElement
    private QueryResult data;

    @DynamicSerializeElement
    private List<ConstraintAndIndices> constraintAndIndices;

    @DynamicSerializeElement
    private List<String> columnNames;

    @DynamicSerializeElement
    private String geometryType;;

    @DynamicSerializeElement
    private List<Double> levels;

    public String getTable() {
        return table;
    }

    public void setTable(String table) {
        this.table = table;
    }

    public String getGeomField() {
        return geomField;
    }

    public void setGeomField(String geomField) {
        this.geomField = geomField;
    }

    public QueryResult getData() {
        return data;
    }

    public void setData(QueryResult data) {
        this.data = data;
    }

    public Map<String, List<Integer>> getConstraint2indices() {
        Map<String, List<Integer>> m = new HashMap<String, List<Integer>>();
        for (ConstraintAndIndices c : constraintAndIndices) {
            m.put(c.getConstraint(), c.getIndices());
        }
        return m;
    }

    public void setConstraint2indices(
            Map<String, List<Integer>> constraint2indices) {
        constraintAndIndices = new ArrayList<ConstraintAndIndices>(
                constraint2indices.size());
        for (Entry<String, List<Integer>> entry : constraint2indices.entrySet()) {
            ConstraintAndIndices c = new ConstraintAndIndices();
            c.setConstraint(entry.getKey());
            c.setIndices(entry.getValue());
            constraintAndIndices.add(c);
        }
    }

    public List<ConstraintAndIndices> getConstraintAndIndices() {
        return constraintAndIndices;
    }

    public void setConstraintAndIndices(
            List<ConstraintAndIndices> constraintAndIndices) {
        this.constraintAndIndices = constraintAndIndices;
    }

    public List<String> getColumnNames() {
        return columnNames;
    }

    public void setColumnNames(List<String> columnNames) {
        this.columnNames = columnNames;
    }

    public String getGeometryType() {
        return geometryType;
    }

    public void setGeometryType(String geometryType) {
        this.geometryType = geometryType;
    }

    public List<Double> getLevels() {
        return levels;
    }

    public void setLevels(List<Double> levels) {
        this.levels = levels;
    }

}
