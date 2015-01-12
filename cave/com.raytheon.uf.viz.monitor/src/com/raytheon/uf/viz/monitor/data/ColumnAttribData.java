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
package com.raytheon.uf.viz.monitor.data;

import com.raytheon.uf.viz.monitor.config.CommonTableConfig.GraphType;
import com.raytheon.uf.viz.monitor.config.CommonTableConfig.SortDirection;

/**
 * Attributes for a table column.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 6, 2009            lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class ColumnAttribData {
    /**
     * Name.
     */
    private String name = "";

    /**
     * Column name to be displayed in the table column header.
     */
    private String columnName = "";

    /**
     * Column name that is created by using an identifier to split the name.
     */
    private String splitColumnName = "";

    private String columnNameWithSpace = "";

    private String originalName;

    /**
     * Sort direction.
     */
    private SortDirection sortDir = SortDirection.None;

    /**
     * Reverse sort flag.
     */
    private boolean reverseFilter = false;

    /**
     * Graph type.
     */
    private GraphType graphType = GraphType.None;

    /**
     * Constructor.
     * 
     * @param name
     *            Name.
     * @param sortDir
     *            Sort direction.
     */
    public ColumnAttribData(String name, SortDirection sortDir) {
        this.name = name.trim();
        this.sortDir = sortDir;

        originalName = name;
        columnName = this.name.replace(' ', '\n');
    }

    /**
     * Constructor.
     * 
     * @param name
     *            Name.
     * @param sortDir
     *            Sort direction.
     * @param grType
     *            Graph type.
     */
    public ColumnAttribData(String name, SortDirection sortDir, GraphType grType) {
        this.name = name.trim();
        this.sortDir = sortDir;
        this.graphType = grType;

        columnName = this.name.replace(' ', '\n');
    }

    /**
     * Constructor.
     * 
     * @param name
     *            Name.
     * @param colName
     *            Column display name.
     * @param sortDir
     *            Sort direction.
     */
    public ColumnAttribData(String name, String colName, SortDirection sortDir) {
        this.name = name.trim();
        this.columnName = colName;
        this.sortDir = sortDir;
    }

    /**
     * Constructor.
     * 
     * @param name
     *            Name.
     * @param colName
     *            Column display name.
     * @param sortDir
     *            Sort direction.
     * @param grType
     */
    public ColumnAttribData(String name, String colName, SortDirection sortDir,
            GraphType grType) {
        this.name = name.trim();
        this.columnName = colName;
        this.sortDir = sortDir;
        this.graphType = grType;
    }

    /**
     * Constructor.
     * 
     * @param name
     *            Name
     * @param sortDir
     *            Sort direction of the column
     * @param reverseFilter
     *            Reverse filter flag
     */
    public ColumnAttribData(String name, SortDirection sortDir,
            boolean reverseFilter, String splitStr) {
        this.name = name.trim();
        this.sortDir = sortDir;
        this.reverseFilter = reverseFilter;

        originalName = name;
        String tmpName = this.name.replaceAll(splitStr, " ");
        columnName = tmpName.replaceAll(" ", "\n");

        // Create the split column name
        splitColumnName = this.name.replaceAll(splitStr, "\n");

        columnNameWithSpace = tmpName;
    }

    /**
     * Constructor.
     * 
     * @param name
     *            Name.
     * @param colName
     *            Column display name.
     */
    public ColumnAttribData(String name, String colName) {
        this.name = name.trim();
        this.columnName = colName;
    }

    /**
     * Get the name.
     * 
     * @return The name.
     */
    public String getName() {
        return name;
    }

    public String getOriginalName() {
        return originalName;
    }

    /**
     * Get the column display name.
     * 
     * @return The column display name.
     */
    public String getColumnName() {
        return columnName;
    }

    /**
     * This returns the column name that has been created by using a split
     * identifier to break apart the string.
     * 
     * @return The split column name.
     */
    public String getSplitColumnName() {
        return splitColumnName;
    }

    public String getColumnNameWithSpace() {
        return columnNameWithSpace;
    }

    /**
     * Get the sort direction.
     * 
     * @return The sort direction.
     */
    public int getSortDir() {
        return sortDir.getSortDir();
    }

    /**
     * Get the graph type.
     * 
     * @return The graph type.
     */
    public GraphType getGraphType() {
        return graphType;
    }

    /**
     * @return the reverseFilter
     */
    public boolean isReverseFilter() {
        return reverseFilter;
    }

    /**
     * @param reverseFilter
     *            the reverseFilter to set
     */
    public void setReverseFilter(boolean reverseFilter) {
        this.reverseFilter = reverseFilter;
    }
}
