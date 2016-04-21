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
package com.raytheon.viz.gfe.rsc;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlTransient;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.gfe.rsc.zones.ZoneDbResource;

/**
 * 
 * Resource data for Zones in the database.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Oct 28, 2013  2491     bsteffen    Add @XmlTransient
 * 
 * </pre>
 * 
 * @author wldougher
 * @version 1.0
 */
@XmlTransient
public class ZoneDbResourceData extends AbstractResourceData {

    public static final RGB DEFAULT_BACKGROUND_COLOR = new RGB(50, 50, 50);

    public static final RGB DEFAULT_CWA_COLOR = new RGB(184, 134, 11);

    public static final RGB DEFAULT_OUTLINE_COLOR = new RGB(0, 0, 0);

    public static final RGB DEFAULT_LABEL_COLOR = new RGB(200, 200, 200);

    public static final RGB DEFAULT_SELECTION_COLOR = new RGB(200, 0, 0);

    public static final String LABEL_KEY = "LABEL";

    public static final String CWA_KEY = "CWA";

    public static final String OUTLINE_KEY = "OUTLINE";

    public static final Integer NO_GROUP = Integer.valueOf(-1);

    public static final Integer SELECTION_GROUP = Integer.valueOf(0);

    /** The human readable name */
    @XmlElement(required = true)
    private String mapName = null;

    /** All the maps DB table names to take data from */
    @XmlElement(required = true)
    private List<String> allTables;

    /** The Maps DB table names for visible zones */
    @XmlElement(required = true)
    private List<String> tables;

    /**
     * Constraints (other than the bounding box) to apply to the queries. The
     * constraints are tied to the table to which they apply.
     */
    @XmlElement
    private Map<String, String[]> constraintsMap;

    /**
     * The columns (other than geometry) to query. The columns are tied to the
     * table they come from. The query columns for all tables must be
     * union-compatible.
     */
    @XmlElement
    private Map<String, String[]> queryColumnsMap;

    @XmlElement
    private Map<Object, RGB> colorMap;

    /**
     * Constructor.
     */
    public ZoneDbResourceData() {
        super();
        colorMap = new HashMap<Object, RGB>();
        this.setBackgroundColor(DEFAULT_BACKGROUND_COLOR);
        this.setCwaColor(DEFAULT_CWA_COLOR);
        this.setLabelColor(DEFAULT_LABEL_COLOR);
        this.setOutlineColor(DEFAULT_OUTLINE_COLOR);
        this.setSelectionColor(DEFAULT_SELECTION_COLOR);
        this.nameGenerator = new AbstractNameGenerator() {

            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
                return mapName;
            }

        };
        allTables = new ArrayList<String>();
        tables = new ArrayList<String>();
        constraintsMap = new HashMap<String, String[]>();
        queryColumnsMap = new HashMap<String, String[]>();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractResourceData#construct(com.raytheon
     * .uf.viz.core.comm.LoadProperties,
     * com.raytheon.uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public ZoneDbResource construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        return new ZoneDbResource(this, loadProperties);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractResourceData#update(java.lang.Object
     * )
     */
    @Override
    public void update(Object updateData) {
        // TODO Auto-generated method stub

    }

    /**
     * @return the mapName
     */
    public String getMapName() {
        return mapName;
    }

    /**
     * @param mapName
     *            the mapName to set
     */
    public void setMapName(String mapName) {
        this.mapName = mapName;
    }

    /**
     * @return the mainTable
     */
    public String getMainTable() {
        return tables.get(0);
    }

    /**
     * @param mainTable
     *            the mainTable to set
     */
    public void setMainTable(String mainTable) {
        tables.clear();
        addTable(mainTable);
    }

    /**
     * @return the constraints
     */
    public String[] getConstraints() {
        String mainTable = getMainTable();
        return constraintsMap.get(mainTable);
    }

    /**
     * @param table
     * @return constraints
     */
    public String[] getConstraints(String table) {
        return constraintsMap.get(table);
    }

    /**
     * @param constraints
     *            the constraints to set
     */
    public void setConstraints(String[] constraints) {
        String mainTable = getMainTable();
        constraintsMap.put(mainTable, constraints);
    }

    /**
     * @param table
     * @param constraints
     */
    public void setConstraints(String table, String[] constraints) {
        constraintsMap.put(table, constraints);
    }

    /**
     * @return the zoneEdgeColor
     */
    public RGB getOutlineColor() {
        RGB outlineColor = colorMap.get(OUTLINE_KEY);
        if (outlineColor == null) {
            outlineColor = DEFAULT_OUTLINE_COLOR;
            setOutlineColor(outlineColor);
        }
        return outlineColor;
    }

    /**
     * @param outlineColor
     *            the outlineColor to set
     */
    public void setOutlineColor(RGB outlineColor) {
        colorMap.put(OUTLINE_KEY, outlineColor);
    }

    /**
     * @return the cwaColor
     */
    public RGB getCwaColor() {
        RGB cwaColor = colorMap.get(CWA_KEY);
        if (cwaColor == null) {
            cwaColor = DEFAULT_CWA_COLOR;
            setCwaColor(cwaColor);
        }
        return cwaColor;
    }

    /**
     * @param cwaColor
     *            the cwaColor to set
     */
    public void setCwaColor(RGB cwaColor) {
        colorMap.put(CWA_KEY, cwaColor);
    }

    /**
     * @return the backgroundColor
     */
    public RGB getBackgroundColor() {
        RGB backgroundColor = colorMap.get(NO_GROUP);
        if (backgroundColor == null) {
            backgroundColor = DEFAULT_BACKGROUND_COLOR;
            setBackgroundColor(backgroundColor);
        }
        return backgroundColor;
    }

    /**
     * @param backgroundColor
     *            the backgroundColor to set
     */
    public void setBackgroundColor(RGB backgroundColor) {
        colorMap.put(NO_GROUP, backgroundColor);
    }

    /**
     * @return the labelColor
     */
    public RGB getLabelColor() {
        RGB labelColor = colorMap.get(LABEL_KEY);
        if (labelColor == null) {
            labelColor = DEFAULT_LABEL_COLOR;
            setLabelColor(labelColor);
        }
        return labelColor;
    }

    /**
     * @param labelColor
     *            the labelColor to set
     */
    public void setLabelColor(RGB labelColor) {
        colorMap.put(LABEL_KEY, labelColor);
    }

    /**
     * @return the selectionColor
     */
    public RGB getSelectionColor() {

        RGB selectionColor = colorMap.get(SELECTION_GROUP);
        if (selectionColor == null) {
            selectionColor = DEFAULT_SELECTION_COLOR;
            setSelectionColor(selectionColor);
        }
        return selectionColor;
    }

    /**
     * @param selectionColor
     *            the selectionColor to set
     */
    public void setSelectionColor(RGB selectionColor) {
        colorMap.put(SELECTION_GROUP, selectionColor);
    }

    /**
     * @return the tables
     */
    public List<String> getTables() {
        return tables;
    }

    /**
     * @param tables
     *            the tables to set
     */
    public void setTables(List<String> tables) {
        this.tables = tables;
        for (String table : tables) {
            if (!allTables.contains(table)) {
                allTables.add(table);
            }
        }
    }

    /**
     * Clear the entire list of visible tables
     */
    public void clearTables() {
        tables.clear();
    }

    /**
     * Clear all the queried tables, visible and hidden.
     */
    public void clearAllTables() {
        allTables.clear();
        clearTables();
    }

    /**
     * Add a table to the list of visible tables.
     * 
     * @param table
     *            The table to add
     */
    public void addTable(String table) {
        tables.add(table);
        if (!allTables.contains(table)) {
            allTables.add(table);
        }
    }

    /**
     * Remove a table from the list of shown tables.
     * 
     * @param table
     */
    public void removeTable(String table) {
        tables.remove(table);
    }

    /**
     * @return the constraintsMap
     */
    public Map<String, String[]> getConstraintsMap() {
        return constraintsMap;
    }

    /**
     * @param constraintsMap
     *            the constraintsMap to set
     */
    public void setConstraintsMap(Map<String, String[]> constraintsMap) {
        this.constraintsMap = constraintsMap;
    }

    /**
     * @param queryColumnsMap
     *            the queryColumnsMap to set
     */
    public void setQueryColumnsMap(Map<String, String[]> queryColumnsMap) {
        this.queryColumnsMap = queryColumnsMap;
    }

    /**
     * @return the queryColumnsMap
     */
    public Map<String, String[]> getQueryColumnsMap() {
        return queryColumnsMap;
    }

    /**
     * @param table
     * @return
     */
    public String[] getQueryColumns(String table) {
        return queryColumnsMap.get(table);
    }

    /**
     * @param table
     * @param queryColumns
     */
    public void setQueryColumns(String table, String[] queryColumns) {
        queryColumnsMap.put(table, queryColumns);
    }

    /**
     * Some applications (GHG Monitor, for one) need to query the database for
     * zone geometry from more tables than those shown on the map.
     * 
     * @return the allTables
     */
    public List<String> getAllTables() {
        return allTables;
    }

    /**
     * @param allTables
     *            the allTables to set
     */
    public void setAllTables(List<String> allTables) {
        this.allTables = allTables;
    }

    /**
     * 
     * @return
     */
    public Map<Object, RGB> getColorMap() {
        return colorMap;
    }

    /**
     * Set the color map.
     * 
     * @param colorMap
     *            The color map to set.
     */
    public void setColorMap(Map<Object, RGB> colorMap) {
        this.colorMap = colorMap;
        // Make sure hidden colors are present
        getBackgroundColor();
        getOutlineColor();
        getCwaColor();
        getLabelColor();
        getSelectionColor();
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || obj instanceof ZoneDbResourceData == false) {
            return false;
        }
        ZoneDbResourceData other = (ZoneDbResourceData) obj;

        if (this.tables != null && other.tables == null) {
            return false;
        } else if (this.tables == null && other.tables != null) {
            return false;
        } else if (this.tables != null
                && this.tables.equals(other.tables) == false) {
            return false;
        }

        if (this.queryColumnsMap != null && other.queryColumnsMap == null) {
            return false;
        } else if (this.queryColumnsMap == null
                && other.queryColumnsMap != null) {
            return false;
        } else if (this.queryColumnsMap != null
                && this.queryColumnsMap.equals(other.queryColumnsMap) == false) {
            return false;
        }

        if (this.mapName != null && other.mapName == null) {
            return false;
        } else if (this.mapName == null && other.mapName != null) {
            return false;
        } else if (this.mapName != null
                && this.mapName.equals(other.mapName) == false) {
            return false;
        }

        if (this.constraintsMap != null && other.constraintsMap == null) {
            return false;
        } else if (this.constraintsMap == null && other.constraintsMap != null) {
            return false;
        } else if (this.constraintsMap != null
                && this.constraintsMap.equals(other.constraintsMap) == false) {
            return false;
        }

        if (this.colorMap != null && other.colorMap == null) {
            return false;
        } else if (this.colorMap == null && other.colorMap != null) {
            return false;
        } else if (this.colorMap != null
                && this.colorMap.equals(other.colorMap) == false) {
            return false;
        }

        if (this.allTables != null && other.allTables == null) {
            return false;
        } else if (this.allTables == null && other.allTables != null) {
            return false;
        } else if (this.allTables != null
                && this.allTables.equals(other.allTables) == false) {
            return false;
        }

        return true;
    }
}
