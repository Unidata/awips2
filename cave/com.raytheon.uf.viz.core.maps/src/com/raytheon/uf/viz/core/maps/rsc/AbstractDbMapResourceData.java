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

import java.util.Arrays;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 10, 2011            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public abstract class AbstractDbMapResourceData extends AbstractResourceData {

    @XmlAccessorType(XmlAccessType.NONE)
    public static class ColumnDefinition {
        @XmlAttribute(required = true)
        private String name;

        @XmlAttribute(required = false)
        private String expression;

        public ColumnDefinition() {
        }

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public String getExpression() {
            return expression;
        }

        public void setExpression(String expression) {
            this.expression = expression;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            String s = name;
            if (expression != null && !expression.isEmpty()) {
                s = "(" + expression + ") AS " + name;
            }
            return s;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result
                    + ((expression == null) ? 0 : expression.hashCode());
            result = prime * result + ((name == null) ? 0 : name.hashCode());
            return result;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            ColumnDefinition other = (ColumnDefinition) obj;
            if (expression == null) {
                if (other.expression != null) {
                    return false;
                }
            } else if (!expression.equals(other.expression)) {
                return false;
            }
            if (name == null) {
                if (other.name != null) {
                    return false;
                }
            } else if (!name.equals(other.name)) {
                return false;
            }
            return true;
        }

    }

    /** The human readable name */
    @XmlElement(required = true)
    private String mapName = null;

    @XmlElement(name = "table", required = true)
    private String table;

    @XmlElement(name = "column")
    private ColumnDefinition[] columns;

    @XmlElement(name = "constraint")
    private String[] constraints;

    @XmlElement()
    private String geomField = "the_geom";

    public AbstractDbMapResourceData() {
        super();
        this.nameGenerator = new AbstractNameGenerator() {

            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
                return mapName;
            }

        };
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
     * @return the constraints
     */
    public String[] getConstraints() {
        return constraints;
    }

    /**
     * @param constraints
     *            the constraints to set
     */
    public void setConstraints(String[] constraints) {
        this.constraints = constraints;
    }

    /**
     * @return the tables
     */
    public String getTable() {
        return table;
    }

    /**
     * @param tables
     *            the tables to set
     */
    public void setTable(String table) {
        this.table = table;
    }

    /**
     * @return the columns
     */
    public ColumnDefinition[] getColumns() {
        return columns;
    }

    /**
     * @param columns
     *            the columns to set
     */
    public void setColumns(ColumnDefinition[] columns) {
        this.columns = columns;
    }

    /**
     * @return the geomField
     */
    public String getGeomField() {
        return geomField;
    }

    /**
     * @param geomField
     *            the geomField to set
     */
    public void setGeomField(String geomField) {
        this.geomField = geomField;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return this.mapName;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        AbstractDbMapResourceData other = (AbstractDbMapResourceData) obj;
        if (!Arrays.equals(columns, other.columns)) {
            return false;
        }
        if (!Arrays.equals(constraints, other.constraints)) {
            return false;
        }
        if (geomField == null) {
            if (other.geomField != null) {
                return false;
            }
        } else if (!geomField.equals(other.geomField)) {
            return false;
        }
        if (mapName == null) {
            if (other.mapName != null) {
                return false;
            }
        } else if (!mapName.equals(other.mapName)) {
            return false;
        }
        if (table == null) {
            if (other.table != null) {
                return false;
            }
        } else if (!table.equals(other.table)) {
            return false;
        }
        return true;
    }

}
