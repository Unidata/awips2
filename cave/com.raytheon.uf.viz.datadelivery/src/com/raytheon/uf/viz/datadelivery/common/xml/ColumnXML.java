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
package com.raytheon.uf.viz.datadelivery.common.xml;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Subscription Manager Column Element Attributes.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 10, 2012            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
public class ColumnXML implements ISerializableObject {

    @XmlAttribute(name = "name")
    protected String name;

    @XmlAttribute(name = "visible")
    protected boolean visible = true;

    @XmlAttribute(name = "sortColumn")
    protected boolean sortColumn = false;

    @XmlAttribute(name = "sortAsc")
    protected boolean sortAsc = true;


    /**
     * Constructor
     */
    public ColumnXML() {

    }

    /**
     * Constructor.
     * 
     * @param name
     *          Column Name
     * @param visible
     *          Column visible flag
     */
    public ColumnXML(String name, boolean visible) {
        this.name = name;
        this.visible = visible;
    }

    /**
     * Get the column name.
     * 
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * Set Column Name.
     * 
     * @param name
     *            the name to set
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Visible flag.
     * 
     * @return the visible
     */
    public boolean isVisible() {
        return visible;
    }

    /**
     * Set the visible flag.
     * 
     * @param visible
     *            the visible to set
     */
    public void setVisible(boolean visible) {
        this.visible = visible;
    }

    /**
     * Sort Column flag
     * 
     * @return boolean
     */
    public boolean isSortColumn() {
        return sortColumn;
    }

    
    /**
     * Set the sort column.
     * 
     * @param sortColumn
     *            sortColumn flag
     */
    public void setSortColumn(boolean sortColumn) {
        this.sortColumn = sortColumn;
    }

    
    /**
     * Sort ascending flag.
     * 
     * @return sort flag
     */
    public boolean isSortAsc() {
        return sortAsc;
    }

    /**
     * Set the sort ascending flag.
     * 
     * @param sortAsc flag
     */
    public void setSortAsc(boolean sortAsc) {
        this.sortAsc = sortAsc;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        String s = "Name: " + name + ", Visible: " + visible + "\nSortColumn: + " + sortColumn + ", Sort Ascending: "
                + sortAsc;
        return s;
    }
}
