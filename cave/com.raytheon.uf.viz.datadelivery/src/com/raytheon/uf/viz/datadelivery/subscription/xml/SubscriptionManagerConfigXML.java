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
package com.raytheon.uf.viz.datadelivery.subscription.xml;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.viz.datadelivery.common.ui.SortImages.SortDirection;
import com.raytheon.uf.viz.datadelivery.common.xml.ColumnXML;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils.TABLE_TYPE;

/**
 * Subscription Manager Configuration Element.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 10, 2012            mpduff     Initial creation
 * Jan 07, 2013 1437       bgonzale   Set default sort column and direction. Added
 *                                    getColumn(String), setSorColumn(String, SortDirection),
 *                                    and removeColumn(ColumnXML).
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@XmlRootElement(name = "SubscriptionManagerColumns")
@XmlAccessorType(XmlAccessType.NONE)
public class SubscriptionManagerConfigXML implements ISerializableObject {
    @XmlElements({ @XmlElement(name = "column", type = ColumnXML.class) })
    protected ArrayList<ColumnXML> columnList = new ArrayList<ColumnXML>();

    public SubscriptionManagerConfigXML() {
        createDefault();
    }

    /**
     * @return the columnList
     */
    public ArrayList<ColumnXML> getColumnList() {
        return columnList;
    }

    /**
     * @param columnList
     *            the columnList to set
     */
    public void setColumnList(ArrayList<ColumnXML> columnList) {
        this.columnList = columnList;
    }

    public void clearColumns() {
        columnList.clear();
    }

    public void addColumn(ColumnXML col) {
        if (columnList == null) {
            columnList = new ArrayList<ColumnXML>();
        }

        columnList.add(col);
    }

    /**
     * Remove the columnXML from the configuration.
     * 
     * @param columnXML
     */
    public void removeColumn(ColumnXML columnXML) {
        columnList.remove(columnXML);
    }

    private void createDefault() {
        String[] titles = DataDeliveryUtils.getColumnTitles(TABLE_TYPE.SUBSCRIPTION);
        for (String title : titles) {
            ColumnXML col = new ColumnXML(title, true);
            this.addColumn(col);
        }
        // set default sort column
        if (!columnList.isEmpty()) {
            columnList.get(0).setSortColumn(true);
            columnList.get(0).setSortAsc(true);
        }
    }

    /**
     * Get the column with the given columnName.
     * 
     * @param columnName
     * @return Column if found; null otherwise.
     */
    public ColumnXML getColumn(String columnName) {
        for (ColumnXML column : columnList) {
            if (column.getName().equals(columnName)) {
                return column;
            }
        }
        return null;
    }

    /**
     * Set the named column to the current sort column with the given direction.
     * 
     * @param columnName
     * @param sortDirection
     */
    public void setSortColumn(String columnName, SortDirection sortDirection) {
        boolean isSortAsc = SortDirection.ASCENDING.equals(sortDirection);
        for (ColumnXML column : columnList) {
            if (column.getName().equals(columnName)) {
                column.setSortColumn(true);
                column.setSortAsc(isSortAsc);
            } else {
                column.setSortColumn(false);
            }
        }
    }
}
