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
package com.raytheon.uf.viz.datadelivery.notification.xml;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.viz.datadelivery.common.xml.ColumnXML;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils.TABLE_TYPE;

/**
 * Notification XML Object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 31, 2012            mpduff     Initial creation.
 * Aug 29, 2012   1115     jpiatt     Set default sort column.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

@XmlRootElement(name = "NotificationColumns")
@XmlAccessorType(XmlAccessType.NONE)
public class NotificationConfigXML implements ISerializableObject {
    @XmlElements({ @XmlElement(name = "column", type = ColumnXML.class) })
    protected ArrayList<ColumnXML> columnList = new ArrayList<ColumnXML>();

    @XmlElement(name = "messageLoad", type = MessageLoadXML.class)
    protected MessageLoadXML messageLoad = new MessageLoadXML();

    @XmlElement(name = "prioritySetting", type = PrioritySettingXML.class)
    protected PrioritySettingXML prioritySetting = new PrioritySettingXML();

    @XmlElement(name = "paginationSetting", type = int.class)
    protected int paginationSetting = 20;

    /**
     * Constructor
     */
    public NotificationConfigXML() {
        createDefault();
    }

    /**
     * Get the list of table columns.
     * 
     * @return the columnList
     */
    public ArrayList<ColumnXML> getColumnList() {
        return columnList;
    }

    /**
     * Set the list of table columns
     * 
     * @param columnList
     *            the columnList to set
     */
    public void setColumnList(ArrayList<ColumnXML> columnList) {
        this.columnList = columnList;
    }

    /**
     * Clear the table columns.
     * 
     */
    public void clearColumns() {
        columnList.clear();
    }

    /**
     * Add to the list of table columns.
     * 
     * @param col
     *            Column
     */
    public void addColumn(ColumnXML col) {
        if (columnList == null) {
            columnList = new ArrayList<ColumnXML>();
        }

        columnList.add(col);
    }

    /**
     * Get message load.
     * 
     * @return the MessageLoadXML
     */
    public MessageLoadXML getMessageLoad() {
        return messageLoad;
    }

    /**
     * Set message load.
     * 
     * @param messageLoad
     *            message load
     */
    public void setMessageLoad(MessageLoadXML messageLoad) {
        this.messageLoad = messageLoad;
    }

    /**
     * Get the priority setting.
     * 
     * @return the prioritySetting
     */
    public PrioritySettingXML getPrioritySetting() {
        return prioritySetting;
    }

    /**
     * Set the priority.
     * 
     * @param prioritySetting
     *            The priority number
     */
    public void setPrioritySetting(PrioritySettingXML prioritySetting) {
        this.prioritySetting = prioritySetting;
    }

    /**
     * Get the number of rows configured per page.
     * 
     * @return the paginationSetting
     */
    public int getPaginationSetting() {
        return paginationSetting;
    }

    /**
     * Set the page number configuration.
     * 
     * @param paginationSetting
     *            The number of rows configured per page
     */
    public void setPaginationSetting(int paginationSetting) {
        this.paginationSetting = paginationSetting;
    }

    private void createDefault() {
        String[] titles = DataDeliveryUtils.getColumnTitles(TABLE_TYPE.NOTIFICATION);
        for (String title : titles) {
            ColumnXML col = new ColumnXML(title, true);
            if (title.equals(DataDeliveryUtils.NotifColumnNames.TIME.getColumnName())) {
                col.setSortColumn(true);
            }
            this.addColumn(col);
        }
        
    }
}
