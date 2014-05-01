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
package com.raytheon.uf.common.dataplugin.text.alarms;

import java.util.Date;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * POJO for storing each alarm product for easy storage and retrieval for
 * alarm/alert functionality
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 29, 2009            mnash     Initial creation
 * 07Jul2010    2187       cjeanbap    Added operational mode flag.
 * May 23, 2012 14952      rferrel     Added refTime.
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

@DynamicSerialize
public class AlarmAlertProduct implements ISerializableObject {

    public enum ProductType {
        Alarm_Alert, Proximity_Alarm
    }

    @DynamicSerializeElement
    private ProductType productType;

    @DynamicSerializeElement
    private String alarmType = "None";

    @DynamicSerializeElement
    private String productId = "";

    @DynamicSerializeElement
    private String actionCmd = "";

    @DynamicSerializeElement
    private boolean aor = false;

    @DynamicSerializeElement
    private String aorDistance = "";

    @DynamicSerializeElement
    private String ugcList = "";

    @DynamicSerializeElement
    private String searchString = "";

    @DynamicSerializeElement
    private String aorLabel = "";

    @DynamicSerializeElement
    private boolean alarm = false;

    @DynamicSerializeElement
    private Date dateReceived = new Date();

    @DynamicSerializeElement
    private boolean operationalMode = true;

    @DynamicSerializeElement
    private long refTime;

    public AlarmAlertProduct() {
    }

    public AlarmAlertProduct(boolean operationalMode) {
        this.operationalMode = operationalMode;
    }

    /**
     * @return the productType
     */
    public ProductType getProductType() {
        return productType;
    }

    /**
     * @param productType
     *            the productType to set
     */
    public void setProductType(ProductType productType) {
        this.productType = productType;
    }

    /**
     * @return the alarmType
     */
    public String getAlarmType() {
        return alarmType;
    }

    /**
     * @param alarmType
     *            the alarmType to set
     */
    public void setAlarmType(String alarmType) {
        this.alarmType = alarmType;
    }

    /**
     * @return the productId
     */
    public String getProductId() {
        return productId;
    }

    /**
     * @param productId
     *            the productId to set
     */
    public void setProductId(String productId) {
        this.productId = productId;
    }

    /**
     * @return the actionCmd
     */
    public String getActionCmd() {
        return actionCmd;
    }

    /**
     * @param actionCmd
     *            the actionCmd to set
     */
    public void setActionCmd(String actionCmd) {
        this.actionCmd = actionCmd;
    }

    /**
     * @return the isAor
     */
    public boolean isAor() {
        return aor;
    }

    /**
     * @param isAor
     *            the isAor to set
     */
    public void setAor(boolean isAor) {
        this.aor = isAor;
    }

    /**
     * @return the aorDistance
     */
    public String getAorDistance() {
        return aorDistance;
    }

    /**
     * @param aorDistance
     *            the aorDistance to set
     */
    public void setAorDistance(String aorDistance) {
        this.aorDistance = aorDistance;
    }

    /**
     * @return the ugcList
     */
    public String getUgcList() {
        return ugcList;
    }

    /**
     * @param ugcList
     *            the ugcList to set
     */
    public void setUgcList(String ugcList) {
        this.ugcList = ugcList;
    }

    /**
     * @return the searchString
     */
    public String getSearchString() {
        return searchString;
    }

    /**
     * @param searchString
     *            the searchString to set
     */
    public void setSearchString(String searchString) {
        this.searchString = searchString;
    }

    /**
     * @return the aorLabel
     */
    public String getAorLabel() {
        return aorLabel;
    }

    /**
     * @param aorLabel
     *            the aorLabel to set
     */
    public void setAorLabel(String aorLabel) {
        this.aorLabel = aorLabel;
    }

    /**
     * @return the alarm
     */
    public boolean isAlarm() {
        return alarm;
    }

    /**
     * @param alarm
     *            the alarm to set
     */
    public void setAlarm(boolean alarm) {
        this.alarm = alarm;
    }

    /**
     * @return the dateReceived
     */
    public Date getDateReceived() {
        return dateReceived;
    }

    /**
     * @param dateReceived
     *            the dateReceived to set
     */
    public void setDateReceived(Date dateReceived) {
        this.dateReceived = dateReceived;
    }

    public void setDateReceived(String dateReceived) {
        Date d = new Date();
        try {
            d.setTime(Long.parseLong(dateReceived));
        } catch (NumberFormatException e) {
            e.printStackTrace();
        }
        this.dateReceived = d;
    }

    @Override
    public String toString() {
        String string = getProductId();
        return string;
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
                + ((actionCmd == null) ? 0 : actionCmd.hashCode());
        result = prime * result + (alarm ? 1231 : 1237);
        result = prime * result
                + ((alarmType == null) ? 0 : alarmType.hashCode());
        result = prime * result
                + ((aorDistance == null) ? 0 : aorDistance.hashCode());
        result = prime * result
                + ((aorLabel == null) ? 0 : aorLabel.hashCode());
        result = prime * result + (aor ? 1231 : 1237);
        result = prime * result
                + ((productId == null) ? 0 : productId.hashCode());
        result = prime * result
                + ((productType == null) ? 0 : productType.hashCode());
        result = prime * result
                + ((searchString == null) ? 0 : searchString.hashCode());
        result = prime * result + ((ugcList == null) ? 0 : ugcList.hashCode());
        result = prime * result + (operationalMode ? 1231 : 1237);
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        AlarmAlertProduct other = (AlarmAlertProduct) obj;
        if (actionCmd == null) {
            if (other.actionCmd != null)
                return false;
        } else if (!actionCmd.equals(other.actionCmd))
            return false;
        if (alarm != other.alarm)
            return false;
        if (alarmType == null) {
            if (other.alarmType != null)
                return false;
        } else if (!alarmType.equals(other.alarmType))
            return false;
        if (aorDistance == null) {
            if (other.aorDistance != null)
                return false;
        } else if (!aorDistance.equals(other.aorDistance))
            return false;
        if (aorLabel == null) {
            if (other.aorLabel != null)
                return false;
        } else if (!aorLabel.equals(other.aorLabel))
            return false;
        if (aor != other.aor)
            return false;
        if (productId == null) {
            if (other.productId != null)
                return false;
        } else if (!productId.equals(other.productId))
            return false;
        if (productType == null) {
            if (other.productType != null)
                return false;
        } else if (!productType.equals(other.productType))
            return false;
        if (searchString == null) {
            if (other.searchString != null)
                return false;
        } else if (!searchString.equals(other.searchString))
            return false;
        if (ugcList == null) {
            if (other.ugcList != null)
                return false;
        } else if (!ugcList.equals(other.ugcList))
            return false;
        if (operationalMode != other.operationalMode)
            return false;
        return true;
    }

    public boolean getOperationalMode() {
        return operationalMode;
    }

    public void setOperationalMode(boolean operationalMode) {
        this.operationalMode = operationalMode;
    }

    public long getRefTime() {
        return refTime;
    }

    public void setRefTime(long refTime) {
        this.refTime = refTime;
    }
}
