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
package com.raytheon.uf.edex.decodertools.core;

/**
 * The DataItem class is used to hold intermediate information especially when
 * decoded data may imply several attributes (time period) besides the item name
 * value. The class is coded in JavaBean style.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20070925            391 jkorman     Initial Coding.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class DataItem {

    private Double dataValue = null;

    private String dataName = null;

    private Integer decodeGroup = null;

    private String dataUnits = null;

    private Integer dataPeriod = null;

    /**
     * Construct a data item with units information only.
     * 
     * @param units
     *            The units for this data item.
     */
    public DataItem(String units) {
        dataUnits = units;
    }

    /**
     * Construct a data item with units and name information.
     * 
     * @param units
     *            The units for this data item.
     * @param name
     *            The name of this data item.
     */
    public DataItem(String units, String name) {
        this(units);
        dataName = name;
    }

    /**
     * Construct a data item with units, name, group information.
     * 
     * @param units
     *            The units for this data item.
     * @param name
     *            The name of this data item.
     * @param group
     *            The decoder group where the data was found.
     * 
     */
    public DataItem(String units, String name, int group) {
        this(units, name);
        decodeGroup = group;
    }

    /**
     * Get the value of this data item.
     * 
     * @return The data item value.
     */
    public Double getDataValue() {
        return dataValue;
    }

    /**
     * Set the value of this data item.
     * 
     * @param dataValue
     *            The data item value.
     */
    public void setDataValue(Double dataValue) {
        this.dataValue = dataValue;
    }

    /**
     * Get the name of this data item.
     * 
     * @return The data item name.
     */
    public String getDataName() {
        return dataName;
    }

    /**
     * Set the name of this data item.
     * 
     * @param dataName
     *            The name of this data item.
     */
    public void setDataName(String dataName) {
        this.dataName = dataName;
    }

    /**
     * Get the group this data item was found in.
     * 
     * @return The decode group.
     */
    public Integer getDecodeGroup() {
        return decodeGroup;
    }

    /**
     * Set the group this data item was found in.
     * 
     * @param decodeGroup
     *            The decodeGroup to set.
     */
    public void setDecodeGroup(Integer decodeGroup) {
        this.decodeGroup = decodeGroup;
    }

    /**
     * Get the data units of this item.
     * 
     * @return The dataUnits.
     */
    public String getDataUnits() {
        return dataUnits;
    }

    /**
     * Set the data units as a string value.
     * 
     * @param dataUnits
     *            The dataUnits.
     */
    public void setDataUnits(String dataUnits) {
        this.dataUnits = dataUnits;
    }

    /**
     * Get the data period in seconds.
     * 
     * @return The dataPeriod in seconds.
     */
    public Integer getDataPeriod() {
        return dataPeriod;
    }

    /**
     * Set the data period (in seconds) of the observed data.
     * 
     * @param dataPeriod
     *            The period (in seconds) to set
     */
    public void setDataPeriod(Integer dataPeriod) {
        this.dataPeriod = dataPeriod;
    }

    /**
     * Create a string representation of this data item.
     * 
     * @return The string representation of this data item.
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(dataName);
        sb.append("=");
        sb.append(dataValue);
        return sb.toString();
    }

    /**
     * Get the value from this data item. If the internal value matches one of
     * the values in the ignore array, then the return value is set to null.
     * 
     * @param di
     *            DataItem to get value from.
     * @param ignore
     *            Array of values to ignore.
     * @return The data value.
     */
    public static final Double getValue(DataItem di, Double[] ignore) {
        Double retValue = null;
        if (di != null) {
            retValue = di.getDataValue();
            if (retValue != null) {
                // If the value equals any of the ignore values, set the return
                // value to null.
                for (int i = 0; i < ignore.length; i++) {
                    if (retValue.equals(ignore[i])) {
                        retValue = null;
                        break;
                    }
                }
            }
        }
        return retValue;
    }

}
