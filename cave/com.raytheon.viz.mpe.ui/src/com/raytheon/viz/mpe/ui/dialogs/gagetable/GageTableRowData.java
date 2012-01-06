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
package com.raytheon.viz.mpe.ui.dialogs.gagetable;

import java.util.Map;

import com.raytheon.viz.mpe.core.MPEDataManager.MPEGageData;

/**
 * Holds the row data for the Gage Table dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 10, 2009 2476       mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class GageTableRowData {
    /**
     * The edited value.
     */
    private double editValue;

    /**
     * The original gage value.
     */
    private double originalGageValue;

    /**
     * HashMap of grid names to products.
     */
    private Map<String, Double> productValueMap = null;
    
    /**
     * The MPEGageData object for this row.
     */
    private MPEGageData gageData = null;
    
    /**
     * Has the value been edited?
     */
    private boolean valueEdited = false;

    public GageTableRowData(double editValue, Map<String, Double> productValueMap,
             MPEGageData gageData) {
        this.editValue = editValue;
        this.productValueMap = productValueMap;
        this.gageData = gageData;
        originalGageValue = gageData.getGval();
    }

    /**
     * @return the originalGageValue
     */
    public double getOriginalGageValue() {
        return originalGageValue;
    }

    /**
     * @param originalGageValue
     *            the originalGageValue to set
     */
    public void setOriginalGageValue(double originalGageValue) {
        this.originalGageValue = originalGageValue;
    }

    /**
     * @return the productValueMap
     */
    public Map<String, Double> getProductValueMap() {
        return productValueMap;
    }

    /**
     * @param productValueMap
     *            the productValueMap to set
     */
    public void setProductValueMap(Map<String, Double> productValueMap) {
        this.productValueMap = productValueMap;
    }

    /**
     * @return the editValue
     */
    public double getEditValue() {
        return editValue;
    }

    /**
     * @param editValue
     *            the editValue to set
     */
    public void setEditValue(double editValue) {
        this.editValue = editValue;
    }

    /**
     * @return the gageData
     */
    public MPEGageData getGageData() {
        return gageData;
    }

    /**
     * @param gageData the gageData to set
     */
    public void setGageData(MPEGageData gageData) {
        this.gageData = gageData;
    }

    /**
     * @return the valueEdited
     */
    public boolean isValueEdited() {
        return valueEdited;
    }

    /**
     * @param valueEdited the valueEdited to set
     */
    public void setValueEdited(boolean valueEdited) {
        this.valueEdited = valueEdited;
    }

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		long temp;
		temp = Double.doubleToLongBits(editValue);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		result = prime * result
				+ ((gageData == null) ? 0 : gageData.hashCode());
		temp = Double.doubleToLongBits(originalGageValue);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		result = prime * result
				+ ((productValueMap == null) ? 0 : productValueMap.hashCode());
		result = prime * result + (valueEdited ? 1231 : 1237);
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		GageTableRowData other = (GageTableRowData) obj;
		if (Double.doubleToLongBits(editValue) != Double
				.doubleToLongBits(other.editValue))
			return false;
		if (gageData == null) {
			if (other.gageData != null)
				return false;
		} else if (!gageData.equals(other.gageData))
			return false;
		if (Double.doubleToLongBits(originalGageValue) != Double
				.doubleToLongBits(other.originalGageValue))
			return false;
		if (productValueMap == null) {
			if (other.productValueMap != null)
				return false;
		} else if (!productValueMap.equals(other.productValueMap))
			return false;
		if (valueEdited != other.valueEdited)
			return false;
		return true;
	}

}
