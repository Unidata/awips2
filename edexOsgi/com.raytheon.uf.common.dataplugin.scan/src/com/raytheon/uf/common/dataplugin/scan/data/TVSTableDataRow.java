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
package com.raytheon.uf.common.dataplugin.scan.data;

import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.DataTime;

/**
 * 
 * SCAN TVS Table Data Row
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * April 29, 2009   2037    dhladky     Initial creation
 * 02/01/13     1569        D. Hladky   removed XML where not needed
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 * 
 */

@DynamicSerialize
public class TVSTableDataRow extends ScanTableDataRow {

	/**
     * 
     */
    private static final long serialVersionUID = 1L;

    public TVSTableDataRow(DataTime time) {
		super(time);
	}
	
	public TVSTableDataRow() {
	    
	}
	
	/** Storm ID **/
	@DynamicSerializeElement
	public String strmID = null;
	/** DMD Ident **/
	@DynamicSerializeElement
	public String DMDIdent = null;
	/** type of TVS **/
	@DynamicSerializeElement
	public String type = TVS_TYPE.NONE.getTVSName();
	/** average delta velocity **/
	@DynamicSerializeElement
	public Double avgDv = 0.0;
	/** average low level delta velocity **/
	@DynamicSerializeElement
	public Double llDV = 0.0;
	/** maximum delta velocity **/
	@DynamicSerializeElement
    @XmlElement
	public Double maxDV = 0.0;
	/** maximum delta velocity height kft **/
	@DynamicSerializeElement
	public Double maxDvHt = 0.0;
	/** base height kft **/
	@DynamicSerializeElement
	public Double base = 0.0;
	/** depth of circulation kft **/
	@DynamicSerializeElement
	public Double depth = 0.0;
	/** top of circulation kft **/
	@DynamicSerializeElement
	public Double top = 0.0;
	/** shear value m/s/km **/
	@DynamicSerializeElement
	public Double shear = 0.0;
	/** height of maximum shear kft **/
	@DynamicSerializeElement
	public Double shrHt = 0.0;
	
	/**
	 * Gets the StormID
	 * @return
	 */
	public String getStrmID() {
		return strmID;
	}
	/**
	 * Sets the StormID
	 * @param type
	 */
	public void setStrmID(String strmID) {
		this.strmID = strmID;
	}
	
	/**
	 * Gets the key for the DMD table
	 * @return
	 */
	public String getDMDIdent() {
		return DMDIdent;
	}
	/**
	 * Sets the key for the DMD table
	 * @param type
	 */
	public void setDMDIdent(String DMDIdent) {
		this.DMDIdent = DMDIdent;
	}
	/**
	 * Gets the type of TVS
	 * @return
	 */
	public String getType() {
		return type;
	}
	/**
	 * Sets the type of TVS
	 * @param type
	 */
	public void setType(String type) {
		this.type = type;
	}
	/**
	 * Get the average Delta Velocity
	 * @return
	 */
	public Double getAvgDv() {
		return avgDv;
	}
	/**
	 * Set the average delta velocity
	 * @param avgDv
	 */
	public void setAvgDv(Double avgDv) {
		this.avgDv = avgDv;
	}
	/**
	 * Get the low level delta velocity
	 * @return
	 */
	public Double getLlDV() {
		return llDV;
	}
	/**
	 * set the low level delta velocity
	 * @param llDV
	 */
	public void setLlDV(Double llDV) {
		this.llDV = llDV;
	}
	/**
	 * get the max delta velocity
	 * @return
	 */
	public Double getMaxDV() {
		return maxDV;
	}
	/**
	 * set the max delta velocity
	 * @param maxDV
	 */
	public void setMaxDV(Double maxDV) {
		this.maxDV = maxDV;
	}
	/**
	 * get the max delta velocity height
	 * @return
	 */
	public Double getMaxDvHt() {
		return maxDvHt;
	}
	/**
	 * set the max delta velocity height
	 * @param maxDvHt
	 */
	public void setMaxDvHt(Double maxDvHt) {
		this.maxDvHt = maxDvHt;
	}
	/**
	 * Get the height of the base
	 * @return
	 */
	public Double getBase() {
		return base;
	}
	/**
	 * Set the base height
	 * @param base
	 */
	public void setBase(Double base) {
		this.base = base;
	}
	/**
	 * get the base depth in kft
	 * @return
	 */
	public Double getDepth() {
		return depth;
	}
	/**
	 * Set the base depth in kft
	 * @param depth
	 */
	public void setDepth(Double depth) {
		this.depth = depth;
	}
	/**
	 * Get the top of the TVS in kft
	 * @return
	 */
	public Double getTop() {
		return top;
	}
	/**
	 * Set the top of the TVS height in kft
	 * @param top
	 */
	public void setTop(Double top) {
		this.top = top;
	}
	/**
	 * Get the value for shear
	 * @return
	 */
	public Double getShear() {
		return shear;
	}
	/**
	 * set the value for shear
	 * @param shear
	 */
	public void setShear(Double shear) {
		this.shear = shear;
	}
	/**
	 * get the shear value height in kft
	 * @return
	 */
	public Double getShrHt() {
		return shrHt;
	}
	/**
	 * Set the shear height in kft
	 * @param shrHt
	 */
	public void setShrHt(Double shrHt) {
		this.shrHt = shrHt;
	}
	@Override
    public ScanTableDataRow copy() {
        TVSTableDataRow row = new TVSTableDataRow(this.getTime());
        row = (TVSTableDataRow) copyCommon(row);
        row.setStrmID(this.getStrmID());
        row.setDMDIdent(this.getDMDIdent());
        row.setType(this.getType());
        row.setAvgDv(this.getAvgDv());
        row.setLlDV(this.getLlDV());
        row.setMaxDV(this.getMaxDV());
        row.setMaxDvHt(this.getMaxDvHt());
        row.setBase(this.getBase());
        row.setDepth(this.getDepth());
        row.setTop(this.getTop());
        row.setShear(this.getShear());
        row.setShrHt(this.getShrHt());
        return row;
    }
    @Override
    public String toString() {
        StringBuffer buf = new StringBuffer();
        buf.append("TVS ID: " + getIdent() + "\n");
        if (getStrmID() != null) {
            buf.append("Storm ID: " + getStrmID() + "\n");
        }
        if (getType() != null) {
            buf.append("Type: " + getType() + "\n");
        }
        if (getAvgDv() > 0) {
            buf.append("Average Delta Velocity: " + getAvgDv() + "\n");
        }
        if (getLlDV() > 0) {
            buf.append("Low Level Depth: " + getLlDV() + "\n");
        }
        if (getMaxDV() > 0) {
            buf.append("Max Depth: " + getMaxDV() + "\n");
        }
        if (getMaxDvHt() > 0) {
            buf.append("Max Delta Velocity Hgt: " + getMaxDvHt() + "\n");
        }
        if (getBase() > 0) {
            buf.append("Base: " + getBase() + "\n");
        }
        if (getDepth() > 0) {
            buf.append("Depth: " + getDepth() + "\n");
        }
        if (getTop() > 0) {
            buf.append("Top: " + getTop() + "\n");
        }
        if (getShear() > 0) {
            buf.append("Shear: " + getShear() + "\n");
        }
        if (getShrHt() > 0) {
            buf.append("Shear Hgt: " + getShrHt() + "\n");
        }
        
        return buf.toString();
    }
    @Override
    public Double getValue(String field) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void clear() {
        // TODO Auto-generated method stub
        
    }
}
