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
package com.raytheon.uf.viz.monitor.scan;

/**
 * The ScanThreshold class
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 20, 2009 2037       dhladky    Initial creation.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class ScanThreshold {

	/** field name **/
	public Enum<?> fieldName = null;
	/** table name **/
	public Enum<?> tableName = null;
	/** high threshold **/
	public Double high = 0.0;
	/** mid threshold **/
	public Double mid = 0.0;
	/** low threshold **/
	public Double low = 0.0;
	
	public ScanThreshold(Enum<?> tableName, Enum<?> fieldName, Double high, Double mid, Double low) {
		this.fieldName = fieldName;
		this.tableName = tableName;
		this.high = high;
		this.mid = mid;
		this.low = low;
	}
	
	public String getFieldName() {
		return fieldName.name();
	}
	public void setFieldName(Enum<?> fieldName) {
		this.fieldName = fieldName;
	}
	public String getTableName() {
		return tableName.name();
	}
	public void setTableName(Enum<?> tableName) {
		this.tableName = tableName;
	}
	public Double getHigh() {
		return high;
	}
	public void setHigh(Double high) {
		this.high = high;
	}
	public Double getMid() {
		return mid;
	}
	public void setMid(Double mid) {
		this.mid = mid;
	}
	public Double getLow() {
		return low;
	}
	public void setLow(Double low) {
		this.low = low;
	}
}
