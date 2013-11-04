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
package com.raytheon.uf.edex.wcs.reg;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class RangeFieldDefinition {

	public enum Closure {
		open, closed, closedOpen, openClosed
	};

	protected double maxValue;

	protected double minValue;

	protected Closure closure;

	protected String units;

	/**
	 * @param maxValue
	 * @param minValue
	 */
	public RangeFieldDefinition(double maxValue, double minValue) {
		this(maxValue, minValue, Closure.closed);
	}

	/**
	 * @param maxValue
	 * @param minValue
	 * @param closure
	 */
	public RangeFieldDefinition(double maxValue, double minValue,
			Closure closure) {
		super();
		this.maxValue = maxValue;
		this.minValue = minValue;
		this.closure = closure;
	}

	public String getUnits() {
		return units;
	}

	public void setUnits(String units) {
		this.units = units;
	}

	public double getMaxValue() {
		return maxValue;
	}

	public void setMaxValue(double maxValue) {
		this.maxValue = maxValue;
	}

	public double getMinValue() {
		return minValue;
	}

	public void setMinValue(double minValue) {
		this.minValue = minValue;
	}

	public Closure getClosure() {
		return closure;
	}

	public void setClosure(Closure closure) {
		this.closure = closure;
	}

}
