/*
 * The following software products were developed by Raytheon:
 *
 * ADE (AWIPS Development Environment) software
 * CAVE (Common AWIPS Visualization Environment) software
 * EDEX (Environmental Data Exchange) software
 * uFrame™ (Universal Framework) software
 *
 * Copyright (c) 2010 Raytheon Co.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/org/documents/epl-v10.php
 *
 *
 * Contractor Name: Raytheon Company
 * Contractor Address:
 * 6825 Pine Street, Suite 340
 * Mail Stop B8
 * Omaha, NE 68106
 * 402.291.0100
 *
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 10, 2011            bclement     Initial creation
 *
 */
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
