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
* Dec 9, 2011            ekladstrup     Initial creation
*
*/ 

package com.raytheon.uf.edex.ogc.common.colormap;

import javax.xml.bind.annotation.XmlAccessOrder;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorOrder;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

/**
 * TODO Add Description
 *
 * @author ekladstrup
 * @version 1.0	
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlAccessorOrder(XmlAccessOrder.UNDEFINED)
public class LevelRange {

	@XmlAttribute
	private String level;

	@XmlElement
	private Float upper;

	@XmlElement
	private Float lower;

	@XmlAttribute
	private String unit;

	public LevelRange() {

	}


	public Float getUpper() {
		return upper;
	}

	public void setUpper(Float upper) {
		this.upper = upper;
	}

	public Float getLower() {
		return lower;
	}

	public void setLower(Float lower) {
		this.lower = lower;
	}

	public void setLevel(String level) {
		this.level = level;
	}

	public String getLevel() {
		return level;
	}


	public void setUnit(String unit) {
		this.unit = unit;
	}


	public String getUnit() {
		return unit;
	}
}
