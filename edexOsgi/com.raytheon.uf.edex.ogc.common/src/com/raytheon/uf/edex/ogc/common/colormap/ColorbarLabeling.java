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
 */
package com.raytheon.uf.edex.ogc.common.colormap;

import javax.xml.bind.annotation.XmlAccessOrder;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorOrder;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

@XmlAccessorType(XmlAccessType.NONE)
@XmlAccessorOrder(XmlAccessOrder.UNDEFINED)
public class ColorbarLabeling {
	@XmlElement
	String increment;

	@XmlElement
	String values;

	public String getIncrement() {
		return increment;
	}

	public void setIncriment(String increment) {
		this.increment = increment;
	}

	public String getValues() {
		return values;
	}

	public void setValues(String values) {
		this.values = values;
	}
}
