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
public class MapRange {

	@XmlAttribute
	private String type;

    /**
     * indicates that this range was pulled from "thin air" and can be replaced
     * if a better range is known
     */
    @XmlAttribute
    private boolean replaceable = false;

	@XmlElement
	private Float upperMinimum;

	@XmlElement
	private Float upperMaximum;

	@XmlElement
	private Float lowerMinimum;

	@XmlElement
	private Float lowerMaximum;

	public MapRange() {

	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public Float getUpperMinimum() {
		return upperMinimum;
	}

	public void setUpperMinimum(Float upperMinimum) {
		this.upperMinimum = upperMinimum;
	}

	public Float getUpperMaximum() {
		return upperMaximum;
	}

	public void setUpperMaximum(Float upperMaximum) {
		this.upperMaximum = upperMaximum;
	}

	public Float getLowerMinimum() {
		return lowerMinimum;
	}

	public void setLowerMinimum(Float lowerMinimum) {
		this.lowerMinimum = lowerMinimum;
	}

	public Float getLowerMaximum() {
		return lowerMaximum;
	}

	public void setLowerMaximum(Float lowerMaximum) {
		this.lowerMaximum = lowerMaximum;
	}

    public void setReplaceable(boolean replaceable) {
        this.replaceable = replaceable;
    }

    public boolean isReplaceable() {
        return replaceable;
    }
}
