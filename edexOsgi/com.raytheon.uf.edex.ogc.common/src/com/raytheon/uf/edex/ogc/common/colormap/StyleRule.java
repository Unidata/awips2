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
 * Dec 8, 2011            ekladstrup     Initial creation
 *
 */
package com.raytheon.uf.edex.ogc.common.colormap;

import javax.xml.bind.annotation.XmlAccessOrder;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorOrder;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

/**
 * TODO Add Description
 * 
 * @author ekladstrup
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlAccessorOrder(XmlAccessOrder.UNDEFINED)
public class StyleRule {
	@XmlElement
	private String layerRegex;

	@XmlElement
	private LevelRange levelRange;

	@XmlElement
	private MapRange mapRange;

    @XmlElement
    private MapRange dataRange;

	@XmlElement
	private ColorbarLabeling colorbarLabeling;

	@XmlElement
	private String colorMapName;

	@XmlElement
	private String displayUnits;

	public StyleRule() {

	}

	public String getLayerRegex() {
		return layerRegex;
	}

	public void setLayerRegex(String layerRegex) {
		this.layerRegex = layerRegex;
	}

	public void setLevelRange(LevelRange levelRange) {
		this.levelRange = levelRange;
	}

	public LevelRange getLevelRange() {
		return levelRange;
	}

	public void setMapRange(MapRange mapRange) {
		this.mapRange = mapRange;
	}

	public MapRange getMapRange() {
		return mapRange;
	}

    public void setDataRange(MapRange dataRange) {
        this.dataRange = dataRange;
    }

    public MapRange getDataRange() {
        return dataRange;
    }

    public void setColorbarLabeling(ColorbarLabeling colorbarLabeling) {
		this.colorbarLabeling = colorbarLabeling;
	}

	public ColorbarLabeling getColorbarLabeling() {
		return colorbarLabeling;
	}

	public void setColorMapName(String colorMapName) {
		this.colorMapName = colorMapName;
	}

	public String getColorMapName() {
		return colorMapName;
	}

	public void setDisplayUnits(String displayUnits) {
		this.displayUnits = displayUnits;
	}

	public String getDisplayUnits() {
		return displayUnits;
	}

}
