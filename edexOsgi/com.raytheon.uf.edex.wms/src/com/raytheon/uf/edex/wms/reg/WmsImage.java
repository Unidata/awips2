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
 * Aug 1, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wms.reg;

import org.geotools.coverage.grid.GridCoverage2D;
import org.geotools.feature.FeatureCollection;
import org.geotools.styling.Style;
import org.opengis.feature.simple.SimpleFeature;
import org.opengis.feature.simple.SimpleFeatureType;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public class WmsImage {

	public enum TYPE {
		COVERAGE, FEATURE, STYLE_EMBEDDED_FEATURE, BLANK
	};

	protected TYPE type;

	protected GridCoverage2D coverage;

	protected FeatureCollection<SimpleFeatureType, SimpleFeature> features;

	protected Style style;


	public WmsImage(GridCoverage2D coverage, Style style) {
		this(coverage);
		this.style = style;
	}

	public WmsImage(GridCoverage2D coverage) {
		this.coverage = coverage;
		this.type = coverage == null ? TYPE.BLANK : TYPE.COVERAGE;
	}

	public WmsImage(
			FeatureCollection<SimpleFeatureType, SimpleFeature> features,
			Style style) {
		this.features = features;
		this.style = style;
		this.type = features == null ? TYPE.BLANK : TYPE.FEATURE;
	}

	public WmsImage(FeatureCollection<SimpleFeatureType, SimpleFeature> features) {
		this.features = features;
		this.type = features == null ? TYPE.BLANK : TYPE.STYLE_EMBEDDED_FEATURE;
	}

	public TYPE getType() {
		return type;
	}

	public GridCoverage2D getCoverage() {
		return coverage;
	}

	public FeatureCollection<SimpleFeatureType, SimpleFeature> getFeatures() {
		return features;
	}

	public Style getStyle() {
		return style;
	}

	public void setStyle(Style style) {
		this.style = style;
		this.type = TYPE.FEATURE;
	}

}
