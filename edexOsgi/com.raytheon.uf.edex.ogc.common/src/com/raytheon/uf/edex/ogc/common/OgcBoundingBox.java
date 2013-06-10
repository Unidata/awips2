/**********************************************************************
 *
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
 **********************************************************************/
package com.raytheon.uf.edex.ogc.common;

import com.vividsolutions.jts.geom.Envelope;

public class OgcBoundingBox extends OgcGeoBoundingBox {

	protected String crs;

	protected double resx = Double.NaN;

	protected double resy = Double.NaN;

	public OgcBoundingBox() {
	}

	public OgcBoundingBox(String crs, double minx, double maxx, double miny,
			double maxy, double resx, double resy) {
		this(crs, minx, maxx, miny, maxy);
		this.resx = resx;
		this.resy = resy;
	}

	public OgcBoundingBox(String crs, double minx, double maxx, double miny,
			double maxy) {
		super(maxx, minx, maxy, miny);
		this.crs = crs;
	}

	/**
	 * @param targetCrs
	 * @param env
	 */
	public OgcBoundingBox(String crs, Envelope env) {
		super(env);
		this.crs = crs;
	}

	public String getCrs() {
		return crs;
	}

	public void setCrs(String crs) {
		this.crs = crs;
	}

	public double getResx() {
		return resx;
	}

	public void setResx(double resx) {
		this.resx = resx;
	}

	public double getResy() {
		return resy;
	}

	public void setResy(double resy) {
		this.resy = resy;
	}

}
