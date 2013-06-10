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
/**
 * 
 */
package com.raytheon.uf.edex.ogc.common;

import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Polygon;

/**
 * @author bclement
 * 
 */
public class OgcGeoBoundingBox {

	protected double maxx;

	protected double minx;

	protected double maxy;

	protected double miny;

	/**
	 * 
	 */
	public OgcGeoBoundingBox() {
	}

	public OgcGeoBoundingBox(double eastLongitude, double westLongitude,
			double northLatitude, double southLatitude) {
		super();
		this.maxx = eastLongitude;
		this.minx = westLongitude;
		this.maxy = northLatitude;
		this.miny = southLatitude;
	}

	public OgcGeoBoundingBox(Polygon polygon, CoordinateReferenceSystem crs) {
		Geometry env = polygon.getEnvelope();
		Coordinate[] coords = env.getCoordinates();
		Coordinate ur = coords[2];
		Coordinate ll = coords[0];
		this.maxx = ur.x;
		this.minx = ll.x;
		this.maxy = ur.y;
		this.miny = ll.y;
	}

	public OgcGeoBoundingBox(Envelope env) {
		this.maxx = env.getMaxX();
		this.maxy = env.getMaxY();
		this.minx = env.getMinX();
		this.miny = env.getMinY();
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		long temp;
		temp = Double.doubleToLongBits(maxx);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		temp = Double.doubleToLongBits(maxy);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		temp = Double.doubleToLongBits(miny);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		temp = Double.doubleToLongBits(minx);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		OgcGeoBoundingBox other = (OgcGeoBoundingBox) obj;
		if (Double.doubleToLongBits(maxx) != Double
				.doubleToLongBits(other.maxx))
			return false;
		if (Double.doubleToLongBits(maxy) != Double
				.doubleToLongBits(other.maxy))
			return false;
		if (Double.doubleToLongBits(miny) != Double
				.doubleToLongBits(other.miny))
			return false;
		if (Double.doubleToLongBits(minx) != Double
				.doubleToLongBits(other.minx))
			return false;
		return true;
	}

	public double getMaxx() {
		return maxx;
	}

	public void setMaxx(double maxx) {
		this.maxx = maxx;
	}

	public double getMinx() {
		return minx;
	}

	public void setMinx(double minx) {
		this.minx = minx;
	}

	public double getMaxy() {
		return maxy;
	}

	public void setMaxy(double maxy) {
		this.maxy = maxy;
	}

	public double getMiny() {
		return miny;
	}

	public void setMiny(double miny) {
		this.miny = miny;
	}

}
