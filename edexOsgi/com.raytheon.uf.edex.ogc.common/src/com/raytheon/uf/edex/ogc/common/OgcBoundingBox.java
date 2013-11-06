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
package com.raytheon.uf.edex.ogc.common;

import com.vividsolutions.jts.geom.Envelope;

/**
 * Bounding box with arbitrary CRS that also stored resolution information.
 * Should be replaced with GridGeometry2D.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2011                    bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
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
