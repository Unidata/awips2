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
package com.raytheon.viz.warngen.gui;

import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Object that holds UI state information for warngen
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 7, 2010            mschenke     Initial creation
 * 03/14/2012   DR 14690  Qinglu Lin   Add clear2().
 * 10/26/2012   DR 15479  Qinglu Lin   Added removeDuplicateCoordinate().
 * 12/06/2012   DR 15559  Qinglu Lin   Added computeSlope(), computeCoordinate(), 
 *                                     and adjustPolygon().
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class WarngenUIState {

    /** The hatched warning area (native is local space) */
    private Geometry warningArea;

    /** The user defined polygon (native is lat/lon space) */
    private Polygon warningPolygon;

    private Polygon markedWarningPolygon;

    private Geometry markedWarningArea;

    private Polygon oldWarningPolygon;

    private Geometry oldWarningArea;

    public Map<Coordinate, String> strings = new ConcurrentHashMap<Coordinate, String>();

    public boolean snappedToArea = false;

    public boolean rightClickSelected = false;

    public boolean geometryChanged = false;

    public FollowupData followupData = null;

    /**
     * Get the warning area in lat/lon projection
     * 
     * @return the warningArea
     */
    public Geometry getWarningArea() {
        return warningArea;
    }

    /**
     * Set the warning area in lat/lon space, will be converted to local
     * 
     * @param warningAreaLatLon
     */
    public void setWarningArea(Geometry warningArea) {
        this.warningArea = warningArea;
    }

    /**
     * Get the old warning polygon in lat/lon projection
     * 
     * @return the oldWarningPolygon
     */
    public Polygon getOldWarningPolygon() {
        return oldWarningPolygon;
    }

    /**
     * Set the old warning polygon in lat/lon projection
     * 
     * @param oldWarningPolygon
     *            the oldWarningPolygon to set
     */
    public void setOldWarningPolygon(Polygon oldWarningPolygon) {
        this.oldWarningPolygon = oldWarningPolygon;
    }

    /**
     * The old warning area in lat/lon projection
     * 
     * @return the oldWarningArea
     */
    public Geometry getOldWarningArea() {
        return oldWarningArea;
    }

    /**
     * removeDuplicateCoordinate
     *     remove duplicate intermediate coordinates in warningPolygon. 
     * History
     * 10-26-2012 Qinglu Lin   DR15479 Created.
     */
    public void removeDuplicateCoordinate() {
    	Coordinate[] verts = warningPolygon.getCoordinates();
    	Set<Coordinate> coords = new LinkedHashSet<Coordinate>();
    	for (Coordinate c: verts)
    		coords.add(c);
        if ((verts.length-coords.size()) < 2)
        	return;
    	Coordinate[] vertices = new Coordinate[coords.size()+1];
    	Iterator<Coordinate> iter = coords.iterator();
    	int i = 0;
    	while (iter.hasNext()) {
    		vertices[i] = new Coordinate(iter.next());
    		i += 1;
    	}
    	vertices[i] = new Coordinate(vertices[0]);
    	GeometryFactory gf = new GeometryFactory();
    	warningPolygon = gf.createPolygon(gf.createLinearRing(vertices), null);
    }
    
    /**
     * computeSlope
     *     compute the slope of a line. 
     *     
     * History
     * 12/06/2012   DR 15559  Qinglu Lin   Created. 
     */
    private double computeSlope(Coordinate[] coords, int i) {
    	double min = 1.0E-08;
    	double dx = coords[i].x-coords[i+1].x;
    	double slope = 0.0;
    	if (Math.abs(dx)>min) {
    		slope = (coords[i].y-coords[i+1].y)/dx;
    	}
    	return slope;
    }
    
    /**
     * computeCoordinate
     *     Compute the x component of a coordinate after its y component 
     * is adjusted. 
     *     
     * History
     * 12/06/2012   DR 15559  Qinglu Lin   Created. 
     */
    private void computeCoordinate(Coordinate[] c, int i, int j) {
    	double slope;
    	slope = computeSlope(c,i); 
    	int iPlus1 = i+1;
    	if (c[j].x>=c[i].x && c[j].x<=c[iPlus1].x ||
    			c[j].x>=c[iPlus1].x && c[j].x<=c[i].x) {

    		double x,y;
    		double min1 = 0.005d;
    		y = slope*(c[j].x-c[i].x) + c[i].y;
    		double d = Math.abs(y-c[j].y);
    		if (d>min1) 
    			return;

    		double min2 = 1.0E-8d;    		
    		double delta = 0.005d; // empirical value
    		double dyMin = 0.01d;
    		int jMinus1 = j-1;
    		if (jMinus1<0) 
    			jMinus1 = c.length-2;
    		int jPlus1 = j+1;
    		if (Math.abs(y-c[j].y)<min1) {
    			double dy1, dy2;
    			dy1 = Math.abs(c[jMinus1].y-y);
    			dy2 = Math.abs(c[jPlus1].y-y);
    			if (dy1>=dy2 && (Math.abs(dy1)>dyMin || Math.abs(dy2)>dyMin)) {
    				// attempt to use l2 for computation
    				if (c[j].y==c[jMinus1].y && Math.abs(c[j].x-c[jMinus1].x)>min2) {
    					// l2 is a horizontal line, use l3 for computation
    					if (c[jPlus1].y<c[j].y) delta = -delta;
    					slope = computeSlope(c,j);
    					if (Math.abs(slope) > min2) {
    						y = c[j].y+delta;
    						x = (y-c[jPlus1].y)/slope + c[jPlus1].x;
    					} else {
    						// l3 is a vertical line
    						y = c[j].y+delta;
    						x = c[j].x;
    					}
    				} else {
    					// use l2 for computation
    					if (c[jMinus1].y<c[j].y) delta = -delta;
    					slope = computeSlope(c,jMinus1);
    					if (Math.abs(slope) > min2) {
    						y = c[j].y+delta;
    						x = (y-c[jMinus1].y)/slope + c[jMinus1].x;
    					} else {
    						// l2 is a vertical line
    						y = c[j].y+delta;
    						x = c[j].x;
    					}
    				}
    			} else {
    				if (Math.abs(dy1)>dyMin || Math.abs(dy2)>dyMin) {
    					// attempt to use l3 for computation
    					if (c[j].y==c[jPlus1].y && Math.abs(c[j].x-c[jPlus1].x)>min2) {
    						// l3 is a horizontal line, use l2 for computation
    						if (c[jMinus1].y<c[j].y) delta = -delta;
    						slope = computeSlope(c,jMinus1);
    						if (Math.abs(slope) > min2) {
    							y = c[j].y+delta;
    							x = (y-c[jMinus1].y)/slope + c[jMinus1].x;
    						} else {
    							// l2 is a vertical line
    							y = c[j].y+delta;
    							x = c[j].x;
    						}
    					} else {
    						// use l3 for computation
    						if (c[jPlus1].y<c[j].y) delta = -delta;
    						slope = computeSlope(c,j);
    						if (Math.abs(slope) > min2) {
    							y = c[j].y+delta;
    							x = (y-c[jPlus1].y)/slope + c[jPlus1].x;
    						} else {
    							// l3 is a vertical line
    							y = c[j].y+delta;
    							x = c[j].x;
    						}
    					}
    				} else {
    					x = c[j].x;
    					y = c[j].y;
    				}
    			}
    			c[j].x = x;
    			c[j].y = y;
    			if (j==0) 
    				c[c.length-1] = c[j];
    		}
    	}
    }

    /**
     * adjustPolygon
     *     When a point is very close to a line in the initial warning polygon, the resulting coordinates
     * cause the failure of polygon drawing in follow-up. The method move that kind of points away from 
     * the line, and return a Polygon.
     *
     * History
     * 12/06/2012   DR 15559  Qinglu Lin   Created.
     */
    public void adjustPolygon(Coordinate[] coords) {	
    	int n = coords.length;
    	for (int i=0; i<n-1; ++i) {
    		int j;
    		for (j=i+2; j<=n-2; j++) {
    			computeCoordinate(coords,i,j);
    		}
    		if (i<=n-3)
    			for (j=0; j<i; j++) {
    				computeCoordinate(coords,i,j);
    			}
    		else
    			for (j=1; j<i; j++) {
    				computeCoordinate(coords,i,j);
    			}
    	}
    }

    /**
     * Set the old warning area in lat/lon projection. Will be converted to
     * local
     * 
     * @param oldWarningAreaLatLon
     */
    public void setOldWarningArea(Geometry oldWarningArea) {
        this.oldWarningArea = oldWarningArea;
    }

    /**
     * get the warning polygon in lat/lon projection
     * 
     * @return the warningPolygon
     */
    public Polygon getWarningPolygon() {
        return warningPolygon;
    }

    /**
     * Set the warning polygon in lat/lon projection
     * 
     * @param warningPolygon
     *            the warningPolygon to set
     */
    public void setWarningPolygon(Polygon warningPolygon) {
        this.warningPolygon = warningPolygon;
    }

    public void clear() {
        warningPolygon = null;
        clear2();
    }

    /**
     * clear2
     *   Same as clear(), except for not assigning null to warningPolygon. 
     * History
     * 03-16-2012 Qinglu Lin   DR14690 Created.
     */
    public void clear2() {
    	oldWarningArea = null;
        oldWarningPolygon = null;
        oldWarningArea = null;
        oldWarningPolygon = null;
        strings.clear();
        warningArea = null;
        markedWarningArea = null;
        markedWarningPolygon = null;
    }

    /**
     * @return the markedWarningPolygon
     */
    public Polygon getMarkedWarningPolygon() {
        return markedWarningPolygon;
    }

    /**
     * @return the markedWarningArea
     */
    public Geometry getMarkedWarningArea() {
        return markedWarningArea;
    }

    public void resetMarked() {
        this.markedWarningArea = null;
        this.markedWarningPolygon = null;
    }

    public void mark(Geometry newHatchedArea) {
        this.markedWarningPolygon = (Polygon) warningPolygon.clone();
        this.markedWarningArea = (Geometry) newHatchedArea.clone();
    }

    public boolean isMarked() {
        return (this.markedWarningArea != null && this.markedWarningPolygon != null);
    }

}