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
package com.raytheon.viz.mpe.ui.dialogs.polygon;

import java.awt.Point;
import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.List;

import com.raytheon.viz.mpe.ui.DisplayFieldData;

/**
 * Define the rubber poly data structure. This contains information about
 * polygons drawn on the display by the user for the purpose of editing
 * precipitation data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 1, 2009  2685       mpduff      Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class RubberPolyData {
    /** Polygon number */
    private int polygonNumber;

    /** HRAP Points */
    private List<Point> hrap = new ArrayList<Point>();

    private int isite;

    private int xpoly;

    private int xpt;

    private int close;

    private float drawPrecipValue;

    private boolean snow_flag;

    private boolean set_flag;

    private boolean raise_flag;

    private boolean lower_flag;

    private boolean scale_flag;

    private boolean sub_flag;

    /** Is the polygon visible */
    private boolean visible = true;

    /** Is the polygon persistent */
    private boolean persistent;

    /** HRAP Extent */
    private Rectangle hrapExtent;
    
    /** The precip value */
    private double precipValue = -999;
    
    /** Is the polygon closed */
    private boolean closed = false;
    
    /** The DisplayFieldData for the display */
    private DisplayFieldData drawSource = null;
    
    /** The DisplayFieldData for the substituted cells */
    private DisplayFieldData subDrawSource = null;
    
    private int numberPoints = 0;
    private int minx = 0;
    private int maxx = 0;
    private int miny = 0;
    private int maxy = 0;

    /**
     * @return the polygonNumber
     */
    public int getPolygonNumber() {
        return polygonNumber;
    }

    /**
     * @param polygonNumber
     *            the polygonNumber to set
     */
    public void setPolygonNumber(int polygonNumber) {
        this.polygonNumber = polygonNumber;
    }

    /**
     * @return the npoints
     */
    public int getNpoints() {
        return hrap.size();
    }

    /**
     * @return the hrap
     */
    public List<Point> getHrap() {
        return hrap;
    }

    /**
     * @param hrap
     *            the hrap to set
     */
    public void setHrap(List<Point> hrap) {
        this.hrap = hrap;
    }
    
    /**
     * Add a point to the list.
     * 
     * @param p
     *      The point to add to the list
     */
    public void addHrapPoint(Point p) {
        hrap.add(p);
    }

    /**
     * @return the isite
     */
    public int getIsite() {
        return isite;
    }

    /**
     * @param isite
     *            the isite to set
     */
    public void setIsite(int isite) {
        this.isite = isite;
    }

    /**
     * @return the xpoly
     */
    public int getXpoly() {
        return xpoly;
    }

    /**
     * @param xpoly
     *            the xpoly to set
     */
    public void setXpoly(int xpoly) {
        this.xpoly = xpoly;
    }

    /**
     * @return the xpt
     */
    public int getXpt() {
        return xpt;
    }

    /**
     * @param xpt
     *            the xpt to set
     */
    public void setXpt(int xpt) {
        this.xpt = xpt;
    }

    /**
     * @return the close
     */
    public int getClose() {
        return close;
    }

    /**
     * @param close
     *            the close to set
     */
    public void setClose(int close) {
        this.close = close;
    }

    /**
     * @return the drawPrecipValue
     */
    public float getDrawPrecipValue() {
        return drawPrecipValue;
    }

    /**
     * @param drawPrecipValue
     *            the drawPrecipValue to set
     */
    public void setDrawPrecipValue(float drawPrecipValue) {
        this.drawPrecipValue = drawPrecipValue;
    }

    /**
     * @return the snow_flag
     */
    public boolean isSnow_flag() {
        return snow_flag;
    }

    /**
     * @param snow_flag
     *            the snow_flag to set
     */
    public void setSnow_flag(boolean snow_flag) {
        this.snow_flag = snow_flag;
    }

    /**
     * @return the set_flag
     */
    public boolean isSet_flag() {
        return set_flag;
    }

    /**
     * @param set_flag
     *            the set_flag to set
     */
    public void setSet_flag(boolean set_flag) {
        this.set_flag = set_flag;
    }

    /**
     * @return the raise_flag
     */
    public boolean isRaise_flag() {
        return raise_flag;
    }

    /**
     * @param raise_flag
     *            the raise_flag to set
     */
    public void setRaise_flag(boolean raise_flag) {
        this.raise_flag = raise_flag;
    }

    /**
     * @return the lower_flag
     */
    public boolean isLower_flag() {
        return lower_flag;
    }

    /**
     * @param lower_flag
     *            the lower_flag to set
     */
    public void setLower_flag(boolean lower_flag) {
        this.lower_flag = lower_flag;
    }

    /**
     * @return the scale_flag
     */
    public boolean isScale_flag() {
        return scale_flag;
    }

    /**
     * @param scale_flag
     *            the scale_flag to set
     */
    public void setScale_flag(boolean scale_flag) {
        this.scale_flag = scale_flag;
    }

    /**
     * @return the sub_flag
     */
    public boolean isSub_flag() {
        return sub_flag;
    }

    /**
     * @param sub_flag
     *            the sub_flag to set
     */
    public void setSub_flag(boolean sub_flag) {
        this.sub_flag = sub_flag;
    }

    /**
     * @return the visible
     */
    public boolean isVisible() {
        return visible;
    }

    /**
     * @param visible
     *            the visible to set
     */
    public void setVisible(boolean visible) {
        this.visible = visible;
    }

    /**
     * @return the persistent
     */
    public boolean isPersistent() {
        return persistent;
    }

    /**
     * @param persistent
     *            the persistent to set
     */
    public void setPersistent(boolean persistent) {
        this.persistent = persistent;
    }

    /**
     * @return the hrapExtent
     */
    public Rectangle getHrapExtent() {
        return hrapExtent;
    }

    /**
     * @param hrapExtent
     *            the hrapExtent to set
     */
    public void setHrapExtent(Rectangle hrapExtent) {
        this.hrapExtent = hrapExtent;
    }

    /**
     * @return the precipValue
     */
    public double getPrecipValue() {
        return precipValue;
    }

    /**
     * @param precipValue the precipValue to set
     */
    public void setPrecipValue(double precipValue) {
        this.precipValue = precipValue;
    }

    /**
     * @return the closed
     */
    public boolean isClosed() {
        return closed;
    }

    /**
     * @param closed the closed to set
     */
    public void setClosed(boolean closed) {
        this.closed = closed;
    }

    /**
     * @return the numberPoints
     */
    public int getNumberPoints() {
        return numberPoints;
    }

    /**
     * @param numberPoints the numberPoints to set
     */
    public void setNumberPoints(int numberPoints) {
        this.numberPoints = numberPoints;
    }

    /**
     * @return the drawSource
     */
    public DisplayFieldData getDrawSource() {
        return drawSource;
    }

    /**
     * @param drawSource the drawSource to set
     */
    public void setDrawSource(DisplayFieldData drawSource) {
        this.drawSource = drawSource;
    }

    /**
     * @return the minx
     */
    public int getMinx() {
        return minx;
    }

    /**
     * @param minx the minx to set
     */
    public void setMinx(int minx) {
        this.minx = minx;
    }

    /**
     * @return the maxx
     */
    public int getMaxx() {
        return maxx;
    }

    /**
     * @param maxx the maxx to set
     */
    public void setMaxx(int maxx) {
        this.maxx = maxx;
    }

    /**
     * @return the miny
     */
    public int getMiny() {
        return miny;
    }

    /**
     * @param miny the miny to set
     */
    public void setMiny(int miny) {
        this.miny = miny;
    }

    /**
     * @return the maxy
     */
    public int getMaxy() {
        return maxy;
    }

    /**
     * @param maxy the maxy to set
     */
    public void setMaxy(int maxy) {
        this.maxy = maxy;
    }
    
    /**
     * @return the subDrawSource
     */
    public DisplayFieldData getSubDrawSource() {
        return subDrawSource;
    }

    /**
     * @param subDrawSource the subDrawSource to set
     */
    public void setSubDrawSource(DisplayFieldData subDrawSource) {
        this.subDrawSource = subDrawSource;
    }

    /**
     * Return the action.
     * 
     * @return The action
     */
    public String getAction() {
        String action = null;
        if (isLower_flag()) {
            action = "Lower";
        } else if (isRaise_flag()) {
            action = "Raise";
        } else if (isScale_flag()) {
            action = "Scale";
        } else if (isSet_flag()) {
            action = "Set";
        } else if (isSnow_flag()) {
            action = "Snow";
        } else if (isSub_flag()){
            action = "Sub";
        }
        
        return action;
    }
}
