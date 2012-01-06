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
package com.raytheon.uf.viz.core.interp;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 8, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class InterpolationRequest {

    /** the x data set */
    protected float[] xData;

    /** The y data set */
    protected float[] yData;

    /** The z data set */
    protected float[] zData;

    /** The min/max of x/y values and the number of grid points to be made */
    protected float minY, maxY, minX, maxX, gridX, gridY;

    public InterpolationRequest() {

    }

    public InterpolationRequest(float[] xData, float[] yData, float[] zData,
            float minX, float maxX, float minY, float maxY) {
        this.yData = yData;
        this.xData = xData;
        this.zData = zData;
        this.minX = minX;
        this.maxX = maxX;
        this.minY = minY;
        this.maxY = maxY;
        this.gridX = 100;
        this.gridY = 100;
    }

    public InterpolationRequest(float[] xData, float[] yData, float minY,
            float maxY, float minX, float maxX, float gridX, float gridY) {
        this.xData = xData;
        this.yData = yData;
        this.minY = minY;
        this.maxY = maxY;
        this.minX = minX;
        this.maxX = maxX;
        this.gridX = gridX;
        this.gridY = gridY;
    }

    public InterpolationRequest(float[] xData, float[] yData, float[] zData,
            float minY, float maxY, float minX, float maxX, float gridX,
            float gridY) {
        this.xData = xData;
        this.yData = yData;
        this.zData = zData;
        this.minY = minY;
        this.maxY = maxY;
        this.minX = minX;
        this.maxX = maxX;
        this.gridX = gridX;
        this.gridY = gridY;
    }

    public float[] getXData() {
        return xData;
    }

    public void setXData(float[] data) {
        xData = data;
    }

    public float[] getYData() {
        return yData;
    }

    public void setYData(float[] data) {
        yData = data;
    }

    public float[] getZData() {
        return zData;
    }

    public void setZData(float[] data) {
        zData = data;
    }

    public float getMinY() {
        return minY;
    }

    public void setMinY(float minY) {
        this.minY = minY;
    }

    public float getMaxY() {
        return maxY;
    }

    public void setMaxY(float maxY) {
        this.maxY = maxY;
    }

    public float getMinX() {
        return minX;
    }

    public void setMinX(float minX) {
        this.minX = minX;
    }

    public float getMaxX() {
        return maxX;
    }

    public void setMaxX(float maxX) {
        this.maxX = maxX;
    }

    public float getGridX() {
        return gridX;
    }

    public void setGridX(float gridX) {
        this.gridX = gridX;
    }

    public float getGridY() {
        return gridY;
    }

    public void setGridY(float gridY) {
        this.gridY = gridY;
    }

}
