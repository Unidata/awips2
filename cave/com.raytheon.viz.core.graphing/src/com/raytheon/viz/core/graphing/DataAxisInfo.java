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

package com.raytheon.viz.core.graphing;

import com.raytheon.uf.common.style.graph.GraphPreferences;

/**
 * Ported from D2D
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 8, 2007             njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 */
public class DataAxisInfo {

    /**
     * the min value of the data
     */
    private float dataMin;

    /**
     * if the data is at zero
     */
    private boolean zeroYes;

    /**
     * ??? // TODO
     */
    private float absMin;

    /**
     * the max value of the data
     */
    private float dataMax;

    /**
     * the interval between axis ticks/labels
     */
    private float interval;

    /**
     * the min value on the axis
     */
    private float divMin;

    /**
     * the max value on the axis
     */
    private float divMax;

    /**
     * ??? // TODO
     */
    private float dLinear;

    /**
     * ??? // TODO
     */
    private int zeroDiv;

    /**
     * the units string
     */
    private String units;

    /**
     * the graph style
     */
    private GraphPreferences style;

    /**
     * @return the dataMin
     */
    public float getDataMin() {
        return dataMin;
    }

    /**
     * @param dataMin
     *            the dataMin to set
     */
    public void setDataMin(float dataMin) {
        this.dataMin = dataMin;
    }

    /**
     * @return the zeroYes
     */
    public boolean isZeroYes() {
        return zeroYes;
    }

    /**
     * @param zeroYes
     *            the zeroYes to set
     */
    public void setZeroYes(boolean zeroYes) {
        this.zeroYes = zeroYes;
    }

    /**
     * @return the absMin
     */
    public float getAbsMin() {
        return absMin;
    }

    /**
     * @param absMin
     *            the absMin to set
     */
    public void setAbsMin(float absMin) {
        this.absMin = absMin;
    }

    /**
     * @return the dataMax
     */
    public float getDataMax() {
        return dataMax;
    }

    /**
     * @param dataMax
     *            the dataMax to set
     */
    public void setDataMax(float dataMax) {
        this.dataMax = dataMax;
    }

    /**
     * @return the interval
     */
    public float getInterval() {
        return interval;
    }

    /**
     * @param interval
     *            the interval to set
     */
    public void setInterval(float interval) {
        this.interval = interval;
    }

    /**
     * @return the divMin
     */
    public float getDivMin() {
        return divMin;
    }

    /**
     * @param divMin
     *            the divMin to set
     */
    public void setDivMin(float divMin) {
        this.divMin = divMin;
    }

    /**
     * @return the divMax
     */
    public float getDivMax() {
        return divMax;
    }

    /**
     * @param divMax
     *            the divMax to set
     */
    public void setDivMax(float divMax) {
        this.divMax = divMax;
    }

    /**
     * @return the dLinear
     */
    public float getDLinear() {
        return dLinear;
    }

    /**
     * @param linear
     *            the dLinear to set
     */
    public void setDLinear(float linear) {
        dLinear = linear;
    }

    /**
     * @return the zeroDiv
     */
    public int getZeroDiv() {
        return zeroDiv;
    }

    /**
     * @param zeroDiv
     *            the zeroDiv to set
     */
    public void setZeroDiv(int zeroDiv) {
        this.zeroDiv = zeroDiv;
    }

    /**
     * @return the units
     */
    public String getUnits() {
        return units;
    }

    /**
     * @param units
     *            the units to set
     */
    public void setUnits(String units) {
        this.units = units;
    }

    /**
     * @return the style
     */
    public GraphPreferences getStyle() {
        return style;
    }

    /**
     * @param style
     *            the style to set
     */
    public void setStyle(GraphPreferences style) {
        this.style = style;
    }

    public String toString() {
        StringBuffer sb = new StringBuffer();
        sb.append("dataAxisInfo { ");
        sb.append("absMin = " + absMin + ", ");
        sb.append("dataMax = " + dataMax + ", ");
        sb.append("dataMin = " + dataMin + ", ");
        sb.append("divMax = " + divMax + ", ");
        sb.append("divMin = " + divMin + ", ");
        sb.append("dLinear = " + dLinear + ", ");
        sb.append("interval = " + interval + ", ");
        sb.append("zeroDiv = " + zeroDiv + ", ");
        sb.append("zeroYes = " + zeroYes);

        sb.append(" } ");
        return sb.toString();
    }

}
