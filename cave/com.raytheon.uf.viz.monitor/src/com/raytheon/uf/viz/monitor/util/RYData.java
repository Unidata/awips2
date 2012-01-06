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
package com.raytheon.uf.viz.monitor.util;

import com.raytheon.uf.common.monitor.data.ObConst.ThreatLevel;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 16, 2009 2076       avarani     Initial creation
 * 
 * </pre>
 * 
 * @author avarani
 * @version 1.0
 */
public class RYData {
    private String rValue;

    private String yValue;

    private final String FILL_VALUE = "-999.9";

    /**
     * Default constructor. Initializes rValue and yValue to -999.9.
     */
    public RYData() {
        this.rValue = FILL_VALUE;
        this.yValue = FILL_VALUE;
    }

    /**
     * Construct the object by specifying rValye and yValue as integers.
     * 
     * @param r
     *            rValue as <b>int</b>.
     * @param y
     *            yValue as <b>int</b>.
     */
    public RYData(int r, int y) {
        this.setValues(r, y);
    }

    /**
     * Construct the object by specifying rValue and yValue as doubles.
     * 
     * @param r
     *            rValue as <b>double</b>.
     * @param y
     *            yValue as <b>double</b>.
     */
    public RYData(double r, double y) {
        this.setValues(r, y);
    }

    /**
     * Construct the object by specifying rValue and yValue as Strings.
     * 
     * @param r
     *            rValue as String.
     * @param y
     *            yValue as String.
     */
    public RYData(String r, String y) {
        if (r.matches("[0-9]*[.]?[0-9]*(E[0-9]{1,3})?")) {
            this.rValue = r;
        } else {
            throw new NumberFormatException("R value is not a number.");
        }

        if (y.matches("[0-9]*[.]?[0-9]*(E[0-9]{1,3})?")) {
            this.yValue = y;
        } else {
            throw new NumberFormatException("Y value is not a number.");
        }
    }

    /**
     * @param level
     *            ThreatLevel
     * @return Red or Yellow value as <b>int</b>.
     */
    public int getValueInt(ThreatLevel level) {
        if (level == ThreatLevel.RED) {
            return Integer.parseInt(rValue);
        } else if (level == ThreatLevel.YELLOW) {
            return Integer.parseInt(yValue);
        } else {
            return Integer.parseInt(FILL_VALUE);
        }
    }

    /**
     * @param level
     *            ThreatLevel
     * @return Red or Yellow value as <b>dobule</b>.
     */
    public double getValueDouble(ThreatLevel level) {
        if (level == ThreatLevel.RED) {
            return Double.parseDouble(rValue);
        } else if (level == ThreatLevel.YELLOW) {
            return Double.parseDouble(yValue);
        } else {
            return Double.parseDouble(FILL_VALUE);
        }
    }

    /**
     * @param level
     *            ThreatLevel
     * @return Red or Yellow value as String.
     */
    public String getValue(ThreatLevel level) {
        if (level == ThreatLevel.RED) {
            return rValue;
        } else if (level == ThreatLevel.YELLOW) {
            return yValue;
        } else {
            return FILL_VALUE;
        }
    }

    /**
     * @return rValue as <b>int</b>.
     */
    public int getRValueInt() {
        return Integer.parseInt(rValue);
    }

    /**
     * @return yValue as <b>int</b>.
     */
    public int getYValueInt() {
        return Integer.parseInt(yValue);
    }

    /**
     * @return return rValue as <b>double</b>.
     */
    public Double getRValueDouble() {
        return Double.parseDouble(rValue);
    }

    /**
     * @return return yValue as <b>double</b>.
     */
    public Double getYValueDouble() {
        return Double.parseDouble(yValue);
    }

    /**
     * @return return rValue as String.
     */
    public String getRValue() {
        return new String(rValue);
    }

    /**
     * @return return yValue as String.
     */
    public String getYValue() {
        return new String(yValue);
    }

    /**
     * Set rValue and yValue by specifying the values as integers.
     * 
     * @param r
     *            rValue
     * @param y
     *            yValue
     */
    public void setValues(int r, int y) {
        this.rValue = Integer.toString(r);
        this.yValue = Integer.toString(y);
    }

    /**
     * Set rValue and yValue by specifying the values as doubles.
     * 
     * @param r
     *            rValue
     * @param y
     *            yValue
     */
    public void setValues(double r, double y) {
        this.rValue = Double.toString(r);
        this.yValue = Double.toString(y);
    }
}
