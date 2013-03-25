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

    public static enum PolygonEditAction {
        SUB, SET, SNOW, RAISE, LOWER, SCALE;

        /**
         * Pretty output name of enum, MUST still match all chars in original
         * enum name
         * 
         * @return
         */
        public String toPrettyName() {
            char[] chars = name().toLowerCase().toCharArray();
            chars[0] = Character.toUpperCase(chars[0]);
            return new String(chars);
        }
    }

    /** HRAP Points */
    private final Point[] editPoints;

    private float drawPrecipValue;

    private PolygonEditAction editAction;

    /** Is the polygon visible */
    private boolean visible = true;

    /** Is the polygon persistent */
    private boolean persistent;

    /** The precip value */
    private double precipValue;

    /** The DisplayFieldData for the substituted cells */
    private DisplayFieldData subDrawSource = null;

    public RubberPolyData(PolygonEditAction editAction,
            DisplayFieldData subSource, double precipValue, Point[] hrapPoints,
            boolean visible, boolean persistent) {
        this.editAction = editAction;
        this.subDrawSource = subSource;
        this.precipValue = precipValue;
        this.editPoints = hrapPoints;
        this.visible = visible;
    }

    /**
     * 
     * @return
     */
    public PolygonEditAction getEditAction() {
        return editAction;
    }

    /**
     * 
     * @return
     */
    public Point[] getEditPoints() {
        return editPoints;
    }

    /**
     * @return the drawPrecipValue
     */
    public float getDrawPrecipValue() {
        return drawPrecipValue;
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
     * @return the precipValue
     */
    public double getPrecipValue() {
        return precipValue;
    }

    /**
     * @return the subDrawSource
     */
    public DisplayFieldData getSubDrawSource() {
        return subDrawSource;
    }

}
