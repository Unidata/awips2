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

package com.raytheon.viz.gfe.temporaleditor;

/**
 * Holds a parm display attributes for the temporal editor
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 30, 2009 2159       rjpeter     Initial creation.
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class TEParmDisplayAttributes {

    private static final int TIMEBAR_STYLE = 1 << 1;

    private static final int RANGEBAR_STYLE = 1 << 2;

    private static final int COLORBAR_STYLE = 1 << 3;

    private static final int COLORRANGEBAR_STYLE = 1 << 4;

    private static final int WIND_BARB_STYLE = 1 << 5;

    private static final int WIND_ARROW_STYLE = 1 << 6;

    private int graphicStyle = TIMEBAR_STYLE | WIND_BARB_STYLE;

    private int imageStyle = COLORBAR_STYLE | WIND_BARB_STYLE;

    /**
     * True if the parm is to be displayed as a graphic, otherwise parm is an
     * image.
     */
    private boolean displayedAsGraphic = true;

    private boolean displayed = true;

    /**
     * 
     * @return
     */
    public boolean isDisplayed() {
        return displayed;
    }

    public void setDisplayed(boolean displayed) {
        this.displayed = displayed;
    }

    public boolean isDisplayedAsGraphic() {
        return displayedAsGraphic;
    }

    public void setDisplayedAsGraphic(boolean displayedAsGraphic) {
        this.displayedAsGraphic = displayedAsGraphic;
    }

    /**
     * 
     * @return
     */
    public boolean hasTimeBar() {
        return checkStyle(TIMEBAR_STYLE);
    }

    /**
     * 
     * @param on
     */
    public void setTimeBar(boolean on) {
        setStyle(TIMEBAR_STYLE, on);
    }

    /**
     * 
     * @return
     */
    public boolean hasRangeBar() {
        return checkStyle(RANGEBAR_STYLE);
    }

    /**
     * 
     * @param on
     */
    public void setRangeBar(boolean on) {
        setStyle(RANGEBAR_STYLE, on);
    }

    /**
     * 
     * @return
     */
    public boolean hasColorBar() {
        return checkStyle(COLORBAR_STYLE);
    }

    /**
     * 
     * @param on
     */
    public void setColorBar(boolean on) {
        setStyle(COLORBAR_STYLE, on);
    }

    /**
     * 
     * @return
     */
    public boolean hasColorRangeBar() {
        return checkStyle(COLORRANGEBAR_STYLE);
    }

    /**
     * 
     * @param on
     */
    public void setColorRangeBar(boolean on) {
        setStyle(COLORRANGEBAR_STYLE, on);
    }

    /**
     * 
     * @return
     */
    public boolean hasWindBarb() {
        return checkStyle(WIND_BARB_STYLE);
    }

    /**
     * 
     * @param on
     */
    public void setWindBarb(boolean on) {
        setStyle(WIND_BARB_STYLE, on);
    }

    /**
     * 
     * @return
     */
    public boolean hasWindArrow() {
        return checkStyle(WIND_ARROW_STYLE);
    }

    /**
     * 
     * @param on
     */
    public void setWindArrow(boolean on) {
        setStyle(WIND_ARROW_STYLE, on);
    }

    public void set(TEParmDisplayAttributes dispAtt) {
        this.graphicStyle = dispAtt.graphicStyle;
        this.imageStyle = dispAtt.imageStyle;
        this.displayedAsGraphic = dispAtt.displayedAsGraphic;
        this.displayed = dispAtt.displayed;

    }

    /**
     * 
     * @param styleToCheck
     * @return
     */
    private boolean checkStyle(int styleToCheck) {
        if (displayedAsGraphic) {
            return (graphicStyle & styleToCheck) != 0;
        }

        return (imageStyle & styleToCheck) != 0;
    }

    /**
     * 
     * @param styleToSet
     * @return
     */
    private void setStyle(int styleToSet, boolean turnStyleOn) {
        if (turnStyleOn) {
            if (displayedAsGraphic) {
                graphicStyle |= styleToSet;
            } else {
                imageStyle |= styleToSet;
            }
        } else {
            if (displayedAsGraphic) {
                graphicStyle &= -styleToSet - 1;
            } else {
                imageStyle &= -styleToSet - 1;
            }
        }
    }
}
