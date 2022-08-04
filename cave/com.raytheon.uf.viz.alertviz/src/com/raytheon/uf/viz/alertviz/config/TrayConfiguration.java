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
package com.raytheon.uf.viz.alertviz.config;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

/**
 * Global configuration items for alertviz
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Sep 18, 2008  1433     chammack  Initial creation
 * Oct 28, 2005  5054     randerso  removed bar position as it was written but
 *                                  never read
 * Sep 24, 2018  7481     randerso  Added constructor with number of horizontal
 *                                  and vertical boxes to TrayMode.
 * Oct 01, 2018  7455     randerso  Added getNumboerofBoxes() and
 *                                  getModePrefix() to TrayMode.
 *
 * </pre>
 *
 * @author chammack
 */
@XmlAccessorType(XmlAccessType.NONE)
public class TrayConfiguration {

    /**
     * The mode of the display:
     * <UL>
     * <LI>Q4 - 2 x 2 display
     * <LI>H1 - 1 x 1 display
     * <LI>H2 - 2 x 1 display
     * <LI>V2 - 1 x 2 display
     * <LI>V3 - 1 x 3 display
     * <LI>V4 - 1 x 4 display
     * </UL>
     */
    public static enum TrayMode {
        Q4(2, 2), H1(1, 1), H2(2, 1), V2(1, 2), V3(1, 3), V4(1, 4), MO(0, 0);

        /** Number of text boxes in the horizontal direction */
        private int horizontalBoxes;

        /** Number of text boxes in the vertical direction */
        private int verticalBoxes;

        TrayMode(int x, int y) {
            this.horizontalBoxes = x;
            this.verticalBoxes = y;
        }

        /**
         * @return the number of horizontal text boxes
         */
        public int getHorizontalBoxes() {
            return horizontalBoxes;
        }

        /**
         * @return the number of vertical text boxes
         */
        public int getVerticalBoxes() {
            return verticalBoxes;
        }

        /**
         * Get the total number of text boxes
         *
         * @return the total number of text boxes
         */
        public int getNumberOfBoxes() {
            return horizontalBoxes * verticalBoxes;
        }

        /**
         * Get the prefix of the layoutMode
         *
         * @return the prefix
         */
        public char getModePrefix() {
            return name().charAt(0);
        }

    };

    /**
     * The mode
     *
     * @see {@link TrayMode}
     */
    @XmlAttribute
    private TrayMode mode;

    /**
     * Is the priority shown in messages?
     *
     */
    @XmlAttribute
    private boolean priorityShown;

    /**
     * Is the source key shown in messages?
     */
    @XmlAttribute
    private boolean sourceKeyShown;

    /**
     * Is the category shown?
     */
    @XmlAttribute
    private boolean categoryShown;

    /**
     * Is the popup showing expanded information by default?
     */
    @XmlAttribute
    private boolean expandedPopup;

    /**
     * The duration of blinking
     */
    @XmlAttribute
    private int blinkDuration;

    /**
     * The duration of audio
     */
    @XmlAttribute
    private int audioDuration;

    /**
     * The message log length
     */
    @XmlAttribute
    private int logLength;

    /**
     * @return the mode
     */
    public TrayMode getMode() {
        return mode;
    }

    /**
     * @param mode
     *            the mode to set
     */
    public void setMode(TrayMode mode) {
        this.mode = mode;
    }

    /**
     * @return the priorityShown
     */
    public boolean isPriorityShown() {
        return priorityShown;
    }

    /**
     * @param priorityShown
     *            the priorityShown to set
     */
    public void setPriorityShown(boolean priorityShown) {
        this.priorityShown = priorityShown;
    }

    /**
     * @return the sourceKeyShown
     */
    public boolean isSourceKeyShown() {
        return sourceKeyShown;
    }

    /**
     * @param sourceKeyShown
     *            the sourceKeyShown to set
     */
    public void setSourceKeyShown(boolean sourceKeyShown) {
        this.sourceKeyShown = sourceKeyShown;
    }

    /**
     * @return the categoryShown
     */
    public boolean isCategoryShown() {
        return categoryShown;
    }

    /**
     * @param categoryShown
     *            the categoryShown to set
     */
    public void setCategoryShown(boolean categoryShown) {
        this.categoryShown = categoryShown;
    }

    /**
     * @return the expandedPopup
     */
    public boolean isExpandedPopup() {
        return expandedPopup;
    }

    /**
     * @param expandedPopup
     *            the expandedPopup to set
     */
    public void setExpandedPopup(boolean expandedPopup) {
        this.expandedPopup = expandedPopup;
    }

    /**
     * @return the blinkDuration
     */
    public int getBlinkDuration() {
        return blinkDuration;
    }

    /**
     * @param blinkDuration
     *            the blinkDuration to set
     */
    public void setBlinkDuration(int blinkDuration) {
        this.blinkDuration = blinkDuration;
    }

    /**
     * @return the audioDuration
     */
    public int getAudioDuration() {
        return audioDuration;
    }

    /**
     * @param audioDuration
     *            the audioDuration to set
     */
    public void setAudioDuration(int audioDuration) {
        this.audioDuration = audioDuration;
    }

    /**
     * @return the messageLogLength
     */
    public int getLogLength() {
        return logLength;
    }

    /**
     * @param logLength
     *            the messageLogLength to set
     */
    public void setLogLength(int logLength) {
        this.logLength = logLength;
    }

    @Override
    public TrayConfiguration clone() {
        TrayConfiguration newConfig = new TrayConfiguration();
        newConfig.audioDuration = audioDuration;
        newConfig.blinkDuration = blinkDuration;
        newConfig.categoryShown = categoryShown;
        newConfig.expandedPopup = expandedPopup;
        newConfig.logLength = logLength;
        newConfig.mode = mode;
        newConfig.priorityShown = priorityShown;
        newConfig.sourceKeyShown = sourceKeyShown;
        return newConfig;
    }

}
