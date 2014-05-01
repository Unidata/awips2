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

import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Global configuration items for alertviz
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 18, 2008 1433       chammack    Initial creation
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class TrayConfiguration implements ISerializableObject {

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
        Q4, H1, H2, V2, V3, V4, MO
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
     * The position of the bar
     */
    @XmlAttribute
    private int xPosition;

    @XmlAttribute
    private int yPosition;

    @XmlAttribute
    private int width;

    @XmlAttribute
    private int height;

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
     * @return the position
     */
    public Rectangle getPosition() {
        Rectangle position = null;
        if (xPosition >= 0 && yPosition >= 0 && width >= 1 && height >= 1) {
            position = new Rectangle(xPosition, yPosition, width, height);
        }
        return position;
    }

    /**
     * @param position
     *            the position to set
     */
    public void setPosition(Rectangle position) {
        this.xPosition = position.x;
        this.yPosition = position.y;
        this.width = position.width;
        this.height = position.height;
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
     * @param messageLogLength
     *            the messageLogLength to set
     */
    public void setLogLength(int logLength) {
        this.logLength = logLength;
    }

    public int getXPosition() {
        return xPosition;
    }

    public void setXPosition(int position) {
        xPosition = position;
    }

    public int getYPosition() {
        return yPosition;
    }

    public void setYPosition(int position) {
        yPosition = position;
    }

    public int getWidth() {
        return width;
    }

    public void setWidth(int width) {
        this.width = width;
    }

    public int getHeight() {
        return height;
    }

    public void setHeight(int height) {
        this.height = height;
    }

    public TrayConfiguration clone() {
        TrayConfiguration newConfig = new TrayConfiguration();
        newConfig.audioDuration = audioDuration;
        newConfig.blinkDuration = blinkDuration;
        newConfig.categoryShown = categoryShown;
        newConfig.expandedPopup = expandedPopup;
        newConfig.height = height;
        newConfig.logLength = logLength;
        newConfig.mode = mode;
        newConfig.priorityShown = priorityShown;
        newConfig.sourceKeyShown = sourceKeyShown;
        newConfig.width = width;
        newConfig.xPosition = xPosition;
        newConfig.yPosition = yPosition;
        return newConfig;
    }

}
