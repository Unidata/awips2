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

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.RGBColors;

/**
 * Provides customizable preferences for an alert message
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Sep 09, 2008  1433     chammack  Initial creation
 * Sep 25, 2018  7486     randerso  Allow color names or hex strings for
 *                                  foreground and background. Code cleanup.
 * Nov 02, 2018  7600     randerso  Changes to support standard script files for
 *                                  AlertViz actions.
 * Nov 05, 2018  7509     randerso  Remove Log attribute
 * Nov 13, 2018  7512     randerso  Added image attribute
 *
 * </pre>
 *
 * @author chammack
 */
@XmlAccessorType(XmlAccessType.NONE)
public class AlertMetadata {

    /**
     * Is audio enabled for this message?
     */
    @XmlAttribute
    private boolean audioEnabled = false;

    @XmlAttribute
    private String audioFile;

    /**
     * The color of the foreground of the text box
     */
    private RGB foreground = RGBColors.getRGBColor("black");

    /**
     * The color of the background of the text box
     */
    private RGB background = RGBColors.getRGBColor("white");

    /**
     * Is the message sent to a text box?
     */
    @XmlAttribute
    private boolean text = false;

    /**
     * Is the message blinked?
     */
    @XmlAttribute
    private boolean blink = false;

    /**
     * Is a message popped up?
     */
    @XmlAttribute
    private boolean popup = false;

    @XmlAttribute
    private String image;

    /**
     * What is the priority?
     */
    @XmlAttribute(required = true)
    private Priority priority;

    /**
     * The python script to run on receipt
     */
    @XmlAttribute
    private String action;

    /**
     * Nullary constructor for serialization
     */
    public AlertMetadata() {

    }

    /**
     * Copy constructor
     *
     * @param metadata
     */
    public AlertMetadata(AlertMetadata metadata) {
        this.audioEnabled = metadata.audioEnabled;
        this.audioFile = metadata.audioFile;
        this.background = metadata.background;
        this.blink = metadata.blink;
        this.foreground = metadata.foreground;
        this.popup = metadata.popup;
        this.image = metadata.image;
        this.priority = metadata.priority;
        this.text = metadata.text;
        this.action = metadata.action;
    }

    /**
     * @return true if audio enabled
     */
    public boolean isAudioEnabled() {
        return audioEnabled;
    }

    /**
     * @param audioEnabled
     *            true if audio enabled
     */
    public void setAudioEnabled(boolean audioEnabled) {
        this.audioEnabled = audioEnabled;
    }

    /**
     * @return true if text display is enabled
     */
    public boolean isText() {
        return text;
    }

    /**
     * @param text
     *            true if text display is enabled
     */
    public void setText(boolean text) {
        this.text = text;
    }

    /**
     * @return true if blink is enabled
     */
    public boolean isBlink() {
        return blink;
    }

    /**
     * @param blink
     *            true if blink is enabled
     */
    public void setBlink(boolean blink) {
        this.blink = blink;
    }

    /**
     * @return true if popup is enabled
     */
    public boolean isPopup() {
        return popup;
    }

    /**
     * @param popup
     *            true if popup is enabled
     */
    public void setPopup(boolean popup) {
        this.popup = popup;
    }

    /**
     * @return the image
     */
    public String getImage() {
        return image;
    }

    /**
     * @param image
     *            the image to set
     */
    public void setImage(String image) {
        this.image = image;
    }

    /**
     * @return the priority
     */
    public Priority getPriority() {
        return priority;
    }

    /**
     * @param priority
     *            the priority
     */
    public void setPriority(Priority priority) {
        this.priority = priority;
    }

    /**
     * @return the action script name
     */
    public String getAction() {
        return action;
    }

    /**
     * @param action
     *            the action script name
     */
    public void setAction(String action) {
        this.action = action;
    }

    /**
     * @param foreground
     *            the foreground color for the text display
     */
    public void setForeground(RGB foreground) {
        this.foreground = foreground;
    }

    /**
     * @param background
     *            the background color for the text display
     */
    public void setBackground(RGB background) {
        this.background = background;
    }

    /**
     * @return the foreground color for the text display
     */
    public RGB getForeground() {
        return foreground;
    }

    /**
     * @return the background color for the text display
     */
    public RGB getBackground() {
        return background;
    }

    /**
     * @return the audio file name
     */
    public String getAudioFile() {
        return audioFile;
    }

    /**
     * @param audioFile
     *            the audio file name
     */
    public void setAudioFile(String audioFile) {
        this.audioFile = audioFile;
    }

    /**
     *
     * Used by serialization only
     *
     * @return the foreground
     */
    public String getForegroundSerialized() {
        return toString(foreground);
    }

    /**
     *
     * Used by serialization only
     *
     * @param foreground
     *            the foreground to set
     */
    @XmlAttribute(name = "foreground", required = true)
    public void setForegroundSerialized(String foreground) {
        this.foreground = RGBColors.getRGBColor(foreground);
    }

    /**
     * Used by serialization only
     *
     * @return the background
     */
    public String getBackgroundSerialized() {
        return toString(background);
    }

    /**
     * Used by serialization only
     *
     * @param background
     *            the background to set
     */
    @XmlAttribute(name = "background", required = true)
    public void setBackgroundSerialized(String background) {
        this.background = RGBColors.getRGBColor(background);
    }

    private static String toString(RGB rgb) {
        if (rgb == null) {
            return "";
        }

        return RGBColors.getColorName(rgb);
    }

    @Override
    public AlertMetadata clone() {
        AlertMetadata am = new AlertMetadata(this);
        return am;
    }
}
