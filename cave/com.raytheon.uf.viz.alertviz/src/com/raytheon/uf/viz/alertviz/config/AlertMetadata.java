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

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Provides customizable preferences for an alert message
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 9, 2008  1433       chammack    Initial creation
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class AlertMetadata implements ISerializableObject {

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
    private RGB foreground = new RGB(0, 0, 0);

    /**
     * The color of the background of the text box
     */
    private RGB background = new RGB(255, 255, 255);

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

    /**
     * What is the priority?
     */
    @XmlAttribute(required = true)
    private Priority priority;

    /**
     * Is the message sent to the log?
     */
    @XmlAttribute
    private boolean log = true;

    /**
     * Is the python script execution enabled?
     */
    @XmlAttribute
    private boolean pythonEnabled = false;

    /**
     * The python script to run on receipt
     */
    @XmlAttribute
    private String pythonScript;

    public AlertMetadata() {

    }

    public AlertMetadata(String audioFile, RGB foreground, RGB background,
            Boolean text, Boolean blink, Boolean popup, Boolean log,
            Boolean audioEnabled, Priority priority, String pythonScript) {
        super();
        this.audioFile = audioFile;
        this.foreground = foreground;
        this.background = background;
        this.text = text;
        this.blink = blink;
        this.popup = popup;
        this.log = log;
        this.audioEnabled = audioEnabled;
        this.priority = priority;
        this.pythonScript = pythonScript;
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
        this.log = metadata.log;
        this.popup = metadata.popup;
        this.priority = metadata.priority;
        this.text = metadata.text;
        this.pythonScript = metadata.pythonScript;
    }

    /**
     * audio enabled
     * 
     * @return
     */
    public boolean isAudioEnabled() {
        return audioEnabled;
    }

    /**
     * audio enabled
     * 
     * @param audioEnabled
     */
    public void setAudioEnabled(boolean audioEnabled) {
        this.audioEnabled = audioEnabled;
    }

    /**
     * Is Text
     * 
     * @return
     */
    public boolean isText() {
        return text;
    }

    /**
     * Set text
     * 
     * @param text
     */
    public void setText(boolean text) {
        this.text = text;
    }

    /**
     * Blink?
     * 
     * @return
     */
    public boolean isBlink() {
        return blink;
    }

    /**
     * Set blink
     * 
     * @param blink
     */
    public void setBlink(boolean blink) {
        this.blink = blink;
    }

    /**
     * popup?
     * 
     * @return
     */
    public boolean isPopup() {
        return popup;
    }

    /**
     * set popup
     * 
     * @param popup
     */
    public void setPopup(boolean popup) {
        this.popup = popup;
    }

    /**
     * Get priority
     * 
     * @return
     */
    public Priority getPriority() {
        return priority;
    }

    /**
     * Set priority
     * 
     * @param priority
     */
    public void setPriority(Priority priority) {
        this.priority = priority;
    }

    /**
     * is logging
     * 
     * @return
     */
    public boolean isLog() {
        return log;
    }

    /**
     * Set log
     * 
     * @param log
     */
    public void setLog(boolean log) {
        this.log = log;
    }

    /**
     * is python enabled
     * 
     * @return
     */
    public boolean isPythonEnabled() {
        return pythonEnabled;
    }

    /**
     * Set python enabled
     * 
     * @param pythonEnabled
     */
    public void setPythonEnabled(boolean pythonEnabled) {
        this.pythonEnabled = pythonEnabled;
    }

    /**
     * Get the script
     * 
     * @return
     */
    public String getPythonScript() {
        return pythonScript;
    }

    /**
     * Set the script
     * 
     * @param pythonScript
     */
    public void setPythonScript(String pythonScript) {
        this.pythonScript = pythonScript;
    }

    /**
     * Set the foreground color
     * 
     * @param foreground
     */
    public void setForeground(RGB foreground) {
        this.foreground = foreground;
    }

    /**
     * Set the background color
     * 
     * @param background
     */
    public void setBackground(RGB background) {
        this.background = background;
    }

    public RGB getForeground() {
        return foreground;
    }

    public RGB getBackground() {
        return background;
    }

    public String getAudioFile() {
        return audioFile;
    }

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
        this.foreground = toRGB(foreground);
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
        this.background = toRGB(background);
    }

    private static String toString(RGB rgb) {
        if (rgb == null)
            return "";

        String red = Integer.toHexString(rgb.red);
        String green = Integer.toHexString(rgb.green);
        String blue = Integer.toHexString(rgb.blue);
        if (red.length() == 1)
            red = "0" + red;
        if (green.length() == 1)
            green = "0" + green;
        if (blue.length() == 1)
            blue = "0" + blue;

        return "#" + red + green + blue;
    }

    private static RGB toRGB(String string) {
        if (string.equals(""))
            return null;

        String str = string.substring(1);
        String redStr = str.substring(0, 2);
        String greenStr = str.substring(2, 4);
        String blueStr = str.substring(4, 6);

        int red = Integer.decode("#" + redStr);
        int green = Integer.decode("#" + greenStr);
        int blue = Integer.decode("#" + blueStr);

        return new RGB(red, green, blue);
    }

    public AlertMetadata clone() {
        AlertMetadata am = new AlertMetadata();
        am.audioEnabled = audioEnabled;
        am.audioFile = audioFile;
        am.background = background;
        am.blink = blink;
        am.foreground = foreground;
        am.log = log;
        am.popup = popup;
        am.priority = priority;
        am.pythonEnabled = pythonEnabled;
        am.pythonScript = pythonScript;
        am.text = text;
        return am;
    }
}
