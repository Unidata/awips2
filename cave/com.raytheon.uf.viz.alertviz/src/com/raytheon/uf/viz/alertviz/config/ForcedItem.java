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

import java.io.File;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * 
 * Describes a single forced AlertViz setting
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 26, 2010 2459       dfriedma     Initial creation
 * </pre>
 * 
 * @author dfriedma
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class ForcedItem {
    /**
     * The source to match against
     */
    @XmlAttribute(required = true)
    private String sourceKey;

    /**
     * The priority to match against
     */
    @XmlAttribute(required = true)
    private Priority priority;

    /**
     * The category to match against (optional)
     */
    @XmlAttribute
    private String categoryKey;

    /**
     * The text to search for in the message (optional)
     */
    @XmlAttribute
    private String textMatch;

    /**
     * Override for the "text" setting (optional)
     */
    @XmlAttribute
    private Boolean text;

    /**
     * Override for the "blink" setting (optional)
     */
    @XmlAttribute
    private Boolean blink;

    /**
     * Override for the "popup" setting (optional)
     */
    @XmlAttribute
    private Boolean popup;

    /**
     * Override for the "audioEnabled" setting (optional)
     */
    private Boolean audioEnabled;

    /**
     * Override for the "audioFile" setting (optional)
     */
    private String audioFile;

    /**
     * Override for the "foreground" setting (optional)
     */
    private RGB foreground;

    /**
     * Override for the "background" setting (optional)
     */
    private RGB background;

    /**
     * Is the python script execution enabled?
     */
    private Boolean pythonEnabled;

    /**
     * The python script to run on receipt
     */
    private String pythonScript;

    /**
     * @return the sourceKey
     */
    public String getSourceKey() {
        return sourceKey;
    }

    /**
     * @param sourceKey
     *            the sourceKey to set
     */
    public void setSourceKey(String sourceKey) {
        this.sourceKey = sourceKey;
    }

    /**
     * @return the priority
     */
    public Priority getPriority() {
        return priority;
    }

    /**
     * @param priority
     *            the priority to set
     */
    public void setPriority(Priority priority) {
        this.priority = priority;
    }

    /**
     * @return the categoryKey
     */
    public String getCategoryKey() {
        return categoryKey;
    }

    /**
     * @param categoryKey
     *            the categoryKey to set
     */
    public void setCategoryKey(String categoryKey) {
        this.categoryKey = categoryKey;
    }

    /**
     * @return the textMatch
     */
    public String getTextMatch() {
        return textMatch;
    }

    /**
     * @param textMatch
     *            the textMatch to set
     */
    public void setTextMatch(String textMatch) {
        this.textMatch = textMatch;
    }

    /**
     * @return the text
     */
    public Boolean getText() {
        return text;
    }

    /**
     * @param text
     *            the text to set
     */
    public void setText(Boolean text) {
        this.text = text;
    }

    /**
     * @return the blink
     */
    public Boolean getBlink() {
        return blink;
    }

    /**
     * @param blink
     *            the blink to set
     */
    public void setBlink(Boolean blink) {
        this.blink = blink;
    }

    /**
     * @return the popup
     */
    public Boolean getPopup() {
        return popup;
    }

    /**
     * @param popup
     *            the popup to set
     */
    public void setPopup(Boolean popup) {
        this.popup = popup;
    }

    /**
     * @return the audioEnabled
     */
    public Boolean getAudioEnabled() {
        return audioEnabled;
    }

    /**
     * @param audioEnabled
     *            the audioEnabled to set
     */
    public void setAudioEnabled(Boolean audioEnabled) {
        this.audioEnabled = audioEnabled;
    }

    /**
     * @return the audioFile
     */
    public String getAudioFile() {
        return audioFile;
    }

    /**
     * @param audioFile
     *            the audioFile to set
     */
    public void setAudioFile(String audioFile) {
        this.audioFile = audioFile;
    }

    /**
     * @return the foreground
     */
    public RGB getForeground() {
        return foreground;
    }

    /**
     * @param foreground
     *            the foreground to set
     */
    public void setForeground(RGB foreground) {
        this.foreground = foreground;
    }

    /**
     * @return the background
     */
    public RGB getBackground() {
        return background;
    }

    /**
     * @param background
     *            the background to set
     */
    public void setBackground(RGB background) {
        this.background = background;
    }

    /**
     * @return the pythonEnabled
     */
    public Boolean getPythonEnabled() {
        return pythonEnabled;
    }

    /**
     * @param pythonEnabled
     *            the pythonEnabled to set
     */
    public void setPythonEnabled(Boolean pythonEnabled) {
        this.pythonEnabled = pythonEnabled;
    }

    /**
     * @return the pythonScript
     */
    public String getPythonScript() {
        return pythonScript;
    }

    /**
     * @param pythonScript
     *            the pythonScript to set
     */
    public void setPythonScript(String pythonScript) {
        this.pythonScript = pythonScript;
    }

    /**
     * @return the foreground
     */
    @XmlAttribute(name = "foreground")
    public String getForegroundAsString() {
        return toString(foreground);
    }

    /**
     * @param foreground
     *            the foreground to set
     */
    public void setForegroundAsString(String foreground) {
        this.foreground = toRGB(foreground);
    }

    /**
     * @return the background
     */
    @XmlAttribute(name = "background")
    public String getBackgroundAsString() {
        return toString(background);
    }

    /**
     * @param background
     *            the background to set
     */
    public void setBackgroundAsString(String background) {
        this.background = toRGB(background);
    }

    @XmlAttribute(name = "audio")
    public String getAudioAsString() {
        if (audioEnabled != null) {
            if (audioFile != null)
                return audioFile;
            else
                return audioEnabled.toString();
        } else
            return null;
    }

    public void setAudioAsString(String audio) {
        if (audio != null) {
            if (audio.equalsIgnoreCase(Boolean.TRUE.toString())) {
                audioEnabled = true;
                audioFile = null;
            } else if (audio.equalsIgnoreCase(Boolean.FALSE.toString())) {
                audioEnabled = false;
                audioFile = null;
            } else {
                audioEnabled = true;
                audioFile = audio;
            }
        } else {
            audioEnabled = null;
            audioFile = null;
        }
    }

    @XmlAttribute(name = "python")
    public String getPythonAsString() {
        if (pythonEnabled != null) {
            if (pythonScript != null)
                return pythonScript;
            else
                return pythonEnabled.toString();
        } else
            return null;
    }

    public void setPythonAsString(String python) {
        if (python != null) {
            if (python.length() == 0
                    || python.equalsIgnoreCase(Boolean.FALSE.toString())) {
                pythonEnabled = false;
                pythonScript = null;
            } else {
                pythonEnabled = true;
                pythonScript = python;
            }
        } else {
            pythonEnabled = null;
            pythonScript = null;
        }
    }

    public AlertMetadata applyForcedSettings(AlertMetadata input) {
        AlertMetadata result = input.clone();
        if (text != null)
            result.setText(text);
        if (blink != null)
            result.setBlink(blink);
        if (popup != null)
            result.setPopup(popup);
        if (audioEnabled != null) {
            result.setAudioEnabled(audioEnabled);
            result.setAudioFile(getFullAudioFilePath());
        } else if (audioFile != null) {
            result.setAudioFile(getFullAudioFilePath());
        }
        if (foreground != null)
            result.setForeground(foreground);
        if (background != null)
            result.setBackground(background);
        if (pythonEnabled != null) {
            result.setPythonEnabled(pythonEnabled);
            result.setPythonScript(pythonScript);
        } else if (pythonScript != null) {
            result.setPythonScript(pythonScript);
        }
        return result;
    }

    private static String toString(RGB rgb) {
        return rgb != null ? String.format("#%02x%02x%02x", rgb.red & 0xff,
                rgb.green & 0xff, rgb.blue & 0xff) : "";
    }

    private static RGB toRGB(String string) {
        if (string != null && string.length() > 0) {
            int v = Integer.decode(string);
            return new RGB((v >> 16) & 0xff, (v >> 8) & 0xff, v & 0xff);
        } else
            return null;
    }

    private String getFullAudioFilePath() {
        File f = null;
        if (audioFile != null) {
            f = new File(audioFile);
            if (!f.isAbsolute())
                f = PathManagerFactory.getPathManager().getStaticFile(
                        "alertVizAudio" + File.separator + audioFile);
        }
        if (f != null)
            return f.getPath();
        else
            return null; // Will result in system beep
    }
}
