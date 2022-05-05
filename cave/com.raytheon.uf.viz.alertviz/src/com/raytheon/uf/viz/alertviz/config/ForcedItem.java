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

import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.RGBColors;

/**
 *
 * Describes a single forced AlertViz setting
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 26, 2010  2459     dfriedma  Initial creation
 * Sep 25, 2018  7486     randerso  Allow color names or hex strings for
 *                                  foreground and background. Code cleanup.
 * Nov 02, 2018  7600     randerso  Changes to support standard script files for
 *                                  AlertViz actions.
 * Nov 13, 2018  7512     randerso  Moved AlertViz audio files
 *
 * </pre>
 *
 * @author dfriedma
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
    private boolean audioEnabled;

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
     * The action script to run on receipt
     */
    @XmlAttribute
    private String action;

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
    public boolean getAudioEnabled() {
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
     * @return the action
     */
    public String getAction() {
        return action;
    }

    /**
     * @param action
     *            the action to set
     */
    public void setAction(String action) {
        this.action = action;
    }

    /**
     * @return the foreground
     */
    @XmlAttribute(name = "foreground")
    public String getForegroundAsString() {
        return RGBColors.getColorName(foreground);
    }

    /**
     * @param foreground
     *            the foreground to set
     */
    public void setForegroundAsString(String foreground) {
        this.foreground = RGBColors.getRGBColor(foreground);
    }

    /**
     * @return the background
     */
    @XmlAttribute(name = "background")
    public String getBackgroundAsString() {
        return RGBColors.getColorName(background);
    }

    /**
     * @param background
     *            the background to set
     */
    public void setBackgroundAsString(String background) {
        this.background = RGBColors.getRGBColor(background);
    }

    /**
     * @return null if audioEnabled is null, else audioFile if audioFile is not
     *         null, else true if audioEnabled, else false
     */
    @XmlAttribute(name = "audio")
    public String getAudioAsString() {
        if (audioEnabled) {
            if (audioFile != null) {
                return audioFile;
            } else {
                return Boolean.toString(audioEnabled);
            }
        } else {
            return null;
        }
    }

    /**
     * @param audio
     *            true, false or audio file name
     */
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
            audioEnabled = false;
            audioFile = null;
        }
    }

    /**
     * Override AlertMetaData settings with forced settings
     *
     * @param input
     *            original AlertMetaData
     * @return the overridden AlertMetaData
     */
    public AlertMetadata applyForcedSettings(AlertMetadata input) {
        AlertMetadata result = input.clone();
        if (text != null) {
            result.setText(text);
        }
        if (blink != null) {
            result.setBlink(blink);
        }
        if (popup != null) {
            result.setPopup(popup);
        }
        if (audioEnabled) {
            result.setAudioEnabled(audioEnabled);
            result.setAudioFile(getFullAudioFilePath());
        }
        if (foreground != null) {
            result.setForeground(foreground);
        }
        if (background != null) {
            result.setBackground(background);
        }
        if (action != null) {
            result.setAction(action);
        }
        return result;
    }

    private String getFullAudioFilePath() {
        File f = null;
        if (audioFile != null) {
            f = new File(audioFile);
            if (!f.isAbsolute()) {
                f = PathManagerFactory.getPathManager().getStaticFile(
                        LocalizationUtil.join("alertViz", "audio", audioFile));
            }
        }
        if (f != null) {
            return f.getPath();
        } else {
            // Will result in system beep
            return null;
        }
    }
}
