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
package com.raytheon.uf.viz.alertview.prefs;

import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.viz.alertview.Alert.Priority;
import com.raytheon.uf.viz.alertview.ui.popup.AlertPopup;

/**
 * Contains the preferences that control how {@link AlertPopup} appears to the
 * user.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Jun 17, 2015  4474     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlRootElement
public class PopUpPreferences {

    public static enum PopUpCorner {
        UPPER_LEFT, UPPER_RIGHT, LOWER_LEFT, LOWER_RIGHT;

        public String getPrettyName() {
            String name = name();
            String[] words = name.split("_");
            StringBuilder pretty = new StringBuilder(name.length());
            for (String word : words) {
                if (pretty.length() > 0) {
                    pretty.append(' ');
                }
                pretty.append(word.charAt(0));
                pretty.append(word.substring(1).toLowerCase());
            }
            return pretty.toString();
        }

        public static PopUpCorner fromPrettyName(String corner) {
            return valueOf(corner.replace(' ', '_').toUpperCase());
        }
    }

    private String filter;

    /* Time in ms. */
    private int duration;

    private PopUpCorner corner;

    private int width;

    private int height;

    public PopUpPreferences() {
        /* Everything needs reasonable defaults to keep PreferenceFile happy. */
        filter = Priority.ERROR.name().toLowerCase();
        duration = 5000;
        corner = PopUpCorner.LOWER_RIGHT;
        width = 500;
        height = 50;
    }

    public PopUpPreferences(PopUpPreferences other) {
        this.filter = other.getFilter();
        this.duration = other.getDuration();
        this.corner = other.getCorner();
        this.width = other.getWidth();
        this.height = other.getHeight();
    }

    public String getFilter() {
        return filter;
    }

    public void setFilter(String filter) {
        this.filter = filter;
    }

    public int getDuration() {
        return duration;
    }

    public void setDuration(int duration) {
        this.duration = duration;
    }

    public PopUpCorner getCorner() {
        return corner;
    }

    public void setCorner(PopUpCorner corner) {
        this.corner = corner;
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

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((corner == null) ? 0 : corner.hashCode());
        result = prime * result + duration;
        result = prime * result + ((filter == null) ? 0 : filter.hashCode());
        result = prime * result + height;
        result = prime * result + width;
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        PopUpPreferences other = (PopUpPreferences) obj;
        if (corner != other.corner)
            return false;
        if (duration != other.duration)
            return false;
        if (filter == null) {
            if (other.filter != null)
                return false;
        } else if (!filter.equals(other.filter))
            return false;
        if (height != other.height)
            return false;
        if (width != other.width)
            return false;
        return true;
    }

    public static PreferenceFile<PopUpPreferences> load(
            PreferenceFile.Listener<? super PopUpPreferences> listener) {
        return new PreferenceFile<>("alert_popup.xml", PopUpPreferences.class,
                listener);
    }

}
