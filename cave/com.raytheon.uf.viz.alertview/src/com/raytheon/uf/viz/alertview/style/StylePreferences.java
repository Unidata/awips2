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
package com.raytheon.uf.viz.alertview.style;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.alertview.Alert.Priority;
import com.raytheon.uf.viz.alertview.prefs.PreferenceFile;

/**
 * 
 * JAXB serializable list of {@link AlertStyle}s.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Jun 18, 2015  4474     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class StylePreferences {

    @XmlElement(name = "style")
    private List<AlertStyle> styles = new ArrayList<>();

    public StylePreferences() {
        AlertStyle errorStyle = new AlertStyle();
        errorStyle.setFilter(Priority.ERROR.name().toLowerCase());
        errorStyle.setForegroundColor(StyleManager.formatColor(new RGB(255, 0,
                0)));
        styles.add(errorStyle);
    }

    public StylePreferences(List<AlertStyle> styles) {
        this.styles = styles;
    }

    public List<AlertStyle> getStyles() {
        return styles;
    }

    public void setStyles(List<AlertStyle> styles) {
        this.styles = styles;
    }

    public void addStyle(AlertStyle style) {
        if (styles == null) {
            styles = new ArrayList<AlertStyle>();
        }
        styles.add(style);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((styles == null) ? 0 : styles.hashCode());
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
        StylePreferences other = (StylePreferences) obj;
        if (styles == null) {
            if (other.styles != null)
                return false;
        } else if (!styles.equals(other.styles))
            return false;
        return true;
    }

    public static PreferenceFile<StylePreferences> load(
            PreferenceFile.Listener<? super StylePreferences> listener) {
        return new PreferenceFile<>("alert_styles.xml", StylePreferences.class,
                listener);
    }

}
