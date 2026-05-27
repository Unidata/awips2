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

import com.raytheon.uf.viz.alertview.Alert;

/**
 * 
 * Styles that apply to an {@link Alert}.
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
public class AlertStyle {

    private String filter;

    private String backgroundColor;

    private String foregroundColor;

    private String fontName;

    private String fontStyle;

    private Integer fontSize;

    public AlertStyle() {

    }

    public AlertStyle(AlertStyle other) {
        this.filter = other.filter;
        this.backgroundColor = other.backgroundColor;
        this.foregroundColor = other.foregroundColor;
    }

    public String getFilter() {
        return filter;
    }

    public void setFilter(String filter) {
        this.filter = filter;
    }

    public String getBackgroundColor() {
        return backgroundColor;
    }

    public void setBackgroundColor(String backgroundColor) {
        this.backgroundColor = backgroundColor;
    }

    public String getForegroundColor() {
        return foregroundColor;
    }

    public void setForegroundColor(String foregroundColor) {
        this.foregroundColor = foregroundColor;
    }

    public String getFontName() {
        return fontName;
    }

    public void setFontName(String fontName) {
        this.fontName = fontName;
    }

    public String getFontStyle() {
        return fontStyle;
    }

    public void setFontStyle(String fontStyle) {
        this.fontStyle = fontStyle;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((backgroundColor == null) ? 0 : backgroundColor.hashCode());
        result = prime * result + ((filter == null) ? 0 : filter.hashCode());
        result = prime * result
                + ((fontName == null) ? 0 : fontName.hashCode());
        result = prime * result
                + ((fontSize == null) ? 0 : fontSize.hashCode());
        result = prime * result
                + ((fontStyle == null) ? 0 : fontStyle.hashCode());
        result = prime * result
                + ((foregroundColor == null) ? 0 : foregroundColor.hashCode());
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
        AlertStyle other = (AlertStyle) obj;
        if (backgroundColor == null) {
            if (other.backgroundColor != null)
                return false;
        } else if (!backgroundColor.equals(other.backgroundColor))
            return false;
        if (filter == null) {
            if (other.filter != null)
                return false;
        } else if (!filter.equals(other.filter))
            return false;
        if (fontName == null) {
            if (other.fontName != null)
                return false;
        } else if (!fontName.equals(other.fontName))
            return false;
        if (fontSize == null) {
            if (other.fontSize != null)
                return false;
        } else if (!fontSize.equals(other.fontSize))
            return false;
        if (fontStyle == null) {
            if (other.fontStyle != null)
                return false;
        } else if (!fontStyle.equals(other.fontStyle))
            return false;
        if (foregroundColor == null) {
            if (other.foregroundColor != null)
                return false;
        } else if (!foregroundColor.equals(other.foregroundColor))
            return false;
        return true;
    }

    public Integer getFontSize() {
        return fontSize;
    }

    public void setFontSize(Integer fontSize) {
        this.fontSize = fontSize;
    }


}
