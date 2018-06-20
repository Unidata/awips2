package com.raytheon.viz.ghg.monitor.data;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.RGBColors;

/**
 * This class containing RGB foreground and background colors.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 25, 2008 1033       lvenable    Initial creation
 * Feb 05, 2016 5316       randerso    Changed to use RGBColors to allow use of 
 *                                     color names not just hex representations
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class GhgColorData {
    /**
     * Foreground color.
     */
    private RGB foregroundRgb;

    /**
     * Background color.
     */
    private RGB backgroundRgb;

    /**
     * Constructor.
     * 
     * @param foreground
     *            Foreground color.
     * @param background
     *            Background color.
     */
    public GhgColorData(RGB foreground, RGB background) {
        foregroundRgb = foreground;
        backgroundRgb = background;
    }

    /**
     * Constructor.
     */
    public GhgColorData() {
        foregroundRgb = new RGB(0, 0, 0);
        backgroundRgb = new RGB(255, 255, 255);
    }

    /**
     * Set the foreground color.
     * 
     * @param rgb
     *            RGB foreground color.
     */
    public void setForegroundRgb(RGB rgb) {
        foregroundRgb = rgb;
    }

    /**
     * Set the background color.
     * 
     * @param rgb
     *            RGB background color.
     */
    public void setBackgroundRgb(RGB rgb) {
        backgroundRgb = rgb;
    }

    /**
     * Get the foreground color.
     * 
     * @return RGB foreground color.
     */
    public RGB getForegroundRgb() {
        return foregroundRgb;
    }

    /**
     * Get the background color.
     * 
     * @return RGB background color.
     */
    public RGB getBackgroundRgb() {
        return backgroundRgb;
    }

    @XmlAttribute
    public String getForegroundRgbAsString() {
        return RGBColors.getColorName(foregroundRgb);
    }

    public void setForegroundRgbAsString(String colorName) {
        foregroundRgb = RGBColors.getRGBColor(colorName);
    }

    @XmlAttribute
    public String getBackgroundRgbAsString() {
        return RGBColors.getColorName(backgroundRgb);
    }

    public void setBackgroundRgbAsString(String colorName) {
        backgroundRgb = RGBColors.getRGBColor(colorName);
    }
}
