package com.raytheon.viz.ghg.monitor.data;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.swt.graphics.RGB;

/**
 * This class containing RGB foreground and background colors.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 25 MAR 2008  1033       lvenable    Initial creation
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
        String result = String.format("#%02x%02x%02x", foregroundRgb.red,
                foregroundRgb.green, foregroundRgb.blue);
        return result;
    }

    public void setForegroundRgbAsString(String stringRep) {
        foregroundRgb.red = Integer.parseInt(stringRep.substring(1, 3), 16);
        foregroundRgb.green = Integer.parseInt(stringRep.substring(3, 5), 16);
        foregroundRgb.blue = Integer.parseInt(stringRep.substring(5), 16);
    }

    @XmlAttribute
    public String getBackgroundRgbAsString() {
        String result = String.format("#%02x%02x%02x", backgroundRgb.red,
                backgroundRgb.green, backgroundRgb.blue);
        return result;
    }

    public void setBackgroundRgbAsString(String stringRep) {
        backgroundRgb.red = Integer.parseInt(stringRep.substring(1, 3), 16);
        backgroundRgb.green = Integer.parseInt(stringRep.substring(3, 5), 16);
        backgroundRgb.blue = Integer.parseInt(stringRep.substring(5), 16);
    }
}
