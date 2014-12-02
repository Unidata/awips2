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
package com.raytheon.uf.viz.collaboration.ui;

import java.util.Map;

import javax.swing.plaf.synth.ColorType;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.swt.graphics.RGB;

/**
 * Contains foreground and background chat colors for a list of users or sites
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 13, 2014 3709       mapeters    Initial creation.
 * Nov 26, 2014 3709       mapeters    Renamed from UserColorInformation, added fgSet getter.
 * 
 * </pre>
 * 
 * @author mapeters
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class ColorInfoMap {

    @XmlElement
    private Map<String, ColorInfo> colors;

    /**
     * @return the colors
     */
    public Map<String, ColorInfo> getColors() {
        return colors;
    }

    /**
     * @param colors
     *            the colors to set
     */
    public void setColors(Map<String, ColorInfo> colors) {
        this.colors = colors;
    }

    @XmlAccessorType(XmlAccessType.NONE)
    public static class ColorInfo {

        /**
         * tells {@link #setColor()} when to use defaultForeground
         */
        @XmlAttribute
        private boolean fgSet;

        @XmlAttribute
        private int fgRed;

        @XmlAttribute
        private int fgGreen;

        @XmlAttribute
        private int fgBlue;

        /**
         * background should default to white
         */
        @XmlAttribute
        private int bgRed = 255;

        @XmlAttribute
        private int bgGreen = 255;

        @XmlAttribute
        private int bgBlue = 255;

        public ColorInfo() {
        }

        /**
         * @param type
         * @return the red
         */
        public int getRed(ColorType type) {
            return type == ColorType.FOREGROUND ? fgRed : bgRed;
        }

        /**
         * @param type
         * @param red
         *            the red to set
         */
        public void setRed(ColorType type, int red) {
            if (type == ColorType.FOREGROUND) {
                this.fgRed = red;
            } else {
                this.bgRed = red;
            }
        }

        /**
         * @param type
         * @return the green
         */
        public int getGreen(ColorType type) {
            return type == ColorType.FOREGROUND ? fgGreen : bgGreen;
        }

        /**
         * @param type
         * @param green
         *            the green to set
         */
        public void setGreen(ColorType type, int green) {
            if (type == ColorType.FOREGROUND) {
                this.fgGreen = green;
            } else {
                this.bgGreen = green;
            }
        }

        /**
         * @param type
         * @return the blue
         */
        public int getBlue(ColorType type) {
            return type == ColorType.FOREGROUND ? fgBlue : bgBlue;
        }

        /**
         * @param type
         * @param blue
         *            the blue to set
         */
        public void setBlue(ColorType type, int blue) {
            if (type == ColorType.FOREGROUND) {
                this.fgBlue = blue;
            } else {
                this.bgBlue = blue;
            }
        }

        /**
         * @param type
         * @return the RGB color of the given type
         */
        public RGB getColor(ColorType type) {
            if (type == ColorType.FOREGROUND) {
                return new RGB(fgRed, fgGreen, fgBlue);
            } else {
                return new RGB(bgRed, bgGreen, bgBlue);
            }
        }

        /**
         * Set the color of the given type to the given rgb
         * 
         * @param type
         * @param rgb
         * @param defaultForeground
         */
        public void setColor(ColorType type, RGB rgb, RGB defaultForeground) {
            if (type == ColorType.FOREGROUND) {
                fgRed = rgb.red;
                fgGreen = rgb.green;
                fgBlue = rgb.blue;
                fgSet = true;
            } else {
                bgRed = rgb.red;
                bgGreen = rgb.green;
                bgBlue = rgb.blue;
                if (!fgSet) {
                    /*
                     * if creating new UserColor, set fgColor to default
                     * foreground color, otherwise it defaults to black
                     */
                    setColor(ColorType.FOREGROUND, defaultForeground, null);
                    fgSet = false;
                }
            }
        }

        /**
         * @return whether the foreground has been set
         */
        public boolean isForegroundSet() {
            return fgSet;
        }
    }
}
