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

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.swt.graphics.RGB;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 16, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class SiteColorInformation {

    @XmlElement
    List<SiteColor> colors;

    /**
     * @return the colors
     */
    public List<SiteColor> getColors() {
        return colors;
    }

    /**
     * @param colors
     *            the colors to set
     */
    public void setColors(List<SiteColor> colors) {
        this.colors = colors;
    }

    @XmlAccessorType(XmlAccessType.NONE)
    public static class SiteColor {

        @XmlAttribute
        private String site;

        @XmlAttribute
        private int red;

        @XmlAttribute
        private int green;

        @XmlAttribute
        private int blue;

        public SiteColor() {

        }

        /**
         * @return the site
         */
        public String getSite() {
            return site;
        }

        /**
         * @param site
         *            the site to set
         */
        public void setSite(String site) {
            this.site = site;
        }

        /**
         * @return the red
         */
        public int getRed() {
            return red;
        }

        /**
         * @param red
         *            the red to set
         */
        public void setRed(int red) {
            this.red = red;
        }

        /**
         * @return the green
         */
        public int getGreen() {
            return green;
        }

        /**
         * @param green
         *            the green to set
         */
        public void setGreen(int green) {
            this.green = green;
        }

        /**
         * @return the blue
         */
        public int getBlue() {
            return blue;
        }

        /**
         * @param blue
         *            the blue to set
         */
        public void setBlue(int blue) {
            this.blue = blue;
        }

        public RGB getColor() {
            return new RGB(red, green, blue);
        }

        public void setColor(RGB rgb) {
            red = rgb.red;
            green = rgb.green;
            blue = rgb.blue;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            if (obj instanceof SiteColor == false) {
                return false;
            } else {
                return this.getSite().equals(((SiteColor) obj).getSite());
            }
        }
    }
}
