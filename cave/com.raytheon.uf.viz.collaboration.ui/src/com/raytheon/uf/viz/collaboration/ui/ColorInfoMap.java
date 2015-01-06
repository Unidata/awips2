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

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.RGB;

/**
 * Contains foreground and background chat colors for a list of users or sites
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Nov 13, 2014 3709        mapeters    Initial creation.
 * Nov 26, 2014 3709        mapeters    Renamed from UserColorInformation, added fgSet getter.
 * Dec 08, 2014 3709        mapeters    Removed fgSet and individual colors' getters/setters, 
 *                                      set foreground and background together.
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

        @XmlAttribute
        private int fgRed;

        @XmlAttribute
        private int fgGreen;

        @XmlAttribute
        private int fgBlue;

        @XmlAttribute
        private int bgRed;

        @XmlAttribute
        private int bgGreen;

        @XmlAttribute
        private int bgBlue;

        public ColorInfo() {
        }

        /**
         * @param type
         * @return the RGB color of the given type
         */
        public RGB getColor(int type) {
            if (type == SWT.FOREGROUND) {
                return new RGB(fgRed, fgGreen, fgBlue);
            } else {
                return new RGB(bgRed, bgGreen, bgBlue);
            }
        }

        /**
         * @param fg
         * @param bg
         */
        public void setColors(RGB fg, RGB bg) {
            fgRed = fg.red;
            fgGreen = fg.green;
            fgBlue = fg.blue;
            bgRed = bg.red;
            bgGreen = bg.green;
            bgBlue = bg.blue;

        }
    }
}
