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
package com.raytheon.uf.viz.collaboration.display.data;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import org.eclipse.swt.graphics.RGB;

/**
 * Color information for a collaboration user which is used when styling text
 * and telestration
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 12, 2015 3709       bclement     Initial creation, moved from ColorInfoMap
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class UserColorInfo {

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

    /**
     * 
     */
    public UserColorInfo() {
    }

    /**
     * @param foreground
     */
    public UserColorInfo(RGB foreground) {
        this(foreground, new RGB(255, 255, 255));
    }

    /**
     * @param foreground
     * @param background
     */
    public UserColorInfo(RGB foreground, RGB background) {
        setForeground(foreground);
        setBackground(background);
    }

    /**
     * @return the background
     */
    public RGB getBackground() {
        return new RGB(bgRed, bgGreen, bgBlue);
    }

    /**
     * @param background
     *            the background to set
     */
    public void setBackground(RGB background) {
        if (background != null) {
            this.bgRed = background.red;
            this.bgGreen = background.green;
            this.bgBlue = background.blue;
        }
    }

    /**
     * @return the foreground
     */
    public RGB getForeground() {
        return new RGB(fgRed, fgGreen, fgBlue);
    }

    /**
     * @param foreground
     *            the foreground to set
     */
    public void setForeground(RGB foreground) {
        if (foreground != null) {
            this.fgRed = foreground.red;
            this.fgGreen = foreground.green;
            this.fgBlue = foreground.blue;
        }
    }

}
