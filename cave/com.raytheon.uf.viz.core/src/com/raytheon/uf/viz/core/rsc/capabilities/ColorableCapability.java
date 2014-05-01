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
package com.raytheon.uf.viz.core.rsc.capabilities;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.RGBColors;

/**
 * Provides colorable capabilities
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 3, 2009             chammack    Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class ColorableCapability extends AbstractCapability {

    public static final RGB DEFAULT_COLOR = new RGB(0, 255, 0);

    protected RGB color;

    public ColorableCapability() {
        color = DEFAULT_COLOR;
    }

    /**
     * @return the color
     */
    public RGB getColor() {
        return color;
    }

    /**
     * @param color
     *            the color to set
     */
    public void setColor(RGB color) {
        if (!this.color.equals(color)) {
            this.color = color;
            this.capabilityChanged();
        }
    }

    @XmlAttribute
    public String getColorAsString() {
        if (this.color == null) {
            return "";
        }

        return RGBColors.getColorName(this.color);
    }

    public void setColorAsString(String string) {
        setColor(RGBColors.getRGBColor(string));
    }

    @Override
    public AbstractCapability clone() {
        ColorableCapability cc = new ColorableCapability();
        cc.color = color;
        return cc;
    }

}
