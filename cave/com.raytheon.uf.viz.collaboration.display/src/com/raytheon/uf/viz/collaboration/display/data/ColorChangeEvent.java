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

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;

/**
 * Event for when a new user enters and a color is added
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 11, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

@DynamicSerialize
public class ColorChangeEvent {
    @DynamicSerializeElement
    private UserId userName;

    @DynamicSerializeElement
    private Integer red;

    @DynamicSerializeElement
    private Integer green;

    @DynamicSerializeElement
    private Integer blue;

    public ColorChangeEvent() {
    }

    public ColorChangeEvent(UserId user, RGB color) {
        this.userName = user;
        if (color != null) {
            red = color.red;
            green = color.green;
            blue = color.blue;
        }
    }

    /**
     * @param userName
     *            the userName to set
     */
    public void setUserName(UserId userName) {
        this.userName = userName;
    }

    /**
     * @return the userName
     */
    public UserId getUserName() {
        return userName;
    }

    /**
     * @param color
     *            the color to set
     */
    public void setColor(RGB color) {
        if (color != null) {
            red = color.red;
            green = color.green;
            blue = color.blue;
        }
    }

    /**
     * @return the color
     */
    public RGB getColor() {
        RGB color = null;
        if (red != null && green != null && blue != null) {
            color = new RGB(red, green, blue);
        }
        return color;
    }

    /**
     * @return the red
     */
    public Integer getRed() {
        return red;
    }

    /**
     * @param red
     *            the red to set
     */
    public void setRed(Integer red) {
        this.red = red;
    }

    /**
     * @return the green
     */
    public Integer getGreen() {
        return green;
    }

    /**
     * @param green
     *            the green to set
     */
    public void setGreen(Integer green) {
        this.green = green;
    }

    /**
     * @return the blue
     */
    public Integer getBlue() {
        return blue;
    }

    /**
     * @param blue
     *            the blue to set
     */
    public void setBlue(Integer blue) {
        this.blue = blue;
    }
}