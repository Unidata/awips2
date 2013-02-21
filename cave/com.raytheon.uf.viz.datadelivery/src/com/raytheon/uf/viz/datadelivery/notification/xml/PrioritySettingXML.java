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
package com.raytheon.uf.viz.datadelivery.notification.xml;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * PrioritySetting XML Object.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 8, 2012            mpduff     Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */
@XmlAccessorType(XmlAccessType.NONE)
public class PrioritySettingXML implements ISerializableObject {
//    Color, ColorNum, ColorNumName, Num, NumName
    @XmlElement(name = "color", type = Boolean.class)
    protected boolean color = false;
    
    @XmlElement(name = "colorNum", type = Boolean.class)
    protected boolean colorNum = false;
    
    @XmlElement(name = "colorNumName", type = Boolean.class)
    protected boolean colorNumName = true;
    
    @XmlElement(name = "num", type = Boolean.class)
    protected boolean num = false;
    
    @XmlElement(name = "numName", type = Boolean.class)
    protected boolean numName = false;

    /**
     * Is color dot selected flag.
     * 
     * @return
     *     true if color dot is selected
     */
    public boolean isColor() {
        return color;
    }

    /**
     * Set the color dot flag.
     * 
     * @param color
     *          true if selected
     */
    public void setColor(boolean color) {
        this.color = color;
    }

    /**
     * Is color/number selected flag.
     * 
     * @return
     *     true if color/number dot is selected
     */
    public boolean isColorNum() {
        return colorNum;
    }

    /**
     * Set the color/number flag.
     * 
     * @param colorNum
     *          true if selected
     */
    public void setColorNum(boolean colorNum) {
        this.colorNum = colorNum;
    }

    /**
     * Is color/number/name selected flag.
     * 
     * @return
     *     true if color/number/name is selected
     */
    public boolean isColorNumName() {
        return colorNumName;
    }

    /**
     * Set the color/number/name flag.
     * 
     * @param colorNumName
     *          true if selected
     */
    public void setColorNumName(boolean colorNumName) {
        this.colorNumName = colorNumName;
    }

    /**
     * Is the number only selected flag.
     * 
     * @return
     *     true if number only is selected
     */
    public boolean isNum() {
        return num;
    }

    /**
     * Set the number only flag.
     * 
     * @param num
     *          true if selected
     */
    public void setNum(boolean num) {
        this.num = num;
    }

    /**
     * Is number/name selected flag.
     * 
     * @return
     *     true if number/name is selected
     */
    public boolean isNumName() {
        return numName;
    }

    /**
     * Set the number/name flag.
     * 
     * @param numName
     *          true if selected
     */
    public void setNumName(boolean numName) {
        this.numName = numName;
    }
    
    
}
