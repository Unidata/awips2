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
package com.raytheon.viz.aviation.xml;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * 
 * This class contains the label menu information.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 7, 2009  2537       lvenable     Initial creation
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class LabelMenu implements ISerializableObject
{
    /**
     * Menu name.
     */
    @XmlAttribute(name = "menuName")
    private String menuName;
    
    /**
     * Value.
     */
    @XmlAttribute(name = "value")
    private String value;
    
    /**
     * Constructor.
     */
    public LabelMenu()
    {        
    }

    /**
     * Get the menu name.
     * @return The menu name.
     */
    public String getMenuName()
    {
        return menuName;
    }

    /**
     * Set the menu name.
     * @param menuName The menu name.
     */
    public void setMenuName(String menuName)
    {
        this.menuName = menuName;
    }

    /**
     * Get the value.
     * @return The value.
     */
    public String getValue()
    {
        return value;
    }

    /**
     * Set the value.
     * @param value The value.
     */
    public void setValue(String value)
    {
        this.value = value;
    }
}
