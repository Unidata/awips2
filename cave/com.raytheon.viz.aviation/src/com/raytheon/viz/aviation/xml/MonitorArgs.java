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
 * This class contains the option arguments for the monitor.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 7, 2009  2537      lvenable     Initial creation
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class MonitorArgs implements ISerializableObject
{
    /**
     * Argument name.
     */
    @XmlAttribute(name = "name")
    private String name;
    
    /**
     * Argument value.
     */
    @XmlAttribute(name = "value")
    private String value;
    
    /**
     * Constructor.
     */
    public MonitorArgs()
    {        
    }

    /**
     * Get the argument name.
     * @return The argument name.
     */
    public String getName()
    {
        return name;
    }

    /**
     * Set the argument name.
     * @param name The argument name.
     */
    public void setName(String name)
    {
        this.name = name;
    }

    /**
     * Get the argument value.
     * @return The argument value.
     */
    public String getValue()
    {
        return value;
    }

    /**
     * Set the argument value.
     * @param value The argument value.
     */
    public void setValue(String value)
    {
        this.value = value;
    }
}
