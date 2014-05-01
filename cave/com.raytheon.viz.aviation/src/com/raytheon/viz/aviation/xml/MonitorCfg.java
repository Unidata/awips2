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

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Class contains information for the TAF monitor.
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
@XmlRootElement(name = "Monitor")
@XmlAccessorType(XmlAccessType.NONE)
public class MonitorCfg implements ISerializableObject
{
    /**
     * Name for the label.
     */
    @XmlElement(name = "LabelName")
    private String labelName;
    
    /**
     * Package and class name of the object to be created.
     */
    @XmlElement(name = "ClassName")
    private String className;
    
    /**
     * A single string containing a comma separated list of monitor labels.
     */
    @XmlElement(name = "MonitorLabels")
    private String monitorLabels;
    
    /**
     * A single string containing a comma separated list of monitor items.
     */
    @XmlElement(name = "MonitorItems")
    private String monitorItems;
    
    /**
     * Array of label menus.  Used for pop-up menus.
     */
    @XmlElements( { @XmlElement(name = "LabelMenu", type = LabelMenu.class) })
    private ArrayList<LabelMenu> labelMenus;
    
    /**
     * Array of additional arguments that may be needed.
     */
    @XmlElements( { @XmlElement(name = "Args", type = MonitorArgs.class) })
    private ArrayList<MonitorArgs> argsArray;
    
    /**
     * Constructor.
     */
    public MonitorCfg()
    {        
    }

    /**
     * Get the label name.
     * @return The label name.
     */
    public String getLabelName()
    {
        return labelName;
    }

    /**
     * Set the label name.
     * @param labelName The label name.
     */
    public void setLabelName(String labelName)
    {
        this.labelName = labelName;
    }

    /**
     * Get the class name.
     * @return The class name.
     */
    public String getClassName()
    {
        return className;
    }

    /**
     * Set the class name.
     * @param className The class name.
     */
    public void setClassName(String className)
    {
        this.className = className;
    }

    /**
     * Get the monitor labels.
     * @return The monitor labels.
     */
    public String getMonitorLabels()
    {
        return monitorLabels;
    }

    /**
     * Set the monitor labels.
     * @param monitorLabels The monitor labels.
     */
    public void setMonitorLabels(String monitorLabels)
    {
        this.monitorLabels = monitorLabels;
    }

    /**
     * Get the monitor items.
     * @return The monitor items.
     */
    public String getMonitorItems() {
        return monitorItems;
    }

    /**
     * Set the monitor items.
     * @param monitorItems The monitor items.
     */
    public void setMonitorItems(String monitorItems) {
        this.monitorItems = monitorItems;
    }

    /**
     * Get an array of arguments (arguments are optional).
     * @return An array of arguments.
     */
    public ArrayList<MonitorArgs> getArgsArray()
    {
        for (MonitorArgs mArgs : argsArray)
        {
            if (mArgs.getName() == null && mArgs.getValue() == null)
            {
                return null;
            }
        }
        
        return argsArray;
    }

    /**
     * Set the array of arguments.
     * @param argsArray Array of arguments.
     */
    public void setArgsArray(ArrayList<MonitorArgs> argsArray)
    {
        this.argsArray = argsArray;
    }

    /**
     * Get the label menus (label menus are optional).
     * @return An array of label menus.
     */
    public ArrayList<LabelMenu> getLabelMenus()
    {        
        for (LabelMenu lm : labelMenus)
        {
            if (lm.getMenuName() == null && lm.getValue() == null)
            {
                return null;
            }
        }
        
        return labelMenus;
    }

    /**
     * Set the label menus.
     * @param labelMenus An array of label menus.
     */
    public void setLabelMenus(ArrayList<LabelMenu> labelMenus)
    {
        this.labelMenus = labelMenus;
    }
}
