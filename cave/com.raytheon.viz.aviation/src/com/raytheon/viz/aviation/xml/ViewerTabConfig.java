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
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * 
 * Class containing configuration data for a viewer tab (read in from XML).
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 3, 2009            lvenable     Initial creation
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class ViewerTabConfig implements ISerializableObject
{
    /**
     * Label name.
     */
    @XmlElement(name = "LabelName")
    private String labelName;
    
    /**
     * Name of the class to be instantiated.
     */
    @XmlElement(name = "ClassName")
    private String className;
    
    /**
     * Name of the model.
     */
    @XmlElement(name = "ModelName")
    private String modelName;
    
    /**
     * Constructor.
     */
    public ViewerTabConfig()
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
     * Get the class name;
     * @return The class name;
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
     * Get the model name.
     * @return The model name.
     */
    public String getModelName()
    {
        return modelName;
    }

    /**
     * Set the model name.
     * @param modelName The model name.
     */
    public void setModelName(String modelName)
    {
        this.modelName = modelName;
    }
}
