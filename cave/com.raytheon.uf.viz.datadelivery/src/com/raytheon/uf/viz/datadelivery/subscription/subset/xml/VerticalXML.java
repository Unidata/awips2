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
package com.raytheon.uf.viz.datadelivery.subscription.subset.xml;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

import com.raytheon.uf.viz.datadelivery.common.xml.IDisplayXml;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 29, 2012            mpduff     Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */
@XmlAccessorType(XmlAccessType.NONE)
public class VerticalXML implements IDisplayXml {

    @XmlElement(name = "layerType", type = String.class)
    protected String layerType;
    
    @XmlElements({ @XmlElement(name = "parameters", type = String.class) })
    protected ArrayList<String> parameterList = new ArrayList<String>();

    @XmlElements({ @XmlElement(name = "levels", type = String.class) })
    protected ArrayList<String> levelList = new ArrayList<String>();

    /**
     * @return the layerType
     */
    public String getLayerType() {
        return layerType;
    }

    /**
     * @param layerType the layerType to set
     */
    public void setLayerType(String layerType) {
        this.layerType = layerType;
    }

    /**
     * @return the parameterList
     */
    public ArrayList<String> getParameterList() {
        return parameterList;
    }

    /**
     * @param parameterList the parameterList to set
     */
    public void setParameterList(ArrayList<String> parameterList) {
        this.parameterList = parameterList;
    }

    /**
     * @return the levels
     */
    public ArrayList<String> getLevels() {
        return levelList;
    }

    /**
     * @param levels the levels to set
     */
    public void setLevels(ArrayList<String> levels) {
        this.levelList = levels;
    }
    
    /**
     * @param level
     */
    public void addLevel(String level) {
        this.levelList.add(level);
    }
    
    /**
     * @param parameter
     */
    public void addParameter(String parameter) {
        this.parameterList.add(parameter);
    }

    /* (non-Javadoc)
     * @see com.raytheon.uf.viz.datadelivery.common.xml.IDisplayXml#getDisplayXmlString()
     */
    @Override
    public String getDisplayXmlString() {
        final String nl = "\n";
        StringBuilder sb = new StringBuilder();
        sb.append(nl);
        sb.append("Layer Type: " + this.layerType + nl);
        if (levelList.size() > 0) {
            sb.append("  Levels: ");
            for (String level: levelList) {
                sb.append(level + " ");
            }
            sb.append(nl);
        }
        sb.append("  Parameters: ");
        for (String parameter: parameterList) {
            sb.append(parameter + " ");
        }
        
        sb.append(nl);
        
        return sb.toString();
    }
}
