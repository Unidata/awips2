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
package com.raytheon.uf.viz.monitor.xml;

import java.util.ArrayList;
import java.util.HashMap;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Class containing the XML data specifying the area ID and an array of
 * AreaThresholdXML data.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 15, 2009 #3963      lvenable     Initial creation
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class AreaXML implements ISerializableObject
{
    @XmlElement(name = "AreaID")
    private String areaId;
    
    @XmlElements( { @XmlElement(name = "AreaThreshold", type = AreaThresholdXML.class) })
    private ArrayList<AreaThresholdXML> areaThresholds;
    
    private HashMap<String, Integer> areaThreshIdxMap;
    
    public AreaXML()
    {        
    }

    public String getAreaId()
    {
        return areaId;
    }

    public void setAreaId(String areaId)
    {
        this.areaId = areaId;
    }

    public ArrayList<AreaThresholdXML> getAreaThresholds()
    {
        return areaThresholds;
    }

    public void setAreaThresholds(ArrayList<AreaThresholdXML> areaThresholds)
    {
        this.areaThresholds = areaThresholds;
    }

    public double getRedValue(String key)
    {
        if (areaThreshIdxMap == null)
        {
            createIndexMap();
        }
        
        int idx = areaThreshIdxMap.get(key);
        return areaThresholds.get(idx).getRed();
    }
    
    public void setRedValue(String key, double value)
    {
        if (areaThreshIdxMap == null)
        {
            createIndexMap();
        }
        
        int idx = areaThreshIdxMap.get(key);
        areaThresholds.get(idx).setRed(value);
    }
    
    public double getYellowValue(String key)
    {
        if (areaThreshIdxMap == null)
        {
            createIndexMap();
        }
        
        int idx = areaThreshIdxMap.get(key);
        return areaThresholds.get(idx).getYellow();
    }
    
    public void setYellowValue(String key, double value)
    {
        if (areaThreshIdxMap == null)
        {
            createIndexMap();
        }
        
        int idx = areaThreshIdxMap.get(key);
        areaThresholds.get(idx).setYellow(value);
    }
    
    public AreaThresholdXML getAreaThresholdXML(String key)
    {
        int idx = areaThreshIdxMap.get(key);
        return areaThresholds.get(idx);
    }
    
    private void createIndexMap()
    {
        areaThreshIdxMap = new HashMap<String, Integer>();
        
        for (int i = 0; i < areaThresholds.size(); i++)
        {
            areaThreshIdxMap.put(areaThresholds.get(i).getKey(), i);
        }
    }
}
