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
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Class containing the XML data specifying an array of AreaXML data.
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
@XmlRootElement(name = "Thresholds")
@XmlAccessorType(XmlAccessType.NONE)
public class ThresholdsXML implements ISerializableObject
{
    @XmlElements( { @XmlElement(name = "Area", type = AreaXML.class) })
    private ArrayList<AreaXML> areas;
    
    private HashMap<String, Integer> areaIdxMap;
    
    public ThresholdsXML()
    {        
    }

    public ArrayList<AreaXML> getAreas()
    {
        return areas;
    }

    public void setAreas(ArrayList<AreaXML> areas)
    {
        this.areas = areas;
    }
    
    public double getRedValue(String areaID, String key)
    {
        //System.out.println("AreaID = " + areaID + "\t\t Key = " + key);
        
        if (areaIdxMap == null)
        {
            createIndexMap();
        }
        
        int idx = areaIdxMap.get(areaID);
        AreaXML area = areas.get(idx);
        
        return area.getRedValue(key);
    }
    
    public void setRedValue(String areaID, String key, double value)
    {
        if (areaIdxMap == null)
        {
            createIndexMap();
        }
        
        int idx = areaIdxMap.get(areaID);
        AreaXML area = areas.get(idx);
        
        area.setRedValue(key, value);
    }
    
    public double getYellowValue(String areaID, String key)
    {
        if (areaIdxMap == null)
        {
            createIndexMap();
        }
        
        int idx = areaIdxMap.get(areaID);
        AreaXML area = areas.get(idx);
        
        return area.getYellowValue(key);
    }
    
    public void setYellowValue(String areaID, String key, double value)
    {
        if (areaIdxMap == null)
        {
            createIndexMap();
        }
        
        int idx = areaIdxMap.get(areaID);
        AreaXML area = areas.get(idx);
        
        area.setYellowValue(key, value);
    }

    private void createIndexMap()
    {
        areaIdxMap = new HashMap<String, Integer>();
        
        for (int i = 0; i < areas.size(); i++)
        {
            areaIdxMap.put(areas.get(i).getAreaId(), i);
        }
    }
    
    public boolean hasAreaId(String areaID)
    {
        if (areaIdxMap == null)
        {
            createIndexMap();            
            
            return areaIdxMap.containsKey(areaID);
        }
        
        return areaIdxMap.containsKey(areaID);
    }
}
