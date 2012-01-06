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
package com.raytheon.uf.common.monitor.xml;

import java.util.ArrayList;
import java.util.HashMap;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

import com.raytheon.uf.common.monitor.config.FFFGConfig.GuidSectType;
import com.raytheon.uf.common.monitor.config.ValueNameIdData;
import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * 
 * This class holds the Source XML data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 10, 2010 #4517      lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class FFFGSourceXML implements ISerializableObject {
    @XmlElement(name = "SourceName")
    private String sourceName;

    @XmlElement(name = "AreaFFG")
    private String areaFFGValue;

    @XmlElements({ @XmlElement(name = "SourceItem", type = FFFGSourceItemXML.class) })
    private ArrayList<FFFGSourceItemXML> sourceItems;

    @XmlElements({ @XmlElement(name = "BasinId", type = FFFGBasinIdXML.class) })
    private ArrayList<FFFGBasinIdXML> basinList = new ArrayList<FFFGBasinIdXML>();

    private HashMap<Long, FFFGSourceItemXML> idMap;

    private HashMap<Long, FFFGBasinIdXML> basinMap;

    public FFFGSourceXML() {
    }

    public String getSourceName() {
        return sourceName;
    }

    public void setSourceName(String sourceName) {
        this.sourceName = sourceName;
    }

    public String getAreaFFGValue() {
        return areaFFGValue;
    }

    public void setAreaFFGValue(String areaFFGValue) {
        this.areaFFGValue = areaFFGValue;
    }

    public ArrayList<FFFGSourceItemXML> getSourceItems() {
        return sourceItems;
    }

    public void setSourceItems(ArrayList<FFFGSourceItemXML> sourceItems) {
        this.sourceItems = sourceItems;
    }

    /**
     * Get all of the Basin and County information.
     * 
     * @return Array of ValueNameIdData containing both basin and county data.
     */
    public ArrayList<ValueNameIdData> getCountyBasinData() {
        ArrayList<ValueNameIdData> dataArray = new ArrayList<ValueNameIdData>();

        if (sourceItems == null) {
            return dataArray;
        }

        ValueNameIdData newData = null;
        GuidSectType type;

        for (FFFGSourceItemXML siXML : sourceItems) {
            try {
                type = GuidSectType.valueOf(siXML.getType().toUpperCase());
                newData = new ValueNameIdData(siXML.getValue(),
                        siXML.getName(), siXML.getId(), type);
                dataArray.add(newData);
            } catch (Exception ex) {
                System.out.println("Could not get the Guidance Section Type.");
                ex.printStackTrace();
            }
        }

        return dataArray;
    }

    /**
     * Check if the ID has associated data.
     * 
     * @param id
     *            ID.
     * @return True if the ID is present and has data.
     */
    public boolean containsID(Long id) {
        verifyIdMap();
        if (idMap.containsKey(id)) {
            return true;
        }
        
        verifyBasinMap();
        if (basinMap.containsKey(id)) {
            return true;
        }
        
        return false;
    }

    /**
     * Get the value of the ID passed in.
     * 
     * @param id
     *            ID.
     * @return Value.
     */
    public String getAssociatedValue(Long id) {
        verifyIdMap();

        if (idMap.containsKey(id) == true) {
            return idMap.get(id).getValue();
        }
        
        verifyBasinMap();
        if (basinMap.containsKey(id)) {
            return basinMap.get(id).getValue();
        }
        
        /*
         * In case something (really) weird happens, return "+0.0". This is a
         * fail-safe that will have no effect on the value being adjusted.
         */
        return "+0.0";
    }

    /**
     * Verify the ID map exists. If not then populate the map with the ID and
     * XML.
     */
    private void verifyIdMap() {
        if (idMap == null) {
            idMap = new HashMap<Long, FFFGSourceItemXML>();

            if (sourceItems != null) {
                for (FFFGSourceItemXML siXML : sourceItems) {
                    idMap.put(siXML.getId(), siXML);
                }
            }
        }
    }

    private void verifyBasinMap() {
        if (basinMap == null) {
            basinMap = new HashMap<Long, FFFGBasinIdXML>();

            if (this.basinList != null) {
                for (FFFGBasinIdXML basin: basinList) {
                    basinMap.put(basin.getBasinId(), basin);
                }
            }
        }
    }

    /**
     * @return the basinList
     */
    public ArrayList<FFFGBasinIdXML> getBasinList() {
        return basinList;
    }

    /**
     * @param basinList
     *            the basinList to set
     */
    public void setBasinList(ArrayList<FFFGBasinIdXML> basinList) {
        this.basinList = basinList;
    }

    public void addBasin(FFFGBasinIdXML basin) {
        if (!basinList.contains(basin)) {
            this.basinList.add(basin);
        }
    }
}
