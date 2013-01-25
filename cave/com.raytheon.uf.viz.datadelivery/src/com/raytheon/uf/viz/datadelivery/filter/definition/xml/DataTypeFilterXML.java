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
package com.raytheon.uf.viz.datadelivery.filter.definition.xml;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2012            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@XmlRootElement(name = "DataTypeFilterList")
@XmlAccessorType(XmlAccessType.NONE)
public class DataTypeFilterXML {
    @XmlElements({ @XmlElement(name = "DataType", type = DataTypeFilterElementXML.class) })
    protected ArrayList<DataTypeFilterElementXML> dataTypeList =
            new ArrayList<DataTypeFilterElementXML>();
    
    private Map<String, DataTypeFilterElementXML> dataTypeMap;
    
    /**
     * @return the dataTypeList
     */
    public ArrayList<DataTypeFilterElementXML> getDataTypeList() {
        return dataTypeList;
    }
    
    public DataTypeFilterElementXML getFilterData(String dataType) {
        verifyFilterTypeMap();
        return dataTypeMap.get(dataType.toUpperCase());
    }

    /**
     * @param dataTypeList
     *            the dataTypeList to set
     */
    public void setDataTypeList(ArrayList<DataTypeFilterElementXML> dataTypeList) {
        this.dataTypeList = dataTypeList;
    }

    public void addDataTypeFilterElement(DataTypeFilterElementXML xml) {
        dataTypeList.add(xml);
    }
    
    private void verifyFilterTypeMap() {
        if (dataTypeMap == null) {
            dataTypeMap = new HashMap<String, DataTypeFilterElementXML>();
        }
        
        for (DataTypeFilterElementXML dataType: dataTypeList) {
            dataTypeMap.put(dataType.getName().toUpperCase(), dataType);
        }
    }
}
