package com.raytheon.uf.common.datadelivery.retrieval.xml;

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


import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Data Set Lookup XML Object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 14, 2014            dhladky      Initial creation.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@XmlRootElement(name = "DataSetInformationLookup")
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class DataSetInformationLookup {
    
    public DataSetInformationLookup() {
        dataSetInformations = new ArrayList<DataSetInformation>(1);
    }
    
    @XmlElements({ @XmlElement(name = "dataSetInformation", type = DataSetInformation.class) })
    @DynamicSerializeElement
    private List<DataSetInformation> dataSetInformations;

    public List<DataSetInformation> getDataSetInformations() {
        return dataSetInformations;
    }

    public void setDataSetInformations(List<DataSetInformation> dataSetInformations) {
        this.dataSetInformations = dataSetInformations;
    }

}
