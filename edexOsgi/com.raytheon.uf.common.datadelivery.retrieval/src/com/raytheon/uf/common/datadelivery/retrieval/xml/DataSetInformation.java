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


import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Data Set Info XML Object.
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

@XmlRootElement(name = "DataSetInformation")
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class DataSetInformation {

    @XmlElement(name = "modelName", type = String.class)
    @DynamicSerializeElement
    protected String modelName;
    
    @XmlElement(name = "multiplier", type = Double.class)
    @DynamicSerializeElement
    protected Double multiplier;
    
    @XmlElement(name = "modelRunIncrement", type = Integer.class)
    @DynamicSerializeElement
    protected Integer modelRunIncrement;
    
    @XmlElement(name = "defaultOffset", type = Integer.class)
    @DynamicSerializeElement
    protected Integer defaultOffset;
        
    public DataSetInformation() {
        
    }
    
    public DataSetInformation(String modelName, Double multiplier, int modelRunIncrement, int defaultOffset) {
  
        this.modelName = modelName;
        this.multiplier = multiplier;
        this.modelRunIncrement = modelRunIncrement;
        this.defaultOffset = defaultOffset;
    }

    public String getModelName() {
        return modelName;
    }

    public void setModelName(String modelName) {
        this.modelName = modelName;
    }

    public Double getMultiplier() {
        return multiplier;
    }

    public void setMultiplier(Double multiplier) {
        this.multiplier = multiplier;
    }

    public Integer getModelRunIncrement() {
        return modelRunIncrement;
    }

    public void setModelRunIncrement(Integer modelRunIncrement) {
        this.modelRunIncrement = modelRunIncrement;
    }

    public Integer getDefaultOffset() {
        return defaultOffset;
    }

    public void setDefaultOffset(Integer defaultOffset) {
        this.defaultOffset = defaultOffset;
    }
    
    public int getRange() {
        return (int) (getMultiplier() * getModelRunIncrement());
    }

}
