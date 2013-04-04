package com.raytheon.edex.transform.shef;
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

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.edex.decodertools.core.filterimpl.AbstractFilterElement;

/**
 * MetarToShefRun is for the metarToShefRun tag in Metar2ShefFilter tag
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date       Ticket# Engineer Description
 * ---------- ------- -------- --------------------------
 * 1/10/2013  15497   wkwock   Initial creation
 *
 * </pre>
 *
 * @author wkwock
 * @version 1.0	
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class MetarToShefRun implements ISerializableObject{
    public static final String INCLUDE_TYPE = "INCLUDE";
    
    public static final String EXCLUDE_TYPE = "EXCLUDE";
    @XmlElement
    @DynamicSerializeElement
    private String configFileName;
    
    @XmlElement
    @DynamicSerializeElement
    private String metarToShefOptions;

    @XmlElement
    @DynamicSerializeElement
    protected List<AbstractFilterElement> filterElements = new ArrayList<AbstractFilterElement>();

    @XmlElement
    @DynamicSerializeElement
    private String filterName;
    
    /**
     * 
     * @param filterFile
     */
    void createFilter(File filterFile) {
    }
    
    /**
     * 
     * @return
     */
    public String getConfigFileName() {
        return configFileName;
    }
    
    /**
     * 
     * @param name
     */
    public void setConfigFileName(String name) {
    	configFileName = name;
    }

    /**
     * 
     * @return
     */
    public String getMetarToShefOptions() {
        return metarToShefOptions;
    }
    
    /**
     * 
     * @param name
     */
    public void setMetarToShefOptions(String name) {
    	metarToShefOptions = name;
    }

    /**
     * 
     */
    public void addFilterElement(AbstractFilterElement element) {
        filterElements.add(element);
    }

    /**
     * 
     * @return
     */
    public List<AbstractFilterElement> getFilterElements() {
        return filterElements;
    }
    
    /**
     * 
     * @param elements
     */
    public void setFilterElements(List<AbstractFilterElement> elements) {
        filterElements = elements;
    }
    
    /**
     * 
     * @return
     */
    public String getFilterName() {
        return filterName;
    }
    
    /**
     * 
     * @param name
     */
    public void setFilterName(String name) {
        filterName = name;
    }
}
