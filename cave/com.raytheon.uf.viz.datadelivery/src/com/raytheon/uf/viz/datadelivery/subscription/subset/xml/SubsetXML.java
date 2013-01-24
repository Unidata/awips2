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
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.edex.util.Util;
import com.raytheon.uf.viz.datadelivery.common.xml.AreaXML;
import com.raytheon.uf.viz.datadelivery.common.xml.IDisplayXml;

/**
 * Saved subset xml object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 29, 2012            mpduff       Initial creation
 * Aug 10, 2012 1022       djohnson     {@link SubsetXML} requires provider name.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@XmlRootElement(name = "Subset")
@XmlAccessorType(XmlAccessType.NONE)
public class SubsetXML<TIMEXML extends TimeXML> implements IDisplayXml {
    private static final String XML_FILE_EXTENSION = ".xml";

    @XmlElement(name = "datasetName", type = String.class)
    protected String datasetName;
    
    @XmlElement(name = "providerName", type = String.class)
    private String providerName;

    @XmlElement(name = "subsetName", type = String.class)
    protected String subsetName;

    @XmlElement(name = "area", type = AreaXML.class)
    protected AreaXML area;
    
    @XmlElements({ @XmlElement(name = "vertical", type = VerticalXML.class) })
    protected ArrayList<VerticalXML> verticalList = new ArrayList<VerticalXML>();
    
    @XmlElementRef
    protected TIMEXML time;

    /**
     * @return the subsetName
     */
    public String getSubsetName() {
        return subsetName;
    }

    /**
     * @param subsetName the subsetName to set
     */
    public void setSubsetName(String subsetName) {
        this.subsetName = subsetName;
    }

    /**
     * @return the area
     */
    public AreaXML getArea() {
        return area;
    }

    /**
     * @param area the area to set
     */
    public void setArea(AreaXML area) {
        this.area = area;
    }

    /**
     * @return the verticalList
     */
    public ArrayList<VerticalXML> getVerticalList() {
        return verticalList;
    }

    /**
     * @param verticalList the verticalList to set
     */
    public void setVerticalList(ArrayList<VerticalXML> verticalList) {
        this.verticalList = verticalList;
    }
    
    /**
     * Add a VerticalXML object to the list
     * 
     * @param vertical VerticalXML object to add
     */
    public void addVertical(VerticalXML vertical) {
        this.verticalList.add(vertical);
    }

    /**
     * @return the time
     */
    public TIMEXML getTime() {
        return time;
    }

    /**
     * @param time the time to set
     */
    public void setTime(TIMEXML time) {
        this.time = time;
    }

    /**
     * @return the datasetName
     */
    public String getDatasetName() {
        return datasetName;
    }

    /**
     * @param datasetName the datasetName to set
     */
    public void setDatasetName(String datasetName) {
        this.datasetName = datasetName;
    }

    /**
     * @return
     */
    public String getProviderName() {
        return providerName;
    }

    /**
     * 
     * @param provider
     */
    public void setProviderName(String providerName) {
        this.providerName = providerName;
    }

    /* (non-Javadoc)
     * @see com.raytheon.uf.viz.datadelivery.common.xml.IDisplayXml#getDisplayXmlString()
     */
    @Override
    public String getDisplayXmlString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Subset Name : ").append(subsetName).append(Util.EOL);
        sb.append("Dataset Name: ").append(datasetName).append(Util.EOL);
        sb.append("Provider: " + providerName);
        
        for (VerticalXML v: verticalList) {
            sb.append(v.getDisplayXmlString());
        }
        
        sb.append(Util.EOL);
        
        sb.append(area.getDisplayXmlString());
        
        sb.append(time.getDisplayXmlString());
        
        return sb.toString();
    }

    /**
     * @return base name of the subset (subset name without file name extension)
     */
    public String getBaseSubsetName() {
        return getBaseSubsetName(getSubsetName());
    }

    /**
     * @return base name of the given subsetName (subsetName without file name
     *         extension)
     */
    public static String getBaseSubsetName(String subsetName) {
        int extensionIndex = subsetName.lastIndexOf(XML_FILE_EXTENSION);
        if (extensionIndex > 0) {
            return subsetName.substring(0, extensionIndex);
        }
        return subsetName;
    }

    /**
     * Set the subsetName from a baseSubsetName (i.e. that does not have the
     * file extension appended.)
     * 
     * @param baseSubsetName
     *            subsetName without file name extension
     */
    public void setBaseSubsetName(String baseSubsetName) {
        if (baseSubsetName.endsWith(XML_FILE_EXTENSION)) {
            setSubsetName(baseSubsetName);
        } else {
            setSubsetName(baseSubsetName + XML_FILE_EXTENSION);
        }
    }
}
