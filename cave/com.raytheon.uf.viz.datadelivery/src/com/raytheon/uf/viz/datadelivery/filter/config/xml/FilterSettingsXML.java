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
package com.raytheon.uf.viz.datadelivery.filter.config.xml;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.viz.datadelivery.common.xml.AreaXML;
import com.raytheon.uf.viz.datadelivery.common.xml.IDisplayXml;

/**
 * XML object holding the saved filter settings.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 27, 2012            mpduff      Initial creation.
 * Jun 04, 2012    223     mpduff      Add data set type and refactor method name change.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@XmlRootElement(name = "FilterSettings")
@XmlAccessorType(XmlAccessType.NONE)
public class FilterSettingsXML implements IDisplayXml {
    @XmlElements({ @XmlElement(name = "Filter", type = FilterTypeXML.class) })
    protected ArrayList<FilterTypeXML> filterTypeList = new ArrayList<FilterTypeXML>();

    @XmlElement(name = "area", type = AreaXML.class)
    protected AreaXML area;

    @XmlElements({ @XmlElement(name = "DataSetType", type = String.class) })
    protected List<String> dataSetTypes = new ArrayList<String>();

    /**
     * @return the filterTypeList
     */
    public ArrayList<FilterTypeXML> getFilterTypeList() {
        return filterTypeList;
    }

    /**
     * @param filterTypeList
     *            the filterTypeList to set
     */
    public void setFilterTypeList(ArrayList<FilterTypeXML> filterTypeList) {
        this.filterTypeList = filterTypeList;
    }

    /**
     * Add a filterType object
     * 
     * @param filterType
     */
    public void addFilterType(FilterTypeXML filterType) {
        filterTypeList.add(filterType);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.datadelivery.common.xml.IDisplayXml#getDisplayXmlString
     * ()
     */
    @Override
    public String getPreviewString() {
        final String newline = "\n";
        StringBuilder sb = new StringBuilder();

        for (FilterTypeXML ftx : filterTypeList) {
            sb.append(ftx.getPreviewString());
            sb.append(newline);
        }

        if (area != null) {
            sb.append(area.getPreviewString()).append(newline);
        }

        return sb.toString();
    }

    /**
     * @return the area
     */
    public AreaXML getArea() {
        return area;
    }

    /**
     * @param area
     *            the area to set
     */
    public void setArea(AreaXML area) {
        this.area = area;
    }

    /**
     * @return the dataSetTypes
     */
    public List<String> getDataSetTypes() {
        return dataSetTypes;
    }

    /**
     * @param dataSetTypes
     *            the dataSetTypes to set
     */
    public void setDataSetTypes(List<String> dataSetTypes) {
        this.dataSetTypes = dataSetTypes;
    }

    /**
     * @param type
     *            the type to add
     */
    public void addDataSetType(String type) {
        this.dataSetTypes.add(type);
    }
}
