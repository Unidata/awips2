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

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

import com.raytheon.uf.viz.datadelivery.common.xml.IDisplayXml;

/**
 * Filter Type Definition object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 30, 2012            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
public class FilterTypeXML implements IDisplayXml {
    @XmlAttribute(name = "type")
    protected String filterType;

    @XmlElements({ @XmlElement(name = "Value", type = String.class) })
    protected ArrayList<String> values = new ArrayList<String>();

    /**
     * @return the filterType
     */
    public String getFilterType() {
        return filterType;
    }

    /**
     * @param filterType
     *            the filterType to set
     */
    public void setFilterType(String filterType) {
        this.filterType = filterType;
    }

    /**
     * @return the values
     */
    public ArrayList<String> getValues() {
        return values;
    }

    /**
     * @param values
     *            the values to set
     */
    public void setValues(ArrayList<String> values) {
        this.values = values;
    }

    public void addValue(String value) {
        this.values.add(value);
    }

    public void clearValues() {
        this.values.clear();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.datadelivery.common.xml.IDisplayXml#getDisplayXmlString
     * ()
     */
    @Override
    public String getDisplayXmlString() {
        StringBuilder sb = new StringBuilder();

        sb.append("Type: ").append(this.filterType).append("\n");
        sb.append("     Values:\n");

        for (String s : values) {
            sb.append("         ").append(s).append("\n");
        }

        return sb.toString();
    }
}
