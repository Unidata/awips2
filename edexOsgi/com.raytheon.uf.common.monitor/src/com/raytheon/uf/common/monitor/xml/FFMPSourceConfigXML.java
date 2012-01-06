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

/**
 * FFMP Source Configuration XML .
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 01, 2010            dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

@XmlRootElement(name = "FFMPSourceConfig")
@XmlAccessorType(XmlAccessType.NONE)
public class FFMPSourceConfigXML implements ISerializableObject {

    @XmlElements({ @XmlElement(name = "source", type = SourceXML.class) })
    private ArrayList<SourceXML> source;

    @XmlElements({ @XmlElement(name = "product", type = ProductXML.class) })
    private ArrayList<ProductXML> products;

    public ArrayList<SourceXML> getSource() {
        return source;
    }

    public void setSource(ArrayList<SourceXML> source) {
        this.source = source;
    }

    /**
     * Get me the particular Source
     * 
     * @param sourceName
     * @return
     */
    public SourceXML getSource(String sourceName) {
        SourceXML xml = null;
        for (SourceXML xml2 : getSource()) {
            if (xml2.getSourceName().equals(sourceName)) {
                xml = xml2;
                break;
            }
        }
        return xml;
    }

    /**
     * find a source by it's display name
     * 
     * @param displayName
     * @return
     */
    public SourceXML getSourceByDisplayName(String displayName) {
        SourceXML xml = null;
        for (SourceXML xml2 : getSource()) {
            if (xml2.getDisplayName().equals(displayName)) {
                xml = xml2;
                break;
            }
        }
        return xml;
    }

    public ArrayList<ProductXML> getProducts() {
        return products;
    }

    public void setProducts(ArrayList<ProductXML> products) {
        this.products = products;
    }
}
