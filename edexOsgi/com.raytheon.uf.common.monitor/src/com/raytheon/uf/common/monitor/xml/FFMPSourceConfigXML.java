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

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * 
 * FFMPSourceConfigXML.  This corresponds to the file FFMPSourceConfig.xml.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ???                                 ???
 * Jul 10, 2018 6695       njensen     Overrode toString(), use List interface
 * Aug 06, 2018 6720       njensen     Added getSourceBySourceFamily(String)
 *
 * </pre>
 *
 */
@XmlRootElement(name = "FFMPSourceConfig")
@XmlAccessorType(XmlAccessType.NONE)
public class FFMPSourceConfigXML {

    @XmlElements({ @XmlElement(name = "source", type = SourceXML.class) })
    private List<SourceXML> source;

    @XmlElements({ @XmlElement(name = "product", type = ProductXML.class) })
    private List<ProductXML> products;

    public List<SourceXML> getSource() {
        return source;
    }

    public void setSource(List<SourceXML> source) {
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
            if (xml2.getDisplayNameForQuestionablePurposes()
                    .equals(displayName)) {
                xml = xml2;
                break;
            }
        }
        return xml;
    }

    /**
     * find a source by its family name
     * 
     * @param familyName
     * @return
     */
    public SourceXML getSourceBySourceFamily(String familyName) {
        SourceXML xml = null;
        for (SourceXML xml2 : getSource()) {
            if (xml2.getSourceFamily() != null
                    && xml2.getSourceFamily().equals(familyName)) {
                xml = xml2;
                break;
            }
        }
        return xml;
    }

    public List<ProductXML> getProducts() {
        return products;
    }

    public void setProducts(List<ProductXML> products) {
        this.products = products;
    }

    @Override
    public String toString() {
        return "FFMPSourceConfigXML [source=" + source + ", products="
                + products + "]";
    }

}
