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
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

/**
 * XML for Source contribution overrides
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 14 Nov, 2011 11456      dhladky     Initial creation
 * Jul 11, 2018 6695       njensen     Overrode to String, use List interface
 * </pre>
 * 
 * @author dhladky
 */
@XmlAccessorType(XmlAccessType.NONE)
public class SourceOverrideXML {

    @XmlAttribute(name = "name")
    protected String sourceName;

    @XmlElements({ @XmlElement(name = "dataKey") })
    private List<SourceOverrideDataKeyXML> sourceDataKeys;

    public void setSourceName(String sourceName) {
        this.sourceName = sourceName;
    }

    public String getSourceName() {
        return sourceName;
    }

    public void setSourceOverrideDataKeys(
            List<SourceOverrideDataKeyXML> sourceDataKeys) {
        this.sourceDataKeys = sourceDataKeys;
    }

    public List<SourceOverrideDataKeyXML> getSourceOverrideDataKeys() {
        return sourceDataKeys;
    }

    /**
     * Returns the correct source override data object for a given data key
     * 
     * @param dataKey
     * @return
     */
    public SourceOverrideDataKeyXML getOverrideByDataKey(String dataKey) {
        if (sourceDataKeys != null) {
            for (SourceOverrideDataKeyXML overrideDataKey : sourceDataKeys) {
                if (overrideDataKey.getDataKey().equals(dataKey)) {
                    return overrideDataKey;
                }
            }
        }
        return null;
    }

    @Override
    public String toString() {
        return "SourceOverrideXML [sourceName=" + sourceName
                + ", sourceDataKeys=" + sourceDataKeys + "]";
    }

}
