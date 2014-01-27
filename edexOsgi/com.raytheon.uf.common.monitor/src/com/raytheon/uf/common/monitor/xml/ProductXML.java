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
 * XML
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 1 Apr, 2010 3915         dhladky     Initial creation
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;

@XmlAccessorType(XmlAccessType.NONE)
public class ProductXML {

    @XmlAttribute(name = "primarySource")
    protected String primarySource;

    @XmlElement(name = "rate")
    protected String rate;

    @XmlElement(name = "qpe")
    protected String qpe;

    @XmlElements({ @XmlElement(name = "qpf", type = String.class) })
    protected ArrayList<String> qpfList;

    @XmlElements({ @XmlElement(name = "guidance", type = String.class) })
    protected ArrayList<String> guidList;

    @XmlElement(name = "virtual")
    private String virtual;

    public String getPrimarySource() {
        return primarySource;
    }

    public void setPrimarySource(String primarySource) {
        this.primarySource = primarySource;
    }

    public String getRate() {
        return rate;
    }

    public void setRate(String rate) {
        this.rate = rate;
    }

    public String getQpe() {
        return qpe;
    }

    public void setQpe(String qpe) {
        this.qpe = qpe;
    }

    public ArrayList<String> getQpfList() {
        return qpfList;
    }

    public void setQpfList(ArrayList<String> qpfList) {
        this.qpfList = qpfList;
    }

    public ArrayList<String> getGuidList() {
        return guidList;
    }

    public void setGuidList(ArrayList<String> guidList) {
        this.guidList = guidList;
    }

    public ArrayList<String> getGuidanceSources() {
        ArrayList<String> sources = new ArrayList<String>();
        for (String guidance : getGuidList()) {
            String[] splits = guidance.split(",");
            for (int i = 0; i < splits.length; i++) {
                sources.add(splits[i].trim());
            }
        }
        return sources;
    }

    public boolean containsSource(String sourceName) {

        for (String source : getGuidanceSources()) {
            if (sourceName.equals(source)) {
                return true;
            }
        }

        if (getQpfList().contains(sourceName)) {
            return true;
        }

        if (getQpe().equals(sourceName)) {
            return true;
        }

        if (getRate().equals(sourceName)) {
            return true;
        }

        if (getVirtual().equals(sourceName)) {
            return true;
        }

        return false;
    }

    public void setVirtual(String virtual) {
        this.virtual = virtual;
    }

    public String getVirtual() {
        return virtual;
    }

    public String getQpf(int index) {
        return qpfList.get(index);
    }

    public ArrayList<String> getSources() {
        ArrayList<String> products = new ArrayList<String>();
        products.add(getRate());
        products.add(getVirtual());
        products.add(getQpe());
        products.addAll(getQpfList());

        for (String guid : getGuidanceSources()) {
            products.add(guid);
        }

        return products;
    }

    /**
     * finds correlated guidance sources
     * 
     * @return
     */
    public ArrayList<String> getAvailableGuidanceTypes() {

        FFMPSourceConfigurationManager sourceManager = FFMPSourceConfigurationManager
                .getInstance();

        ArrayList<String> guidanceTypes = new ArrayList<String>();

        for (String sourceName : getGuidanceSources()) {
            SourceXML source = sourceManager.getSource(sourceName);
            if (source != null) {
                if (!guidanceTypes.contains(source.getDisplayName())) {
                    guidanceTypes.add(source.getDisplayName());
                }
            }
        }

        return guidanceTypes;

    }

    /**
     * finds correlated qpf sources
     * 
     * @return
     */
    public ArrayList<String> getAvailableQpfTypes() {

        FFMPSourceConfigurationManager sourceManager = FFMPSourceConfigurationManager
                .getInstance();

        ArrayList<String> qpfTypes = new ArrayList<String>();

        for (String sourceName : getQpfList()) {
            SourceXML source = sourceManager.getSource(sourceName);
            if (source != null) {
                if (!qpfTypes.contains(source.getDisplayName())) {
                    qpfTypes.add(source.getDisplayName());
                }
            }
        }

        return qpfTypes;

    }

    /**
     * gets list of sources by type of guidance (DisplayName)
     * 
     * @param type
     * @return
     */
    public ArrayList<SourceXML> getGuidanceSourcesByType(String type) {

        FFMPSourceConfigurationManager sourceManager = FFMPSourceConfigurationManager
                .getInstance();

        ArrayList<SourceXML> guidanceSources = new ArrayList<SourceXML>();

        for (String sourceName : getGuidanceSources()) {
            SourceXML source = sourceManager.getSource(sourceName);
            if (source != null) {
                if (source.getDisplayName().equals(type)) {
                    guidanceSources.add(source);
                }
            }
        }

        return guidanceSources;
    }

    /**
     * gets list of sources by type of qpf (DisplayName)
     * 
     * @param type
     * @return
     */
    public ArrayList<SourceXML> getQpfSourcesByType(String type) {

        FFMPSourceConfigurationManager sourceManager = FFMPSourceConfigurationManager
                .getInstance();

        ArrayList<SourceXML> qpfSources = new ArrayList<SourceXML>();

        for (String sourceName : getQpfList()) {
            SourceXML source = sourceManager.getSource(sourceName);
            if (source != null) {
                if (source.getDisplayName().equals(type)) {
                    qpfSources.add(source);
                }
            }
        }

        return qpfSources;
    }
}
