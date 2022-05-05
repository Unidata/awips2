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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;

/**
 * Holds attributes of a product. This object corresponds to the product tag in
 * the FFMPSourceConfig.xml file.
 * 
 * These objects are used to generate submenus of the FFMP menus.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 1  Apr, 2010 3915       dhladky     Initial creation
 * 21 Sep, 2015 4756       dhladky     Sort guidance types.
 * Jul 10, 2018 6695       njensen     Overrode toString(), use List interface
 * Jul 30, 2018 6720       njensen     Made method names explicit
 * Aug 07, 2018 6720       njensen     Added more ways to get source families
 * 
 * </pre>
 * 
 * @author dhladky
 */
@XmlAccessorType(XmlAccessType.NONE)
public class ProductXML {

    @XmlAttribute(name = "primarySource")
    protected String primarySource;

    @XmlElement(name = "rate")
    protected String rate;

    @XmlElement(name = "qpe")
    protected String qpe;

    @XmlElements({ @XmlElement(name = "qpf", type = String.class) })
    protected List<String> qpfList;

    @XmlElements({ @XmlElement(name = "guidance", type = String.class) })
    protected List<String> guidList;

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

    public List<String> getQpfList() {
        return qpfList;
    }

    public void setQpfList(List<String> qpfList) {
        this.qpfList = qpfList;
    }

    public List<String> getGuidList() {
        return guidList;
    }

    public void setGuidList(List<String> guidList) {
        this.guidList = guidList;
    }

    public List<String> getGuidanceSourceNames() {
        List<String> sources = new ArrayList<>();
        for (String guidance : getGuidList()) {
            String[] splits = guidance.split(",");
            for (String split : splits) {
                sources.add(split.trim());
            }
        }

        /*
         * this sort is ridiculously important as it will be used to index into
         * the basin table dialog
         * 
         * TODO: make ffmp more robust so the order is not that important
         */
        Collections.sort(sources);

        return sources;
    }

    public boolean containsSource(String sourceName) {
        for (String source : getGuidanceSourceNames()) {
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

    public List<String> getSourceNames() {
        List<String> products = new ArrayList<>();
        products.add(getRate());
        products.add(getVirtual());
        products.add(getQpe());
        products.addAll(getQpfList());

        for (String guid : getGuidanceSourceNames()) {
            products.add(guid);
        }

        return products;
    }

    /**
     * finds correlated guidance sources
     * 
     * @return
     */
    public List<String> getAvailableGuidanceDisplayNames() {
        FFMPSourceConfigurationManager sourceManager = FFMPSourceConfigurationManager
                .getInstance();

        List<String> guidanceTypes = new ArrayList<>();

        for (String sourceName : getGuidanceSourceNames()) {
            SourceXML source = sourceManager.getSource(sourceName);
            if (source != null) {
                String dispName = source
                        .getDisplayNameForQuestionablePurposes();
                if (!guidanceTypes.contains(dispName)) {
                    guidanceTypes.add(dispName);
                }
            }
        }

        Collections.sort(guidanceTypes);

        return guidanceTypes;
    }

    public List<String> getAvailableGuidanceSourceFamilies() {
        FFMPSourceConfigurationManager sourceManager = FFMPSourceConfigurationManager
                .getInstance();

        List<String> guidanceFamilies = new ArrayList<>();

        for (String sourceName : getGuidanceSourceNames()) {
            SourceXML source = sourceManager.getSource(sourceName);
            if (source != null) {
                String familyName = source.getSourceFamily();
                if (!guidanceFamilies.contains(familyName)) {
                    guidanceFamilies.add(familyName);
                }
            }
        }

        Collections.sort(guidanceFamilies);

        return guidanceFamilies;
    }

    /**
     * finds correlated qpf sources
     * 
     * @return
     */
    public List<String> getAvailableQpfDisplayNames() {
        FFMPSourceConfigurationManager sourceManager = FFMPSourceConfigurationManager
                .getInstance();

        List<String> qpfTypes = new ArrayList<>();

        for (String sourceName : getQpfList()) {
            SourceXML source = sourceManager.getSource(sourceName);
            if (source != null) {
                String displayName = source
                        .getDisplayNameForQuestionablePurposes();
                if (!qpfTypes.contains(displayName)) {
                    qpfTypes.add(displayName);
                }
            }
        }

        return qpfTypes;
    }

    /**
     * gets list of sources by type of guidance (DisplayName)
     * 
     * @param displayName
     * @return
     */
    public List<SourceXML> getGuidanceSourcesByDisplayName(String displayName) {
        FFMPSourceConfigurationManager sourceManager = FFMPSourceConfigurationManager
                .getInstance();

        List<SourceXML> guidanceSources = new ArrayList<>();

        for (String sourceName : getGuidanceSourceNames()) {
            SourceXML source = sourceManager.getSource(sourceName);
            if (source != null) {
                if (source.getDisplayNameForQuestionablePurposes()
                        .equals(displayName)) {
                    guidanceSources.add(source);
                }
            }
        }

        return guidanceSources;
    }

    public List<SourceXML> getGuidanceSourcesBySourceFamily(
            String sourceFamily) {
        FFMPSourceConfigurationManager sourceManager = FFMPSourceConfigurationManager
                .getInstance();

        List<SourceXML> guidanceSources = new ArrayList<>();

        for (String sourceName : getGuidanceSourceNames()) {
            SourceXML source = sourceManager.getSource(sourceName);
            if (source != null) {
                if (source.getSourceFamily().equals(sourceFamily)) {
                    guidanceSources.add(source);
                }
            }
        }

        return guidanceSources;
    }

    /**
     * gets list of sources by type of qpf (DisplayName)
     * 
     * @param displayName
     * @return
     */
    public List<SourceXML> getQpfSourcesByDisplayName(String displayName) {
        FFMPSourceConfigurationManager sourceManager = FFMPSourceConfigurationManager
                .getInstance();

        List<SourceXML> qpfSources = new ArrayList<>();

        for (String sourceName : getQpfList()) {
            SourceXML source = sourceManager.getSource(sourceName);
            if (source != null) {
                if (source.getDisplayNameForQuestionablePurposes()
                        .equals(displayName)) {
                    qpfSources.add(source);
                }
            }
        }

        return qpfSources;
    }

    @Override
    public String toString() {
        return "ProductXML [primarySource=" + primarySource + ", rate=" + rate
                + ", qpe=" + qpe + ", qpfList=" + qpfList + ", guidList="
                + guidList + ", virtual=" + virtual + "]";
    }

}
