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
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

/**
 * FFMPRunXML
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 14, 2010 3734       dhladky     Initial creation
 * Jul 11, 2018 6695       njensen     Overrode toString(), use List interface
 * 
 * </pre>
 * 
 * @author dhladky
 */
@XmlAccessorType(XmlAccessType.NONE)
public class FFMPRunXML {

    @XmlElements({ @XmlElement(name = "sourceIngestConfig", type = SourceIngestConfigXML.class) })
    private List<SourceIngestConfigXML> sourceIngests;

    @XmlElements({ @XmlElement(name = "sourceOverride", type = SourceOverrideXML.class) })
    private List<SourceOverrideXML> sourceOverrides;

    @XmlElements({ @XmlElement(name = "product", type = ProductRunXML.class) })
    private List<ProductRunXML> products;

    @XmlElements({ @XmlElement(name = "domain", type = DomainXML.class) })
    private List<DomainXML> domains;

    @XmlElement(name = "cacheDir")
    private String cacheDir;

    public void setProducts(List<ProductRunXML> products) {
        this.products = products;
    }

    public List<ProductRunXML> getProducts() {
        return products;
    }

    public void setDomains(List<DomainXML> domains) {
        this.domains = domains;
    }

    public List<DomainXML> getDomains() {
        return domains;
    }

    public DomainXML getPrimaryDomain() {
        for (DomainXML domain : domains) {
            if (domain.isPrimary()) {
                return domain;
            }
        }
        return null;
    }

    public ArrayList<DomainXML> getBackupDomains() {
        ArrayList<DomainXML> backups = null;
        for (DomainXML domain : domains) {
            if (!domain.isPrimary()) {
                if (backups == null) {
                    backups = new ArrayList<>();
                }
                backups.add(domain);
            }
        }
        return backups;
    }

    public void addDomain(DomainXML domain) {
        if (domains == null) {
            domains = new ArrayList<>();
        }

        domains.add(domain);
    }

    public ProductRunXML getProduct(String productKey) {
        ProductRunXML product = null;

        for (ProductRunXML iproduct : getProducts()) {
            if (iproduct.getProductKey().equals(productKey)) {
                product = iproduct;
                break;
            }
        }
        return product;
    }

    public List<ProductRunXML> getProducts(String productKey) {
        List<ProductRunXML> products = new ArrayList<>();

        for (ProductRunXML iproduct : getProducts()) {
            if (iproduct.getProductKey().equals(productKey)) {
                products.add(iproduct);
            }
        }
        return products;
    }

    public void setSourceIngests(
            List<SourceIngestConfigXML> sourceIngests) {
        this.sourceIngests = sourceIngests;
    }

    public List<SourceIngestConfigXML> getSourceIngests() {
        return sourceIngests;
    }

    /**
     * Gets ingest config for this source
     * 
     * @param sourceName
     * @return
     */
    public SourceIngestConfigXML getSourceIngest(String sourceName) {
        if (sourceIngests != null) {
            for (SourceIngestConfigXML source : sourceIngests) {
                if (source.getSourceName().equals(sourceName)) {
                    return source;
                }
            }
        }
        return null;
    }

    public void addSourceIngest(SourceIngestConfigXML sourceIngest) {
        if (sourceIngests == null) {
            sourceIngests = new ArrayList<>();
        }
        sourceIngests.add(sourceIngest);
    }

    public void removeSourceIngest(String sourceName) {
        if (sourceIngests != null) {
            SourceIngestConfigXML remove = null;
            for (SourceIngestConfigXML source : sourceIngests) {
                if (source.getSourceName().equals(sourceName)) {
                    remove = source;
                    break;
                }
            }

            sourceIngests.remove(remove);
        }
    }

    public void setSourceOverrides(
            List<SourceOverrideXML> sourceOverrides) {
        this.sourceOverrides = sourceOverrides;
    }

    public List<SourceOverrideXML> getSourceOverrides() {
        return sourceOverrides;
    }

    public List<String> getOverrideSources() {
        List<String> overrides = null;
        if (sourceOverrides != null) {
            overrides = new ArrayList<>();
            for (SourceOverrideXML over : getSourceOverrides()) {
                overrides.add(over.getSourceName());
            }
        }
        return overrides;
    }

    public SourceOverrideXML getSourceOverride(String sourceName) {
        if (sourceOverrides != null) {
            for (SourceOverrideXML over : getSourceOverrides()) {
                if (over.getSourceName().equals(sourceName)) {
                    return over;
                }
            }
        }
        return null;
    }

    public boolean isOverride() {
        if (sourceOverrides != null) {
            return true;
        } else {
            return false;
        }
    }

    public void setCacheDir(String cacheDir) {
        this.cacheDir = cacheDir;
    }

    public String getCacheDir() {
        return cacheDir;
    }

    @Override
    public String toString() {
        return "FFMPRunXML [sourceIngests=" + sourceIngests
                + ", sourceOverrides=" + sourceOverrides + ", products="
                + products + ", domains=" + domains + ", cacheDir=" + cacheDir
                + "]";
    }

}
