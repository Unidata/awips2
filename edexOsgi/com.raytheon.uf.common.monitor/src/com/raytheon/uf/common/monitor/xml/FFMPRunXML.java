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

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * VGB's
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * July 14, 2010     3734       dhladky    Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class FFMPRunXML implements ISerializableObject {

    @XmlElements({ @XmlElement(name = "sourceIngestConfig", type = SourceIngestConfigXML.class) })
    private ArrayList<SourceIngestConfigXML> sourceIngests;

    @XmlElements({ @XmlElement(name = "sourceOverride", type = SourceOverrideXML.class) })
    private ArrayList<SourceOverrideXML> sourceOverrides;

    @XmlElements({ @XmlElement(name = "product", type = ProductRunXML.class) })
    private ArrayList<ProductRunXML> products;

    @XmlElements({ @XmlElement(name = "domain", type = DomainXML.class) })
    private ArrayList<DomainXML> domains;
    
    @XmlElement(name = "cacheDir")
    private String cacheDir;

    public void setProducts(ArrayList<ProductRunXML> products) {
        this.products = products;
    }

    public ArrayList<ProductRunXML> getProducts() {
        return products;
    }

    public void setDomains(ArrayList<DomainXML> domains) {
        this.domains = domains;
    }

    public ArrayList<DomainXML> getDomains() {
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
                    backups = new ArrayList<DomainXML>();
                }
                backups.add(domain);
            }
        }
        return backups;
    }

    public void addDomain(DomainXML domain) {
        if (domains == null) {
            domains = new ArrayList<DomainXML>();
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

    public ArrayList<ProductRunXML> getProducts(String productKey) {
        ArrayList<ProductRunXML> products = new ArrayList<ProductRunXML>();

        for (ProductRunXML iproduct : getProducts()) {
            if (iproduct.getProductKey().equals(productKey)) {
                products.add(iproduct);
            }
        }
        return products;
    }

    public void setSourceIngests(ArrayList<SourceIngestConfigXML> sourceIngests) {
        this.sourceIngests = sourceIngests;
    }

    public ArrayList<SourceIngestConfigXML> getSourceIngests() {
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
            sourceIngests = new ArrayList<SourceIngestConfigXML>();
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

    public void setSourceOverrides(ArrayList<SourceOverrideXML> sourceOverrides) {
        this.sourceOverrides = sourceOverrides;
    }

    public ArrayList<SourceOverrideXML> getSourceOverrides() {
        return sourceOverrides;
    }

    public ArrayList<String> getOverrideSources() {
        ArrayList<String> overrides = null;
        if (sourceOverrides != null) {
            overrides = new ArrayList<String>();
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

}
