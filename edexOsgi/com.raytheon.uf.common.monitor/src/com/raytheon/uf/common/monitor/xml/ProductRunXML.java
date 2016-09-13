package com.raytheon.uf.common.monitor.xml;

import java.util.ArrayList;
import java.util.Collections;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager.SOURCE_TYPE;

/**
 * XML
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20 Jan, 2011 7484         dhladky     Initial creation
 * 21 Sep, 2015 4756         dhladky     Source ordering for guidance sources.
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
@XmlRootElement(name = "FFMPRunXML")
@XmlAccessorType(XmlAccessType.NONE)
public class ProductRunXML {

    @XmlAttribute(name = "name")
    private String productName;

    @XmlAttribute(name = "key")
    private String productKey;

    /**
     * Allow you to include and exclude any source from your product in the
     * RUNNER, essentially is a manual override of what's in the
     * FFMPSourceConfig product
     */

    @XmlElements({ @XmlElement(name = "exclude", type = String.class) })
    private ArrayList<String> excludes;

    @XmlElements({ @XmlElement(name = "include", type = String.class) })
    private ArrayList<String> includes;

    public void setProductKey(String productKey) {
        this.productKey = productKey;
    }

    public String getProductKey() {
        return productKey;
    }

    public void setProductName(String productName) {
        this.productName = productName;
    }

    public String getProductName() {
        return productName;
    }

    public ArrayList<String> getExcludes() {
        return excludes;
    }

    public void setExcludes(ArrayList<String> excludes) {
        this.excludes = excludes;
    }

    public ArrayList<String> getIncludes() {
        return includes;
    }

    public void setIncludes(ArrayList<String> includes) {
        this.includes = includes;
    }

    /**
     * Do we run this one?
     * 
     * @param sourceName
     * @return
     */
    public boolean isIncluded(String sourceName) {
        if (includes != null) {
            if (includes.contains(sourceName)) {
                return true;
            } else {
                return false;
            }
        } else {
            return true;
        }
    }

    /**
     * Do we run this one?
     * 
     * @param sourceName
     * @return
     */
    public boolean isExcluded(String sourceName) {
        if (excludes != null) {
            if (excludes.contains(sourceName)) {
                return true;
            } else {
                return false;
            }
        } else {
            return true;
        }
    }

    /**
     * Do we have exclusions
     * 
     * @return
     */
    public boolean hasExcludes() {
        boolean isExcluded = false;
        if (excludes != null) {
            isExcluded = true;
        }
        return isExcluded;
    }

    /**
     * Do we have includes
     * 
     * @return
     */
    public boolean hasIncludes() {
        boolean isIncluded = false;
        if (includes != null) {
            isIncluded = true;
        }
        return isIncluded;
    }

    /**
     * Gets the types with inclusion and exclusion filtering
     * 
     * @param qpfProduct
     * @return
     */
    public ArrayList<String> getQpfTypes(ProductXML qpfProduct) {

        ArrayList<String> qpfTypes = qpfProduct.getAvailableQpfTypes();

        if (hasIncludes()) {
            for (String incSource : getIncludes()) {
                SourceXML source = FFMPSourceConfigurationManager.getInstance()
                        .getSource(incSource);
                if (source != null) {
                    if (source.getSourceType().equals(
                            SOURCE_TYPE.QPF.getSourceType())) {
                        if (!qpfTypes.contains(source.getDisplayName())) {
                            qpfTypes.add(source.getDisplayName());
                        }
                    }
                }
            }
        }

        if (hasExcludes()) {

            ArrayList<String> removes = new ArrayList<String>();

            for (String exSource : getExcludes()) {
                SourceXML source = FFMPSourceConfigurationManager.getInstance()
                        .getSource(exSource);
                if (source != null) {
                    if (source.getSourceType().equals(
                            SOURCE_TYPE.QPF.getSourceType())) {
                        if (qpfTypes.contains(source.getDisplayName())) {
                            removes.add(source.getDisplayName());
                        }
                    }
                }
            }

            if (removes.size() > 0) {

                ArrayList<String> qpfSources = qpfProduct.getQpfList();
                ArrayList<String> overrideQpfTypes = new ArrayList<String>();

                for (String remove : removes) {
                    qpfSources.remove(remove);
                }

                for (String qpfSource : qpfSources) {
                    String type = FFMPSourceConfigurationManager.getInstance()
                            .getSource(qpfSource).getDisplayName();

                    if (!overrideQpfTypes.contains(type)) {
                        overrideQpfTypes.add(type);
                    }
                }

                qpfTypes = overrideQpfTypes;
            }
        }

        return qpfTypes;
    }

    /**
     * Gets the types with inclusion and exclusion filtering
     * 
     * @param guidProduct
     * @return
     */
    public ArrayList<String> getGuidanceTypes(ProductXML guidProduct) {

        ArrayList<String> guidTypes = guidProduct.getAvailableGuidanceTypes();

        if (hasIncludes()) {
            for (String incSource : getIncludes()) {
                SourceXML source = FFMPSourceConfigurationManager.getInstance()
                        .getSource(incSource);
                if (source != null) {
                    if (source.getSourceType().equals(
                            SOURCE_TYPE.GUIDANCE.getSourceType())) {
                        if (!guidTypes.contains(source.getDisplayName())) {
                            guidTypes.add(source.getDisplayName());
                        }
                    }
                }
            }
        }

        if (hasExcludes()) {

            ArrayList<String> removes = new ArrayList<String>();

            for (String exSource : getIncludes()) {
                SourceXML source = FFMPSourceConfigurationManager.getInstance()
                        .getSource(exSource);
                if (source != null) {
                    if (source.getSourceType().equals(
                            SOURCE_TYPE.GUIDANCE.getSourceType())) {
                        if (guidTypes.contains(source.getDisplayName())) {
                            removes.add(source.getDisplayName());
                        }
                    }
                }
            }

            if (removes.size() > 0) {

                ArrayList<String> guidSources = guidProduct
                        .getGuidanceSources();
                ArrayList<String> overrideGuidTypes = new ArrayList<String>();

                for (String remove : removes) {
                    guidSources.remove(remove);
                }

                for (String guidSource : guidSources) {
                    String type = FFMPSourceConfigurationManager.getInstance()
                            .getSource(guidSource).getDisplayName();

                    if (!overrideGuidTypes.contains(type)) {
                        overrideGuidTypes.add(type);
                    }
                }

                guidTypes = overrideGuidTypes;
            }
        }
        
        Collections.sort(guidTypes);

        return guidTypes;
    }

    /**
     * Gets a sources listing with includes and excludes
     * 
     * @param guidProduct
     * @param guidType
     * @return
     */
    public ArrayList<SourceXML> getGuidanceSources(ProductXML guidProduct,
            String guidType) {

        ArrayList<SourceXML> defaultGuidSources = guidProduct
                .getGuidanceSourcesByType(guidType);
        ArrayList<SourceXML> guidSources = new ArrayList<SourceXML>();
        // if includes are present
        if (hasIncludes()) {
            for (String incSource : getIncludes()) {
                SourceXML source = FFMPSourceConfigurationManager.getInstance()
                        .getSource(incSource);
                if (source != null) {
                    if (source.getSourceType().equals(
                            SOURCE_TYPE.GUIDANCE.getSourceType())
                            && source.getDisplayName().equals(guidType)) {
                        guidSources.add(source);
                    }
                }
            }
        }

        // if excludes are present
        if (hasExcludes()) {
            // start with the default, no sources otherwise
            if (!hasIncludes()) {
                guidSources = defaultGuidSources;
            }

            ArrayList<SourceXML> removals = new ArrayList<SourceXML>();

            for (SourceXML source : defaultGuidSources) {
                if (isExcluded(source.getSourceName())) {
                    if (guidSources.contains(source)) {
                        removals.remove(source);
                    }
                }
            }

            if (guidSources.size() == 0 && removals.size() == 0) {
                guidSources = defaultGuidSources;
            }

            guidSources.removeAll(removals);
        }

        // default setup
        if (!hasIncludes() && !hasExcludes()) {
            guidSources = defaultGuidSources;
        } else {
            if (guidSources.size() == 0) {
                guidSources = defaultGuidSources;
            }
        }

        return guidSources;
    }

    /**
     * Gets the QPF sources with includes and excludes
     * 
     * @param qpfProduct
     * @param qpfType
     * @return
     */
    public ArrayList<SourceXML> getQpfSources(ProductXML qpfProduct,
            String qpfType) {
        ArrayList<SourceXML> defaultQpfSources = qpfProduct
                .getQpfSourcesByType(qpfType);
        ArrayList<SourceXML> qpfSources = new ArrayList<SourceXML>();

        if (hasIncludes()) {
            for (String incSource : getIncludes()) {
                SourceXML source = FFMPSourceConfigurationManager.getInstance()
                        .getSource(incSource);
                if (source != null) {
                    if (source.getSourceType().equals(
                            SOURCE_TYPE.QPF.getSourceType())
                            && source.getDisplayName().equals(qpfType)) {
                        qpfSources.add(source);
                    }
                }
            }
        }

        // if excludes are present
        if (hasExcludes()) {

            // start with the default, no sources otherwise
            if (!hasIncludes()) {
                qpfSources = defaultQpfSources;
            }

            ArrayList<SourceXML> removals = new ArrayList<SourceXML>();

            for (SourceXML source : defaultQpfSources) {
                if (isExcluded(source.getSourceName())) {
                    if (qpfSources.contains(source)) {
                        removals.add(source);
                    }
                }
            }

            if (qpfSources.size() == 0 && removals.size() == 0) {
                qpfSources = defaultQpfSources;
            }

            qpfSources.removeAll(removals);
        }

        // default setup
        if (!hasIncludes() && !hasExcludes()) {
            qpfSources = defaultQpfSources;
        } else {
            if (qpfSources.size() == 0) {
                qpfSources = defaultQpfSources;
            }
        }

        return qpfSources;
    }

}
