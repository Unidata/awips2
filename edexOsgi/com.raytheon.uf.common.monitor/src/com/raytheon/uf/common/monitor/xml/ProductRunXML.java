package com.raytheon.uf.common.monitor.xml;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager;
import com.raytheon.uf.common.monitor.config.FFMPSourceConfigurationManager.SourceType;

/**
 * Holds the product name and product key, and any includes and excludes. This
 * object corresponds to the product tag in the FFMPRunConfig.xml file.
 * 
 * The productKey is used to generate a top level menu item under the SCAN menu.
 * The productName is linked to the FFMPSourceConfig.xml file's product tags.
 * The FFMPRunConfig.xml's product's productName matches/links to the
 * FFMPSourceConfig.xml's product's primarySource. The matching
 * FFMPSourceConfig.xml file's product is then used to generate submenus of the
 * top level menu items.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20 Jan, 2011 7484       dhladky     Initial creation
 * 21 Sep, 2015 4756       dhladky     Source ordering for guidance sources.
 * Jul 09, 2018 6695       njensen     Added javadoc, overrode toString()
 * Jul 30, 2018 6720       njensen     Made method names explicit
 * Aug 07, 2018 6720       njensen     Added more ways to get source families
 * Aug 14, 2018 6720       njensen     Use simplified enums
 * 
 * </pre>
 * 
 * @author dhladky
 */
@XmlRootElement(name = "FFMPRunXML")
@XmlAccessorType(XmlAccessType.NONE)
public class ProductRunXML {

    @XmlAttribute(name = "name")
    private String productName;

    @XmlAttribute(name = "key")
    private String productKey;

    /*
     * Allow you to include and exclude any source from your product in the
     * RUNNER, essentially is a manual override of what's in the
     * FFMPSourceConfig product
     */

    @XmlElements({ @XmlElement(name = "exclude", type = String.class) })
    private List<String> excludes;

    @XmlElements({ @XmlElement(name = "include", type = String.class) })
    private List<String> includes;

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

    public List<String> getExcludes() {
        return excludes;
    }

    public void setExcludes(List<String> excludes) {
        this.excludes = excludes;
    }

    public List<String> getIncludes() {
        return includes;
    }

    public void setIncludes(List<String> includes) {
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
        return excludes != null;
    }

    /**
     * Do we have includes
     * 
     * @return
     */
    public boolean hasIncludes() {
        return includes != null;
    }

    /*
     * TODO If possible, consolidate the code within some of the getters that
     * have more logic than just getting. If possible, reduce the number of
     * these special getters by removing calls to them or moving the code to
     * FFMPXmlUtils.
     */

    /**
     * Gets the types with inclusion and exclusion filtering
     * 
     * @param qpfProduct
     * @return
     */
    public List<String> getQpfDisplayNames(ProductXML qpfProduct) {
        List<String> qpfDispNames = qpfProduct.getAvailableQpfDisplayNames();

        if (hasIncludes()) {
            for (String incSource : getIncludes()) {
                SourceXML source = FFMPSourceConfigurationManager.getInstance()
                        .getSource(incSource);
                if (source != null) {
                    if (source.getSourceType() == SourceType.QPF) {
                        String dispName = source
                                .getDisplayNameForQuestionablePurposes();
                        if (!qpfDispNames.contains(dispName)) {
                            qpfDispNames.add(dispName);
                        }
                    }
                }
            }
        }

        if (hasExcludes()) {
            List<String> removes = new ArrayList<>();

            for (String exSource : getExcludes()) {
                SourceXML source = FFMPSourceConfigurationManager.getInstance()
                        .getSource(exSource);
                if (source != null) {
                    if (source.getSourceType() == SourceType.QPF) {
                        String dispName = source
                                .getDisplayNameForQuestionablePurposes();
                        if (qpfDispNames.contains(dispName)) {
                            removes.add(dispName);
                        }
                    }
                }
            }

            if (!removes.isEmpty()) {
                List<String> qpfSources = qpfProduct.getQpfList();
                List<String> overrideQpfTypes = new ArrayList<>();

                for (String remove : removes) {
                    qpfSources.remove(remove);
                }

                for (String qpfSource : qpfSources) {
                    String type = FFMPSourceConfigurationManager.getInstance()
                            .getSource(qpfSource)
                            .getDisplayNameForQuestionablePurposes();

                    if (!overrideQpfTypes.contains(type)) {
                        overrideQpfTypes.add(type);
                    }
                }

                qpfDispNames = overrideQpfTypes;
            }
        }

        return qpfDispNames;
    }

    /**
     * Gets the types with inclusion and exclusion filtering
     * 
     * @param guidProduct
     * @return
     */
    public List<String> getGuidanceDisplayNames(ProductXML guidProduct) {
        List<String> guidDispNames = guidProduct
                .getAvailableGuidanceDisplayNames();

        if (hasIncludes()) {
            for (String incSource : getIncludes()) {
                SourceXML source = FFMPSourceConfigurationManager.getInstance()
                        .getSource(incSource);
                if (source != null) {
                    if (source.isGuidance()) {
                        String dispName = source.getDisplayName();
                        if (!guidDispNames.contains(dispName)) {
                            guidDispNames.add(dispName);
                        }
                    }
                }
            }
        }

        if (hasExcludes()) {
            List<String> removes = new ArrayList<>();

            for (String exSource : getExcludes()) {
                SourceXML source = FFMPSourceConfigurationManager.getInstance()
                        .getSource(exSource);
                if (source != null) {
                    if (source.isGuidance()) {
                        String dispName = source.getDisplayName();
                        if (guidDispNames.contains(dispName)) {
                            removes.add(dispName);
                        }
                    }
                }
            }

            if (!removes.isEmpty()) {
                List<String> guidSources = guidProduct.getGuidanceSourceNames();
                List<String> overrideGuidTypes = new ArrayList<>();

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

                guidDispNames = overrideGuidTypes;
            }
        }

        Collections.sort(guidDispNames);

        return guidDispNames;
    }

    public List<String> getGuidanceSourceFamilies(ProductXML guidProduct) {
        List<String> guidFamilyNames = guidProduct
                .getAvailableGuidanceSourceFamilies();

        if (hasIncludes()) {
            for (String incSource : getIncludes()) {
                SourceXML source = FFMPSourceConfigurationManager.getInstance()
                        .getSource(incSource);
                if (source != null) {
                    if (source.isGuidance()) {
                        String dispName = source.getSourceFamily();
                        if (!guidFamilyNames.contains(dispName)) {
                            guidFamilyNames.add(dispName);
                        }
                    }
                }
            }
        }

        if (hasExcludes()) {
            List<String> removes = new ArrayList<>();

            for (String exSource : getExcludes()) {
                SourceXML source = FFMPSourceConfigurationManager.getInstance()
                        .getSource(exSource);
                if (source != null) {
                    if (source.isGuidance()) {
                        String dispName = source.getSourceFamily();
                        if (guidFamilyNames.contains(dispName)) {
                            removes.add(dispName);
                        }
                    }
                }
            }

            if (!removes.isEmpty()) {
                List<String> guidSources = guidProduct.getGuidanceSourceNames();
                List<String> overrideGuidFamilies = new ArrayList<>();

                for (String remove : removes) {
                    guidSources.remove(remove);
                }

                for (String guidSource : guidSources) {
                    String type = FFMPSourceConfigurationManager.getInstance()
                            .getSource(guidSource).getSourceFamily();

                    if (!overrideGuidFamilies.contains(type)) {
                        overrideGuidFamilies.add(type);
                    }
                }

                guidFamilyNames = overrideGuidFamilies;
            }
        }

        /*
         * this sort is ridiculously important as it will be used to index into
         * the basin table dialog
         * 
         * TODO: make ffmp more robust so the order is not that important
         */
        Collections.sort(guidFamilyNames);

        return guidFamilyNames;
    }

    /**
     * Gets a sources listing with includes and excludes
     * 
     * @param guidProduct
     * @param displayName
     * @return
     */
    public List<SourceXML> getGuidanceSourcesByDisplayName(
            ProductXML guidProduct, String displayName) {
        List<SourceXML> defaultGuidSources = guidProduct
                .getGuidanceSourcesByDisplayName(displayName);
        List<SourceXML> guidSources = new ArrayList<>();
        // if includes are present
        if (hasIncludes()) {
            for (String incSource : getIncludes()) {
                SourceXML source = FFMPSourceConfigurationManager.getInstance()
                        .getSource(incSource);
                if (source != null) {
                    if (source.isGuidance()
                            && source.getDisplayName().equals(displayName)) {
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

            List<SourceXML> removals = new ArrayList<>();

            for (SourceXML source : defaultGuidSources) {
                if (isExcluded(source.getSourceName())) {
                    if (guidSources.contains(source)) {
                        removals.remove(source);
                    }
                }
            }

            if (guidSources.isEmpty() && removals.isEmpty()) {
                guidSources = defaultGuidSources;
            }

            guidSources.removeAll(removals);
        }

        // default setup
        if (!hasIncludes() && !hasExcludes()) {
            guidSources = defaultGuidSources;
        } else {
            if (guidSources.isEmpty()) {
                guidSources = defaultGuidSources;
            }
        }

        return guidSources;
    }

    public List<SourceXML> getGuidanceSourcesBySourceFamily(
            ProductXML guidProduct, String sourceFamily) {
        List<SourceXML> defaultGuidSources = guidProduct
                .getGuidanceSourcesBySourceFamily(sourceFamily);
        List<SourceXML> guidSources = new ArrayList<>();
        // if includes are present
        if (hasIncludes()) {
            for (String incSource : getIncludes()) {
                SourceXML source = FFMPSourceConfigurationManager.getInstance()
                        .getSource(incSource);
                if (source != null) {
                    if (source.isGuidance()
                            && source.getSourceFamily().equals(sourceFamily)) {
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

            List<SourceXML> removals = new ArrayList<>();

            for (SourceXML source : defaultGuidSources) {
                if (isExcluded(source.getSourceName())) {
                    if (guidSources.contains(source)) {
                        removals.remove(source);
                    }
                }
            }

            if (guidSources.isEmpty() && removals.isEmpty()) {
                guidSources = defaultGuidSources;
            }

            guidSources.removeAll(removals);
        }

        // default setup
        if (!hasIncludes() && !hasExcludes()) {
            guidSources = defaultGuidSources;
        } else {
            if (guidSources.isEmpty()) {
                guidSources = defaultGuidSources;
            }
        }

        return guidSources;
    }

    /**
     * Gets the QPF sources with includes and excludes
     * 
     * @param qpfProduct
     * @param qpfDisplayName
     * @return
     */
    public List<SourceXML> getQpfSourcesByDisplayName(ProductXML qpfProduct,
            String qpfDisplayName) {
        List<SourceXML> defaultQpfSources = qpfProduct
                .getQpfSourcesByDisplayName(qpfDisplayName);
        List<SourceXML> qpfSources = new ArrayList<>();

        if (hasIncludes()) {
            for (String incSource : getIncludes()) {
                SourceXML source = FFMPSourceConfigurationManager.getInstance()
                        .getSource(incSource);
                if (source != null) {
                    if (source.getSourceType() == SourceType.QPF
                            && source.getDisplayNameForQuestionablePurposes()
                                    .equals(qpfDisplayName)) {
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

            List<SourceXML> removals = new ArrayList<>();

            for (SourceXML source : defaultQpfSources) {
                if (isExcluded(source.getSourceName())) {
                    if (qpfSources.contains(source)) {
                        removals.add(source);
                    }
                }
            }

            if (qpfSources.isEmpty() && removals.isEmpty()) {
                qpfSources = defaultQpfSources;
            }

            qpfSources.removeAll(removals);
        }

        // default setup
        if (!hasIncludes() && !hasExcludes()) {
            qpfSources = defaultQpfSources;
        } else {
            if (qpfSources.isEmpty()) {
                qpfSources = defaultQpfSources;
            }
        }

        return qpfSources;
    }

    @Override
    public String toString() {
        return "ProductRunXML [productName=" + productName + ", productKey="
                + productKey + ", excludes=" + excludes + ", includes="
                + includes + "]";
    }

}
