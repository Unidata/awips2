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
package com.raytheon.uf.viz.alertviz.config;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.viz.alertviz.config.TrayConfiguration.TrayMode;

/**
 * Contains the configuration for the AlertViz capability.
 *
 * A "master copy" of the configuration is kept in memory for performance
 * reasons for dispatching alerts. This copy is read only...and is reloaded when
 * a non-master copy performs a save.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- -------------------------------------------
 * Sep 09, 2008  1433     chammack   Initial creation
 * May 03, 2011  9067     cjeanbap   Add isMonitorLayoutChanged() method.
 * Apr 27, 2012  13744    Xiaochuan  Update isMonitorLayoutChanged() to compare
 *                                   source size in Previous and current.
 * Oct 28, 2005  5054     randerso   removed bar position as it was written but
 *                                   never read
 * Sep 13, 2018  7461     dgilling   Return true from isMonitorLayoutChanged if
 *                                   monitor image has been changed.
 * Nov 02, 2018  7600     randerso   Changes to support standard script files
 *                                   for AlertViz actions.
 * Nov 05, 2018  7509     randerso   Remove Log attribute from AlertMetaData
 *
 * </pre>
 *
 * @author chammack
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "alertConfiguration")
public class Configuration {

    @XmlAttribute
    private String name;

    /**
     * The global configuration items
     */
    @XmlElement
    private TrayConfiguration globalConfiguration;

    /**
     * The sources
     */
    private Map<String, Source> sourceMap = new HashMap<>();

    /**
     * The categories
     */
    private Map<String, Category> categoryMap = new HashMap<>();

    /**
     * @return the sources
     */
    public Map<String, Source> getSources() {
        return sourceMap;
    }

    /**
     * @param sources
     *            the sources to set
     */
    public void setSources(Map<String, Source> sources) {
        this.sourceMap = sources;
    }

    /**
     * @return the categories
     */
    public Map<String, Category> getCategories() {
        return categoryMap;
    }

    /**
     * @param categories
     *            the categories to set
     */
    public void setCategories(Map<String, Category> categories) {
        this.categoryMap = categories;
    }

    /**
     * @return the globalConfiguration
     */
    public TrayConfiguration getGlobalConfiguration() {
        return globalConfiguration;
    }

    /**
     * @param globalConfiguration
     *            the globalConfiguration to set
     */
    public void setGlobalConfiguration(TrayConfiguration globalConfiguration) {
        this.globalConfiguration = globalConfiguration;
    }

    /**
     * Look up a source
     *
     * @param componentName
     * @return the source
     */
    public Source lookupSource(String componentName) {
        return this.sourceMap.get(componentName);
    }

    /**
     * Look up a category
     *
     * @param componentName
     * @return the category
     */
    public Category lookupCategory(String componentName) {
        return this.categoryMap.get(componentName);
    }

    /**
     * Set the collection of sources as an array
     *
     * This is usually used by serialization, not end users.
     *
     * @param sources
     */
    @XmlElement(name = "source")
    public void setSourceCollection(Source[] sources) {
        sourceMap.clear();
        for (Source source : sources) {
            sourceMap.put(source.getName(), source);
        }
    }

    /**
     * Return the collection of sources as an array
     *
     * This is usually used by serialization, not end users.
     *
     * @return the source collection
     */
    public Source[] getSourceCollection() {
        return sourceMap.values().toArray(new Source[sourceMap.size()]);
    }

    /**
     * Set the collection of categories as an array
     *
     * This is usually used by serialization, not end users.
     *
     * @param categories
     */
    @XmlElement(name = "category")
    public void setCategoryCollection(Category[] categories) {
        categoryMap.clear();
        for (Category category : categories) {
            categoryMap.put(category.getCategoryName(), category);
        }
    }

    /**
     * Return the collection of categories as an array
     *
     * This is usually used by serialization, not end users.
     *
     * @return the category collection
     */
    public Category[] getCategoryCollection() {
        return categoryMap.values().toArray(new Category[categoryMap.size()]);
    }

    /**
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * @param name
     */
    public void setName(String name) {
        this.name = name;
    }

    @Override
    public Configuration clone() {
        Configuration newConfig = new Configuration();
        newConfig.name = name;
        newConfig.categoryMap = new HashMap<>();
        newConfig.sourceMap = new HashMap<>();
        if (globalConfiguration != null) {
            newConfig.globalConfiguration = globalConfiguration.clone();
        }

        Set<String> names = categoryMap.keySet();
        for (String name : names) {
            newConfig.categoryMap.put(name, categoryMap.get(name).clone());
        }

        names = sourceMap.keySet();
        for (String name : names) {
            newConfig.sourceMap.put(name, sourceMap.get(name).clone());
        }

        return newConfig;
    }

    @Override
    public String toString() {
        StringBuilder rval = new StringBuilder(getName()).append(":\n");
        rval.append("\tTrayConfig:\n");
        rval.append("\t\t").append("audioDuration=")
                .append(globalConfiguration.getAudioDuration())
                .append(" blinkDuration=")
                .append(globalConfiguration.getBlinkDuration())
                .append(" logLength=")
                .append(globalConfiguration.getLogLength()).append('\n');
        rval.append("\t\t").append("mode=")
                .append(globalConfiguration.getMode()).append('\n');
        rval.append("\t\t").append("categoryShown=")
                .append(globalConfiguration.isCategoryShown())
                .append(" expandedPopup=")
                .append(globalConfiguration.isExpandedPopup())
                .append(" priorityShown=")
                .append(globalConfiguration.isPriorityShown())
                .append(" sourceKeyShown=")
                .append(globalConfiguration.isSourceKeyShown()).append('\n');
        rval.append("\tCategories:\n");
        for (Category c : categoryMap.values()) {
            rval.append("\t\t").append(c.getCategoryName()).append(" - ")
                    .append(c.getLongName()).append(":\n");
            rval.append("\t\t\tlocked=").append(c.isLocked())
                    .append(" textBox=").append(c.getTextBox()).append('\n');
        }
        rval.append("\tSources:\n");
        for (Source s : sourceMap.values()) {
            rval.append("\t\t").append(s.getName()).append(" - ")
                    .append(s.getLongName()).append(":\n");
            rval.append("\t\t\tlocked=").append(s.isLocked()).append('\n');
            rval.append("\t\t\tConfiguration Item:\n");
            rval.append("\t\t\t\tMetadata:\n");
            for (AlertMetadata am : s.getConfigurationItem()
                    .getPreferenceMapping().values()) {
                rval.append("\t\t\t\t\t").append(am.getPriority()).append(": ")
                        .append("background=")
                        .append(am.getBackgroundSerialized())
                        .append(" foreground=")
                        .append(am.getForegroundSerialized()).append('\n');
                rval.append("\t\t\t\t\t\taudioEnable=")
                        .append(am.isAudioEnabled()).append(" audioFile=")
                        .append(am.getAudioFile()).append(" text=")
                        .append(am.isText()).append('\n');
                rval.append("\t\t\t\t\t\tblink=").append(am.isBlink())
                        .append(" popup=").append(am.isPopup()).append('\n');
                rval.append("\t\t\t\t\t\taction=").append(am.getAction())
                        .append('\n');
            }
        }
        return rval.toString();
    }

    /**
     * Merge the categories and sources in the other config over the categories
     * and sources in this object and return the resulting configuration. As the
     * base of the merge, this object determines if a source or category will be
     * locked in the resulting config. As the overlay of the merge, the sources
     * and categories of the other config will overlay the base in the resulting
     * config.
     *
     * <pre>
     * this    other   Result
     * ----    -----   ------
     *  C1t     C1o     C1o
     *          C2o     C2o
     *  C3t             C3t
     * </pre>
     *
     * @param other
     * @param preserveLocking
     *            if this configuration's locking should be used in the result.
     * @return merged config
     */
    public Configuration mergeUnder(Configuration other,
            boolean preserveLocking) {
        Configuration result = other.clone();

        Set<String> catKeys = new HashSet<>();
        catKeys.addAll(this.categoryMap.keySet());
        catKeys.addAll(other.categoryMap.keySet());

        for (String catKey : catKeys) {
            if (result.categoryMap.containsKey(catKey)) {
                if (preserveLocking && this.categoryMap.containsKey(catKey)) {
                    Category resultCat = result.categoryMap.get(catKey);
                    Category baseCat = this.categoryMap.get(catKey);

                    resultCat.setLocked(baseCat.isLocked());
                    result.categoryMap.put(catKey, resultCat);
                }
            } else if (this.categoryMap.containsKey(catKey)) {
                result.categoryMap.put(catKey, this.categoryMap.get(catKey));
            }
        }

        Set<String> sourceKeys = new HashSet<>();
        sourceKeys.addAll(sourceMap.keySet());
        sourceKeys.addAll(other.sourceMap.keySet());

        for (String sourceKey : sourceKeys) {
            if (result.sourceMap.containsKey(sourceKey)) {
                if (preserveLocking && this.sourceMap.containsKey(sourceKey)) {
                    Source resultSource = result.sourceMap.get(sourceKey);
                    Source baseSource = this.sourceMap.get(sourceKey);

                    resultSource.setLocked(baseSource.isLocked());
                    result.sourceMap.put(sourceKey, resultSource);
                }
            } else if (this.sourceMap.containsKey(sourceKey)) {
                result.sourceMap.put(sourceKey, this.sourceMap.get(sourceKey));
            }
        }
        return result;
    }

    /**
     * Overlay the categories and sources in the given config over the existing
     * categories and sources in this object and return the resulting
     * configuration. As the base of the merge, this object determines if a
     * source or category will be in the resulting config, i.e. if this config
     * has as source or category and the overlaying config does not, the
     * resulting config will have it; if this config does not have a source or
     * category, the the resulting category will not. As the overlay, the
     * sources and categories of the other config will be in the resulting
     * config (but only if they exist in the base.)
     *
     * <pre>
     * this    other   Result
     * ----    -----   ------
     *  C1t     C1o     C1o
     *          C2o
     *  C3t             C3t
     * </pre>
     *
     * @param other
     * @param preserveLocking
     *            if this configuration's locking should be used in the result.
     * @return merged config
     */
    public Configuration overlayWith(Configuration other,
            boolean preserveLocking) {
        Configuration result = other.clone();

        Set<String> catKeys = new HashSet<>();
        catKeys.addAll(this.categoryMap.keySet());
        catKeys.addAll(other.categoryMap.keySet());

        for (String catKey : catKeys) {
            if (this.categoryMap.containsKey(catKey)) {
                Category baseCat = this.categoryMap.get(catKey);
                if (!result.categoryMap.containsKey(catKey)) {
                    result.categoryMap.put(catKey, baseCat);
                } else if (preserveLocking) {
                    Category resultCat = result.categoryMap.get(catKey);
                    resultCat.setLocked(baseCat.isLocked());
                    result.categoryMap.put(catKey, resultCat);
                }
            } else {
                Category cat = result.categoryMap.get(catKey);
                if (cat != null) {
                    result.categoryMap.remove(catKey);
                }
            }
        }

        Set<String> sourceKeys = new HashSet<>();
        sourceKeys.addAll(sourceMap.keySet());
        sourceKeys.addAll(other.sourceMap.keySet());

        for (String sourceKey : sourceKeys) {
            if (this.sourceMap.containsKey(sourceKey)) {
                Source baseSource = this.sourceMap.get(sourceKey);
                if (!result.sourceMap.containsKey(sourceKey)) {
                    result.sourceMap.put(sourceKey, baseSource);
                } else if (preserveLocking) {
                    Source resultSource = result.sourceMap.get(sourceKey);
                    resultSource.setLocked(baseSource.isLocked());
                    result.sourceMap.put(sourceKey, resultSource);
                }
            } else {
                Source source = result.sourceMap.get(sourceKey);
                if (source != null) {
                    result.sourceMap.remove(sourceKey);
                }
            }
        }
        return result;
    }

    /**
     * Determine if a Monitor was added/omitted (enable/disabled) and/or if the
     * Layout of the Alert Message Dialog was changed.
     *
     * @param configData
     *            the current save Configuration Data.
     * @return boolean, true if either Monitor and/or Layout was changed
     *         otherwise false.
     */
    public boolean isMonitorLayoutChanged(Configuration configData) {
        boolean modified = false;

        TrayMode prevLayoutMode = configData.getGlobalConfiguration().getMode();
        if (!prevLayoutMode.equals(this.getGlobalConfiguration().getMode())) {
            modified = true;
        }

        Map<String, Category> prevCategoryMap = configData.getCategories();
        for (Iterator<String> categories = this.getCategories().keySet()
                .iterator(); categories.hasNext() && !modified;) {
            String categoryName = categories.next();
            Category prevCategory = prevCategoryMap.get(categoryName);
            Category newCategory = this.getCategories().get(categoryName);
            if (prevCategory != null && newCategory == null) {
                modified = true;
            } else if (prevCategory == null && newCategory != null) {
                modified = true;
            } else if (prevCategory != null && newCategory != null) {
                if (prevCategory.getTextBox() != newCategory.getTextBox()) {
                    modified = true;
                }
            }
        }

        Map<String, Source> prevSources = configData.getSources();

        if (prevSources.size() != this.getSources().size()) {
            modified = true;
        } else {
            for (Iterator<String> sources = this.getSources().keySet()
                    .iterator(); sources.hasNext() && !modified;) {
                String sourceName = sources.next();
                Source prevSource = prevSources.get(sourceName);
                Source newSource = this.getSources().get(sourceName);
                if (prevSource == null) {
                    modified = true;
                } else if (prevSource != null && newSource != null) {
                    MonitorMetadata newMonitorMetadata = newSource
                            .getConfigurationMonitor().getMonitorMetadata();
                    MonitorMetadata prevMonitorMetadata = prevSource
                            .getConfigurationMonitor().getMonitorMetadata();

                    if (newMonitorMetadata != null
                            && prevMonitorMetadata == null) {
                        modified = true;
                    } else if ((newMonitorMetadata.getOmit())
                            && (!prevMonitorMetadata.getOmit())) {
                        modified = true;
                    }
                    if ((!newMonitorMetadata.getOmit())
                            && (prevMonitorMetadata.getOmit())) {
                        modified = true;
                    } else if (newMonitorMetadata.getImageFile() != null
                            && prevMonitorMetadata.getImageFile() == null) {
                        modified = true;
                    } else if (newMonitorMetadata.getImageFile() == null
                            && prevMonitorMetadata.getImageFile() != null) {
                        modified = true;
                    } else if ((newMonitorMetadata.getImageFile() != null)
                            && (prevMonitorMetadata.getImageFile() != null)
                            && (!newMonitorMetadata.getImageFile().equals(
                                    prevMonitorMetadata.getImageFile()))) {
                        modified = true;
                    }
                }
            }
        }

        return modified;
    }
}
