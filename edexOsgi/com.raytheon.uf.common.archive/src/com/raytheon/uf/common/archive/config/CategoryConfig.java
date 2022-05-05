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
package com.raytheon.uf.common.archive.config;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.TreeSet;
import java.util.regex.Pattern;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Information on a category for a given archive.
 *
 * An example of a processed data category:
 *
 * <pre>
 * &lt;category>
 *   &lt;name>redbook&lt;/name>
 *   &lt;!-- When 0 default to the parent archive's retentionHours -->
 *   &lt;selectedRetentionHours>0&lt;/selectedRetentionHours>
 *   &lt;dataSet>
 *      &lt;dirPattern>hdf5/(redbook)&lt;/dirPattern>
 *      &lt;displayLabel>{1}&lt;/displayLabel>
 *      &lt;filePattern>redbook-(\d{4})-(\d{2})-(\d{2})-(\d{2})\..*&lt;/filePattern>
 *      &lt;timeType>Date&lt;/timeType>
 *      &lt;dateGroupIndices>2,3,4,5&lt;/dateGroupIndices>
 *   &lt;/dataSet>
 * &lt;/category>
 * </pre>
 *
 * An example of a raw data category:
 *
 * <pre>
 * &lt;category>
 *   &lt;name>Model grib&lt;/name>
 *   &lt;selectedRetentionHours>0&lt;/selectedRetentionHours>
 *   &lt;dataSet>
 *      &lt;dirPattern>grib/(\d{4})(\d{2})(\d{2})/(\d{2})/(.*)&lt;/dirPattern>
 *      &lt;displayLabel>{5}&lt;/displayLabel>
 *      &lt;timeType>Date&lt;/timeType>
 *      &lt;dateGroupIndices>1,2,3,4&lt;/dateGroupIndices>
 *   &lt/dataSet>
 * &lt;/category>
 * </pre>
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 1, 2013  1966       rferrel     Initial creation
 * Aug 03, 2013 2224       rferrel     Changes to include DataSet.
 * Jan 09, 2014 2603       rferrel     Fix bug in setSelectedDisplayNames
 * Apr 17, 2014 3045       rferrel     Code cleanup to prevent null pointer.
 * Jan 22, 2014 3763       rferrel     Change tag extRententionHours to selectedRetentionHours
 * Mar 03, 2021 88877      tjensen     Added optional preserveDirPatterns
 *
 * </pre>
 *
 * @author rferrel
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "category")
public class CategoryConfig implements Comparable<CategoryConfig> {

    /**
     * The category's name.
     */
    @XmlElement(name = "name")
    private String name;

    /**
     * Minimum number of hours the purger should retain data. When 0 use the
     * parent archive's value.
     */
    @XmlElement(name = "selectedRetentionHours")
    private int retentionHours;

    /**
     * List of directory patterns to not be removed.
     */
    @XmlElement(name = "preserveDirPattern")
    private List<String> preserveDirPatterns;

    private List<Pattern> compiledPreserveDirPatterns;

    @XmlElement(name = "dataSet")
    private final List<CategoryDataSet> dataSetList = new ArrayList<>();

    @XmlElement(name = "selectedDisplayName")
    private final Collection<String> selectedDisplayNames = new TreeSet<>();

    /*
     * Constructor.
     */
    public CategoryConfig() {
        super();
    }

    /**
     * @return name
     */
    public String getName() {
        return name;
    }

    /**
     * Set the name; must not be null.
     *
     * @param name
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Get the retention hours.
     *
     * @return retentionHours
     */
    public int getRetentionHours() {
        return retentionHours;
    }

    /**
     * Set the retention hours must be greater then zero.
     */
    public void setRetentionHours(int retentionHours) {
        if (retentionHours > 0) {
            this.retentionHours = retentionHours;
        } else {
            this.retentionHours = 1;
        }
    }

    /**
     * @return the preserveDirPatterns
     */
    public List<String> getPreserveDirPatterns() {
        return preserveDirPatterns;
    }

    /**
     * @param preserveDirPatterns
     *            the preserveDirPatterns to set
     */
    public void setPreserveDirPatterns(List<String> preserveDirPatterns) {
        this.preserveDirPatterns = preserveDirPatterns;
    }

    /**
     * Compiles the patterns in preserveDirPatterns once, then returns the
     * compiled patterns.
     *
     * @return List of compiled patterns
     */
    public List<Pattern> getCompiledPreserveDirPatterns() {
        if (compiledPreserveDirPatterns == null) {
            if (preserveDirPatterns == null || preserveDirPatterns.isEmpty()) {
                compiledPreserveDirPatterns = Collections.emptyList();
            } else {
                compiledPreserveDirPatterns = new ArrayList<>(
                        preserveDirPatterns.size());
                for (String dirPattern : preserveDirPatterns) {
                    Pattern pattern = Pattern.compile(dirPattern);
                    compiledPreserveDirPatterns.add(pattern);
                }
            }
        }
        return compiledPreserveDirPatterns;
    }

    /**
     *
     * @return dataSetList
     */
    public List<CategoryDataSet> getDataSetList() {
        List<CategoryDataSet> list = new ArrayList<>(dataSetList.size());
        list.addAll(dataSetList);
        return list;
    }

    /**
     *
     * @param dataSetList
     */
    public void setDataSetList(List<CategoryDataSet> dataSetList) {
        this.dataSetList.clear();
        if (dataSetList != null) {
            this.dataSetList.addAll(dataSetList);
        }
    }

    /**
     *
     * @return selectedDisplayNames
     */
    public Collection<String> getSelectedDisplayNames() {
        return selectedDisplayNames;
    }

    /**
     *
     * @param selectedDisplayNames
     */
    public void setSelectedDisplayNames(
            Collection<String> selectedDisplayNames) {
        this.selectedDisplayNames.clear();
        if (selectedDisplayNames != null) {
            this.selectedDisplayNames.addAll(selectedDisplayNames);
        }
    }

    /**
     *
     * @param displayName
     */
    public void addSelectedDisplayName(String displayName) {
        selectedDisplayNames.add(displayName);
    }

    /**
     *
     * @param displayName
     */
    public void removeSelectedDisplayName(String displayName) {
        selectedDisplayNames.remove(displayName);
    }

    /*
     * (non-Javadoc)
     *
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
    public int compareTo(CategoryConfig o) {
        return getName().compareToIgnoreCase(o.getName());
    }

    /*
     * (non-Javadoc)
     *
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Category [ name: ").append(getName());
        sb.append(", retentionHours: ").append(getRetentionHours());
        sb.append(", dataSetList[ ");
        for (CategoryDataSet dataSet : getDataSetList()) {
            sb.append(dataSet).append(", ");
        }
        sb.append(", selectedDisplayNames: ");
        if (selectedDisplayNames == null) {
            sb.append("null");
        } else {
            sb.append("[");
            String prefix = "\"";
            for (String displayName : selectedDisplayNames) {
                sb.append(prefix).append(displayName).append("\"");
                prefix = " ,\"";
            }
            sb.append("]");
        }
        sb.append("]");
        return sb.toString();
    }
}
