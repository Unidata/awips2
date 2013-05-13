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
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.junit.Assert;

/**
 * Archive data information for retention, purging and archiving. An example:
 * 
 * <pre>
 * &lt;archive>
 *   &lt;name>Raw&lt;/name>
 *   &lt;rootDir>/data_store/&lt;/rootDir>
 *   &lt;!-- default retention hours for a category. -->
 *   &lt;retentionHours>168&lt;/retentionHours>
 *   &lt;category>
 *     &lt;name>Model grib&lt;/name>
 *     ...
 *   &lt;/category>
 *   &lt;category>
 *     &lt;name>Model grib2&lt;/name>
 *     ...
 *   &lt;/category>
 *   ...
 * &lt;archive>
 * </pre>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 1, 2013  1966       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "archive")
public class ArchiveConfig implements Comparable<ArchiveConfig> {
    /**
     * Archive name.
     */
    @XmlElement(name = "name")
    private String name;

    /**
     * Fully qualified directory controlled by this archive.
     */
    @XmlElement(name = "rootDir")
    private String rootDir;

    /**
     * Minimum number of hours the purger should retain data. May be overridden
     * for a given category.
     */
    @XmlElement(name = "retentionHours")
    private int retentionHours;

    /**
     * A category list that contain information on files/directories maintain
     * for the archiver.
     */
    @XmlElement(name = "category")
    List<CategoryConfig> categoryList;

    /**
     * Constructor.
     */
    public ArchiveConfig() {
        super();
    }

    /**
     * Get archive name. This should never be null.
     * 
     * @return name
     */
    public String getName() {
        return name;
    }

    /**
     * Set the name for the archiver.
     * 
     * @param name
     */
    public void setName(String name) {
        Assert.assertNotNull(name);
        this.name = name;
    }

    /**
     * Get the fully qualified name of the directory all components are relative
     * to.
     * 
     * @return rootDir
     */
    public String getRootDir() {
        return rootDir;
    }

    /**
     * Set the fully qualified name of the directory all components are relative
     * 
     * @param rootDir
     */
    public void setRootDir(String rootDir) {
        Assert.assertNotNull(rootDir);
        this.rootDir = rootDir;
    }

    /**
     * Retrive the archive's retention hours.
     * 
     * @return retentionHours
     */
    public int getRetentionHours() {
        return retentionHours;
    }

    /**
     * Set the retention hours; must be a value greater then zero.
     * 
     * @param retentionHours
     */
    public void setRetentionHours(int retentionHours) {
        Assert.assertTrue(retentionHours > 0);
        this.retentionHours = retentionHours;
    }

    /**
     * Obtain a copy the list of categories.
     * 
     * @return
     */
    public List<CategoryConfig> getCategoryList() {
        return new ArrayList<CategoryConfig>(categoryList);
    }

    /**
     * Set the category list.
     * 
     * @param categoryList
     */
    public void setCategoryList(List<CategoryConfig> categoryList) {
        this.categoryList = categoryList;
    }

    @Override
    public int compareTo(ArchiveConfig o) {
        return getName().compareTo(o.getName());
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("ArchiveData [label: ").append(getName());
        sb.append(", rootDir: ").append(getRootDir());
        sb.append(", retentionHours: ").append(getRetentionHours());
        for (CategoryConfig subc : getCategoryList()) {
            sb.append("\n\t").append(subc.toString());
        }
        sb.append("]");
        return sb.toString();
    }
}
