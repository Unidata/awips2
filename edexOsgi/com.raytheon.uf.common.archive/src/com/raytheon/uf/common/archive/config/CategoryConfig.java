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
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.junit.Assert;

/**
 * Information on a category for a given archive.
 * 
 * An example of a processed data category:
 * 
 * <pre>
 * &lt;category>
 *   &lt;name>redbook&lt;/name>
 *   &lt;!-- When 0 default to the parent archive's retentionHours -->
 *   &lt;retentionHours>0&lt;/retentionHours>
 *   &lt;dirPattern>hdf5/redbook/([^/]*)/&lt;/dirPattern>
 *   &lt;display>redbook - \1&lt;/display>
 *   &lt;saveDir>hdf5/redbook/([^/]*)/&lt;/saveDir>
 *   &lt;saveFiles>redbook-${YYYY}-${MM}-${DD}-${HH}\..*&lt;/saveFiles>
 * &lt;/category>
 * </pre>
 * 
 * An example of a raw data category:
 * 
 * <pre>
 * &lt;category>
 *   &lt;name>Model grib&lt;/name>
 *   &lt;retentionHours>0&lt;/retentionHours>
 *   &lt;dirPattern>grib/[^/]*&#47[^/]*&#47(.*\)&lt;/dirPattern>
 *   &lt;display>\1&lt;/display>
 *   &lt;saveDir>grib/${YYYY}${MM}${DD}/${HH}/(.*)&lt;/saveDir>
 *   &lt;saveFiles>.*&lt;/saveFiles>
 *   &lt;selectList>grib/${YYYY}${MM}${DD}/${HH}/NCEP_QPF/&lt;/selectList>
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
    @XmlElement(name = "retentionHours")
    private int retentionHours;

    /**
     * A regex pattern to find directories controlled by the category. These
     * directories should be relative to the parent archive's directory. For
     * example:
     * 
     * <pre>
     * &lt;dirPattern>grib2/[^/]*&#47;[^/]*&#47;(.*)/&lt;/dirPattern>
     * </pre>
     */
    @XmlElement(name = "dirPattern")
    private String dirPattern;

    /**
     * Use to display the information found by the dirPattern. Any groups in the
     * dirPatern may be displayed. For example:
     * 
     * <pre>
     * &lt;dirName>grib2/[^/]*&#47;[^/]*&#47;(.*)&lt;/dirName>
     * &lt;display>grib2 - \1&lt;/display>
     * </pre>
     * 
     * The \1 will be replaced with directory names 2 levels down from the grib2
     * directory.
     */
    @XmlElement(name = "display")
    private String display;

    /**
     * A pattern to find the directories to save. This may contain the year,
     * month, day and hour information. For example:
     * 
     * <pre>
     * &lt;saveDir>grib2/${YYYY}${MM}{$DD}/${HH}/.*&lt;/saveDir>
     * </pre>
     */
    @XmlElement(name = "saveDir")
    private String saveDir;

    /**
     * A saveDir directory may contain files with data for several days. This
     * allows any of the year, month, day and hour to be part of a file name.
     * Default is all files in the directory. For example:
     * 
     * <pre>
     * &lt;saveDir>hd5/redbook/(^/]*&#47/)&lt/saveDir>
     * &lt;saveFile>redbook-${YYYY}-${MM}-${DD}-${HH}\..*&lt;saveFiles>
     * </pre>
     */
    @XmlElement(name = "saveFiles")
    private String saveFiles;

    /**
     * This is list of selected directories. To be used to specify which
     * processed directories to move to /awips2/edex/data/archive.
     */
    @XmlElement(name = "selectList")
    private final List<String> selectList = new ArrayList<String>();

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
        Assert.assertNotNull(name);
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
        Assert.assertTrue(retentionHours > 0);
        this.retentionHours = retentionHours;
    }

    /**
     * Obtain the directory pattern.
     * 
     * @return dirPattern
     */
    public String getDirPattern() {
        return dirPattern;
    }

    /**
     * Set the directory pattern; must not be null.
     * 
     * @param dirPattern
     */
    public void setDirPattern(String dirPattern) {
        Assert.assertNotNull(dirPattern);
        this.dirPattern = dirPattern;
    }

    /**
     * Get the diaplay pattern.
     * 
     * @return display
     */
    public String getDisplay() {
        return display == null ? "" : display;
    }

    /**
     * Set the disaplay pattern.
     * 
     * @param display
     */
    public void setDisplay(String display) {
        Assert.assertNotNull(display);
        this.display = display;
    }

    /**
     * Get the save directory pattern..
     * 
     * @return savedir
     */
    public String getSaveDir() {
        return saveDir;
    }

    /**
     * Set the save directory pattern; must not be null.
     * 
     * @param saveDir
     */
    public void setSaveDir(String saveDir) {
        Assert.assertNotNull(saveDir);
        this.saveDir = saveDir;
    }

    /**
     * Get the save files pattern.
     * 
     * @return saveFiles
     */
    public String getSaveFiles() {
        return saveFiles;
    }

    /**
     * Set the save files pattern; may be null.
     * 
     * @param saveFiles
     */
    public void setSaveFiles(String saveFiles) {
        this.saveFiles = saveFiles;
    }

    /**
     * Get list of selected directories.
     * 
     * @return selectList
     */
    public List<String> getSelectList() {
        return new ArrayList<String>(selectList);
    }

    /**
     * Rest set the list of selected directories.
     * 
     * @param selectDir
     */
    public void setSelectList(Collection<String> selectDir) {
        this.selectList.clear();
        this.selectList.addAll(selectDir);
    }

    /**
     * Add an entry to the select list.
     * 
     * @param select
     */
    public void addSelectList(String select) {
        selectList.add(select);
    }

    /**
     * Remove an entry from the select list.
     * 
     * @param select
     */
    public void removeSelectList(String select) {
        selectList.remove(select);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
    public int compareTo(CategoryConfig o) {
        return getDisplay().compareTo(o.getDisplay());
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
        sb.append(", dirPattern: ").append(getDirPattern());
        sb.append(", display: ").append(getDisplay());
        sb.append(", saveDir: ").append(getSaveDir());
        sb.append(", saveFiles: ").append(getSaveFiles());
        sb.append(", selectList: [");
        String prefix = "\"";
        for (String sel : selectList) {
            sb.append(prefix).append(sel).append("\"");
            prefix = ", \"";
        }

        sb.append("]]");
        return sb.toString();
    }
}
