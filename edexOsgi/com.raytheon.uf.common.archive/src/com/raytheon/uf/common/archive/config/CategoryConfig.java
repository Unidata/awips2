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
 *   &lt;retentionHours>0&lt;/retentionHours>
 *   &lt;dirPattern>hdf5/(redbook)&lt;/dirPattern>
 *   &lt;displayLabel>{1}&lt;/displayLabel>
 *   &lt;filePattern>redbook-(\d{4})-(\d{2})-(\d{2})-(\d{2})\..*&lt;/filePattern>
 *   &lt;dateGroupIndices>2,3,4,5&lt;/dateGroupIndices>
 * &lt;/category>
 * </pre>
 * 
 * An example of a raw data category:
 * 
 * <pre>
 * &lt;category>
 *   &lt;name>Model grib&lt;/name>
 *   &lt;retentionHours>0&lt;/retentionHours>
 *   &lt;dirPattern>grib/(\d{4})(\d{2})(\d{2})/(\d{2})/(.*)&lt;/dirPattern>
 *   &lt;displayLabel>{5}&lt;/displayLabel>
 *   &lt;dateGroupIndices>1,2,3,4&lt;/dateGroupIndices>
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
     * &lt;dirPattern>grib2/\d{8}/\d{2}/(.*)/&lt;/dirPattern>
     * </pre>
     */
    @XmlElement(name = "dirPattern")
    private String dirPattern;

    /**
     * Use to display the information found by the dirPattern. Any groups in the
     * dirPatern may be displayed. For example:
     * 
     * <pre>
     * &lt;dirName>(grib2)/(\d{4})(\d{2})(\d{2})/(\d{2})/(.*)&lt;/dirName>
     * &lt;displayLabel>{1} - {6}&lt;/displayLabel>
     * </pre>
     * 
     * The {1} will be replaced by the first group (grib2) in the regex
     * expression in dirName. The {6} is the sixth group (.*). {0} is the whole
     * expression match.
     */
    @XmlElement(name = "displayLabel")
    private String display;

    /**
     * A comma separated list of 4 numbers representing the group indices
     * specifying the location of the numeric date time stamp. The indices must
     * be in the order of year, month, day and hour. The group numbering starts
     * with the first group in the dirPattern and continues with any grouping in
     * the filePattern.
     * 
     * <pre>
     *   &lt;dirPattern>hdf5/(redbook)&lt;/dirPattern>
     *   &lt;displayLabel>{1}&lt;/displayLabel>
     *   &lt;filePattern>redbook-(\d{4})-(\d{2})-(\d{2})-(\d{2})\..*&lt;/filePattern>
     *   &lt;dateGroupIndices>2,3,4,5&lt;/dateGroupIndices>
     * </pre>
     */
    @XmlElement(name = "dateGroupIndices")
    private String dateGroupIndices;

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
    @XmlElement(name = "filePattern")
    private String filePattern;

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
        this.dirPattern = dirPattern;
    }

    /**
     * Get the display label pattern.
     * 
     * @return display
     */
    public String getDisplay() {
        return display == null ? "" : display;
    }

    /**
     * Set the display label pattern.
     * 
     * @param display
     */
    public void setDisplay(String display) {
        this.display = display;
    }

    /**
     * Get the save directory pattern..
     * 
     * @return dateGroups
     */
    public String getDateGroupIndices() {
        return dateGroupIndices;
    }

    /**
     * Set the save directory pattern; must not be null.
     * 
     * @param saveDir
     */
    public void setDateGroupIndices(String dateGroupIndices) {
        this.dateGroupIndices = dateGroupIndices;
    }

    /**
     * Get the save files pattern.
     * 
     * @return saveFiles
     */
    public String getFilePattern() {
        return filePattern;
    }

    /**
     * Set the save files pattern; may be null.
     * 
     * @param saveFiles
     */
    public void setFilePattern(String filePattern) {
        this.filePattern = filePattern;
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
        sb.append(", filePattern: ").append(getFilePattern());
        sb.append(", displayLabel: ").append(getDisplay());
        sb.append(", dateGroupIndices: ").append(getDateGroupIndices());
        sb.append("]");
        return sb.toString();
    }
}
