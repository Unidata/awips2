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
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.archive.config.select.ArchiveSelect;
import com.raytheon.uf.common.archive.config.select.CategorySelect;

/**
 * Select configuraton information for retention and case creation.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 19, 2013 2221       rferrel     Initial creation
 * Dec 11, 2013 2603       rferrel     Make selections a set.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "selection")
public class SelectConfig {
    /**
     * Name of selection.
     */
    @XmlElement(name = "name")
    private String name;

    /**
     * Hours to back off Start time from the End time.
     */
    @XmlElement(name = "startRetentionHours")
    private long starRetentionHours = 24L;

    /**
     * List of archives
     */
    @XmlElement(name = "archive")
    private final List<ArchiveSelect> archiveList = new ArrayList<ArchiveSelect>();

    /**
     * Constructor.
     */
    public SelectConfig() {
        super();
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public long getStarRetentionHours() {
        return starRetentionHours;
    }

    public void setStarRetentionHours(long startRetentionHours) {
        this.starRetentionHours = startRetentionHours;
    }

    public List<ArchiveSelect> getArchiveList() {
        return archiveList;
    }

    public void setArchiveList(List<ArchiveSelect> archiveList) {
        this.archiveList.clear();
        this.archiveList.addAll(archiveList);
    }

    public void add(ArchiveSelect archiveSelect) {
        archiveList.add(archiveSelect);
    }

    /**
     * 
     * @return true when no selections
     */
    public boolean isEmpty() {
        return archiveList.isEmpty();
    }

    /**
     * Get a set of selected display names for the archive and its category.
     * 
     * @param archiveName
     * @param categoryName
     * @return displayLabelList may be an empty list.
     */
    public Set<String> getSelectedSet(String archiveName, String categoryName) {
        ArchiveSelect archiveSelect = getArchive(archiveName);
        if (archiveSelect == null || archiveSelect.isEmpty()) {
            return new HashSet<String>(0);
        }
        CategorySelect categorySelect = getCategorySelect(categoryName,
                archiveSelect);
        if (categorySelect == null || categorySelect.isEmpty()) {
            return new HashSet<String>(0);
        }

        Set<String> selected = categorySelect.getSelectSet();

        return selected;
    }

    /**
     * Find archive with given name
     * 
     * @param archiveName
     * @return archive select or null if none found. message
     */
    private ArchiveSelect getArchive(String archiveName) {
        if (!archiveList.isEmpty()) {
            for (ArchiveSelect archiveSelect : archiveList) {
                if (archiveName.equals(archiveSelect.getName())) {
                    return archiveSelect;
                }
            }
        }
        return null;
    }

    /**
     * Find category for the given name under the desired archive.
     * 
     * @param categoryName
     * @param archiveSelect
     * @return categorySelect or null if none found
     */
    private CategorySelect getCategorySelect(String categoryName,
            ArchiveSelect archiveSelect) {
        if (!archiveSelect.isEmpty()) {
            for (CategorySelect categorySelect : archiveSelect
                    .getCategorySelectList()) {
                if (categoryName.equals(categorySelect.getName())) {
                    return categorySelect;
                }
            }
        }
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("selectData [name: ").append(getName());
        sb.append(", startRetentionHours: ").append(getStarRetentionHours());
        sb.append("[");
        for (ArchiveSelect archiveConfig : getArchiveList()) {
            sb.append(archiveConfig).append(", ");
        }
        sb.append("]]");
        return sb.toString();
    }
}
