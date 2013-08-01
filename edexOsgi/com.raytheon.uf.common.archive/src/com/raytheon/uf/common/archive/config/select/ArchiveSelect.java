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
package com.raytheon.uf.common.archive.config.select;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Select configuration archive information.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 19, 2013 2221       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "archive")
public class ArchiveSelect {
    /**
     * The archive name.
     */
    @XmlElement(name = "name")
    private String name;

    /**
     * List of categories with selections.
     */
    @XmlElement(name = "category")
    private final List<CategorySelect> categorySelectList = new ArrayList<CategorySelect>();

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public List<CategorySelect> getCategorySelectList() {
        return categorySelectList;
    }

    public void setCategorySelectList(List<CategorySelect> categorySelectList) {
        this.categorySelectList.clear();
        this.categorySelectList.addAll(categorySelectList);
    }

    public void add(CategorySelect categorySelect) {
        categorySelectList.add(categorySelect);
    }

    public boolean isEmpty() {
        return categorySelectList.isEmpty();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Archive Select [name : ").append(getName());
        sb.append("[");
        for (CategorySelect categorySelect : categorySelectList) {
            sb.append(categorySelect).append(", ");
        }
        sb.append("]");
        return sb.toString();
    }

}
