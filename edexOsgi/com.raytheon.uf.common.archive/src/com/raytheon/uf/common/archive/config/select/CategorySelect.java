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
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Select configuration class that contains a list of selected display labels
 * for a given category. It is assumed this is associated with a given archive.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 19, 2013 2221       rferrel     Initial creation
 * Dec 11, 2013 2603       rferrel     Selected now a Set.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "archive")
public class CategorySelect {
    /**
     * The category name.
     */
    @XmlElement(name = "name")
    private String name;

    /**
     * List of selected labels.
     */
    @XmlElement(name = "selectedDisplayName")
    private final Set<String> selectSet = new HashSet<String>();

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Set<String> getSelectSet() {
        return selectSet;
    }

    public void setSelectSet(Set<String> selectList) {
        this.selectSet.clear();
        this.selectSet.addAll(selectList);
    }

    public void add(String displayName) {
        selectSet.add(displayName);
    }

    public boolean isEmpty() {
        return selectSet.isEmpty();
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("CategorySelect [ name: ").append(getName());
        sb.append("[ ");
        for (String select : getSelectSet()) {
            sb.append("\"").append(select).append("\", ");
        }
        sb.append("]");
        return sb.toString();
    }
}
