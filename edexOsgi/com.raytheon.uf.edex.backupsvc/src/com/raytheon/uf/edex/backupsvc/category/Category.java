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
package com.raytheon.uf.edex.backupsvc.category;

import java.util.ArrayList;
import java.util.List;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElementWrapper;

/**
 * Localization Category as specified in config XML file
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 23, 2021 96321      Robert.Blum Initial creation
 *
 * </pre>
 *
 * @author Robert.Blum
 */

@XmlAccessorType(XmlAccessType.NONE)
public class Category {

    @XmlElement(name = "categoryName")
    private String categoryName;

    @XmlElementWrapper(name = "include")
    @XmlElement(name = "file")
    private List<String> includeFileList = new ArrayList<>();

    public Category() {

    }

    public Category(String categoryName, List<String> includeFileList) {
        this.categoryName = categoryName;
        this.includeFileList = includeFileList;
    }

    public String getCategoryName() {
        return categoryName;
    }

    public void setCategoryName(String name) {
        this.categoryName = name;
    }

    public List<String> getIncludeFileList() {
        return includeFileList;
    }

    public void setIncludeFileList(List<String> includeFileList) {
        this.includeFileList = includeFileList;
    }
}
