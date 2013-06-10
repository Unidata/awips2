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
package com.raytheon.uf.viz.archive.data;

import java.util.List;

import com.raytheon.uf.common.archive.config.ArchiveConfigManager;

/**
 * This class used to maintain the state of a category so it can be restored
 * when reselected.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 24, 2013 1966        rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class CategoryInfo {
    private final String archiveName;

    private final String categoryName;

    private final List<ArchiveConfigManager.DisplayData> displayInfoList;

    public CategoryInfo(String archiveName, String categoryName,
            List<ArchiveConfigManager.DisplayData> displayInfoList) {
        this.archiveName = archiveName;
        this.categoryName = categoryName;
        this.displayInfoList = displayInfoList;
    }

    public String getArchiveName() {
        return archiveName;
    }

    public String getCategoryName() {
        return categoryName;
    }

    public List<ArchiveConfigManager.DisplayData> getDisplayInfoList() {
        return displayInfoList;
    }
}
