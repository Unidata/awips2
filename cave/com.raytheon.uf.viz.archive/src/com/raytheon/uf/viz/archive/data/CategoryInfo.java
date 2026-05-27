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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.raytheon.uf.common.archive.config.DisplayData;

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
 * Aug 14, 2013 2220       rferrel     Make sure displayDataList is never null.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class CategoryInfo {
    /** Archive name for the category. */
    private final String archiveName;

    /** Category's name. */
    private final String categoryName;

    /** List of display items for the category. */
    private List<DisplayData> displayDataList;

    /**
     * Constructor.
     * 
     * @param archiveName
     * @param categoryName
     * @param displayInfoList
     */
    public CategoryInfo(String archiveName, String categoryName,
            List<DisplayData> displayDataList) {
        this.archiveName = archiveName;
        this.categoryName = categoryName;
        if (displayDataList == null) {
            this.displayDataList = new ArrayList<DisplayData>(0);
        } else {
            setDisplayDataList(displayDataList);
        }
    }

    /**
     * Change the display data list.
     * 
     * @param displayDataList
     */
    public void setDisplayDataList(List<DisplayData> displayDataList) {
        this.displayDataList = new ArrayList<DisplayData>(
                displayDataList.size());
        this.displayDataList.addAll(displayDataList);
    }

    /**
     * @return archiveName
     */
    public String getArchiveName() {
        return archiveName;
    }

    /**
     * @return categoryName
     */
    public String getCategoryName() {
        return categoryName;
    }

    /**
     * Get unmodifiable display data list.
     * 
     * @return displayDataList
     */
    public List<DisplayData> getDisplayDataList() {
        return Collections.unmodifiableList(displayDataList);
    }
}
