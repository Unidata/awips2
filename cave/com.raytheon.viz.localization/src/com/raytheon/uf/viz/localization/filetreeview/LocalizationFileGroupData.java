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
package com.raytheon.uf.viz.localization.filetreeview;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

/**
 * File Tree data object for node holding the actual files. (node is the
 * filename and is expandable)
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 3, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class LocalizationFileGroupData extends FileTreeEntryData {

    private Set<LocalizationFileEntryData> childrenData;

    /**
     * @param pathData
     * @param path
     *            should be a actual file, not a directory
     */
    public LocalizationFileGroupData(PathData pathData, String path) {
        super(pathData, path, false);
        childrenData = new HashSet<LocalizationFileEntryData>();
    }

    public Collection<LocalizationFileEntryData> getChildrenData() {
        return childrenData;
    }

    public void addChildData(LocalizationFileEntryData data) {
        childrenData.add(data);
    }
}
