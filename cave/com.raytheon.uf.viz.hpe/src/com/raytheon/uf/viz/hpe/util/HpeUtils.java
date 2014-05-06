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
package com.raytheon.uf.viz.hpe.util;

import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.grid.dataset.DatasetInfo;
import com.raytheon.uf.common.dataplugin.grid.dataset.DatasetInfoLookup;

/**
 * HPE Utilities
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 6, 2014     3026    mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class HpeUtils {
    private static final String HPE = "HPE";

    /**
     * Determine if this title represents an HPE model.
     * 
     * @param gridRecord
     *            The gridRecord to check
     * @return true if model is HPE, false otherwise
     * 
     */
    public static boolean isHpe(GridRecord gridRecord) {
        String title = null;
        if (gridRecord != null) {
            DatasetInfo info = DatasetInfoLookup.getInstance().getInfo(
                    gridRecord.getDatasetId());
            if (info != null) {
                title = info.getTitle();
            }
        }

        if (title == null) {
            return false;
        }
        return HPE.equals(title);
    }
}
