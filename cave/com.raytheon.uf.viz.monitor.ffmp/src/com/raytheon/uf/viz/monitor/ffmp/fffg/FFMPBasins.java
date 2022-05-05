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
package com.raytheon.uf.viz.monitor.ffmp.fffg;

import java.util.Collections;
import java.util.List;

/**
 * 
 * Container of FFMPBasinNameIdData
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ???                                 Initial creation
 * Jul 02, 2018 6641       njensen     Use List interface instead of ArrayList
 *
 * </pre>
 *
 */
public class FFMPBasins {
    public enum BasinSort {
        NAME, ID
    }

    private static BasinSort sortBy = BasinSort.ID;

    private List<FFMPBasinNameIdData> basinArray;

    public FFMPBasins(List<FFMPBasinNameIdData> basinArray, BasinSort sortBy) {
        this.basinArray = basinArray;
        sortBy(sortBy);
    }

    public List<FFMPBasinNameIdData> getBasins() {
        return basinArray;
    }

    public void setBasins(List<FFMPBasinNameIdData> basinArray) {
        this.basinArray = basinArray;
        Collections.sort(basinArray);
    }

    public void sortBy(BasinSort sortBy) {
        FFMPBasins.sortBy = sortBy;

        if (basinArray != null) {
            Collections.sort(basinArray);
        }
    }

    public static BasinSort getSortBy() {
        return sortBy;
    }
}
