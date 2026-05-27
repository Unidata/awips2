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
package com.raytheon.uf.common.dataplugin.ffmp;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * 
 * Contains a list of FFMPCounty objects
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ???                                 Initial creation
 * Jul 02, 2018 6641       njensen     Use List instead of ArrayList
 *
 * </pre>
 *
 */
public class FFMPCounties {

    public enum CountySort {
        NAME, ID, DISPLAY_NAME
    }

    private static CountySort sortBy = CountySort.ID;

    private List<FFMPCounty> countyArray;

    public FFMPCounties(List<FFMPCounty> counties) {
        setCounties(counties);
    }

    public void setCounties(List<FFMPCounty> counties) {
        countyArray = new ArrayList<>();

        for (FFMPCounty county : counties) {
            if (county.getGid() != null) {
                countyArray.add(county);
            }
        }

        Collections.sort(this.countyArray);
    }

    public List<FFMPCounty> getCounties() {
        return countyArray;
    }

    public void sortBy(CountySort sortBy) {
        FFMPCounties.sortBy = sortBy;
        Collections.sort(countyArray);
    }

    public static CountySort getSortBy() {
        return sortBy;
    }

    public void addCounty(FFMPCounty county) {
        countyArray.add(county);
    }
}
