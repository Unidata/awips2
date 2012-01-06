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

public class FFMPCounties {
    public enum CountySort {
        NAME, ID, DISPLAY_NAME
    };

    private static CountySort sortBy = CountySort.ID;

    private ArrayList<FFMPCounty> countyArray;

    public FFMPCounties(ArrayList<FFMPCounty> counties) {

        setCounties(counties);
    }

    public void setCounties(ArrayList<FFMPCounty> counties) {
        ArrayList<FFMPCounty> removes = new ArrayList<FFMPCounty>();

        for (FFMPCounty county : counties) {
            if (county.getGid() == null) {
                removes.add(county);
            }
        }

        for (FFMPCounty county : removes) {
            counties.remove(county);
        }

        this.countyArray = counties;

        Collections.sort(this.countyArray);
    }

    public ArrayList<FFMPCounty> getCounties() {
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
