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
package com.raytheon.viz.aviation.climatology;

import java.util.ArrayList;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 28, 2009            avarani     Initial creation
 * 
 * </pre>
 * 
 * @author avarani
 * @version 1.0
 */

public class WindRoseData {
    private ArrayList<Object> list;

    private int year1;

    private int year2;

    public WindRoseData() {
        list = new ArrayList<Object>();
    }

    public void addDataPoint(String hour, String windDirType, String windDir,
            String windSpd) {
        ArrayList<String> temp = new ArrayList<String>();
        temp.add(hour);
        temp.add(windDirType);
        temp.add(windDir);
        temp.add(windSpd);
        list.add(temp);
    }

    public ArrayList<Object> getList() {
        return list;
    }

    public void setYears(int year1, int year2) {
        this.year1 = year1;
        this.year2 = year2;
    }

    public String getYears() {
        return year1 + "-" + year2;
    }
}
