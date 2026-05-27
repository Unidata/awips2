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
package com.raytheon.viz.hydro.pointprecipitation;

import java.util.HashMap;
import java.util.Map;

/**
 * Point Precipitation Data Object.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 5, 2009  2257       mpduff      Initial creation
 * Feb 1, 2019  6951       dgilling    Major refactor.
 *
 * </pre>
 *
 * @author mpduff
 */

public class PointPrecipData {

    private final String lid;

    private final String pe;

    private final String ts;

    private final Map<Integer, Boolean> summedFlagMap;

    private final Map<Integer, Float> hrFillMap;

    private final Map<Integer, Float> amountMap;

    private float maxValue;

    public PointPrecipData(String lid, String pe, String ts) {
        this.lid = lid;
        this.pe = pe;
        this.ts = ts;
        this.summedFlagMap = new HashMap<>();
        this.hrFillMap = new HashMap<>();
        this.amountMap = new HashMap<>();
        this.maxValue = PointPrecipConstants.MISSING_PRECIP;
    }

    public String getLid() {
        return lid;
    }

    public String getPe() {
        return pe;
    }

    public String getTs() {
        return ts;
    }

    public float getMaxValue() {
        return maxValue;
    }

    public void addData(int duration, float hours_covered, float value,
            boolean summedFlag) {
        amountMap.put(duration, value);
        hrFillMap.put(duration, hours_covered);
        summedFlagMap.put(duration, summedFlag);
        maxValue = Math.max(maxValue, value);
    }

    public float getValue(int duration) {
        return amountMap.getOrDefault(duration,
                PointPrecipConstants.MISSING_PRECIP);
    }

    public float getHrFill(int duration) {
        return hrFillMap.getOrDefault(duration, 0f);
    }

    public boolean isSummedFlag(int duration) {
        return summedFlagMap.getOrDefault(duration, false);
    }
}
