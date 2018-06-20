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
package com.raytheon.uf.edex.plugin.mpe.fieldgen.hpe;

import java.util.Calendar;
import java.util.Collections;
import java.util.Map;

import com.raytheon.uf.edex.plugin.mpe.precip.GageData;

/**
 * POJO containing a collection of inputs gathered for HPE Field Gen.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 19, 2016 5631       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class HPEFieldGenInputs {

    private Map<Calendar, GageData> gageDataMap;

    private Map<String, RadarLocRecord> radarLocMap;

    private String categoryName;

    private double[][] radarBeamHeight;

    public Map<Calendar, GageData> getGageDataMap() {
        if (gageDataMap == null) {
            return Collections.emptyMap();
        }
        return gageDataMap;
    }

    public void setGageDataMap(Map<Calendar, GageData> gageDataMap) {
        this.gageDataMap = gageDataMap;
    }

    public Map<String, RadarLocRecord> getRadarLocMap() {
        return radarLocMap;
    }

    public void setRadarLocMap(Map<String, RadarLocRecord> radarLocMap) {
        this.radarLocMap = radarLocMap;
    }

    public String getCategoryName() {
        return categoryName;
    }

    public void setCategoryName(String categoryName) {
        this.categoryName = categoryName;
    }

    public double[][] getRadarBeamHeight() {
        return radarBeamHeight;
    }

    public void setRadarBeamHeight(double[][] radarBeamHeight) {
        this.radarBeamHeight = radarBeamHeight;
    }
}