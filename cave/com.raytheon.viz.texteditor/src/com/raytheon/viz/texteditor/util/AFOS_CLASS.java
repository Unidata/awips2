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
package com.raytheon.viz.texteditor.util;

import java.util.HashMap;

/**
 * Automation of Field Operations and Services classification enum
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 13, 2008            jkorman     Initial creation
 * May 20, 2014 2536       bclement    moved from edex.textdb to viz.texteditor
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public enum AFOS_CLASS {

    Public("Public"), Aviation("Aviation"), Hydrology("Hydrology"), LocalObs(
            "Local Obs."), RadarUpperAir("Radar/Upper Air"), WatchWarn(
            "Watch/Warning"), FireWxAirPoll("Fire Wx Air Poll."), Agriculture(
            "Agriculture"), Marine("Marine"), Administrative("Administrative"), Miscellaneous(
            "Miscellaneous"), NationalGuidance("National Guidance"), NatlReglAviation(
            "Natl/Regl/Aviation"), SevereLocalStorm("Severe Local Storm"), NationalObs(
            "National Obs."), NarrDiscCodes("Narr/Disc./Codes"), TablesSummaries(
            "Tables/Summaries"), HurricaneTropical("Hurricane/Tropical");

    private final String value;
    private static final HashMap<String, AFOS_CLASS> CLASS_MAP = new HashMap<String, AFOS_CLASS>();
    static {
        for (AFOS_CLASS origin : AFOS_CLASS.values()) {
            CLASS_MAP.put(origin.value(), origin);
        }
    }
    
    private AFOS_CLASS(String value) {
        this.value = value;
    }

    public String value() {
        return value;
    }

    /**
     * 
     * @param key
     * @return
     */
    public static AFOS_CLASS getFromClassKey(String key) {
        return CLASS_MAP.get(key);
    }



}
