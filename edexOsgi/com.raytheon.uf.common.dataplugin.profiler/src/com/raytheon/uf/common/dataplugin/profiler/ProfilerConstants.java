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
package com.raytheon.uf.common.dataplugin.profiler;

import java.util.HashMap;

/**
 * The ProfilerObs class encapsulates the location and time information for a
 * profiler observation as well as providing a container for the vertical level
 * data above the location.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20090415           2251 jsanchez    Initial Creation.
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class ProfilerConstants {

    public static final HashMap<String, String> PROFILER_MAP = new HashMap<String, String>();
    
    public static final HashMap<String, String> AIRPORT_MAP = new HashMap<String, String>();
    
    static {
        PROFILER_MAP.put("70197","CENA2");
        PROFILER_MAP.put("72246","LDBT2");
        PROFILER_MAP.put("74357","BLRW3");
        PROFILER_MAP.put("74431","MBWW4");
        PROFILER_MAP.put("74440","WLCI3");
        PROFILER_MAP.put("74445","NLGN1");
        PROFILER_MAP.put("74449","MBWW4");
        PROFILER_MAP.put("74466","WLCI3");
        PROFILER_MAP.put("74533","PLTC2");
        PROFILER_MAP.put("74541","HVLK1");
        PROFILER_MAP.put("74542","NDSK1");
        PROFILER_MAP.put("74546","HBRK1");
        PROFILER_MAP.put("74550","CNWM7");
        PROFILER_MAP.put("74551","LTHM7");
        PROFILER_MAP.put("74556","WNCI2");
        PROFILER_MAP.put("74630","AZCN5");
        PROFILER_MAP.put("74640","VCIO2");
        PROFILER_MAP.put("74647","LMNO2");
        PROFILER_MAP.put("74648","HKLO2");
        PROFILER_MAP.put("74649","PRCO2");
        PROFILER_MAP.put("74662","BLMM7");
        PROFILER_MAP.put("74731","TCUN5");
        PROFILER_MAP.put("74735","JTNT2");
        PROFILER_MAP.put("74750","PATT2");
        PROFILER_MAP.put("74752","DQUA4");
        PROFILER_MAP.put("74769","OKOM6");
    }
}
