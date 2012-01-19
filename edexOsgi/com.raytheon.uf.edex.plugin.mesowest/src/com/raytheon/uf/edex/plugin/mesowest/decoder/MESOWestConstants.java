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
package com.raytheon.uf.edex.plugin.mesowest.decoder;

import java.util.HashMap;
import java.util.Map;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 10, 2009            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public class MESOWestConstants {

    public static final String LAST_DATA_ITEM = "##### LAST DATA ITEM #####";
    
    public static final String TRACEID = "traceId";
    
    public static final String K_DATATYPE = "datatype";

    public static final String T_PARMHEADER = "parmheader";

    public static final String T_REPORTDATA = "report";
    
    public static final String T_LASTITEM = LAST_DATA_ITEM;
    
    public static final String D_PARMLEADER = "PARM =";
    
    public static final String D_PARM_DELIMIT = ";";
    
    public static final String P_STNID = "STNID";

    public static final String P_LAT = "latitude";

    public static final String P_LON = "longitude";

    public static final String P_ELEV = "ELEV";

    public static final String P_STNTYPE = "STNTYPE";

    public static final String P_DATATIME = "DATETIME";
    
    //*******************************************
    
    public static final String P_SFCTEMP = "TMPF";
    
    public static final String P_SFCDWPT = "DWPF";
    //************************

    public static final String P_MIN_T_24H = "LO24";

    public static final String P_MAX_T_24H = "HI24";
    
    //************************
    public static final String P_WINDSPD = "SKNT";
    
    public static final String P_WINDDIR = "DRCT";
    
    public static final String P_WINDGST = "GUST";

    public static final String P_HUMIDITY = "RELH";
    
    public static final String P_PRESSURE = "PRES";
    
    public static final String P_ALTIMETER = "ALTI";

    public static final String P_SEA_LVL_PRES = "PMSL";
    
    //************************
    public static final String P_PRECIP_01M = "P1MI";
        
    public static final String P_PRECIP_05M = "P05I";

    public static final String P_PRECIP_10M = "P10I";

    public static final String P_PRECIP_15M = "P15I";

    public static final String P_PRECIP_30M = "P30I";

    public static final String P_PRECIP_01H = "P01I";

    public static final String P_PRECIP_03H = "P03I";

    public static final String P_PRECIP_06H = "P06I";

    public static final String P_PRECIP_24H = "P24I";
    
    private static int PARM_POS = 0;
    private static Map<Integer,String> parmMap = new HashMap<Integer,String>();
    static {
        parmMap.put(PARM_POS++,P_STNID);
        parmMap.put(PARM_POS++,P_LAT);
        parmMap.put(PARM_POS++,P_LON);
        parmMap.put(PARM_POS++,P_ELEV);
        parmMap.put(PARM_POS++,P_STNTYPE);
        parmMap.put(PARM_POS++,P_DATATIME);
    }
    
    // YYYYMMDD/HHMM 
    // 20090303/1950
    
    public static final String D_DATEFMT = "yyyyMMdd/HHmm";
    
    
    /**
     * 
     * @return
     */
    public static int getParmPos() {
        return PARM_POS;
    }
    
    /**
     * 
     * @return
     */
    public static Map<Integer,String> getParmMap() {
        Map<Integer,String> map = new HashMap<Integer,String>();
        map.putAll(parmMap);
        
        return map;
    }
    
    /**
     * 
     * @return
     */
    public static Map<String, MESOWestElement> getValuesMap() {
        Map<String, MESOWestElement> values = new HashMap<String, MESOWestElement>();
        for(String s : parmMap.values()) {
            values.put(s, new MESOWestElement(s));
        }
        return values;
    }
}
