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
package com.raytheon.uf.common.dataplugin.lsr;

import java.util.HashMap;
import java.util.Map;


/**
 * Local Storm Report event type
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 14, 2009            jkorman     Initial creation
 * Dec 09, 2013 2581       njensen     Added freezing drizzle
 * Jan 03, 2014 2581       njensen     Added coastal flood
 * Jan 13, 2014 2581       njensen     Added debris flow
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public enum LSREventType {

    AVALANCHE("AVALANCHE",0,LSRUnits.NOUNITS),
    BLIZZARD("BLIZZARD",1,LSRUnits.NOUNITS),
    DENSEFOG("DENSE FOG",2,LSRUnits.NOUNITS),
    DOWNBURST("DOWNBURST",3,LSRUnits.NOUNITS),
    DROUGHT("DROUGHT",4,LSRUnits.NOUNITS),
    DUSTSTORM("DUST STORM",5,LSRUnits.NOUNITS),
    EXCESSIVEHEAT("EXCESSIVE HEAT",6,LSRUnits.FAHRENHEIT),
    EXTREMECOLD("EXTREME COLD",7,LSRUnits.FAHRENHEIT),
    EXTRWINDCHILL("EXTR WIND CHILL",8,LSRUnits.FAHRENHEIT),
    FLASHFLOOD("FLASH FLOOD",9,LSRUnits.NOUNITS),
    FLOOD("FLOOD",10,LSRUnits.NOUNITS),
    FREEZE("FREEZE",11,LSRUnits.FAHRENHEIT),
    FREEZINGRAIN("FREEZING RAIN",12,LSRUnits.INCH),
    FUNNELCLOUD("FUNNEL CLOUD",13,LSRUnits.NOUNITS),
    HAIL("HAIL",14,LSRUnits.INCH),
    HEAVYRAIN("HEAVY RAIN",15,LSRUnits.INCH),
    HEAVYSLEET("HEAVY SLEET",16,LSRUnits.INCH),
    HEAVYSNOW("HEAVY SNOW",17,LSRUnits.INCH),
    HIGHASTRTIDES("HIGH ASTR TIDES",18,LSRUnits.NOUNITS),
    HIGHSURF("HIGH SURF",19,LSRUnits.NOUNITS),
    HIGHSUSTWINDS("HIGH SUST WINDS",20,LSRUnits.MPH),
    HURRICANE("HURRICANE",21,LSRUnits.NOUNITS),
    ICESTORM("ICE STORM",22,LSRUnits.NOUNITS),
    LIGHTNING("LIGHTNING",23,LSRUnits.NOUNITS),
    LOWASTRTIDES("LOW ASTR TIDES",24,LSRUnits.NOUNITS),
    MARINEHAIL("MARINE HAIL",25,LSRUnits.INCH),
    MARINETSTMWIND("MARINE TSTM WIND",26,LSRUnits.MPH),
    NONTSTMWNDDMG("NON-TSTM WND DMG",27,LSRUnits.NOUNITS),
    NONTSTMWNDGST("NON-TSTM WND GST",28,LSRUnits.MPH),
    RIPCURRENTS("RIP CURRENTS",29,LSRUnits.NOUNITS),
    SEICHE("SEICHE",30,LSRUnits.NOUNITS),
    SLEET("SLEET",31,LSRUnits.INCH),
    SNOW("SNOW",32,LSRUnits.INCH),
    STORMSURGE("STORM SURGE",33,LSRUnits.FT),
    TORNADO("TORNADO",34,LSRUnits.FUJITA),
    TROPICALSTORM("TROPICAL STORM",35,LSRUnits.NOUNITS),
    TSTMWNDDMG("TSTM WND DMG",36,LSRUnits.NOUNITS),
    TSTMWNDGST("TSTM WND GST",37,LSRUnits.MPH),
    WATERSPOUT("WATER SPOUT",38,LSRUnits.NOUNITS),
    WILDFIRE("WILDFIRE",39,LSRUnits.NOUNITS),
    FREEZINGDRIZZLE("FREEZING DRIZZLE", 40, LSRUnits.NOUNITS),
    COASTALFLOOD("COASTAL FLOOD", 41, LSRUnits.NOUNITS),
    DEBRISFLOW("DEBRIS FLOW", 42, LSRUnits.NOUNITS);

    private final String eventName;
    
    private final int value;
    
    private final LSRUnits eventUnits;

    private static Map<String,LSREventType> lookup = new HashMap<String,LSREventType>();
    static {
        for(LSREventType t : LSREventType.values()) {
            lookup.put(t.getEventName(), t);
        }
    }
    
    private LSREventType(String name, int value, LSRUnits units) {
        eventName = name;
        this.value = value;
        eventUnits = units;
    }

    /**
     * @return the eventName
     */
    public String getEventName() {
        return eventName;
    }

    /**
     * @return the value
     */
    public int getValue() {
        return value;
    }

    /**
     * @return the eventUnits
     */
    public LSRUnits getEventUnits() {
        return eventUnits;
    }

    public static LSREventType lookup(String event) {
        return lookup.get(event);
    }
    
}
