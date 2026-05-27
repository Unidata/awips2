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
package com.raytheon.edex.transform.shef.obs;

import java.util.HashMap;

import com.raytheon.uf.common.dataplugin.obs.metar.util.WeatherCondition;

/**
 * The MetarSynopticCode provides a mapping of METAR weather codes to appropriate
 * Synoptic (WMO 306 Vol I.1-C Table 4677). Note that these code mappings were taken
 * from the MetarToSHEF code and that some mappings do not appear to be correct. For
 * example TSRA should map to code 95, not 96. Also all MetarToSHEF mappings containing
 * the Metar "PE" were replaced with "PL" as the "PE" code is obsolete.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 2, 2008        1659 jkorman     Initial creation
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class MetarSynopticCode {
    
    private static final HashMap<WeatherCondition,MetarSynopticCode> codes = new HashMap<WeatherCondition,MetarSynopticCode>();
    static {
        MetarSynopticCode code = null;
        code = new MetarSynopticCode(19, new WeatherCondition( "+",  "",  "",  "","FC"));
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(19, new WeatherCondition(  "",  "",  "",  "","FC")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(97, new WeatherCondition( "+","TS","RA",  "",  ""));
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(96, new WeatherCondition( " ","TS","RA",  "",  ""));
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(95, new WeatherCondition( "-","TS","RA",  "",  ""));
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(81, new WeatherCondition( "+","SH","RA",  "",  ""));
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(81, new WeatherCondition( " ","SH","RA",  "",  ""));
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(80, new WeatherCondition( "-","SH","RA",  "",  ""));
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(64, new WeatherCondition( "+",  "","RA",  "",  ""));
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(62, new WeatherCondition( " ",  "","RA",  "",  ""));
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(60, new WeatherCondition( "-",  "","RA",  "",  ""));
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(97, new WeatherCondition( "+","TS","SN",  "",  ""));
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(97, new WeatherCondition( " ","TS","SN",  "",  ""));
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(97, new WeatherCondition( "-","TS","SN",  "",  ""));
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(74, new WeatherCondition( "+",  "","SN",  "",  "")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(72, new WeatherCondition( " ",  "","SN",  "",  "")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(70, new WeatherCondition( "-",  "","SN",  "",  "")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(86, new WeatherCondition( "+","SH","SN",  "",  "")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(86, new WeatherCondition( " ","SH","SN",  "",  "")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(85, new WeatherCondition( "-","SH","SN",  "",  ""));
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(97, new WeatherCondition( "+","TS","PL",  "",  ""));
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(97, new WeatherCondition( " ","TS","PL",  "",  "")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(97, new WeatherCondition( "-","TS","PL",  "",  "")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(67, new WeatherCondition( "+","FZ","RA",  "",  "")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(67, new WeatherCondition( " ","FZ","RA",  "",  "")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(66, new WeatherCondition( "-","FZ","RA",  "",  "")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(57, new WeatherCondition( "+","FZ","DZ",  "",  "")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(57, new WeatherCondition( " ","FZ","DZ",  "",  ""));
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(56, new WeatherCondition( "-","FZ","DZ",  "",  "")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(54, new WeatherCondition( "+",  "","DZ",  "",  "")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(52, new WeatherCondition( " ",  "","DZ",  "",  "")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(50, new WeatherCondition( "-",  "","DZ",  "",  "")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(87, new WeatherCondition( "+","SH","PL",  "",  ""));
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(87, new WeatherCondition( " ","SH","PL",  "",  ""));
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(87, new WeatherCondition( "-","SH","PL",  "",  ""));
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(79, new WeatherCondition( "+",  "","PL",  "",  "")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(79, new WeatherCondition( " ", ",","PL",  "",  "")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(79, new WeatherCondition( "-",  "","PL",  "",  "")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(87, new WeatherCondition(  "","SH","GR",  "",  ""));
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(87, new WeatherCondition(  "","SH","GS",  "",  ""));
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(76, new WeatherCondition(  "",  "","IC",  "",  "")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(17, new WeatherCondition(  "","TS",  "",  "",  "")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(16, new WeatherCondition("VC","SH",  "",  "",  ""));
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(41, new WeatherCondition(  "",  "",  "","BR",  "")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(41, new WeatherCondition(  "",  "",  "","FG",  "")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(42, new WeatherCondition(  "","MI",  "","FG",  "")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(43, new WeatherCondition(  "","PR",  "","FG",  "")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(42, new WeatherCondition(  "","BC",  "","FG",  "")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(40, new WeatherCondition("VC",  "",  "","FG",  ""));
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(41, new WeatherCondition(  "","FZ",  "","FG",  "")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(4,  new WeatherCondition(  "",  "",  "","FU",  "")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(4,  new WeatherCondition(  "",  "",  "","VA",  "")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(5,  new WeatherCondition(  "",  "",  "","HZ",  "")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(38, new WeatherCondition(  "","BL","SN",  "",  ""));
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(38, new WeatherCondition("VC","BL","SN",  "",  ""));
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(37, new WeatherCondition(  "","DR","SN",  "",  "")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(6,  new WeatherCondition(  "",  "",  "","DU",  "")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(7,  new WeatherCondition(  "","BL",  "","DU",  "")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(7,  new WeatherCondition("VC","BL",  "","DU",  "")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(7,  new WeatherCondition(  "","DR",  "","DU",  "")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(7,  new WeatherCondition(  "",  "",  "","SA", "")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(8,  new WeatherCondition(  "","BL",  "","SA", ""));
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(8,  new WeatherCondition("VC","BL",  "","SA", "")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(7,  new WeatherCondition(  "","DR",  "","SA", "")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(7,  new WeatherCondition(  "",  "",  "","PY",  "")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(7,  new WeatherCondition(  "","BL",  "",  "","PY")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(8,  new WeatherCondition(  "",  "",  "",  "","PO")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(8,  new WeatherCondition("VC",  "",  "",  "","PO")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(18, new WeatherCondition(  "",  "",  "",  "","SQ")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(30, new WeatherCondition(  "",  "",  "",  "","SS")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(33, new WeatherCondition( "+",  "",  "",  "","SS")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(30, new WeatherCondition("VC",  "",  "",  "","SS")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(30, new WeatherCondition( " ",  "",  "",  "","DS")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(33, new WeatherCondition( "+",  "",  "",  "","DS"));
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(30, new WeatherCondition("VC",  "",  "",  "","DS")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(77, new WeatherCondition( "+",  "","SG",  "",  "")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(77, new WeatherCondition(  "",  "","SG",  "",  "")); 
        codes.put(code.getMetarCode(),code);
        code = new MetarSynopticCode(77, new WeatherCondition( "-",  "","SG",  "",  "")); 
        codes.put(code.getMetarCode(),code);
    }
    
    private final int synopticCode;

    private final WeatherCondition metarCode;
    
    /**
     * Create a MetarSynopticCode instance using a given synoptic code and Metar
     * weather condition code.
     * @param synopticCode The synoptic code for this instance.
     * @param metarCode The WeatherCondition Metar code for this instance.
     */
    public MetarSynopticCode(int synopticCode, WeatherCondition metarCode) {
        this.synopticCode = synopticCode;
        this.metarCode = metarCode;
    }

    /**
     * Get the synoptic code for this instance.
     * @return The synopticCode.
     */
    public int getSynopticCode() {
        return synopticCode;
    }

    /**
     * Get the WeatherCondition Metar code for this instance.
     * @return The WeatherCondition Metar code.
     */
    public WeatherCondition getMetarCode() {
        return metarCode;
    }

    /**
     * The the MetarSynopticCode instance associated with a given
     * weather condition code.
     * @param weather The WeatherCondition code to lookup.
     * @return The associated 
     */
    public static MetarSynopticCode instance(WeatherCondition weather) {
        return codes.get(weather);
    }

    /**
     * 
     * @param weather
     * @return
     */
    public static int getSynopticCode(WeatherCondition weather) {
        MetarSynopticCode code = codes.get(weather);
        
        return (code != null) ? code.getSynopticCode() : -1;
    }
    
}
