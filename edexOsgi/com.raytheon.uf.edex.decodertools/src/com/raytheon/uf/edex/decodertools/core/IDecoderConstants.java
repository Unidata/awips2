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
package com.raytheon.uf.edex.decodertools.core;

/**
 * Constants common to most decoders.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20070925            391 jkorman     Initial Coding.
 * 20071217            453 jkorman     Added SYNOPTIC_MAROB report type.
 * 20080102            384 jkorman     Added additional obs type constants. 
 * 20080114            763 jkorman     Added constants for vertical sig values.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public interface IDecoderConstants {
    public static final byte ASCII_SOM = 0x01;

    public static final byte ASCII_ETX = 0x03;

    public static final byte ASCII_LF = 0x0A;

    public static final byte ASCII_CR = 0x0D;

    public static final byte ASCII_SP = 0x20;

    public static final char SPACE_CHAR = ' ';

    public static final String CARRIAGECONTROL = "CARRIAGECONTROL";

    public static final String REPORT_END = "RPT_END";

    public static final String MESSAGE_START = "MSG_START";

    public static final String MESSAGE_END = "MSG_END";

    public static final String SOM = String.valueOf((char) ASCII_SOM);

    public static final String ETX = String.valueOf((char) ASCII_ETX);

    public static final String WMO_HEADER = "[A-Z]{4}\\d{0,2} [A-Z][A-Z0-9]{3} \\d{6}[^\\r\\n]*[\\r\\n]+";

    public static final Integer VAL_ERROR = new Integer(-9999999);

    public static final Integer VAL_MISSING = new Integer(-9999998);

    public static final int SYNOPTIC_FIXED_LAND = 1001;

    public static final int SYNOPTIC_MOBILE_LAND = 1002;

    public static final int SYNOPTIC_SHIP = 1003;

    public static final int SYNOPTIC_CMAN = 1004;

    public static final int SYNOPTIC_MOORED_BUOY = 1005;

    public static final int DRIFTING_BUOY = 1006;
    
    public static final int SYNOPTIC_MAROB = 1007;

    // BUFR vertical significance defines.
    public static final Integer SIGWND_LEVEL = new Integer(2);
    public static final Integer SIGPRE_LEVEL = new Integer(4);
    public static final Integer MAXWND_LEVEL = new Integer(8);
    public static final Integer TROP_LEVEL = new Integer(16);
    public static final Integer MANPRE_LEVEL = new Integer(32);
    public static final Integer SFC_LEVEL = new Integer(64);
    public static final Integer GENERIC_LEVEL = new Integer(128);

    // Mandatory level data - Pressure : Geopotential
    public static final int MANLVL_LO = 2020;
    public static final int MANLVL_HI = 2030;
    // Significant level wind data - Geopotential
    public static final int SIGWLVL_LO = 2021;
    public static final int SIGWLVL_HI = 2031;
    // Significant level temperature data - Pressure
    public static final int SIGTLVL_LO = 2022;
    public static final int SIGTLVL_HI = 2032;
    
    // TODO For now - refine names and labels
    public static final int BUFRUA_2020 = 2020;
    
    public static final int BUFRUA_2030 = 2030;

    public static final int BUFRUA_2040 = 2040;

    public static final int BUFRUA_2050 = 2050;

    public static final int BUFRUA_2060 = 2060;
    
    public static final int RECCO_MANOBS = 4500; 
    
    public static final int RECCO_INTEROBS = 4510;
    
    public static final int AIREP_NORMAL = 4600;
    
    public static final int AIREP_SPECIAL = 4610;
    
    public static final int PIREP_NORMAL = 4700;
    
    public static final int PROFILER_DATA = 3000;
    
    
    
    
    
    
    
}
