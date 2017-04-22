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
package com.raytheon.uf.common.dataplugin.bufrua;


/**
 * Common layer manipulation tools exposed so that they can be used by UAObs.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ---------------------------
 * Nov 27, 2007  763      jkorman   Initial Coding.
 * Jul 12, 2016  5736     bsteffen  add sfcPressure and prSigW
 * 
 * </pre>
 * 
 * @author jkorman
 */
public class LayerTools {

    public static final String BUFRUA_PLUGIN_NAME = "bufrua";
    
    public static final String SFC_PRESSURE = "sfcPressure";
    
    public static final String NUM_MWND = "numMwnd";
    public static final String PR_MAXW = "prMaxW";
    public static final String WD_MAXW = "wdMaxW";
    public static final String WS_MAXW = "wsMaxW";
    
    public static final String NUM_MAND = "numMand";
    public static final String PR_MAN = "prMan";
    public static final String HT_MAN = "htMan";
    public static final String TP_MAN = "tpMan";
    public static final String TD_MAN = "tdMan";
    public static final String WD_MAN = "wdMan";
    public static final String WS_MAN = "wsMan";

    public static final String NUM_TROP = "numTrop";
    public static final String PR_TROP = "prTrop";
    public static final String TP_TROP = "tpTrop";
    public static final String TD_TROP = "tdTrop";
    public static final String WD_TROP = "wdTrop";
    public static final String WS_TROP = "wsTrop";

    public static final String NUM_SIGT = "numSigT";
    public static final String PR_SIGT = "prSigT";
    public static final String TP_SIGT = "tpSigT";
    public static final String TD_SIGT = "tdSigT";

    public static final String NUM_SIGW = "numSigW";
    public static final String HT_SIGW = "htSigW";
    public static final String PR_SIGW = "prSigW";
    public static final String WD_SIGW = "wdSigW";
    public static final String WS_SIGW = "wsSigW";
    
    
    public  static final String[] requiredParamMANLVL = new String[] {
        NUM_MAND, PR_MAN, HT_MAN, TP_MAN, TD_MAN, WD_MAN, WS_MAN, };

    public static final String[] requiredParamSIGLVLT = new String[] {
        NUM_SIGT, PR_SIGT, TP_SIGT, TD_SIGT };

    public static final String[] requiredParamSIGLVLW = new String[] {
        NUM_SIGW, HT_SIGW, WD_SIGW, WS_SIGW };
    
    public static final String[] requiredParamTROP = new String[] {
        NUM_TROP, PR_TROP, TP_TROP, TD_TROP, WD_TROP, WS_TROP };

    public static final String[] requiredParamMAXW = new String[] {
        NUM_MWND, PR_MAXW, WD_MAXW, WS_MAXW };

    
    // Mandatory level data - Pressure
    public static final int MANLVL_LO = 2020;
    public static final int MANLVL_HI = 2030;
    // Significant level temperature data - Geopotential
    public static final int SIGWLVL_LO = 2021;
    public static final int SIGWLVL_HI = 2031;
    // Significant level temperature data - Pressure
    public static final int SIGTLVL_LO = 2022;
    public static final int SIGTLVL_HI = 2032;

    // Vertical sounding significance.
    // BUFR Code tables Version 13-07/11/2007 - 0 08 001
    // 0     1 - - -
    // 1     2 Surface
    // 2     4 Standard level
    // 3     8 Tropopause level
    // 4    16 Maximum wind level
    // 5    32 Significant level, temperature and/or relative humidity
    // 6    64 Significant level, wind
    // All 7 Missing value
    // Bit No.
    // 0 - - - -
    // 1 Significant level, wind
    // 2 Significant level, temperature and/or relative humidity
    // 3 Maximum wind level
    // 4 Tropopause level
    // 5 Standard level
    // 6 Surface
    // 7 
    // All 7 Missing value
    public static final int SIGWND_LEVEL = 2;
    public static final int SIGPRE_LEVEL = 4;
    public static final int MAXWND_LEVEL = 8;
    public static final int TROP_LEVEL = 16;
    public static final int MANPRE_LEVEL = 32;
    public static final int SFC_LEVEL = 64;
    public static final int GENERIC_LEVEL = 128;
    
    /**
     * Is this layer is lower (higher pressure, or lower height) than another
     * layer? Checks if (layer2 is lower than layer1). Note that this should not
     * be thought of as less than.
     * 
     * @param layer1
     *            An upper air data layer.
     * @param layer2
     *            A layer to compare to.
     * @return Is the layer lower than a specified layer?
     */
    public static boolean isLowerThan(UAObsLevel layer1, UAObsLevel layer2) {
        boolean retValue = isLowerThan(layer1.getPressure(), layer2.getPressure());
        if (!retValue) {
            retValue = isHigherThan(layer1.getGeoHeight(), layer2.getGeoHeight());
        }
        return retValue;
    }

    /**
     * Is this layer is higher (lower pressure, or higher height) than another
     * layer? Checks if (layer2 is higher than layer1). Note that this should not
     * be thought of as greater than.
     * 
     * @param layer1
     *            An upper air data layer.
     * @param layer2
     *            A layer to compare to.
     * @return Is this layer lower than a specified layer?
     */
    public static boolean isHigherThan(UAObsLevel layer1, UAObsLevel layer2) {
        boolean retValue = isHigherThan(layer1.getPressure(), layer2.getPressure());
        if (!retValue) {
            retValue = isLowerThan(layer1.getGeoHeight(), layer2.getGeoHeight());
        }
        return retValue;
    }

    private static boolean isLowerThan(Integer level1, Integer level2) {
        boolean retValue = false;

        if ((level1 != null) && (level2 != null)) {
            retValue = level1 < level2;
        }
        return retValue;
    }

    private static boolean isHigherThan(Integer level1, Integer level2) {
        boolean retValue = false;

        if ((level1 != null) && (level2 != null)) {
            retValue = level1 > level2;
        }
        return retValue;
    }

}
