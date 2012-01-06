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
package com.raytheon.edex.plugin.taf.common;

/**
 * String constants used in the TAF Plugin.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 4, 2008  1444       grichard    Initial creation.
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public final class TafConstants {

    /** Regular expression for extracting wind information */
    public static final String WIND_GROUP_EXP = "(\\d{3}|VRB)(\\d{2,3})((G)(\\d{2,3}))?(KT|MPS|KMH)()";

    /** Regular expression for extracting visibility information */
    public static final String VISIBILITY_GROUP_EXP = "(P?[0-6]|[0-6] [13]/[24]|[13]/[24])SM";

    /** Regular expression for extracting sky coverage information */
    public static final String SKY_COVER_GROUP_EXP = "(((FEW|SCT|BKN|OVC|VV)((\\d{3}|///)((CB|TCU)?)))|((SKC|CLR|NSC)))";

    /** Regular expression for extracting wind shear information */
    public static final String WIND_SHEAR_GROUP_EXP = "WS([0-9]{3})/([0-9]{3})([0-9]{2})KT";

    /** Regular expression for extracting icing information */
    public static final String ICING_GROUP_EXP = "6([0-9])([0-9]{3})([0-9])";

    /** Regular expression for extracting turbulence information */
    public static final String TURBULENCE_GROUP_EXP = "5([0-9])([0-9]{3})([0-9])";

    /** Regular expression for extracting temperature information */
    public static final String TEMP_GROUP_EXP = "T(M)?([0-9]{2})/([0-9]{2})Z";

    /** Regular expression for extracting altimeter information */
    public static final String ALTIMETER_GROUP_EXP = "QNH([23]\\d{3})INS";

    /**
     * Pattern used for extracting change groups
     * 
     * <pre>
     *  Group #
     *   0
     *   1
     *   2 Change group id = {FM|BECMG|PROBxx|TEMPO|PROBxx TEMPO
     *   3
     *   4
     *   5
     *   6 null | PROBxx When in conjunction with PROBxx or PROBxx TEMPO
     *   7 null | TEMPO  When in conjunction with PROBxx TEMPO
     *   8 null | TEMPO
     *   9 time group
     * </pre>
     */

    public static final String CHANGE_GROUP_EXP = "((FM|((BECMG )|((PROB[34]0) +(TEMPO )?)|(TEMPO )))((\\d{6})|(([0-3]\\d{3})/([0-3]\\d{3})))( ))";

    public static final String CG_FM = "FM";

    public static final String CG_BECMG = "BECMG";

    public static final String CG_INITIAL = "INITIAL";

    public static final String CG_TEMPO = "TEMPO";

    public static final String CG_PROB = "PROB";

    public static final String CG_PROB_TEMPO = "PROB TEMPO";

    public static final String VS_6PLUS_SM = "P6";

    public static final String VS_6PLUS_M = "9999";

    public static final String WX_NSW = "NSW";

    public static final String WX_CAVOK = "CAVOK";

    public static final String CLD_SKC = "SKC";

    public static final String CLD_VV = "VV";

    public static final String TAF_IND = "TAF";

    public static final String AMD_IND = "AMD";

    public static final String COR_IND = "COR";

    public static final String REPORT_HEADER = "(TAF )?((AMD|COR) )*?"
            + "([A-Z][A-Z,0-9]{3}) ([0-3]\\d{5}Z )?([0-3]\\d{5})( )";

    public static final String REPORT_HEADER30 = "(TAF )?((AMD|COR) )*?"
            + "([A-Z][A-Z,0-9]{3}) ([0-3]\\d{5}Z )?(\\d{4}/\\d{4})( )";

    public static final String RPT_HDR_TAF = "(TAF((\\s)+(AMD|COR))?)";

    public static final String RPT_HDR_CCCC = "([A-Z][A-Z,0-9]{3}) ([0-3]\\d{5}Z )?([0-3]\\d{3})/([0-3]\\d{3})( )";

    /**
     * Constructor.
     */
    private TafConstants() {
    }

}
