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
package gov.noaa.nws.ncep.common.tools;

import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 29, 2009            jkorman     Initial creation
 * May 17, 2010			   LLin		   Modify Integer and float missing
 * 									   identical to Raytheon's.
 * 4/2011                   T. Lee      Added UAIR_INTEGER_MISSING
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public interface IDecoderConstantsN extends IDecoderConstants {

    /** Missing values */
    public static final Integer INTEGER_MISSING = IDecoderConstants.VAL_MISSING;

    public static final Float FLOAT_MISSING = 999999.f;

    public static final Float UAIR_FLOAT_MISSING = -9999.f;

    public static final Integer UAIR_INTEGER_MISSING = -9999;

    public static final float GRID_MISSING = -999999.f;

    /** FFG separator */
    public static final String FFG_BULLSEPARATOR = "\\d{3} \\r\\r\\n"
            + IDecoderConstants.WMO_HEADER + "(FFG[A-Z]{2}).\\r\\r\\n";

    /** Regular expression for FFG report */
    public static final String FFG_REPORT = "([A-Z]{3}\\d{3})\\s{2}(.*)(\\r\\r\\n)";

    /** SCD separator */
    public static final String SCD_BULLSEPARATOR = "\\d{3} \\r\\r\\n"
            + IDecoderConstants.WMO_HEADER + "(SCD[A-Z]{2}).\\r\\r\\n";

    /** Regular expression for SCD report */
    public static final String SCD_REPORT = "([A-Z]{4}) (SCD) ([A-Z]{3} )*(\\d{4})(.*)(\\r\\r\\n)"
            + "(([0-9]{1}|/)(.*)(\\r\\r\\n))*";
}