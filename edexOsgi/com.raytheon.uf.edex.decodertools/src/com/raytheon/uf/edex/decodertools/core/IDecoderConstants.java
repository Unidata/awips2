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
 * Sep 18, 2014       3627 mapeters    Removed unused constants.
 * Sep 26, 2014       3629 mapeters    Removed constants specific to particular packages.
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

    public static final String ETX = String.valueOf((char) ASCII_ETX);

    public static final String WMO_HEADER = "[A-Z]{4}\\d{0,2} [A-Z][A-Z0-9]{3} \\d{6}[^\\r\\n]*[\\r\\n]+";

    public static final Integer VAL_ERROR = new Integer(-9999999);

    public static final Integer VAL_MISSING = new Integer(-9999998);

    public static final int SYNOPTIC_FIXED_LAND = 1001;

    public static final int SYNOPTIC_MOBILE_LAND = 1002;

    public static final int AIREP_NORMAL = 4600;

    public static final int AIREP_SPECIAL = 4610;
}
