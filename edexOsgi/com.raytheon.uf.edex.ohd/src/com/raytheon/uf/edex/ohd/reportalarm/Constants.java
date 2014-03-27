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
package com.raytheon.uf.edex.ohd.reportalarm;

import java.text.SimpleDateFormat;

import com.raytheon.uf.common.dataplugin.shef.util.ShefConstants;

/**
 * Constants needed by the run_report_alarm process.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 13, 2014  #2378     dgilling     Removed unused constants.
 * 
 * </pre>
 * 
 * @author xxxxxxxx
 * @version 1.0
 */
class Constants {

    public static final SimpleDateFormat REPORT_TIME_PATTERN = new SimpleDateFormat(
            "HH:mm:ss z, yyyy.MM.dd ");

    public static final int MISSING_VALUE_INT = ShefConstants.SHEF_MISSING_INT;

    public static final double MISSING_VALUE_DOUBLE = -9999999.87654321;

    public static final String NEWLINE = System.getProperty("line.separator");

    public final static String EOL = NEWLINE;

    public static final String SPACE = " ";

    public static final String WHFS_PRODUCT_DIR = "whfs_product_dir";

    /**
     * A private constructor so that Java does not attempt to create one for us.
     * As this class should not be instantiated, do not attempt to ever call
     * this constructor; it will simply throw an AssertionError.
     * 
     */
    private Constants() {
        throw new AssertionError();
    }
}