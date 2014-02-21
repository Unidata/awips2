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
package com.raytheon.edex.plugin.shef.alarms;

import java.text.SimpleDateFormat;

import com.raytheon.uf.common.dataplugin.shef.util.ShefConstants;

/**
 * Constants for alert alarm.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 10, 2014 2781       rjpeter     Initial history, update IHFS_CONFIG path.
 * </pre>
 * 
 * @author jnjanga
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

    public static final String REPORT_ALARM_LOG = "report_alarm.log";

    public static final String WHFS_PRODUCT_DIR = "whfs_product_dir";

    public static final String WHFS_UTIL_LOG_DIR = "whfs_util_log_dir";

    public static final String IHFS_CONFIG = "/res/spring/ohd-common-database.xml";
}