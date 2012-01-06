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
package com.raytheon.uf.viz.core.localization;

/**
 * Provides a set of localization constants
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 24, 2008            chammack     Initial creation
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class LocalizationConstants {

    private LocalizationConstants() {

    }

    public static final String P_LOCALIZATION_HTTP_SERVER = "httpServerAddress";

    public static final String P_LOCALIZATION_USER_NAME = "userName";

    public static final String P_LOCALIZATION_SITE_NAME = "siteName";

    public static final String P_ALERT_SERVER = "alertServer";

    public static final String DEFAULT_LOCALIZATION_SERVER = "http://localhost:9581/services";

    public static final String DEFAULT_ALERT_SERVER = "tcp://localhost:61998";

}
