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
package com.raytheon.uf.viz.thinclient.preferences;

/**
 * Preference constants for the thin client mode
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 20, 2011            mschenke     Initial creation
 * Jan 14, 2013 1469       bkowal       The hdf5 data directory is no longer a preference constant.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ThinClientPreferenceConstants {

    public static String P_CACHE_DIR = "cacheDir";

    public static String P_DATA_REFRESH_INTERVAL = "dataRefreshRate";

    public static String P_MENU_TIME_REFRESH_INTERVAL = "menuRefreshInterval";

    public static String P_USE_PROXIES = "useHttpProxy";

    public static String P_SERVICES_PROXY = "servicesProxyAddress";

    public static String P_PYPIES_PROXY = "pypiesProxyAddress";

    public static String P_MENU_TIME_UPDATE_INTERVALS = "menuTimeUpdateInterval";

    public static String P_DATA_UPDATE_INTERVALS = "dataUpdateInterval";

    public static String P_CACHE_WEATHER = "cachePypies";

    public static String P_CACHE_MAPS = "cacheMaps";

    public static String P_CACHE_LOCALIZATION = "cacheLocalization";

    public static String P_DISABLE_REMOTE_LOCALIZATION = "disableRemoteLocalization";

    public static String P_DISABLE_MENU_TIMES = "disableMenuTimes";

    public static String P_DISABLE_JMS = "disableJms";

    public static String P_ENABLE_REQUEST_COMPRESSION = "enableRequestCompression";

    public static String P_PREFERENCE_PLACEHOLDER = "placeholderPreference";
}
