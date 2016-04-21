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

import org.eclipse.jface.preference.IPreferenceStore;

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
 * Feb 04, 2014 2704       njensen      Consolidate services and pypies proxy addresses
 * Jun 24, 2014 3236       njensen      Added proxy address options
 * May 29, 2015 4532       bsteffen     Add sync localization option.
 * 
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

    public static String P_MENU_TIME_UPDATE_INTERVALS = "menuTimeUpdateInterval";

    public static String P_DATA_UPDATE_INTERVALS = "dataUpdateInterval";

    public static String P_CACHE_WEATHER = "cachePypies";

    public static String P_CACHE_MAPS = "cacheMaps";

    public static String P_CACHE_LOCALIZATION = "cacheLocalization";

    public static String P_DISABLE_REMOTE_LOCALIZATION = "disableRemoteLocalization";

    /**
     * This preference is not stored but is used to send notification through
     * the preference store that the {@link #P_CACHE_LOCALIZATION} and
     * {@link #P_DISABLE_REMOTE_LOCALIZATION} should be temporarily ignored so
     * that the localization files can be synchronized with the server. Before
     * performing synchronization, an event should be fired using
     * {@link IPreferenceStore#firePropertyChangeEvent(String, Object, Object)}
     * with the preference name as {@link #P_SYNC_REMOTE_LOCALIZATION} and a
     * newValue of true. When synchronization has completed another event should
     * be fired with a newValue of false.
     */
    public static String P_SYNC_REMOTE_LOCALIZATION = "syncRemoteFiles";

    public static String P_DISABLE_MENU_TIMES = "disableMenuTimes";

    public static String P_DISABLE_JMS = "disableJms";

    public static String P_ENABLE_REQUEST_COMPRESSION = "enableRequestCompression";

    public static String P_PREFERENCE_PLACEHOLDER = "placeholderPreference";

    public static String P_PROXY_ADDRESS = "proxyAddress";

    public static final String P_PROXY_SERVER_OPTIONS = "proxyAddressOptions";
}
