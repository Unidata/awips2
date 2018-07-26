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
package com.raytheon.uf.viz.damagepath;

import java.text.ParseException;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import org.apache.commons.lang3.time.DateFormatUtils;
import org.apache.commons.lang3.time.FastDateFormat;

import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.localization.LocalizationManager;

/**
 * Utility class for the damage path tool's GeoJSON format.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 18, 2016  #5287     dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class DamagePathGeoJsonUtils {

    private static final FastDateFormat DAMAGE_PATH_DATE_FORMAT = DateFormatUtils.ISO_DATETIME_TIME_ZONE_FORMAT;

    static final String EVENTID_PROP_NAME = "eventID";

    static final String HAZARD_TYPE_PROP_NAME = "hazard";

    static final String NAME_PROP_NAME = "name";

    static final String EVENT_TIME_PROP_NAME = "eventTime";

    static final String COMMENTS_PROP_NAME = "comments";

    static final String SITEID_PROP_NAME = "cwa";

    static final String WORKSTATION_PROP_NAME = "workstation";

    static final String USER_PROP_NAME = "user";

    private DamagePathGeoJsonUtils() {
        throw new AssertionError();
    }

    /**
     * Ensures the required properties for the damage path tool's GeoJSON format
     * (event time, event ID, creating CWA, creating workstation and creating
     * user) have been filled. If not any missing properties will be filled and
     * the properties map returned.
     * 
     * @param geoJsonProperties
     *            A damage path polygon's properties map
     * @return A copy of the properties map with any missing required properties
     *         populated.
     */
    public static Map<String, String> fillRequiredProperties(
            Map<String, String> geoJsonProperties) {
        Map<String, String> validatedProps = new HashMap<>(geoJsonProperties);

        if (!validatedProps.containsKey(EVENTID_PROP_NAME)) {
            validatedProps.put(EVENTID_PROP_NAME, generateEventID());
        }

        if (!validatedProps.containsKey(EVENT_TIME_PROP_NAME)) {
            validatedProps.put(EVENT_TIME_PROP_NAME,
                    formatEventTime(getDefaultEventTime()));
        }

        if (!validatedProps.containsKey(SITEID_PROP_NAME)) {
            validatedProps.put(SITEID_PROP_NAME, getDefaultSiteID());
        }

        if (!validatedProps.containsKey(WORKSTATION_PROP_NAME)) {
            validatedProps
                    .put(WORKSTATION_PROP_NAME, getDefaultWorkstationID());
        }

        if (!validatedProps.containsKey(USER_PROP_NAME)) {
            validatedProps.put(USER_PROP_NAME, getDefaultUser());
        }

        return validatedProps;
    }

    public static String generateEventID() {
        return UUID.randomUUID().toString();
    }

    public static Date getDefaultEventTime() {
        return SimulatedTime.getSystemTime().getTime();
    }

    public static String formatEventTime(Date date) {
        return DateFormatUtils.formatUTC(date,
                DAMAGE_PATH_DATE_FORMAT.getPattern());
    }

    public static Date parseDateString(String dateString) throws ParseException {
        return DAMAGE_PATH_DATE_FORMAT.parse(dateString);
    }

    public static String getDefaultUser() {
        return VizApp.getWsId().getUserName();
    }

    public static String getDefaultWorkstationID() {
        return VizApp.getWsId().getHostName();
    }

    public static String getDefaultSiteID() {
        return LocalizationManager.getInstance().getCurrentSite();
    }
}
