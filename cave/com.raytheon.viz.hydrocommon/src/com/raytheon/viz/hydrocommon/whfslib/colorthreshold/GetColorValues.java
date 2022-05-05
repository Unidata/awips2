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
package com.raytheon.viz.hydrocommon.whfslib.colorthreshold;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.shef.tables.Colorname;
import com.raytheon.uf.common.dataplugin.shef.tables.Colorvalue;
import com.raytheon.uf.common.dataplugin.shef.tables.ColorvalueId;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.util.MPEColors;
import com.raytheon.viz.hydrocommon.whfslib.IHFSDbGenerated;

/**
 * Various utility methods used to retrieve color information from the Hydro
 * color tables/schemas.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 10, 2008            randerso    Initial creation
 * May 27, 2014 3133       njensen     Organized imports, fixed == to equals
 * Jul 21, 2015 4500       rjpeter     Use Number in blind cast.
 * Feb 12, 2016 5356       bsteffen    Fall back to using the provided
 *                                     colorsets when no values are found.
 * Feb 28, 2017 6157       bkowal      Eliminated use of System err to report invalid
 *                                     application-specified arguments.
 * Mar 01, 2017 6160       bkowal      Alternatively use the "database color name" when
 *                                     retrieving user and office level color information.
 * Jul 06, 2018 6885       mduff       Changes to handle the NamedColorUseSet encapsulation of fields.
 * </pre>
 * 
 * @author randerso
 */

public class GetColorValues {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GetColorValues.class);

    /** Return value if no close duration found */
    public static final int NO_DURATION_FOUND = -1;

    private static final String OFFICE_COLOR_SET_ID = "default";

    /**
     * Systematically tries to find the color scheme for a requested user id,
     * application name, product, duration and threshold unit.<BR>
     * 
     * First, the ColorValue table is queried for the given user id, application
     * name, product, duration, and threshold unit.<BR>
     * 
     * If this fails then an attempt is made to find the user color scheme with
     * the closest duration.<BR>
     * 
     * If this fails, then the ColorValue table is queried to find a an office
     * color set.<BR>
     * 
     * If this fails, then an attempt is made to find the office color scheme
     * with the closest duration.<BR>
     * 
     * If this fails, then the hard-coded color scheme for this product is used.
     * <BR>
     * 
     * If no color set can be found for this product, then this application
     * returns a null value.<BR>
     * 
     * @param user_id
     *            The userid. Cannot be null.
     * @param application_name
     *            The name of the application. Cannot be null.
     * @param coloruse_name
     *            The name of the product Cannot be null.
     * @param duration
     *            The duration in seconds.
     * @param threshold_unit
     *            The threshold unit, English or Metric. Must be "E" or "M"
     * @param pColorSetGroup
     *            Contains the default color sets and durations.
     * 
     * @return A list containing the color scheme retrieved for the specified
     *         product. Returns null if no color set could be found.
     */
    public static List<Colorvalue> get_colorvalues(final String user_id,
            final String application_name, final String coloruse_name,
            int duration, String threshold_unit,
            final List<NamedColorUseSet> pColorSetGroup) {
        return get_colorvalues(user_id, application_name, coloruse_name, null,
                duration, threshold_unit, pColorSetGroup);
    }

    /**
     * Systematically tries to find the color scheme for a requested user id,
     * application name, product, duration and threshold unit.<BR>
     * 
     * First, the ColorValue table is queried for the given user id, application
     * name, product, duration, and threshold unit.<BR>
     * 
     * If this fails then an attempt is made to find the user color scheme with
     * the closest duration.<BR>
     * 
     * If this fails, then the ColorValue table is queried to find a an office
     * color set.<BR>
     * 
     * If this fails, then an attempt is made to find the office color scheme
     * with the closest duration.<BR>
     * 
     * If this fails, then the hard-coded color scheme for this product is used.
     * <BR>
     * 
     * If no color set can be found for this product, then this application
     * returns a null value.<BR>
     * 
     * @param user_id
     *            The userid. Cannot be null.
     * @param application_name
     *            The name of the application. Cannot be null.
     * @param coloruse_name
     *            The name of the product Cannot be null.
     * @param displayString
     *            The Display String for the product.
     * @param duration
     *            The duration in seconds.
     * @param threshold_unit
     *            The threshold unit, English or Metric. Must be "E" or "M"
     * @param pColorSetGroup
     *            Contains the default color sets and durations.
     * 
     * @return A list containing the color scheme retrieved for the specified
     *         product. Returns null if no color set could be found.
     */
    public static List<Colorvalue> get_colorvalues(final String user_id,
            final String application_name, final String coloruse_name,
            final String displayString, int duration, String threshold_unit,
            final List<NamedColorUseSet> pColorSetGroup) {
        final String method = "get_colorvalues()";
        List<Colorvalue> cvHead = null;

        // Check to make sure that all input parameters were supplied.
        if ((user_id == null) || user_id.isEmpty()) {
            throw new IllegalArgumentException(
                    "Required 'user_id' argument cannot be NULL or empty.");
        } else if ((application_name == null) || application_name.isEmpty()) {
            throw new IllegalArgumentException(
                    "Required 'application_name' argument cannot be NULL or empty.");
        } else if ((coloruse_name == null) || coloruse_name.isEmpty()) {
            throw new IllegalArgumentException(
                    "Required 'coloruse_name' argument cannot be NULL or empty.");
        } else if (!"E".equals(threshold_unit) && !"M".equals(threshold_unit)) {
            /*
             * TODO: make this argument an enum instead of a String.
             */
            throw new IllegalArgumentException(
                    "Required 'threshold_unit' argument must be one of: { E, M }.");
        }

        // get color values from default setting in xml file.
        if ("hydroview".equals(application_name)
                && "HEIGHT".equals(coloruse_name)) {
            cvHead = getDefaultColorSet(application_name, coloruse_name,
                    threshold_unit, null);
        } else {
            // Try to find a user defined color set.
            cvHead = getUserColorSet(user_id, application_name, coloruse_name,
                    duration, threshold_unit);
            if (cvHead == null && displayString != null) {
                /*
                 * Attempt to find a user defined color set based on the display
                 * String. TODO: determine if this is actually necessary during
                 * a MPE Enhancement. It is possible that all queries should be
                 * completed using the display string by default.
                 */
                final String dbColorName = lookupDBColorNameByDisplayName(
                        displayString, pColorSetGroup);
                if (dbColorName != null) {
                    cvHead = getUserColorSet(user_id, application_name,
                            dbColorName, duration, threshold_unit);
                }
            }
        }

        if (cvHead == null) {
            // Try to find an office-defined color set.
            cvHead = getUserColorSet(OFFICE_COLOR_SET_ID, application_name,
                    coloruse_name, duration, threshold_unit);
            if (cvHead == null && displayString != null) {
                /*
                 * Attempt to find an office defined color set based on the
                 * display String.
                 */
                final String dbColorName = lookupDBColorNameByDisplayName(
                        displayString, pColorSetGroup);
                if (dbColorName != null) {
                    cvHead = getUserColorSet(OFFICE_COLOR_SET_ID,
                            application_name, dbColorName, duration,
                            threshold_unit);
                }
            }
            if (cvHead == null) {
                // Try to find a default color set.
                cvHead = getDefaultColorSet(application_name, coloruse_name,
                        threshold_unit, null);
                if ((cvHead == null) || (cvHead.isEmpty())) {
                    cvHead = getDefaultColorSet(application_name, coloruse_name,
                            threshold_unit, pColorSetGroup);
                }
                if ((cvHead == null) || (cvHead.isEmpty())) {
                    statusHandler.handle(Priority.PROBLEM,
                            "ERROR in " + method
                                    + " Colors/levels not defined for application "
                                    + application_name + " use_name = "
                                    + coloruse_name + " user_id = " + user_id);
                }
            }
        }

        return cvHead;
    }

    /**
     * Attempts to retrieve the "database color name" for the specified "display
     * color name" from the specified {@link List} of {@link NamedColorUseSet}s.
     * 
     * @param colorDisplayName
     *            the specified "display color name". Cannot be {@code null}.
     * @param pColorSetGroup
     *            the specified {@link List} of {@link NamedColorUseSet}s.
     *            Cannot be {@code null}.
     * @return the "database color name" if found; {@code null}, otherwise.
     */
    private static String lookupDBColorNameByDisplayName(
            final String colorDisplayName,
            final List<NamedColorUseSet> pColorSetGroup) {
        if (colorDisplayName == null) {
            throw new IllegalArgumentException(
                    "Required argument 'colorDisplayName' cannot be NULL.");
        }
        if (pColorSetGroup == null) {
            throw new IllegalArgumentException(
                    "Required argument 'pColorSetGroup' cannot be NULL.");
        }
        for (NamedColorUseSet namedColorUseSet : pColorSetGroup) {
            if (colorDisplayName.trim()
                    .equals(namedColorUseSet.getColor_use_display_string())) {
                return namedColorUseSet.getColor_use_db_name();
            }
        }
        return null;
    }

    /**
     * Retrieves a color set for the given user id, application name, color use
     * name, duration, and unit.
     * 
     * @param user_id
     *            The userid. Cannot be null.
     * @param application_name
     *            The name of the application. Cannot be null.
     * @param coloruse_name
     *            The name of the product Cannot be null.
     * @param duration
     *            The duration in seconds.
     * @param threshold_unit
     *            The threshold unit, English or Metric. Must be "E" or "M"
     * 
     * @return A collection of ColorValue structures representing a specific
     *         color set. Returns null if no data are found.
     */
    private static List<Colorvalue> getUserColorSet(final String user_id,
            final String application_name, final String coloruse_name,
            int duration, String threshold_unit) {
        List<Colorvalue> cvHead = null;

        int closest_duration;

        // Attempt to find the closest color duration match for the given user
        // id.
        closest_duration = get_closest_multihour_duration(user_id,
                application_name, coloruse_name, duration, threshold_unit);

        // does the closest one match?
        if ((closest_duration != NO_DURATION_FOUND)
                && (duration == closest_duration)) {
            cvHead = getColorValueTableEntries(user_id, application_name,
                    coloruse_name, closest_duration, threshold_unit);
        }

        return cvHead;
    }

    /**
     * Retrieves the default color set and duration for the given color use
     * name.
     * 
     * @param application_name
     *            The name of the application. Cannot be null.
     * @param coloruse_name
     *            The name of the product Cannot be null.
     * @param threshold_unit
     *            The threshold unit, English or Metric. Must be "E" or "M"
     * @param pColorSetGroup
     * 
     * @return A collection of ColorValue structures representing a specific
     *         color set. Returns null if no data are found.
     */
    private static List<Colorvalue> getDefaultColorSet(
            final String application_name, final String coloruse_name,
            String threshold_unit, List<NamedColorUseSet> pColorSetGroup) {

        if ("hmapmpe".equals(application_name)) {
            pColorSetGroup = MPEColors.build_mpe_colors();
        } else if (pColorSetGroup == null) {
            pColorSetGroup = HydroDisplayManager.getInstance()
                    .getDefaultNamedColorUseSetList();
        }

        // Loop over the default colors provided by the user.
        // Look for the specified color use name.
        if (pColorSetGroup == null) {
            return null;
        }

        List<Colorvalue> cvList = new ArrayList<>();
        for (NamedColorUseSet pColorSet : pColorSetGroup) {
            int status = coloruse_name
                    .compareTo(pColorSet.getColor_use_db_name());

            if (status == 0) {

                // Build a linked list containing the default color value
                // information.
                for (ColorThreshold threshold : pColorSet.getThreshold_array()
                        .getThresholds()) {
                    Colorvalue cvNode = new Colorvalue(
                            new ColorvalueId("default", application_name,
                                    coloruse_name,
                                    pColorSet.getDefault_duration(),
                                    threshold.getValue(), threshold_unit),
                            new Colorname(threshold.getColorName()));
                    cvList.add(cvNode);
                }
                break;
            }
        }
        return cvList;
    }

    /**
     * Query the ColorValue table, retrieving all records containing the given
     * user id, application name, color use name, duration, and unit.
     * 
     * @param user_id
     *            The userid. Cannot be null.
     * @param application_name
     *            The name of the application. Cannot be null.
     * @param coloruse_name
     *            The name of the product Cannot be null.
     * @param duration
     *            The duration in seconds.
     * @param threshold_unit
     *            The threshold unit, English or Metric. Must be "E" or "M"
     * 
     * @return A collection of ColorValue structures representing a specific
     *         color set. Returns null if no data are found.
     */
    private static List<Colorvalue> getColorValueTableEntries(
            final String user_id, final String application_name,
            final String coloruse_name, int duration, String threshold_unit) {

        List<Colorvalue> cvHead = null;

        String where_clause = String.format(
                " WHERE id.userid = '%s'" + " AND id.applicationName = '%s'"
                        + " AND id.colorUseName = '%s'"
                        + " AND id.duration = %d"
                        + " AND id.thresholdUnit = '%s' "
                        + " ORDER BY id.colorUseName , id.duration , "
                        + " id.thresholdValue ",
                user_id, application_name, coloruse_name, duration,
                threshold_unit);

        cvHead = IHFSDbGenerated.GetColorValue(where_clause);

        return cvHead;
    }

    /**
     * For a given user id, application name, color use name, and unit, search
     * the ColorValue table to find all of the available durations. Determine
     * the duration which is the closest to the user-specified duration and
     * return it to the caller.
     * 
     * @param user_id
     *            The userid. Cannot be null.
     * @param application_name
     *            The name of the application. Cannot be null.
     * @param coloruse_name
     *            The name of the product Cannot be null.
     * @param duration
     *            The duration in seconds.
     * @param threshold_unit
     *            The threshold unit, English or Metric. Must be "E" or "M"
     * @return The closest duration found in the ColorValue table for the
     *         specified user id, application, color use, and threshold unit or
     *         NO_DURATION_FOUND if a close duration could not be found.
     */
    private static int get_closest_multihour_duration(final String user_id,
            final String application_name, final String coloruse_name,
            int duration, String threshold_unit) {
        int closest_duration = NO_DURATION_FOUND;
        int duration_diff;
        int duration_value;
        int min_dur_diff = Integer.MAX_VALUE;

        String query;
        // Build the where clause.
        query = String.format(
                "SELECT DISTINCT duration from ColorValue WHERE userid = '%s'"
                        + " AND application_name = '%s'"
                        + " AND color_use_name = '%s'"
                        + " AND threshold_unit = '%s'"
                        + " ORDER BY duration ASC",
                user_id, application_name, coloruse_name, threshold_unit);

        // Load the unique durations for the specified user_id,
        // application_name, color_use_name, and threshold_unit.
        try {
            List<Object[]> results = DirectDbQuery.executeQuery(query, "ihfs",
                    QueryLanguage.SQL);

            if ((results != null) && (!results.isEmpty())) {
                for (Object[] item : results) {
                    duration_value = ((Number) item[0]).intValue();
                    duration_diff = Math.abs(duration - duration_value);

                    // Is this duration the closest to the original duration
                    // found so far?
                    if (duration_diff < min_dur_diff) {
                        closest_duration = duration_value;
                        min_dur_diff = duration_diff;
                    } else {
                        // The closest duration match has been found.
                        break;
                    }
                }
            }

            // Free the memory used for the linked list of distinct durations.
            //
            if (results != null) {
                results.clear();
                results = null;
            }
        } catch (VizException e) {
            statusHandler.error("Error retrieving durations.", e);
        }

        return closest_duration;
    }
}
