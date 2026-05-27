package com.raytheon.viz.mpe.ui.colors;

import java.util.List;

import com.raytheon.uf.common.dataplugin.shef.tables.Colorvalue;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.GetColorValues;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.NamedColorUseSet;

/**
 * Various utility methods used to retrieve color information from the Hydro
 * color tables/schemas.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket# Engineer Description 
 * ------------ ------- ------------ ---------------------------- 
 * Feb 03, 2022   22005   jrohwein Initial creation
 * Dec 08, 2022   23330 alockleigh Fixed for paint error when loading Best Estimate QPE in MPE 
 * Oct 27, 2023 2036378 alockleigh Add additional checks to get_colorvalues when searching for ColorValues.
 */

public class MPEGetColorValues extends GetColorValues {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(MPEGetColorValues.class);

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

        // Try to find a user defined color set.
        cvHead = getUserColorSet(user_id, application_name, coloruse_name,
                duration, threshold_unit);

        if ((cvHead == null || cvHead.isEmpty()) && displayString != null) {
            /*
             * Attempt to find a user defined color set based on the display
             * String. TODO: determine if this is actually necessary during a
             * MPE Enhancement. It is possible that all queries should be
             * completed using the display string by default.
             */
            final String dbColorName = GetColorValues
                    .lookupDBColorNameByDisplayName(displayString,
                            pColorSetGroup);
            if (dbColorName != null) {
                cvHead = getUserColorSet(user_id, application_name, dbColorName,
                        duration, threshold_unit);
            }
        }

        if (cvHead == null || cvHead.isEmpty()) {
            // Try to find an office-defined color set.
            cvHead = getUserColorSet(OFFICE_COLOR_SET_ID, application_name,
                    coloruse_name, duration, threshold_unit);
            if ((cvHead == null || cvHead.isEmpty()) && displayString != null) {
                // Handle the color use name RFCWIDE_XMRG case
                if (coloruse_name.contains("_")) {
                    String[] splitColorUse = coloruse_name.split("_");
                    if (splitColorUse.length >= 2) {
                        String newApplicationName = splitColorUse[0];
                        String newColoruseName = splitColorUse[1];
                        cvHead = GetColorValues.getDefaultColorSet(
                                newApplicationName, newColoruseName,
                                threshold_unit, pColorSetGroup);
                    }
                }
                /*
                 * Attempt to find an office defined color set based on the
                 * display String.
                 */
                final String dbColorName = GetColorValues
                        .lookupDBColorNameByDisplayName(displayString,
                                pColorSetGroup);
                if (dbColorName != null) {
                    cvHead = getUserColorSet(OFFICE_COLOR_SET_ID,
                            application_name, dbColorName, duration,
                            threshold_unit);
                }
            }
            if (cvHead == null || cvHead.isEmpty()) {
                // Try to find a default color set.
                cvHead = GetColorValues.getDefaultColorSet(application_name,
                        coloruse_name, threshold_unit, null);
                if ((cvHead == null) || (cvHead.isEmpty())) {
                    cvHead = GetColorValues.getDefaultColorSet(application_name,
                            coloruse_name, threshold_unit, pColorSetGroup);
                }
                if ((cvHead == null) || (cvHead.isEmpty())) {
                    statusHandler.handle(Priority.PROBLEM, "ERROR in " + method
                            + " Colors/levels not defined for application "
                            + application_name + " use_name = " + coloruse_name
                            + " user_id = " + user_id);
                }
            }
        }

        return cvHead;
    }

    private static List<Colorvalue> getUserColorSet(final String user_id,
            final String application_name, final String coloruse_name,
            int duration, String threshold_unit) {
        List<Colorvalue> cvHead = null;

        int closest_duration;

        // Attempt to find the closest color duration match for the given user
        // id.
        closest_duration = GetColorValues.get_closest_multihour_duration(
                user_id, application_name, coloruse_name, duration,
                threshold_unit);
        // does the closest one match?
        if ((closest_duration != NO_DURATION_FOUND)
                && (duration == closest_duration)) {
            cvHead = GetColorValues.getColorValueTableEntries(user_id,
                    application_name, coloruse_name, closest_duration,
                    threshold_unit);
        } else {

            closest_duration = GetColorValues.get_closest_multihour_duration(
                    user_id, application_name, "PRECIP_ACCUM", duration,
                    threshold_unit);
            if ((closest_duration != NO_DURATION_FOUND)
                    && (duration == closest_duration)) {
                cvHead = GetColorValues.getColorValueTableEntries(user_id,
                        application_name, "PRECIP_ACCUM", closest_duration,
                        threshold_unit);

            }
        }

        return cvHead;
    }

}
