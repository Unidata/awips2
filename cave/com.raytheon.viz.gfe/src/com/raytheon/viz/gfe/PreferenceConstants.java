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

package com.raytheon.viz.gfe;

/**
 * Constant definitions for plug-in preferences
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date       	Ticket#		Engineer	Description
 *    ------------	----------	-----------	--------------------------
 *    25Jan07                 ebabin      Initial Creation.
 *    12Jan10      3500          njensen      Removed unused constants
 * 
 * 
 * </pre>
 * 
 * @author ebabin
 */
public class PreferenceConstants {

    public static final String PREFERENCE_DEFAULT_VALUE = "prefdefaultvalue";

    public static final String PREFERENCE_ID = "prefid";

    public static final String PREFERENCE_VALUE = "prefvalue";

    public static final String PREFERENCE_SELECTED = "prefselected";

    public static final String GFE_GRID_MODE = "InitialGMDisplayMode";

    public static final String GFE_AUTO_SAVE_INTERVAL = "AutoSaveInterval";

    public static final String GFE_TEMPORAL_EDITOR_STATISTICS_MODE = "TemporalEditorStatisticsMode";

    public static final String GFE_TEMPORAL_EDITOR_STATISTICS_MODE_MODERATED_MIN = "TemporalEditorStatisticsModeModeratedMin";

    public static final String GFE_TEMPORAL_EDITOR_STATISTICS_MODE_MODERATED_MAX = "TemporalEditorStatisticsModeModeratedMax";

    public static final String GFE_TEMPORAL_EDITOR_STATISTICS_MODE_SD_MIN = "TemporalEditorStatisticsModeStandardDeviationMin";

    public static final String GFE_TEMPORAL_EDITOR_STATISTICS_MODE_SD_MAX = "TemporalEditorStatisticsModeStandardDeviationMax";

    public static final String GFE_TIME_SCALE_DISPLAY_PERIODS = "TimeScalePeriods";

    public static final String GFE_WE_INTERPOLATE_ALGORITHMS = "weatherElementInterpolateAlgorithms";

    public static final String GFE_WE_INTERPOLATE_ALGORITHM = "weAlgorithm";

    public static final String GFE_WE = "weatherElement";

    public static final String GFE_ALGORITHM = "algorithm";

    public static final String GFE_EDIT_AREA_REFERENCE_SET_COLOR = "ReferenceSet_color";

    public static final String GFE_EDIT_AREA_REFERENCE_SET_WIDTH = "ReferenceSet_width";

    public static final String GFE_WIND_FORMAT_SUFFIX = "_windFormat";

    public static final String GFE_WIND_FORMAT_STR = "WindFormat";

    public static final String[] GFE_INTERPOLATE_ALGORITHM_LIST = {
            "Linear/NoAdvection", "Cubic/NoAdvection", "Linear/Advection",
            "Cubic/Advection" };

    public static final String GFE_GRIDMANAGER_SORT_ORDER = "GridManagerSortOrder";
}
