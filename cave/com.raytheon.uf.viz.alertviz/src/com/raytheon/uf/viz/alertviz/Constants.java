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
package com.raytheon.uf.viz.alertviz;

/**
 * Defines constants used in alertviz
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 8, 2008  1433       chammack    Initial creation
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class Constants {

    public static final String P_NUMBER_OF_LOGS = "numberOfLogsToKeep";

    public static final String P_MAX_AGE_OF_LOGS = "maxAgeOfLogEntries";

    public static final String LOCAL = "LOCAL";

    public static final String MONITOR = "MONITOR";

    public static final String LOG_DIR = "logs";

    public static final String PREF_PREFIX = "alertviz.";

    public static final String SOURCES_PREFIX = "sources";

    public static final String CATEGORIES_PREFIX = "categories";

    public static final String LOCKED_SUFFIX = "[@locked]";

    public static final String CATEGORY_NAME_SUFFIX = "[@categoryName]";

    public static final String TEXTBOX_SUFFIX = "[@textBox]";

    public static final String LONGNAME_SUFFIX = "[@longName]";

    public static final String POPUP_SUFFIX = "[@popup]";

    public static final String BLINK_SUFFIX = "[@blink]";

    public static final String TEXT_SUFFIX = "[@text]";

    public static final String AUDIOFILE_SUFFIX = "[@audioFile]";

    public static final String AUDIO_ENABLED_SUFFIX = "[@audioEnabled]";

    public static final String PYTHONFILE_SUFFIX = "[@pythonScript]";

    public static final String PYTHON_ENABLED_SUFFIX = "[@pythonEnabled]";

    public static final String BGCOLOR_SUFFIX = "[@backgroundColor]";

    public static final String FGCOLOR_SUFFIX = "[@foregroundColor]";

    public static final String LOG_SUFFIX = "[@log]";

    public static final String GLOBAL_PREFIX = "global.";

    public static final String GLOBAL_MODE_SUFFIX = "[@mode]";

    public static final String GLOBAL_PRIORITY_SHOWN_SUFFIX = "[@priorityShown]";

    public static final String GLOBAL_SOURCE_SHOWN_SUFFIX = "[@sourceKeyShown]";

    public static final String GLOBAL_CATEGORY_SHOWN_SUFFIX = "[@categoryShown]";

    public static final String GLOBAL_EXPANDED_POPUP_SUFFIX = "[@expandedPopup]";

    public static final String GLOBAL_BLINK_DURATION_SUFFIX = "[@blinkDuration]";

    public static final String GLOBAL_AUDIO_DURATION_SUFFIX = "[@audioDuration]";

    public static final String GLOBAL_LOG_LENGTH_SUFFIX = "[@logLength]";

    public static final String GLOBAL_X_SUFFIX = "[@x]";

    public static final String GLOBAL_Y_SUFFIX = "[@y]";

    public static final String GLOBAL_HEIGHT_SUFFIX = "[@height]";

    public static final String GLOBAL_WIDTH_SUFFIX = "[@width]";

    private Constants() {
        // no instantiation
    }
}
