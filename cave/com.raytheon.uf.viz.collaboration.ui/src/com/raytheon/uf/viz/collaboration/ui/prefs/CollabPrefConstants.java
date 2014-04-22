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
package com.raytheon.uf.viz.collaboration.ui.prefs;

/**
 * Collaboration preferences constants used to interact with preference store
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 24, 2012            njensen     Initial creation
 * Jan 14, 2014 2630       bclement    added away on idle constants
 * Jan 27, 2014 2700       bclement    added auto accept subscribe
 * Feb  3, 2014 2699       bclement    added handle preferences
 * Feb 18, 2014 2631       mpduff      Add constants for room change events.
 * Mar 24, 2014 2936       mpduff      Remove INCLUDE_NWS_FEED_FIELD_EDITOR_ID.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */
public class CollabPrefConstants {

    public static final String P_SERVER = "collaborationServer";

    public static final String P_USERNAME = "username";

    public static final String P_STATUS = "status";

    public static final String P_MESSAGE = "message";

    public static final String AUTO_JOIN = "autojoin";

    public static final String AWAY_ON_IDLE = "awayOnIdle";

    public static final String AWAY_TIMEOUT = "awayTimeOut";

    public static final String AUTO_ACCEPT_SUBSCRIBE = "autoAcceptSubscribe";

    public static final String DEFAULT_HANDLE = "defaultHandle";

    public static final String CUSTOM_HANDLE = "customHandle";

    public static final int AWAY_TIMEOUT_DEFAULT = 10; // ten minutes

    /** Enable join events field editor id */
    public static final String ENABLE_JOIN_EVENTS_FIELD_EDITOR_ID = "enableJoinAlerts";

    /** Join file field editor id */
    public static final String JOIN_FILE_FIELD_EDITOR_ID = "roomJoinSoundFile";

    public class HttpCollaborationConfiguration {
        public static final String P_SESSION_CONFIGURED = "http.sessionConfigured";

        public static final String P_HTTP_SESSION_URL = "http.sessionURL";
    }

    public static enum HandleOption {
        BLANK("Blank"), USERNAME("User Name"), FULLNAME("Full Name"), ROLE(
                "Role"), CUSTOM("Custom");

        public final String display;

        private HandleOption(String display) {
            this.display = display;
        }

        public static String[][] displayValues() {
            HandleOption[] values = values();
            String[][] rval = new String[values.length][2];
            for (int i = 0; i < rval.length; ++i) {
                HandleOption op = values[i];
                rval[i] = new String[] { op.display, op.name() };
            }
            return rval;
        }
    }
}
