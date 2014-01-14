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

    public static final int AWAY_TIMEOUT_DEFAULT = 10; // ten minutes

    public class HttpCollaborationConfiguration {
        public static final String P_SESSION_CONFIGURED = "http.sessionConfigured";

        public static final String P_HTTP_SESSION_URL = "http.sessionURL";
    }
}
