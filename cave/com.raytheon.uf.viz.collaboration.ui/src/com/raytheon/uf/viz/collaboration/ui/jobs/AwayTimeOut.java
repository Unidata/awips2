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
package com.raytheon.uf.viz.collaboration.ui.jobs;

import java.awt.MouseInfo;
import java.awt.Point;
import java.awt.PointerInfo;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.preference.IPersistentPreferenceStore;
import org.jivesoftware.smack.packet.Presence;
import org.jivesoftware.smack.packet.Presence.Mode;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.provider.Tools;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.prefs.CollabPrefConstants;

/**
 * Monitors user input to change collaboration status to away if inactive for a
 * set period of time
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 14, 2014 2630       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class AwayTimeOut extends Job {

    private static final IUFStatusHandler log = UFStatus
            .getHandler(AwayTimeOut.class);

    // poll every 30 seconds when active
    public static final long ACTIVE_POLL_PERIOD = 30 * TimeUtil.MILLIS_PER_SECOND;

    // look for new activity every second when away
    public static final long AWAY_POLL_PERIOD = TimeUtil.MILLIS_PER_SECOND;

    private Point previousLocation;

    private long previousTime;

    private boolean timerSetAway = false;

    private Presence previousPresence;

    /**
     * @param name
     *            job name
     * @param period
     *            how often to poll for activity
     */
    public AwayTimeOut(String name) {
        super(name);
        this.previousLocation = getMouseLocation();
        this.previousTime = System.currentTimeMillis();
    }

    /**
     * Default constructor, gets configuration from system properties
     */
    public AwayTimeOut() {
        this("Away Timout Monitor");
    }

    /* (non-Javadoc)
     * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {
        try {
            IPersistentPreferenceStore prefs = Activator.getDefault()
                    .getPreferenceStore();
            boolean idleEnabled = prefs
                    .getBoolean(CollabPrefConstants.AWAY_ON_IDLE);

            CollaborationConnection connection = CollaborationConnection
                    .getConnection();
            if (!idleEnabled || connection == null || !connection.isConnected()) {
                return Status.OK_STATUS;
            }
            Point location = getMouseLocation();
            long time = System.currentTimeMillis();
            long sinceLastChange = time - previousTime;
            long timeoutMillis = prefs.getInt(CollabPrefConstants.AWAY_TIMEOUT)
                    * TimeUtil.MILLIS_PER_MINUTE;
            if (location.equals(previousLocation)) {
                if (sinceLastChange > timeoutMillis) {
                    goAway();
                }
            } else {
                if (timerSetAway) {
                    // only set to available if timer changed the status to away
                    comeBack();
                }
                this.previousLocation = location;
                this.previousTime = time;
            }
            return Status.OK_STATUS;
        } finally {
            schedule(timerSetAway ? AWAY_POLL_PERIOD : ACTIVE_POLL_PERIOD);
        }
    }

    /**
     * Set user status to away if they are available
     */
    private void goAway() {
        CollaborationConnection connection = CollaborationConnection
                .getConnection();
        Presence presence = connection.getPresence();
        if (presence.getMode().equals(Mode.available)) {
            previousPresence = presence;
            Presence newPresence = new Presence(presence.getType(),
                    presence.getStatus(), presence.getPriority(), Mode.away);
            Tools.copyProperties(presence, newPresence);
            timerSetAway = sendPresence(connection, newPresence);
        }
    }

    /**
     * Restore status to what it was before timeout
     */
    private void comeBack() {
        if (previousPresence != null) {
            CollaborationConnection connection = CollaborationConnection
                    .getConnection();
            sendPresence(connection, previousPresence);
            timerSetAway = false;
        }
    }

    /**
     * Update user status
     * 
     * @param connection
     * @param presence
     * @return true if update was successful
     */
    private boolean sendPresence(CollaborationConnection connection,
            Presence presence) {
        boolean rval = false;
        try {
            connection.getAccountManager().sendPresence(presence);
            rval = true;
        } catch (CollaborationException e) {
            log.error("Unable to change status based on inactivity", e);
        }
        return rval;
    }

    /**
     * @return current mouse location
     */
    private static Point getMouseLocation() {
        PointerInfo info = MouseInfo.getPointerInfo();
        return info.getLocation();
    }

}
