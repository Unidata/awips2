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
package com.raytheon.rcm.server;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Timer;
import java.util.TimerTask;

import com.raytheon.rcm.config.Configuration;
import com.raytheon.rcm.config.RadarConfig;
import com.raytheon.rcm.event.ConfigEvent;
import com.raytheon.rcm.event.RadarEvent;
import com.raytheon.rcm.event.RadarEventAdapter;

/**
 * Keeps dedicated RPGs connected.
 */
public class DedicatedRadarActivator extends RadarEventAdapter implements
        Runnable {

    enum State {
        UNCONNECTED, CONNECTING, CONNECTED, RETRY
    }

    private static class RadarStatus {
        public String radarID;

        public long nextTime = -1;

        public State state = State.UNCONNECTED;

        public RadarStatus(String radarID) {
            this.radarID = radarID;
        }
    }

    private static final int RETRY_WAIT_TIME = 30 * 1000;

    private static final int RECONNECT_WAIT_TIME = 2 * 1000;

    protected RadarServer radarServer;

    protected HashMap<String, RadarStatus> radarState = new HashMap<String, RadarStatus>();

    private Thread t;

    public DedicatedRadarActivator(RadarServer radarServer) {
        this.radarServer = radarServer;

        addDedicatedRadars();

        t = new Thread(this);
        t.setDaemon(true);
        t.setName("RadarActivator");
        t.start();

        addKeepAliveTimer();

        checkRadars();
    }

    protected void addDedicatedRadars() {
        Configuration config = radarServer.getConfiguration();
        synchronized (radarState) {
            HashMap<String, RadarStatus> newState = new HashMap<String, RadarStatus>();
            ArrayList<String> debugList = new ArrayList<String>();
            for (String radarID : config.getConfiguredRadarList()) {
                RadarConfig rc = config.getConfigForRadar(radarID);
                if (rc.getActivation() == RadarConfig.ACTIVATE_ALWAYS) {
                    RadarStatus status = radarState.get(radarID);
                    if (status == null)
                        status = new RadarStatus(radarID);
                    newState.put(radarID, status);
                    debugList.add(radarID);
                }
            }
            radarState.clear();
            radarState.putAll(newState);
        }
    }

    protected void checkRadars() {
        long now = System.currentTimeMillis();
        Configuration config = radarServer.getConfiguration();

        synchronized (radarState) {
            for (RadarStatus rs : radarState.values()) {
                RadarConfig rc = config.getConfigForRadar(rs.radarID);

                if (rc == null) {
                    radarState.remove(rs.radarID);
                    continue;
                }

                if (rc.getActivation() == RadarConfig.ACTIVATE_ALWAYS) {
                    if (rs.state == State.UNCONNECTED
                            || (rs.state == State.RETRY && now >= rs.nextTime))
                        tryConnect(rs);
                } else {
                    rs.nextTime = -1;
                }
            }

            radarState.notify();
        }
    }

    private void tryConnect(RadarStatus rs) {
        rs.nextTime = -1;
        rs.state = State.CONNECTING;
        radarServer.getConnectionManager().connectRadar(rs.radarID);
    }

    private void retryConnect(RadarStatus rs) {
        rs.nextTime = System.currentTimeMillis() + RETRY_WAIT_TIME;
        rs.state = State.RETRY;
    }

    @Override
    public void handleRadarEvent(RadarEvent event) {
        if (event.getType() == RadarEvent.MESSAGE_RECEIVED)
            return;

        synchronized (radarState) {
            RadarStatus rs = radarState.get(event.getRadarID());
            if (rs == null)
                return;
            if (event.getType() == RadarEvent.CONNECTION_UP) {
                rs.state = State.CONNECTED;
                rs.nextTime = -1;
            } else if (event.getType() == RadarEvent.CONNECTION_DOWN) {
                rs.state = State.UNCONNECTED;
                rs.nextTime = System.currentTimeMillis() + RECONNECT_WAIT_TIME;
            } else if (event.getType() == RadarEvent.CONNECTION_ATTEMPT_FAILED) {
                retryConnect(rs);
            } else
                return;
            radarState.notify();
        }
    }

    @Override
    public void run() {
        // There is no 'shutdown' event, so this has no exit condition.
        while (true) {
            try {
                long nextTime = -1;
                synchronized (radarState) {
                    for (RadarStatus rs : radarState.values()) {
                        if (rs.nextTime != -1
                                && (nextTime == -1 || rs.nextTime < nextTime))
                            nextTime = rs.nextTime;
                    }
                    if (nextTime == -1) {
                        // add long wait time here so that if it goes down, it
                        // will try again after that period of time
                        // 3 minutes = (60 * 3 * 1000)
                        radarState.wait(60000);
                    } else {
                        long waitTime = nextTime != -1 ? nextTime
                                - System.currentTimeMillis() : 0;
                        if (waitTime > 0)
                            radarState.wait(waitTime);
                    }
                }
                checkRadars();
            } catch (Throwable e) {
                Log.eventf("%s", "Caught exception while running" + e);
            }
        }
    }

    @Override
    public void handleConfigEvent(ConfigEvent event) {
        String radarID = event.getRadarID();
        if (radarID == null)
            return; // Nothing in the global config affects us

        synchronized (radarState) {
            RadarStatus status = radarState.get(radarID);
            RadarConfig oldCfg = event.getOldConfig();
            RadarConfig newCfg = event.getNewConfig();
            if (status != null
                    && oldCfg != null
                    && newCfg != null
                    && (oldCfg.getActivation() == RadarConfig.ACTIVATE_ALWAYS) != (newCfg
                            .getActivation() == RadarConfig.ACTIVATE_ALWAYS)) {
                radarServer.getConnectionManager().disconnectRadar(radarID);
            }
            if (newCfg.getActivation() != RadarConfig.ACTIVATE_ALWAYS)
                return;
        }
        addDedicatedRadars();
        checkRadars();
    }

    private void addKeepAliveTimer() {
        Timer timer = new Timer("RadarServerKeepAliveTimer", true);
        TimerTask timerTask = new TimerTask() {
            @Override
            public void run() {
                try {
                    if (!t.isAlive()) {
                        Log.eventf("%s",
                                "Radar activator thread died, restarting...");
                        t = new Thread(this);
                        t.setName("RadarActivator");
                        t.setDaemon(true);
                        t.start();
                    }
                } catch (Throwable e) {
                    Log.eventf("%s",
                            "Caught exception while trying to restart the radar activator thread"
                                    + e);
                }
            }
        };
        timer.schedule(timerTask, 0, 60000);
    }
}
