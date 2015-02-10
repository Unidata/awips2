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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import com.raytheon.rcm.config.RadarConfig;
import com.raytheon.rcm.server.StatusManager.RadarStatus;

/**
 * 
 * This class encapsulates the watchdog activity into a plugin for the
 * RadarServer.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#    Engineer    Description
 * ------------- ---------- ----------- --------------------------
 * May 12, 2014  DR 16319   dhuffman    Initial creation.
 * Feb 10, 2015  DR 17112   D. Friedman Only alarm on dedicated radars and
 *                                      once per detected failure.
 * 
 * </pre>
 * 
 * @author dhuffman
 * @version 1.0
 */

public class RadarWatchdog extends Thread {

    private static Object semifore = new Object();

    protected static class GsmItem {
        protected String radarID;
        protected int trackedVcp;
        protected int currentVcp;
        protected boolean failed;

        protected GsmItem() {
        }
    }

    protected static class RadarItem {
        protected boolean isNew;
        protected long time;
        protected long messageTime;

        protected RadarItem() {
        }
    }

    protected static class WatchedRadar {

    }

    private long shortestWait = 0;
    private static final long fudgeTime = 30;

    private static Map<String, GsmItem> mapGSM = new ConcurrentHashMap<String, GsmItem>();
    private static Map<String, Map<String, RadarItem>> mapMnemonic = new ConcurrentHashMap<String, Map<String, RadarItem>>();
    private static Map<Integer, Integer> mapDuration = new ConcurrentHashMap<Integer, Integer>();
    private static List<String> mapMnemonicProducts = new ArrayList<String>();

    protected RadarServer radarServer;
    private static String configFileName = "radarWatchdog.txt";

    protected RadarWatchdog(RadarServer radarServer) {
        super("Watchdog");
        setDaemon(true);
        this.radarServer = radarServer;

        loadConfigFile(configFileName);

        Iterator<String> mnem = mapMnemonicProducts.iterator();
        while (mnem.hasNext()) {
            String mn = mnem.next();
            Map<String, RadarItem> mapRadar = new ConcurrentHashMap<String, RadarItem>();
            mapMnemonic.put(mn, mapRadar);
        }
    }

    public synchronized void notifyGsm(String radarID, int vcp) {
        GsmItem item = mapGSM.get(radarID);
        if (item != null) {
            item.currentVcp = vcp;
            notifyWatchdog();
        } else {
            if (isTrackedRadar(radarID)) {
                item = new GsmItem();
                item.radarID = radarID;
                item.currentVcp = vcp;
                mapGSM.put(radarID, item);
                notifyWatchdog();
            }
        }
    }

    protected boolean isTrackedRadar(String radarID) {
        RadarConfig rc = radarServer.getConfiguration().getConfigForRadar(radarID);
        return rc != null && rc.isDedicated();
    }

    public synchronized void notifyRadarItem(String radarID, String mnemonic, long messageTime, long time) {
        if (! isTrackedRadar(radarID))
            return;

        RadarItem ri = getRadarItem(mnemonic, radarID);
        if (ri != null) {
            ri.isNew = false;
            ri.messageTime = messageTime;
            ri.time = time;
        }
    }

    private RadarItem getRadarItem(final String mnemonic, final String radarID) {
        Map<String, RadarItem> mapRadar = mapMnemonic.get(mnemonic);
        if (mapRadar != null) {
            RadarItem ri = mapRadar.get(radarID);
            if (ri == null) {
                ri = new RadarItem();
                ri.isNew = true;
                mapRadar.put(radarID, ri);
            }
            return ri;
        }
        return null;
    }

    @Override
    public void run() {
        shortestWait = 0;
        while (true) {

            try {
                synchronized (semifore) {
                    semifore.wait(shortestWait < 800 ? 800 : shortestWait);
                }
            } catch (InterruptedException e) {
                e.printStackTrace();
            }

            patrol(System.currentTimeMillis());

        }
    }

    private synchronized void patrol(long currentTime) {
        long duration = 0;
        long earliestCheckTime = 0;
        Iterator<GsmItem> git = mapGSM.values().iterator();
        while (git.hasNext()) {
            GsmItem gi = git.next();

            if (! isTrackedRadar(gi.radarID)) {
                git.remove();
                continue;
            }

            /* There will be an alarm when the radar connection goes down, so
             * do not do any additional alarming.
             */
            RadarStatus rs = radarServer.getStatusManager().getRadarStatus(gi.radarID);
            if (rs != null && rs.getCurrentGSM() == null) {
                gi.trackedVcp = 0;
                continue;
            }

            if (gi.currentVcp != gi.trackedVcp) {
                for (String mn : mapMnemonicProducts) {
                    RadarItem ri = getRadarItem(mn, gi.radarID);
                    if (ri != null) {
                        ri.isNew = true;
                    }
                }
                gi.trackedVcp = gi.currentVcp;
            }

            if (mapDuration.get(gi.trackedVcp) != null) {
                boolean allOk = true;
                duration = (mapDuration.get(gi.trackedVcp) + fudgeTime) * 1000;

                for (String mn : mapMnemonicProducts) {
                    RadarItem ri = getRadarItem(mn, gi.radarID);
                    if (ri == null) {
                        continue;
                    }

                    if (ri.isNew) {
                        ri.isNew = false;
                        ri.time = currentTime;
                    } else {
                        long diff = currentTime - ri.time;
                        if (diff < duration) {
                            long nextTime = ri.time + duration;
                            if (earliestCheckTime == 0 || nextTime < earliestCheckTime) {
                                earliestCheckTime = nextTime;
                            }
                        } else {
                            allOk = false;
                            alert(duration, gi, mn);
                        }
                    }
                }
                if (allOk) {
                    gi.failed = false;
                }
            }
        }

        shortestWait = earliestCheckTime > 0 ? earliestCheckTime - currentTime : 0;
    }

    private void alert(final long duration, final GsmItem gi, final String mn) {
        if (! gi.failed) {
            gi.failed = true;
            String AlertVizMessage = "Watchdog: Radar ";
            AlertVizMessage += gi.radarID + " has not produced a '" + mn
                    + "' product in the last " + duration / 1000 + " seconds.";
            RadarServerAvailable.sendNotification(gi.radarID, AlertVizMessage);
        }
    }

    public void notifyWatchdog() {
        synchronized (semifore) {
            semifore.notify();
        }
    }

    private boolean loadConfigFile(final String filename) {
        try {
            InputStream inFile = radarServer.getConfiguration().
                    getDropInData(filename);
            InputStreamReader reader = new InputStreamReader(inFile);
            BufferedReader in = new BufferedReader(reader);
            String line;

            while ((line = in.readLine()) != null) {
                if (line.contains("#"))
                    continue;
                if (line.contains("["))
                    break;
            }

            while ((line = in.readLine()) != null) {
                if (line.contains("#"))
                    continue;
                if (line.contains("["))
                    break;
                String s[] = line.split("\\|");
                mapDuration.put(Integer.parseInt(s[0].trim()),
                        Integer.parseInt(s[1].trim()));
            }

            while ((line = in.readLine()) != null) {
                if (line.contains("#"))
                    continue;
                if (line.contains("["))
                    break;
                mapMnemonicProducts.add(line.trim());
            }

        } catch (IOException e) {
            Log.errorf(": watchdog: Error while loading config file %s",
                    filename, e);
        }

        return true;
    }

}
