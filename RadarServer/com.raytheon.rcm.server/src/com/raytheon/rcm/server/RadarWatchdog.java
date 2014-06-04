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

import com.raytheon.rcm.config.Configuration;

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
        protected int vcp;
        protected long time;
        protected long alarmTime;
        protected long nextAlarmTime;

        protected GsmItem() {
        }
    }

    protected static class RadarItem {
        protected String radarID;
        protected String mnemonic;
        protected long time;
        protected long messageTime;

        protected RadarItem() {
        }
    }

    private long startTime = 0;
    private long shortestWait = 0;
    private static final long fudgeTime = 30;

    private static Map<String, GsmItem> mapGSM = new ConcurrentHashMap<String, GsmItem>();
    private static Map<String, Map<String, RadarItem>> mapMnemonic = new ConcurrentHashMap<String, Map<String, RadarItem>>();
    private static Map<Integer, Integer> mapDuration = new ConcurrentHashMap<Integer, Integer>();
    private static List<String> mapMnemonicProducts = new ArrayList<String>();

    protected Configuration configuration;
    private static String configFileName = "radarWatchdog.txt";

    protected RadarWatchdog(Configuration conf) {
        setDaemon(true);
        startTime = System.currentTimeMillis();
        configuration = conf;

        loadConfigFile(configFileName);

        Iterator<String> mnem = mapMnemonicProducts.iterator();
        while (mnem.hasNext()) {
            String mn = mnem.next();
            Map<String, RadarItem> mapRadar = new ConcurrentHashMap<String, RadarItem>();
            mapMnemonic.put(mn, mapRadar);
        }
    }

    public GsmItem getGSMItem(final String radarID) {
        return mapGSM.get(radarID);
    }

    public void putGSMItem(GsmItem gi) {
        if (gi != null) {
            mapGSM.put(gi.radarID, gi);
        }
    }

    public RadarItem getRadarItem(final String Mnemonic, final String radarID) {
        Map<String, RadarItem> mapRadar = mapMnemonic.get(Mnemonic);
        if (mapRadar != null)
            return mapRadar.get(radarID);
        return null;
    }

    public void putRadarItem(RadarItem ri) {
        if (ri != null) {
            Map<String, RadarItem> mapRadar = mapMnemonic.get(ri.mnemonic);
            if (mapRadar != null) {
                mapRadar.put(ri.radarID, ri);
            }
        }
    }

    @Override
    public void run() {
        long currentTime = 0;
        shortestWait = 0;
        while (true) {

            try {
                synchronized (semifore) {
                    semifore.wait(shortestWait < 800 ? 800 : shortestWait);
                    currentTime = System.currentTimeMillis();
                }
            } catch (InterruptedException e) {
                e.printStackTrace();
            }

            patrol(currentTime);

        }
    }

    private void patrol(long currentTime) {
        long duration = 0;
        long adjustedTime = currentTime - fudgeTime;
        shortestWait = 0;
        Iterator<GsmItem> git = mapGSM.values().iterator();
        while (git.hasNext()) {
            GsmItem gi = git.next();
            if (mapDuration.get(gi.vcp) != null) {
                duration = mapDuration.get(gi.vcp) * 1000;

                Iterator<String> mnem = mapMnemonicProducts.iterator();
                while (mnem.hasNext()) {
                    String mn = mnem.next();
                    Map<String, RadarItem> mapRadar = mapMnemonic.get(mn);
                    if (mapRadar == null)
                        continue;
                    RadarItem ri = mapRadar.get(gi.radarID);

                    if (ri == null) {
                        if (duration + startTime < adjustedTime
                                && gi.alarmTime != startTime) {
                            alert(duration, gi, mn);
                            gi.alarmTime = startTime;
                            gi.nextAlarmTime = startTime + duration;
                        }

                        if (shortestWait < 1 || duration < shortestWait)
                            shortestWait = duration;
                    }

                    if (ri != null) {

                        if (ri.time + duration < adjustedTime) {
                            if (ri.time <= gi.alarmTime
                                    && gi.nextAlarmTime < currentTime) {
                                alert(duration, gi, ri.mnemonic);
                                gi.alarmTime = ri.time;
                                gi.nextAlarmTime = currentTime + duration;
                            }
                            if (gi.nextAlarmTime < currentTime)
                                gi.alarmTime = ri.time;
                        }

                        if ((duration + ri.time) - adjustedTime < shortestWait
                                && 1 <= (duration + ri.time) - adjustedTime)
                            shortestWait = (duration + ri.time) - adjustedTime;
                        if (shortestWait < 1)
                            shortestWait = duration;
                    }

                }
            }
        }
    }

    private void alert(final long duration, final GsmItem gi, final String mn) {
        String AlertVizMessage = "Watchdog: Radar ";
        AlertVizMessage += gi.radarID + " has not produced a '" + mn
                + "' product in the last " + duration / 1000 + " seconds.";
        RadarServerAvailable.sendNotification(gi.radarID, AlertVizMessage);
    }

    public void notifyWatchdog() {
        synchronized (semifore) {
            semifore.notify();
        }
    }

    private boolean loadConfigFile(final String filename) {
        try {
            InputStream inFile = configuration.getDropInData(filename);
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
