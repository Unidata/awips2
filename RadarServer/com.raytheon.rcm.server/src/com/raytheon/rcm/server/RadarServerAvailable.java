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

import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Scanner;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;

import com.raytheon.rcm.event.RadarEvent;
import com.raytheon.rcm.event.RadarEventAdapter;

/**
 * Send AlertViz notifications 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 9, 2011            mnash     Initial creation
 * 2012-07-27   DR 14896   D. Friedman  Handle multiple RPGs.
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class RadarServerAvailable extends RadarEventAdapter {

    private static final String CONNECTION_DOWN_MESSAGE = "RPG connection is down.";
    private static final String CONNECTION_UP_MESSAGE = "RPG connection is back up.";
    
    private static final String AWIPS2_FXA_PROPERTY = "awips2_fxa";
    private static final String DEFAULT_AWIPS2_FXA = "/awips2/fxa";    
    private static final String ANNOUNCER_PATH = "bin" + File.separator + "fxaAnnounce";

    private HashSet<String> knownFailures = new HashSet<String>();

    public RadarServerAvailable(RadarServer server) {
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.rcm.event.RadarEventAdapter#handleRadarEvent(com.raytheon
     * .rcm.event.RadarEvent)
     */
    @Override
    public void handleRadarEvent(RadarEvent event) {
        final String radarId = event.getRadarID();
        if (event.getType() == RadarEvent.CONNECTION_ATTEMPT_FAILED) {
            if (! knownFailures.contains(radarId)) {
                knownFailures.add(radarId);
                sendNotification(radarId, CONNECTION_DOWN_MESSAGE);
            }
        } else if (event.getType() == RadarEvent.CONNECTION_UP) {
            if (knownFailures.contains(radarId)) {
                knownFailures.remove(radarId);
                sendNotification(radarId, CONNECTION_UP_MESSAGE);
            }
        }
    }
    
    private void sendNotification(final String radarId, final String message) {
        getExecutorService().submit(new Runnable() {
            @Override
            public void run() {
                sendNotification2(radarId, message);
            }
        });
    }
    
    private void sendNotification2(String radarId, String message) {
        ProcessBuilder builder;
        Process proc = null;
        
        String fxaDir = System.getProperty(AWIPS2_FXA_PROPERTY);
        if (fxaDir == null)
            fxaDir = DEFAULT_AWIPS2_FXA;
        
        List<String> values = new ArrayList<String>();
        values.add(fxaDir + File.separator + ANNOUNCER_PATH);
        Log.event("Executing " + values.get(0));
        values.add(radarId + ' ' + message);
        values.add("RADAR");
        values.add("URGENT");
        builder = new ProcessBuilder(values);
        builder.redirectErrorStream(true);

        try {
            proc = builder.start();
            Scanner s = new Scanner(proc.getInputStream());
            while (s.hasNextLine())
                s.nextLine();
            proc.waitFor();
        } catch (Exception e) {
            Log.errorf("Error running fxaAnnounce: %s", e);
        } finally {
            if (proc != null) {
                proc.destroy();
            }
        }
    }

    // TODO: has to be daemon until there is a shutdown notification
    private static ExecutorService executorService = Executors
            .newSingleThreadExecutor(new ThreadFactory() {
                @Override
                public Thread newThread(Runnable r) {
                    Thread t = new Thread(r);
                    t.setDaemon(true);
                    return t;
                }
            });

    private static ExecutorService getExecutorService() {
        return executorService;
    }
}
