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
import java.util.List;
import java.util.Scanner;

import com.raytheon.rcm.event.RadarEvent;
import com.raytheon.rcm.event.RadarEventAdapter;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 9, 2011            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class RadarServerAvailable extends RadarEventAdapter {

    private static boolean attempted = false;

    private ProcessBuilder builder;

    /**
     * 
     */
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
        Process proc = null;
        String home = System.getProperty("awips2_fxa");
        if (home != null && !home.endsWith(File.separator)) {
            home += File.separator;
        } else if (home == null) {
            Log.event("Cannot find awips2_fxa system variable");
            return;
        }
        List<String> values = new ArrayList<String>();
        values.add(home + "bin" + File.separator + "fxaAnnounce");
        try {
            if (event.getType() == RadarEvent.CONNECTION_ATTEMPT_FAILED) {
                if (!attempted) {
                    Log.event("Executing " + values.get(0));
                    values.add(event.getRadarID() + " rpg connection is down.");
                    values.add("RADAR");
                    values.add("URGENT");
                    builder = new ProcessBuilder(values);
                    builder.redirectErrorStream(true);

                    proc = builder.start();
                    StringBuilder output = new StringBuilder();
                    Scanner s = new Scanner(proc.getInputStream());
                    while (s.hasNextLine()) {
                        if (output.length() > 0)
                            output.append('\n');
                        output.append(s.nextLine());
                    }
                    proc.waitFor();
                    attempted = true;
                }
            } else if (event.getType() == RadarEvent.CONNECTION_UP) {
                if (attempted) {
                    Log.event("Executing " + values.get(0));
                    values.add(event.getRadarID()
                            + " rpg connection is back up.");
                    values.add("RADAR");
                    values.add("URGENT");
                    builder = new ProcessBuilder(values);
                    builder.redirectErrorStream(true);
                    proc = builder.start();
                    StringBuilder output = new StringBuilder();
                    Scanner s = new Scanner(proc.getInputStream());
                    while (s.hasNextLine()) {
                        if (output.length() > 0)
                            output.append('\n');
                        output.append(s.nextLine());
                    }
                    proc.waitFor();
                    attempted = false;
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            if (proc != null) {
                proc.destroy();
            }
        }
    }
}
