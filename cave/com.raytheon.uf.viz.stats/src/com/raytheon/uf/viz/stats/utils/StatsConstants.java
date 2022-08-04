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
package com.raytheon.uf.viz.stats.utils;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.InetAddress;
import java.net.UnknownHostException;

/**
 * Constants for CAVE statistics.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 6, 2013    1584     mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class StatsConstants {
    /**
     * Hostname of the workstation running CAVE.
     */
    private static final String hostname = defineHostName();

    /**
     * CAVE version.
     */
    private static final String release = defineRelease();

    /**
     * Return the host name.
     * 
     * @return the host name
     */
    private static String defineHostName() {
        String hn = null;
        InetAddress addr;
        try {
            addr = InetAddress.getLocalHost();
            hn = addr.getHostName();
        } catch (UnknownHostException e) {
            // Nothing we can do
        }

        return hn;
    }

    /**
     * Define the release that is currently running.
     * 
     * @return The release
     */
    private static String defineRelease() {
        String release = null;
        BufferedReader in = null;
        try {
            Process p = Runtime.getRuntime().exec(
                    "rpm -q --queryformat '%{VERSION}\n' awips2");
            in = new BufferedReader(new InputStreamReader(p.getInputStream()));
            String line = null;
            while ((line = in.readLine()) != null) {
                if (!line.contains("not installed")) {
                    release = line;
                }
            }
        } catch (IOException e) {
            // do nothing
        } finally {
            if (in != null) {
                try {
                    in.close();
                } catch (IOException ioe) {
                    // do nothing
                }
            }
        }

        return release;
    }

    /**
     * @return CAVE version
     */
    public static String getRelease() {
        return release;
    }

    /**
     * @return Hostname of the workstation running CAVE
     */
    public static String getHostname() {
        return hostname;
    }
}
