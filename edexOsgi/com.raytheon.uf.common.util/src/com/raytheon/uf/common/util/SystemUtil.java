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
package com.raytheon.uf.common.util;

import java.lang.management.ManagementFactory;
import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.SocketException;
import java.util.Enumeration;

/**
 * System utilities such as hostname and pid lookup.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 26, 2011            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class SystemUtil {
    protected static String hostName;

    protected static Integer pid;

    public static int getPid() {
        if (pid == null) {
            pid = new Integer(ManagementFactory.getRuntimeMXBean().getName()
                    .split("@")[0]);
        }

        return pid.intValue();
    }

    public static String getHostName() {
        if (hostName == null) {
            InetAddress addrToUse = null;
            boolean found = false;
            try {
                Enumeration<NetworkInterface> nis = NetworkInterface
                        .getNetworkInterfaces();
                while (nis.hasMoreElements() && !found) {
                    NetworkInterface ni = nis.nextElement();
                    ni.isVirtual();
                    ni.isUp();
                    Enumeration<InetAddress> addrs = ni.getInetAddresses();
                    while (addrs.hasMoreElements() && !found) {
                        InetAddress addr = addrs.nextElement();
                        if (addr.isLinkLocalAddress() == false
                                && addr.isSiteLocalAddress() == false
                                && addr.isLoopbackAddress() == false) {
                            addrToUse = addr;
                            found = true;
                        }
                    }
                }
            } catch (SocketException e) {
                e.printStackTrace();
            }

            if (addrToUse == null) {
                String hostNameProp = System.getenv("HOSTNAME");
                if (hostNameProp != null && hostNameProp.trim().length() == 0) {
                    hostName = hostNameProp;
                } else {
                    hostName = "localhost";
                }
            } else {
                hostName = addrToUse.getHostName();
            }
        }
        return hostName;
    }
}
