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
import java.net.UnknownHostException;
import java.util.Collections;
import java.util.Map;

import com.raytheon.uf.common.util.collections.BoundedMap;

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
 * Apr 10, 2014 2726       rjpeter     Moved hostName caching logic from WsId to here.
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class SystemUtil {
    private static final Map<InetAddress, String> hostNameCache = Collections
            .synchronizedMap(new BoundedMap<InetAddress, String>(100));

    protected static String hostName;

    protected static Integer pid;

    protected static InetAddress addr;

    static {
        /* attempt to initialize fields */
        getPid();
        getHostName();
    }

    /**
     * Returns the pid of the current process.
     * 
     * @return
     */
    public static int getPid() {
        if (pid == null) {
            pid = new Integer(ManagementFactory.getRuntimeMXBean().getName()
                    .split("@")[0]);
        }

        return pid.intValue();
    }

    /**
     * Returns the local INetAddress.
     * 
     * @return
     */
    public static InetAddress getLocalAddress() {
        if (addr == null) {
            try {
                addr = InetAddress.getLocalHost();
            } catch (UnknownHostException e) {
                try {
                    return InetAddress.getByAddress(new byte[] { 0, 0, 0, 0 });
                } catch (UnknownHostException e1) {
                    // won't happen
                }
            }
        }
        return addr;
    }

    /**
     * Returns the hostName for the given inet address. The address is cached
     * for the lifetime of the jvm after first look up. If a machine changes IPs
     * it will get mapped to the wrong hostName until a reboot.
     * 
     * @param address
     * @return
     */
    public static String getHostName(InetAddress address) {
        String hostName = hostNameCache.get(address);
        if (hostName == null) {
            hostName = address.getHostName();
            hostNameCache.put(address, hostName);
        }

        return hostName;
    }

    /**
     * Returns local host name.
     * 
     * @return
     */
    public static String getHostName() {
        if (hostName == null) {
            InetAddress addr = getLocalAddress();
            hostName = getHostName(addr);

            if (hostName == null) {
                String hostNameProp = System.getenv("HOSTNAME");
                if ((hostNameProp != null)
                        && (hostNameProp.trim().length() == 0)) {
                    hostName = hostNameProp;
                } else {
                    hostName = "localhost";
                }
            }
        }

        return hostName;
    }
}
