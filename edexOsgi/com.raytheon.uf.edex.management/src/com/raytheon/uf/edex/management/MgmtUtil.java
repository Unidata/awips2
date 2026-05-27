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
package com.raytheon.uf.edex.management;

import java.net.InetAddress;
import java.net.UnknownHostException;

import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterTask;

/**
 * Utilities for edex management
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 9, 2010            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class MgmtUtil {

    protected static final String TASK = "edexManagement";

    protected static String buildLocalDetails() throws UnknownHostException {
        return buildDetails(getHostname(), getRunMode());
    }

    protected static String buildDetails(String hostname, String runMode) {
        return hostname + ":" + runMode;
    }

    public static String getHostname() throws UnknownHostException {
        return InetAddress.getLocalHost().getCanonicalHostName();
    }

    public static String getRunMode() {
        return System.getProperty("edex.run.mode");
    }

    public static String getPortNumber(String hostname, String jvmName) {
        ClusterTask lock = ClusterLockUtils.lookupLock(TASK,
                buildDetails(hostname, jvmName));
        return lock.getExtraInfo();
    }

    public static String buildAddress(String hostname, String portNumber) {
        return "http://" + hostname + ":" + portNumber + "/services/edexMgmt";
    }

}
