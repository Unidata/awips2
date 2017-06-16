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
package com.raytheon.viz.texteditor;

import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.SocketException;
import java.net.UnknownHostException;
import java.util.Enumeration;

import com.raytheon.viz.core.mode.CAVEMode;

/**
 * Constants for the text workstation
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 11, 2009            mschenke    Initial creation
 * Nov 12, 2015 4834       njensen     Changed LocalizationOpFailedException to LocalizationException
 * Jun 15, 2017            mjames@ucar Remove preferences store, assume localhost.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 * 
 * 
 */

public class TextWorkstationConstants {

    private static final String TEXTWORKSTATION_QUEUE = "textWorkstation";

    public static final String P_TEXTWORKSTATION_ID = "workstationId";

    private static String host = "localhost";

    public static String getId() {
        return host;
    }

    /**
     * Gets the ip address of the host machine calling the function
     * 
     * @return
     */
    public static String getHostName() {
        if (host == null) {
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
                try {
                    // Grab whatever is in the preference for cave
                    host = InetAddress.getByName(getId()).getHostName();
                } catch (UnknownHostException e) {
                    e.printStackTrace();
                }
            } else {
                host = addrToUse.getHostName();
            }
        }
        return host;
    }

    public static String getDestinationTextWorkstationQueueName()
            throws UnknownHostException {
        StringBuilder queueName = getTextWorkstationQueueNameBuilder();
        queueName.append("_");
        queueName.append(getHostNameStr());
        return queueName.toString();
    }

    public static String getTextWorkstationQueueName() {
        StringBuilder queueName = getTextWorkstationQueueNameBuilder();
        queueName.append("_");
        queueName.append(TextWorkstationConstants.getHostName());
        return queueName.toString();
    }

    private static StringBuilder getTextWorkstationQueueNameBuilder() {
        StringBuilder queueName = new StringBuilder(
                TextWorkstationConstants.TEXTWORKSTATION_QUEUE);
        if (CAVEMode.getMode().equals(CAVEMode.PRACTICE)) {
            queueName.append("_");
            queueName.append(CAVEMode.PRACTICE.name());
        }
        return queueName;
    }

    private static String getHostNameStr() throws UnknownHostException {
        String host = TextWorkstationConstants.getId();
        if (host == null || "".equals(host)) {
            throw new UnknownHostException("Host: " + host + " is not valid");
        }
        return host;
    }

}
