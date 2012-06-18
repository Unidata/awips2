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
package com.raytheon.uf.common.comm;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Class for logging network sent/received amounts for various types
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 1, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class NetworkStatistics {

    public static class NetworkTraffic {

        private String identifier;

        private long bytesSent;

        private long bytesReceived;

        private long requestCount;

        private NetworkTraffic(String identifier) {
            this.identifier = identifier;
        }

        private void addBytesSent(long sent) {
            bytesSent += sent;
        }

        private void addBytesReceived(long received) {
            bytesReceived += received;
        }

        private void incrementRequestCount() {
            requestCount += 1;
        }

        public long getBytesSent() {
            return bytesSent;
        }

        public long getBytesReceived() {
            return bytesReceived;
        }

        public long getRequestCount() {
            return requestCount;
        }

        public String getIdentifier() {
            return identifier;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#clone()
         */
        @Override
        public NetworkTraffic clone() {
            NetworkTraffic newTraffic = new NetworkTraffic(identifier);
            newTraffic.identifier = identifier;
            newTraffic.bytesReceived = bytesReceived;
            newTraffic.bytesSent = bytesSent;
            newTraffic.requestCount = requestCount;
            return newTraffic;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            String sentString = NetworkStatistics.toString(bytesSent), receivedString = NetworkStatistics
                    .toString(bytesReceived);

            return "Network Traffic Stats for '" + identifier + "' : "
                    + requestCount + " messages, sent " + sentString
                    + ", received " + receivedString;
        }

    }

    private NetworkTraffic totalTraffic = new NetworkTraffic(null);

    private Map<String, NetworkTraffic> mappedTraffic = new LinkedHashMap<String, NetworkTraffic>();

    public NetworkStatistics() {

    }

    /**
     * Add to the log of bytes sent/received for total network traffic. Since
     * *all* network communication *should* go through HttpClient, this method
     * has package level visibility. Other methods should log using an
     * identifier string
     * 
     * @param bytesSent
     * @param bytesRecieved
     */
    void log(long bytesSent, long bytesReceived) {
        synchronized (totalTraffic) {
            totalTraffic.addBytesSent(bytesSent);
            totalTraffic.addBytesReceived(bytesReceived);
            if (bytesSent > 0) {
                totalTraffic.incrementRequestCount();
            }
        }
    }

    /**
     * Add to the log of bytes sent/received for the traffic tracked by the type
     * identifier passed in
     * 
     * @param typeIdentifier
     * @param bytesSent
     * @param bytesRecieved
     */
    public synchronized void log(String typeIdentifier, long bytesSent,
            long bytesReceived) {
        NetworkTraffic traffic = mappedTraffic.get(typeIdentifier);
        if (traffic == null) {
            traffic = new NetworkTraffic(typeIdentifier);
            mappedTraffic.put(typeIdentifier, traffic);
        }
        traffic.addBytesSent(bytesSent);
        traffic.addBytesReceived(bytesReceived);
        traffic.incrementRequestCount();
        this.log(bytesSent, bytesReceived);
    }

    /**
     * Get a copy of the total traffic stats at point of calling
     * 
     * @return
     */
    public NetworkTraffic getTotalTrafficStats() {
        synchronized (totalTraffic) {
            return totalTraffic.clone();
        }
    }

    /**
     * Get the NetworkTraffic objects for mapped entries. Theoretically total
     * from mapped should add up to values from getMappedTrafficStats()
     * 
     * @return copy of network traffic stats
     */
    public NetworkTraffic[] getMappedTrafficStats() {
        Collection<NetworkTraffic> trafficStats = mappedTraffic.values();
        NetworkTraffic[] traffic = trafficStats.toArray(
                new NetworkTraffic[trafficStats.size()]).clone();
        for (int i = 0; i < traffic.length; ++i) {
            traffic[i] = traffic[i].clone();
        }
        return traffic;
    }

    private static final long[] divisions = new long[] { 1, 1024, 1024 * 1024,
            1024 * 1024 * 1024 };

    private static final String[] units = new String[] { "B", "kB", "MB", "GB" };

    public static String toString(long amount) {
        String unit = units[units.length - 1];
        long divideBy = divisions[divisions.length - 1];
        for (int i = 0; i < divisions.length - 1; ++i) {
            if (amount < divisions[i + 1]) {
                divideBy = divisions[i];
                unit = units[i];
                break;
            }
        }

        return ((amount / divideBy) + unit);
    }
}
