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
package com.raytheon.uf.common.message;

import java.io.Serializable;
import java.net.InetAddress;

import com.raytheon.uf.common.message.adapter.WsIdAdapter;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeTypeAdapter;
import com.raytheon.uf.common.util.SystemUtil;

/**
 * The WsId contains the work station identification for the user. *
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 10, 2009            randerso    Initial creation
 * Apr 25, 2012       545  randerso    Repurposed the lockKey field as threadId
 * Sep 19, 2012     #1190  dgilling    Cache host names so toPrettyString() doesn't
 *                                     get delayed behind DNS requests.
 * Sep 20, 2012     #1190  dgilling    Create method getHostName().
 * Mar 20, 2014      2726  rjpeter     Moved hostNameCache to SystemUtil.
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

@DynamicSerialize
@DynamicSerializeTypeAdapter(factory = WsIdAdapter.class)
public class WsId implements Serializable, ISerializableObject {

    private static final long serialVersionUID = 1L;

    private final InetAddress networkId;

    private final String userName;

    private final String progName;

    private int pid;

    // replaced A1 lockKey with threadId for A2.
    // lockKey was not used in A2. This allows A2 to retain
    // compatibility with A1 for ISC purposes by not changing
    // the format of the WsId
    private final long threadId;

    /**
     * Constructs an default wsid. Only for use by serialization
     */
    public WsId() {
        this(null, null, null);
    }

    /**
     * Constructs a wsid from a string
     */
    public WsId(String s) {
        String[] token = s.split(":");

        if (token.length != 5) {
            throw new IllegalArgumentException(
                    "argument not of proper format for WsId");
        }

        try {
            long addr = Long.parseLong(token[0]);
            byte[] bytes = new byte[addr > 0xFFFFFFFFL ? 6 : 4];

            for (int i = 0; i < bytes.length; i++) {
                bytes[i] = (byte) (addr & 0xff);
                addr >>= 8;
            }

            this.networkId = InetAddress.getByAddress(bytes);
        } catch (Exception e) {
            throw new IllegalArgumentException(
                    "networkId argument not of proper format for WsId");
        }
        this.userName = token[1];
        this.progName = token[2];
        try {
            this.pid = Integer.parseInt(token[3]);
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException(
                    "pid argument not of proper format for WsId");
        }
        this.threadId = Long.parseLong(token[4]);

    }

    /**
     * Constructor for WsId taking the networkId, user name, progName.
     * 
     * @param networkId
     *            If null local IP address will be used if available, otherwise
     *            will use 0.0.0.0
     * @param userName
     *            if null current login name will be used
     * 
     * @param progName
     *            if null "unknown" will be used
     * 
     */
    public WsId(InetAddress networkId, final String userName,
            final String progName) {

        if (userName != null) {
            this.userName = userName;
        } else {
            this.userName = System.getProperty("user.name");
        }

        if (progName != null) {
            this.progName = progName;
        } else {
            this.progName = "unknown";
        }

        this.pid = SystemUtil.getPid();

        this.threadId = Thread.currentThread().getId();

        if (networkId != null) {
            this.networkId = networkId;
        } else {
            this.networkId = SystemUtil.getLocalAddress();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder o = new StringBuilder();

        long addr = 0;
        byte[] bytes = networkId.getAddress();
        for (int i = bytes.length - 1; i >= 0; i--) {
            addr = (addr << 8) | (0xff & bytes[i]);
        }

        o.append(addr).append(':').append(userName).append(':')
                .append(progName).append(':').append(pid).append(':')
                .append(threadId);
        return o.toString();
    }

    /**
     * Returns WsId as a pretty text string. A pretty text string has the
     * network address in dotted decimal form or domain name.
     * 
     * @return WsId as pretty string
     */
    public String toPrettyString() {
        String hostname = SystemUtil.getHostName(networkId);

        StringBuilder o = new StringBuilder();
        o.append(userName).append('@').append(hostname).append(':')
                .append(progName).append(':').append(pid).append(':')
                .append(threadId);

        return o.toString();
    }

    /**
     * @return the _networkId
     */
    public InetAddress getNetworkId() {
        return networkId;
    }

    public String getHostName() {
        return SystemUtil.getHostName(networkId);
    }

    /**
     * @return the _userName
     */
    public String getUserName() {
        return userName;
    }

    /**
     * @return the _progName
     */
    public String getProgName() {
        return progName;
    }

    /**
     * @return the _pid
     */
    public int getPid() {
        return pid;
    }

    /**
     * @return the threadId
     */
    public long getThreadId() {
        return threadId;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = (prime * result) + (int) (threadId ^ (threadId >>> 32));
        result = (prime * result)
                + ((networkId == null) ? 0 : networkId.hashCode());
        result = (prime * result) + pid;
        result = (prime * result)
                + ((progName == null) ? 0 : progName.hashCode());
        result = (prime * result)
                + ((userName == null) ? 0 : userName.hashCode());
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        WsId other = (WsId) obj;
        if (threadId != other.threadId) {
            return false;
        }
        if (networkId == null) {
            if (other.networkId != null) {
                return false;
            }
        } else if (!networkId.equals(other.networkId)) {
            return false;
        }
        if (pid != other.pid) {
            return false;
        }
        if (progName == null) {
            if (other.progName != null) {
                return false;
            }
        } else if (!progName.equals(other.progName)) {
            return false;
        }
        if (userName == null) {
            if (other.userName != null) {
                return false;
            }
        } else if (!userName.equals(other.userName)) {
            return false;
        }
        return true;
    }

}
