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
package com.raytheon.uf.edex.activetable.vtecsharing;

import java.util.Map;

/**
 * Configuration information for a given site's vtec active table fetching.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 28, 2013            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class FetchATJobConfig {

    private String siteId;

    private long interval;

    private String ancfAddress;

    private String bncfAddress;

    private String serverHost;

    private String port;

    private String protocolV;

    private String mhsId;

    private String transmitScript;

    public FetchATJobConfig(Map<String, Object> configMap) {
        siteId = configMap.get("siteId").toString();
        interval = ((Number) configMap.get("interval")).longValue();
        ancfAddress = configMap.get("ancf").toString();
        bncfAddress = configMap.get("bncf").toString();
        serverHost = configMap.get("serverHost").toString();
        port = configMap.get("port").toString();
        protocolV = configMap.get("protocolV").toString();
        mhsId = configMap.get("mhsid").toString();
        transmitScript = configMap.get("transmitScript").toString();
    }

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
        FetchATJobConfig other = (FetchATJobConfig) obj;
        if (ancfAddress == null) {
            if (other.ancfAddress != null) {
                return false;
            }
        } else if (!ancfAddress.equals(other.ancfAddress)) {
            return false;
        }
        if (bncfAddress == null) {
            if (other.bncfAddress != null) {
                return false;
            }
        } else if (!bncfAddress.equals(other.bncfAddress)) {
            return false;
        }
        if (interval != other.interval) {
            return false;
        }
        if (mhsId == null) {
            if (other.mhsId != null) {
                return false;
            }
        } else if (!mhsId.equals(other.mhsId)) {
            return false;
        }
        if (port == null) {
            if (other.port != null) {
                return false;
            }
        } else if (!port.equals(other.port)) {
            return false;
        }
        if (protocolV == null) {
            if (other.protocolV != null) {
                return false;
            }
        } else if (!protocolV.equals(other.protocolV)) {
            return false;
        }
        if (serverHost == null) {
            if (other.serverHost != null) {
                return false;
            }
        } else if (!serverHost.equals(other.serverHost)) {
            return false;
        }
        if (siteId == null) {
            if (other.siteId != null) {
                return false;
            }
        } else if (!siteId.equals(other.siteId)) {
            return false;
        }
        if (transmitScript == null) {
            if (other.transmitScript != null) {
                return false;
            }
        } else if (!transmitScript.equals(other.transmitScript)) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("FetchATJobConfig [siteId=");
        builder.append(siteId);
        builder.append(", interval=");
        builder.append(interval);
        builder.append(", ancfAddress=");
        builder.append(ancfAddress);
        builder.append(", bncfAddress=");
        builder.append(bncfAddress);
        builder.append(", serverHost=");
        builder.append(serverHost);
        builder.append(", port=");
        builder.append(port);
        builder.append(", protocolV=");
        builder.append(protocolV);
        builder.append(", mhsId=");
        builder.append(mhsId);
        builder.append(", transmitScript=");
        builder.append(transmitScript);
        builder.append("]");
        return builder.toString();
    }

    public String getSiteId() {
        return siteId;
    }

    public long getInterval() {
        return interval;
    }

    public String getAncfAddress() {
        return ancfAddress;
    }

    public String getBncfAddress() {
        return bncfAddress;
    }

    public String getServerHost() {
        return serverHost;
    }

    public String getPort() {
        return port;
    }

    public String getProtocolV() {
        return protocolV;
    }

    public String getMhsId() {
        return mhsId;
    }

    public String getTransmitScript() {
        return transmitScript;
    }

}
