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
package com.raytheon.rcm.ccmmgr;

import com.raytheon.rcm.ccmreq.CcmRequestDefinition;
import com.raytheon.rcm.config.Configuration;
import com.raytheon.rcm.config.RadarConfig;
import com.raytheon.rcm.event.RadarEventAdapter;
import com.raytheon.rcm.message.CCM;
import com.raytheon.rcm.server.Log;
import com.raytheon.rcm.server.RadarServer;

/**
 * Manages Command Control Message Requests for the RPGs.
 * <pre>
 *
 *  SOFTWARE HISTORY
 *  Date         Ticket#    Engineer    Description
 *  ------------ ---------- ----------- --------------------------
 *  2016-05-10   18795      jdynina     Initial creation
 * </pre>
 *
 */

public class CCMManager extends RadarEventAdapter{

    RadarServer radarServer;
 
    public CCMManager(RadarServer radarServer) {
        this.radarServer = radarServer;
    }

    public String sendCcmRequest(String radarID, CcmRequestDefinition ccmRequest) {
        Configuration config = radarServer.getConfiguration();
        RadarConfig rc = config.getConfigForRadar(radarID);
        StringBuilder o = new StringBuilder();

        if (rc == null) {
            return "Unknown radar";
        }
        if (! rc.isDedicated()) {
            return "Not a dedicated radar";
        }

        o.append("restartVcp=" + ccmRequest.getRestartVcp());
        o.append(" vcp=" + ccmRequest.getVcp());
        o.append(" avsetEnabled=" + ccmRequest.getAvsetEnabled());
        o.append(" sailsCount=" + ccmRequest.getSailsCount());

        Log.eventf("Sending Command Control Message %s to %s", o,
                radarID);
        doSendCcmRequest(radarID, ccmRequest);

        return null;
    }

    private void doSendCcmRequest(String radarID, CcmRequestDefinition ccmRequest) {
        byte[] msg = new byte[0];
        if (ccmRequest != null) {
            msg = CCM.encode(ccmRequest);
            radarServer.getConnectionManager().sendMessageToRadar(radarID, msg);
        }
    }

}
