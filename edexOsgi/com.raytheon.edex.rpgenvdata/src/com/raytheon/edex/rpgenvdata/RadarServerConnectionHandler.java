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
package com.raytheon.edex.rpgenvdata;

import com.raytheon.uf.common.dataplugin.radar.request.RadarServerConnectionRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * Handles a RadarServerConnectionRequest, and returns the address to the radar
 * server in the system
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 31, 2011            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class RadarServerConnectionHandler implements
        IRequestHandler<RadarServerConnectionRequest> {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest
     * (com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @Override
    public String handleRequest(RadarServerConnectionRequest request)
            throws Exception {
        String radarServerConnectionURL = System.getenv("RADAR_SERVER");
        if (radarServerConnectionURL == null) {
            radarServerConnectionURL = "tcp://localhost:8813";
        }
        return radarServerConnectionURL;
    }

}
