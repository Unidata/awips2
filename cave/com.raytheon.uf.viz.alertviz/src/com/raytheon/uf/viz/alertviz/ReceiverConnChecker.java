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
package com.raytheon.uf.viz.alertviz;

import java.net.ServerSocket;

/**
 * Checks if the AlertViz receiver is already running on this machine on the
 * specified port.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 2, 2015  4473       njensen     Initial creation
 *
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class ReceiverConnChecker {

    private ReceiverConnChecker() {
        // don't allow instantiation
    }

    public static boolean isReceiverRunning(int port) {
        try {
            ServerSocket socket = new ServerSocket(port);
            socket.close();
            // port is not in use
            return false;
        } catch (Exception e) {
            // port is already used
            return true;
        }

    }

}
