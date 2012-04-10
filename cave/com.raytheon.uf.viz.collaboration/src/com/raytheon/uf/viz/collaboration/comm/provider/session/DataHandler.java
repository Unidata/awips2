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
package com.raytheon.uf.viz.collaboration.comm.provider.session;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IDisplayEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IInitData;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IVenueInvitationEvent;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 27, 2012            jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class DataHandler {

    @Subscribe
    public void handle(IInitData initdata) {
        System.out.println("DataHandler---------------------------------");
        System.out.println("   Handling IInitData " + initdata);
    }

    @Subscribe
    public void handle(IDisplayEvent event) {
        System.out.println("DataHandler---------------------------------");
        System.out.println("Handling IDisplayEvent " + event);
    }

    @Subscribe
    public void handle(IVenueInvitationEvent event) {
        System.out.println("DataHandler---------------------------------");
        System.out.println("   venue identifer  " + event.getRoomId());
        System.out.println("   inviter          " + event.getInviter());
        System.out.println("   sessionid        "
                + event.getInvite().getSessionId());
        // System.out.println("   body             " + event.getBody());
    }
}
