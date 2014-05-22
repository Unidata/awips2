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
package com.raytheon.uf.viz.collaboration.ui.data;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import com.raytheon.uf.viz.collaboration.comm.identity.ISession;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.provider.connection.CollaborationConnection;

/**
 * Retrieve session objects from contacts list
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 6, 2012            rferrel     Initial creation
 * Jan 28, 2014 2698       bclement    removed venue info
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class SessionGroupContainer {

    public List<Object> getObjects() {
        CollaborationConnection connection = CollaborationConnection
                .getConnection();
        if (connection == null) {
            return Collections.emptyList();
        }
        Collection<ISession> sessions = connection.getSessions();
        List<Object> result = new ArrayList<Object>();
        for (ISession session : sessions) {
            if (session instanceof IVenueSession) {
                result.add(session);
            }
        }
        return result;
    }

}
