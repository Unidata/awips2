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
import java.util.Collections;
import java.util.List;

import org.jivesoftware.smack.RosterGroup;

import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.user.LocalGroups.LocalGroup;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 23, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class CollaborationGroupContainer {

    private SessionGroupContainer sessionGroup = new SessionGroupContainer();

    public CollaborationGroupContainer() {
    }

    public List<Object> getObjects() {
        CollaborationConnection connection = CollaborationConnection
                .getConnection();
        if (connection == null) {
            return Collections.emptyList();
        }
        List<Object> result = new ArrayList<Object>();
        result.add(connection.getUser());
        result.add(sessionGroup);
        for (RosterGroup obj : connection.getRosterManager().getRoster()
                .getGroups()) {
            result.add(obj);
        }
        for (LocalGroup group : connection.getContactsManager()
                .getLocalGroups()) {
            result.add(group);
        }
        return result;
    }

    public SessionGroupContainer getSessionGroup() {
        return sessionGroup;
    }

}
