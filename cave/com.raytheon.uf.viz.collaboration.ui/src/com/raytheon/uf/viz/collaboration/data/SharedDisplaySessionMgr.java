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
package com.raytheon.uf.viz.collaboration.data;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.identity.user.SharedDisplayRole;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.ui.role.DataProviderEventController;
import com.raytheon.uf.viz.collaboration.ui.role.IRoleEventController;
import com.raytheon.uf.viz.collaboration.ui.role.ParticipantEventController;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Tracks all of the active sessions that are SharedDisplaySessions. Provides
 * SessionContainers that contain the data related to those sessions.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 16, 2012            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class SharedDisplaySessionMgr {

    private static Map<String, SessionContainer> sharedDisplaySessionMap = new HashMap<String, SessionContainer>();

    public static SessionContainer getSessionContainer(String sessionId) {
        return sharedDisplaySessionMap.get(sessionId);
    }

    public static Set<String> getActiveSessionIds() {
        return sharedDisplaySessionMap.keySet();
    }

    protected static void joinSession(ISharedDisplaySession session,
            SharedDisplayRole initialRole, Map<UserId, RGB> colors) {
        SessionContainer container = new SessionContainer();
        container.setSessionId(session.getSessionId());
        container.setSession(session);

        IRoleEventController rec = null;
        switch (initialRole) {
        case DATA_PROVIDER:
            rec = new DataProviderEventController(session);
            List<AbstractEditor> editorList = new ArrayList<AbstractEditor>();
            // TODO better way to determine which editor to start sharing?
            // or better yet, maybe we should add this elsewhere after it has
            // already been initialized with the correct target/resources
            AbstractEditor sharedEditor = (AbstractEditor) VizWorkbenchManager
                    .getInstance().getActiveEditor();
            editorList.add(sharedEditor);
            container.setSharedEditors(editorList);
            break;
        case PARTICIPANT:
            rec = new ParticipantEventController(session);
            // don't need to set the CollaborationEditor, as it won't be created
            // until we receive the initial data
            break;
        default:
            throw new IllegalArgumentException(
                    "ParticipantRole must be DataProvider or Participant for initialization");
        }
        container.setRoleEventController(rec);
        if (colors != null) {
            container.getColorManager().setColors(colors);
        }
        sharedDisplaySessionMap.put(session.getSessionId(), container);

        rec.startup();
    }

}
