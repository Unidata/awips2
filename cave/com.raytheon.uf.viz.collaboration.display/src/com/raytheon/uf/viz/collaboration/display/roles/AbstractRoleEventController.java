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
package com.raytheon.uf.viz.collaboration.display.roles;

import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.display.IRemoteDisplayContainer;
import com.raytheon.uf.viz.collaboration.display.data.SessionContainer;
import com.raytheon.uf.viz.collaboration.display.data.SharedDisplaySessionMgr;

/**
 * Abstract role event controller that shares fields and methods that are common
 * to other event controllers.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 26, 2012            njensen     Initial creation
 * Feb 12, 2014 2751       njensen     Renamed container to displayContainer
 * Mar 07, 2014 2848       bclement    moved event handler registration to constructor
 * Mar 18, 2014 2895       njensen     Fixed shutdown order
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public abstract class AbstractRoleEventController<T extends IRemoteDisplayContainer>
        implements IRoleEventController {

    protected ISharedDisplaySession session;

    protected T displayContainer;

    protected AbstractRoleEventController(ISharedDisplaySession session) {
        this.session = session;
        session.registerEventHandler(this);
    }

    @Override
    public void startup() {
        SessionContainer sc = SharedDisplaySessionMgr
                .getSessionContainer(session.getSessionId());
        displayContainer = createDisplayContainer();
        sc.setDisplayContainer(displayContainer);
    }

    @Override
    public void shutdown() {
        session.unregisterEventHandler(this);
        SessionContainer sc = SharedDisplaySessionMgr
                .getSessionContainer(session.getSessionId());
        if (displayContainer != null) {
            displayContainer.disposeContainer();
            displayContainer = null;
        }

        /*
         * We need to set the session's display container to null to properly
         * fire listeners on the container, but this needs to occur after the
         * displayContainer has been disposed. Otherwise we potentially leak
         * memory and get displayIds wrong in the future.
         */
        sc.setDisplayContainer(null);
    }

    protected abstract T createDisplayContainer();

}
