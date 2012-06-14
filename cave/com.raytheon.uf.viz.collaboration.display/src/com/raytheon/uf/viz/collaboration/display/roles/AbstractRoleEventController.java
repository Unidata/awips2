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
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public abstract class AbstractRoleEventController<T extends IRemoteDisplayContainer>
        implements IRoleEventController {

    protected ISharedDisplaySession session;

    protected T container;

    protected AbstractRoleEventController(ISharedDisplaySession session) {
        this.session = session;
    }

    @Override
    public void startup() {
        session.registerEventHandler(this);
        SessionContainer sc = SharedDisplaySessionMgr
                .getSessionContainer(session.getSessionId());
        container = createDisplayContainer();
        sc.setDisplayContainer(container);
    }

    @Override
    public void shutdown() {
        session.unregisterEventHandler(this);
        container.disposeContainer();
    }

    protected abstract T createDisplayContainer();

}
