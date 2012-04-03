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
package com.raytheon.uf.viz.collaboration.ui.role;

import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.ui.telestrator.CollaborationPathToolbar;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.drawing.PathToolbar;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 2, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class SessionLeaderEventController extends AbstractRoleEventController {

    /**
     * @param session
     */
    public SessionLeaderEventController(ISharedDisplaySession session) {
        super(session);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.role.AbstractRoleEventController
     * #startup()
     */
    @Override
    public void startup() {
        super.startup();
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                PathToolbar toolbar = CollaborationPathToolbar.getToolbar();
                toolbar.open();
            }
        });
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.role.AbstractRoleEventController
     * #shutdown()
     */
    @Override
    public void shutdown() {
        super.shutdown();
        PathToolbar.getToolbar().close();
    }

}
