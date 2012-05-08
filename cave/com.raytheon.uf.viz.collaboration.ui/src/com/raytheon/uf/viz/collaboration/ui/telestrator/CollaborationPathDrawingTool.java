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
package com.raytheon.uf.viz.collaboration.ui.telestrator;

import com.raytheon.uf.viz.collaboration.comm.identity.user.SharedDisplayRole;
import com.raytheon.uf.viz.collaboration.data.SharedDisplaySessionMgr;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.drawing.tools.PathDrawingTool;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 3, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class CollaborationPathDrawingTool extends PathDrawingTool {

    private String session;

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.drawing.tools.PathDrawingTool#constructData()
     */
    @Override
    public AbstractResourceData constructData() {
        CollaborationPathDrawingResourceData data = new CollaborationPathDrawingResourceData();
        data.setSessionId(session);
        return data;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.drawing.AbstractDrawingTool#activateTool()
     */
    @Override
    protected void activateTool() {
        super.activateTool();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.drawing.tools.PathDrawingTool#getMouseHandler()
     */
    @Override
    public IInputHandler getMouseHandler() {
        if (theHandler == null) {
            theHandler = new CollaborationPathDrawingHandler();
        }
        return theHandler;
    }

    public class CollaborationPathDrawingHandler extends PathDrawingHandler {

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDown(int,
         * int, int)
         */
        @Override
        public boolean handleMouseDown(int anX, int aY, int button) {
            if (theDrawingLayer == null) {
                return false;
            }
            boolean allowDraw = ((CollaborationDrawingLayer) theDrawingLayer)
                    .isAllowDraw();
            boolean isSessionLeader = SharedDisplaySessionMgr
                    .getSessionContainer(session).getSession()
                    .hasRole(SharedDisplayRole.SESSION_LEADER);
            if (allowDraw && !isSessionLeader) {
                return false;
            }
            return super.handleMouseDown(anX, aY, button);
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseDownMove(int,
         * int, int)
         */
        @Override
        public boolean handleMouseDownMove(int x, int y, int button) {
            boolean allowDraw = ((CollaborationDrawingLayer) theDrawingLayer)
                    .isAllowDraw();
            boolean isSessionLeader = SharedDisplaySessionMgr
                    .getSessionContainer(session).getSession()
                    .hasRole(SharedDisplayRole.SESSION_LEADER);
            if (allowDraw && !isSessionLeader) {
                return false;
            }
            return super.handleMouseDownMove(x, y, button);
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.viz.ui.input.IInputHandler#handleMouseUp(int, int,
         * int)
         */
        @Override
        public boolean handleMouseUp(int anX, int aY, int button) {
            boolean allowDraw = ((CollaborationDrawingLayer) theDrawingLayer)
                    .isAllowDraw();
            boolean isSessionLeader = SharedDisplaySessionMgr
                    .getSessionContainer(session).getSession()
                    .hasRole(SharedDisplayRole.SESSION_LEADER);
            if (allowDraw && !isSessionLeader) {
                return false;
            }
            return super.handleMouseUp(anX, aY, button);
        }
    }

    /**
     * @param session
     *            the session to set
     */
    public void setSession(String session) {
        this.session = session;
    }

    /**
     * @return the session
     */
    public String getSession() {
        return session;
    }

}
