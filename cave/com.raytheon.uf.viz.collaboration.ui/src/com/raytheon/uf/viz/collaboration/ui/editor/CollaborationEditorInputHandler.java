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
package com.raytheon.uf.viz.collaboration.ui.editor;

import org.eclipse.swt.widgets.Event;

import com.raytheon.uf.viz.core.rsc.IInputHandler;

/**
 * An input handler that disables mouse and key input if it's not the Session
 * Leader. If it is the Session Leader, it sends out events related to the
 * input.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 20, 2012            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class CollaborationEditorInputHandler implements IInputHandler {

    protected boolean isSessionLeader() {
        // TODO this should query the session somehow for the current role
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.IInputHandler#handleMouseDown(int, int,
     * int)
     */
    @Override
    public boolean handleMouseDown(int x, int y, int mouseButton) {
        return !isSessionLeader();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.IInputHandler#handleMouseDownMove(int,
     * int, int)
     */
    @Override
    public boolean handleMouseDownMove(int x, int y, int mouseButton) {
        return !isSessionLeader();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.IInputHandler#handleMouseUp(int, int,
     * int)
     */
    @Override
    public boolean handleMouseUp(int x, int y, int mouseButton) {
        return !isSessionLeader();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.IInputHandler#handleMouseHover(int,
     * int)
     */
    @Override
    public boolean handleMouseHover(int x, int y) {
        return !isSessionLeader();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.IInputHandler#handleMouseMove(int, int)
     */
    @Override
    public boolean handleMouseMove(int x, int y) {
        return !isSessionLeader();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.IInputHandler#handleDoubleClick(int,
     * int, int)
     */
    @Override
    public boolean handleDoubleClick(int x, int y, int button) {
        return !isSessionLeader();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.IInputHandler#handleMouseWheel(org.eclipse
     * .swt.widgets.Event, int, int)
     */
    @Override
    public boolean handleMouseWheel(Event event, int x, int y) {
        return !isSessionLeader();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.IInputHandler#handleMouseExit(org.eclipse
     * .swt.widgets.Event)
     */
    @Override
    public boolean handleMouseExit(Event event) {
        return !isSessionLeader();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.IInputHandler#handleMouseEnter(org.eclipse
     * .swt.widgets.Event)
     */
    @Override
    public boolean handleMouseEnter(Event event) {
        return !isSessionLeader();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.IInputHandler#handleKeyDown(int)
     */
    @Override
    public boolean handleKeyDown(int keyCode) {
        return !isSessionLeader();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.IInputHandler#handleKeyUp(int)
     */
    @Override
    public boolean handleKeyUp(int keyCode) {
        return !isSessionLeader();
    }

}
