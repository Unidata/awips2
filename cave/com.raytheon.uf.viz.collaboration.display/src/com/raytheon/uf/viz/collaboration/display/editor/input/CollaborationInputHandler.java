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
package com.raytheon.uf.viz.collaboration.display.editor.input;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.swt.widgets.Event;

import com.raytheon.uf.viz.core.rsc.IInputHandler;

/**
 * CollaborationInputHandler that holds other handlers that should only be used
 * when part of a collaboration session. Essentially it's a layer of handlers
 * specific for collaboration.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 4, 2012            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class CollaborationInputHandler implements IInputHandler {

    private List<IInputHandler> handlers = Collections
            .synchronizedList(new LinkedList<IInputHandler>());

    public void registerInputHandler(IInputHandler handler) {
        handlers.add(handler);
    }

    public void unregisterInputHandler(IInputHandler handler) {
        handlers.remove(handler);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.IInputHandler#handleMouseDown(int, int,
     * int)
     */
    @Override
    public boolean handleMouseDown(int x, int y, int mouseButton) {
        for (IInputHandler handler : handlers) {
            if (handler.handleMouseDown(x, y, mouseButton)) {
                return true;
            }
        }
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.IInputHandler#handleMouseDownMove(int,
     * int, int)
     */
    @Override
    public boolean handleMouseDownMove(int x, int y, int mouseButton) {
        for (IInputHandler handler : handlers) {
            if (handler.handleMouseDownMove(x, y, mouseButton)) {
                return true;
            }
        }
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.IInputHandler#handleMouseUp(int, int,
     * int)
     */
    @Override
    public boolean handleMouseUp(int x, int y, int mouseButton) {
        for (IInputHandler handler : handlers) {
            if (handler.handleMouseUp(x, y, mouseButton)) {
                return true;
            }
        }
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.IInputHandler#handleMouseHover(int,
     * int)
     */
    @Override
    public boolean handleMouseHover(int x, int y) {
        for (IInputHandler handler : handlers) {
            if (handler.handleMouseHover(x, y)) {
                return true;
            }
        }
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.IInputHandler#handleMouseMove(int, int)
     */
    @Override
    public boolean handleMouseMove(int x, int y) {
        for (IInputHandler handler : handlers) {
            if (handler.handleMouseMove(x, y)) {
                return true;
            }
        }
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.IInputHandler#handleDoubleClick(int,
     * int, int)
     */
    @Override
    public boolean handleDoubleClick(int x, int y, int button) {
        for (IInputHandler handler : handlers) {
            if (handler.handleDoubleClick(x, y, button)) {
                return true;
            }
        }
        return true;
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
        for (IInputHandler handler : handlers) {
            if (handler.handleMouseWheel(event, x, y)) {
                return true;
            }
        }
        return true;
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
        for (IInputHandler handler : handlers) {
            if (handler.handleMouseExit(event)) {
                return true;
            }
        }
        return true;
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
        for (IInputHandler handler : handlers) {
            if (handler.handleMouseEnter(event)) {
                return true;
            }
        }
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.IInputHandler#handleKeyDown(int)
     */
    @Override
    public boolean handleKeyDown(int keyCode) {
        for (IInputHandler handler : handlers) {
            if (handler.handleKeyDown(keyCode)) {
                return true;
            }
        }
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.IInputHandler#handleKeyUp(int)
     */
    @Override
    public boolean handleKeyUp(int keyCode) {
        for (IInputHandler handler : handlers) {
            if (handler.handleKeyUp(keyCode)) {
                return true;
            }
        }
        return true;
    }

    /**
     * Get the list of registered handlers
     * 
     * @return
     */
    public List<IInputHandler> getRegisteredHandlers() {
        return handlers;
    }

}
