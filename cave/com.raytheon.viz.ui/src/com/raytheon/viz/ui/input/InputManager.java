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

package com.raytheon.viz.ui.input;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.IInputHandler.InputPriority;

/**
 * Manages input events for the display
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 7/1/06                   chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class InputManager implements Listener {

    private class PrioritizedHandler implements Comparable<PrioritizedHandler> {
        InputPriority priority;

        IInputHandler handler;

        public PrioritizedHandler(IInputHandler handler, InputPriority priority) {
            this.handler = handler;
            this.priority = priority;
        }

        @Override
        public int compareTo(PrioritizedHandler o) {
            return priority.value.compareTo(o.priority.value);
        }

        @Override
        public boolean equals(Object o) {
            if (o instanceof IInputHandler) {
                return o == handler;
            }
            if (o instanceof PrioritizedHandler) {
                return ((PrioritizedHandler) o).handler == this.handler;
            }
            return false;
        }
    }

    private boolean isMouseDown = false;

    private boolean menuDetected = false;

    private final List<PrioritizedHandler> handlers;

    private final List<PrioritizedHandler> perspectiveHandlers;

    private int lastMouseButton;

    private final IDisplayPaneContainer container;

    /**
     * Constructor
     * 
     * @param anEditor
     */
    public InputManager(IDisplayPaneContainer container) {
        this.handlers = new ArrayList<PrioritizedHandler>();
        this.perspectiveHandlers = new ArrayList<PrioritizedHandler>();
        this.container = container;
    }

    /**
     * Get all input handlers registered at a particular priority
     * 
     * @param priority
     * @return
     */
    public IInputHandler[] getHandlersForPriority(InputPriority priority) {
        List<IInputHandler> handlers = new ArrayList<IInputHandler>();
        for (PrioritizedHandler handler : this.handlers) {
            if (handler.priority == priority) {
                handlers.add(handler.handler);
            }
        }
        return handlers.toArray(new IInputHandler[handlers.size()]);
    }

    /**
     * Handle Mouse Click events
     */
    public void handleEvent(Event event) {

        if ((container == null)
                || (container.getActiveDisplayPane() == null)
                || (event.display != container.getActiveDisplayPane()
                        .getDisplay())) {
            return;
        }

        switch (event.type) {
        case SWT.MouseDown:
            handleMouseDown(event);
            break;
        case SWT.MouseUp:
            handleMouseUp(event);
            break;
        case SWT.MouseWheel:
            handleMouseWheel(event);
            break;
        case SWT.MouseMove:
            handleMouseMove(event);
            break;
        case SWT.MouseHover:
            handleMouseHover(event);
            break;
        case SWT.MouseDoubleClick:
            handleMouseDoubleClick(event);
            break;
        case SWT.KeyDown:
            handleKeyDown(event);
            break;
        case SWT.KeyUp:
            handleKeyUp(event);
            break;
        case SWT.MenuDetect:
            isMouseDown = false;
            menuDetected = true;
            break;
        case SWT.MouseExit: {
            handleMouseExit(event);
            break;
        }
        case SWT.MouseEnter: {
            handleMouseEnter(event);
            break;
        }
        default:
            break;
        }
    }

    private void handleMouseEnter(Event event) {
        for (int i = handlers.size() - 1; i >= 0; i--) {
            // Let all handlers know of event...
            handlers.get(i).handler.handleMouseEnter(event);
        }
    }

    private void handleMouseExit(Event event) {
        for (int i = handlers.size() - 1; i >= 0; i--) {
            // Let all handlers know of event...
            handlers.get(i).handler.handleMouseExit(event);
        }
    }

    private void handleMouseDoubleClick(Event event) {
        isMouseDown = false;
        for (int i = handlers.size() - 1; i >= 0; i--) {
            if (handlers.get(i).handler.handleDoubleClick(event.x, event.y, event.button))
                return;
        }
    }

    private void handleMouseUp(Event event) {
        isMouseDown = false;

        for (int i = handlers.size() - 1; i >= 0; i--) {
            if (handlers.get(i).handler.handleMouseUp(event.x, event.y,
                    lastMouseButton))
                return;
        }

    }

    private void handleMouseDown(Event event) {
        if (menuDetected && (event.button != 3)) {
            menuDetected = false;
            return;
        }

        if (event.type == SWT.MouseDoubleClick)
            return;

        lastMouseButton = event.button;

        if (!menuDetected)
            isMouseDown = true;
        else
            menuDetected = false;

        for (int i = handlers.size() - 1; i >= 0; i--) {
            if (handlers.get(i).handler.handleMouseDown(event.x, event.y,
                    event.button)) {
                return;
            }
        }

    }

    private void handleMouseWheel(Event event) {
        for (int i = handlers.size() - 1; i >= 0; i--) {
            if (handlers.get(i).handler.handleMouseWheel(event, event.x,
                    event.y))
                return;
        }
    }

    private void handleMouseHover(final Event e) {
        for (int i = handlers.size() - 1; i >= 0; i--) {
            if (handlers.get(i).handler.handleMouseHover(e.x, e.y))
                return;
        }
    }

    private void handleKeyDown(final Event e) {
        for (int i = handlers.size() - 1; i >= 0; i--) {
            if (handlers.get(i).handler.handleKeyDown(e.keyCode))
                return;
        }
    }

    private void handleKeyUp(final Event e) {
        for (int i = handlers.size() - 1; i >= 0; i--) {
            if (handlers.get(i).handler.handleKeyUp(e.keyCode))
                return;
        }
    }

    /**
     * Mouse Move event
     */
    private void handleMouseMove(Event e) {

        if (isMouseDown) {
            for (int i = handlers.size() - 1; i >= 0; i--) {
                if (handlers.get(i).handler.handleMouseDownMove(e.x, e.y,
                        lastMouseButton))
                    return;
            }
        } else {
            for (int i = 0; i < handlers.size(); i++) {
                if (handlers.get(i).handler.handleMouseMove(e.x, e.y))
                    return;
            }
        }
    }

    /**
     * Register a mouse handler, lowest priority are handled last
     * 
     * @param aHandler
     */
    public void registerMouseHandler(IInputHandler aHandler,
            InputPriority priority) {
        PrioritizedHandler pHandler = new PrioritizedHandler(aHandler, priority);
        synchronized (this) {
            if (!handlers.contains(pHandler)) {
                handlers.add(pHandler);
            }
            Collections.sort(handlers);
        }
    }

    /**
     * Register a mouse handler
     * 
     * @param aHandler
     */
    public void registerMouseHandler(IInputHandler aHandler) {
        registerMouseHandler(aHandler, InputPriority.RESOURCE);
    }

    /**
     * Unregister a mouse handler
     * 
     * @param aHandler
     */
    public void unregisterMouseHandler(IInputHandler aHandler) {
        PrioritizedHandler pHandler = new PrioritizedHandler(aHandler,
                InputPriority.RESOURCE);
        synchronized (this) {
            handlers.remove(pHandler);
        }
    }

    /**
     * Notify the manager that the perspective has changed and the
     * perspective-specific handlers should be changed out
     * 
     * @param newPerspectiveSpecificHandlers
     *            a new set of perspective specific handlers
     */
    public void firePerspectiveChanged(
            IInputHandler[] newPerspectiveSpecificHandlers) {
        synchronized (this) {
            List<IInputHandler> newPerspectiveHandlerList = Arrays
                    .asList(newPerspectiveSpecificHandlers);
            // The perspective has changed

            // First, remove from the handler list the old perspective specific
            // handlers
            this.handlers.removeAll(this.perspectiveHandlers);

            // Then, clear the old perspective handlers
            this.perspectiveHandlers.clear();

            for (IInputHandler handler : newPerspectiveHandlerList) {
                PrioritizedHandler pHandler = new PrioritizedHandler(handler,
                        InputPriority.fromValue(0));
                // And add the new perspective handlers
                this.perspectiveHandlers.add(pHandler);

                // Then add to the master handler list
                this.handlers.add(pHandler);
            }

        }
        Collections.sort(handlers);

    }

}
