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

package com.raytheon.viz.ui.tools;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;
import org.eclipse.ui.commands.ICommandService;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.perspectives.AbstractVizPerspectiveManager;
import com.raytheon.viz.ui.perspectives.VizPerspectiveListener;

/**
 * Describes a tool that is a "selected" operation (e.g. pan/zoom) where only
 * one operation may be selected and stays selected until another modal tool is
 * chosen
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
public abstract class AbstractModalTool extends AbstractTool {

    private AbstractModalTool restore = null;

    protected ExecutionEvent event = null;

    /**
     * Constructor
     * 
     */
    public AbstractModalTool() {
        super();
        // Default modal tool command id
        this.commandId = "com.raytheon.viz.ui.modalTool";
    }

    /**
     * Check if the modal tool is selected
     * 
     * @return
     */
    public boolean isSelected() {
        return isEnabled;
    }

    /**
     * Get the active display pane's descriptor
     * 
     * @return
     */
    protected IDescriptor getActiveDescriptor() {
        if (editor != null) {
            return editor.getActiveDisplayPane().getDescriptor();
        }
        return null;
    }

    public IDisplayPaneContainer getCurrentEditor() {
        return editor;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.tools.AbstractTool#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public final Object execute(ExecutionEvent arg0) throws ExecutionException {
        this.event = arg0;
        super.execute(arg0);
        isEnabled = !isEnabled;

        AbstractVizPerspectiveManager mgr = VizPerspectiveListener
                .getCurrentPerspectiveManager();

        boolean selected = isSelected();
        boolean attemptRestore = false;
        if (mgr != null) {
            Object trigger = arg0.getTrigger();
            if (trigger instanceof Event) {
                Event event = (Event) trigger;
                if (event.keyCode != SWT.NONE) {
                    attemptRestore = true;
                }
            }

            if (!attemptRestore) {
                restore = null;
            }

            if (selected) {
                if (attemptRestore) {
                    restore = mgr.getToolManager().getSelectedModalTool(
                            categoryId);
                }
                mgr.getToolManager().selectModalTool(this);
            } else {
                mgr.getToolManager().deselectModalTool(this);
            }
        }

        if (selected) {
            activate();
        } else {
            deactivate();
            if (attemptRestore && restore != null) {
                restore.execute(new ExecutionEvent(null, arg0.getParameters(),
                        null, arg0.getApplicationContext()));
            }
        }

        return null;
    }

    public void activate() {
        this.setEnabled(true);
        if (editor != null) {
            activateTool();
        }
    }

    public void deactivate() {
        this.setEnabled(false);
        if (editor != null) {
            deactivateTool();
            editor = null;
        }
    }

    @Override
    public void setEnabled(boolean isEnabled) {
        super.setEnabled(isEnabled);

        ICommandService service = (ICommandService) VizWorkbenchManager
                .getInstance().getCurrentWindow()
                .getService(ICommandService.class);

        if (service != null) {
            Command[] commands = service.getDefinedCommands();

            for (Command c : commands) {
                try {
                    if (!c.getCategory().getId().equals(categoryId)
                            || !c.isHandled() || !c.isDefined()
                            || !c.isEnabled())
                        continue;
                    service.refreshElements(c.getId(), null);
                } catch (Exception e) {
                    // who cares
                }
            }
        }
    }

    protected abstract void deactivateTool();

    protected abstract void activateTool();
}
