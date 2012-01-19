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

import java.util.Iterator;
import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.commands.IElementUpdater;
import org.eclipse.ui.handlers.HandlerUtil;
import org.eclipse.ui.menus.UIElement;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.VizWorkbenchManager;

/**
 * Defines a generic tool.
 * 
 * All tools should extend this, or one of the subclasses
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 7/1/06                   chammack    Initial Creation.
 * 6/29/07                  chammack    Change to use Eclipse 3.3 API
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public abstract class AbstractTool extends AbstractHandler implements
        IElementUpdater {

    protected IDisplayPaneContainer editor;

    protected boolean isSurrogate;

    protected boolean isEnabled;

    protected String commandId;

    protected String categoryId;

    public void setEditor(IDisplayPaneContainer editor) {
        this.editor = editor;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        IDisplayPaneContainer oldEditor = this.editor;

        if (event.getCommand() != null) {
            this.commandId = event.getCommand().getId();
            try {
                if (event.getCommand().getCategory() != null) {
                    this.categoryId = event.getCommand().getCategory().getId();
                }
            } catch (Exception e) {
                // No category for the command
            }
        }

        IEditorPart eventEditor = HandlerUtil.getActiveEditor(event);
        if (eventEditor != null && eventEditor instanceof IDisplayPaneContainer) {
            this.editor = (IDisplayPaneContainer) eventEditor;
        } else {
            IWorkbenchWindow window = HandlerUtil
                    .getActiveWorkbenchWindow(event);
            if (window == null) {
                window = VizWorkbenchManager.getInstance().getCurrentWindow();
            }
            this.editor = EditorUtil.getActiveVizContainer(window);
        }

        if (oldEditor != this.editor) {
            // editor has changed
            this.setEditor(this.editor);
        }

        return null;
    }

    /**
     * Refresh the screen and layer list
     * 
     */
    protected void refresh() {
        VizApp.runAsync(new Runnable() {

            public void run() {
                editor.refresh();
            }

        });

    }

    public void setEnabled(boolean isEnabled) {

        this.isEnabled = isEnabled;

        if (this.editor == null) {
            this.editor = EditorUtil.getActiveVizContainer();
            if (this.editor == null)
                return;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.commands.IElementUpdater#updateElement(org.eclipse.ui.
     * menus.UIElement, java.util.Map)
     */
    @SuppressWarnings("unchecked")
    public void updateElement(UIElement element, Map parameters) {
        element.setChecked(this.isEnabled);
    }

    public AbstractVizResource<?, ?> containsResource(
            Class<? extends AbstractVizResource<?, ?>> rsc) {
        IDisplayPaneContainer currContainer = EditorUtil
                .getActiveVizContainer();
        if (currContainer != null) {
            if (editor != currContainer) {
                editor = currContainer;
            }
        } else {
            return null;
        }

        Iterator<ResourcePair> iter = editor.getActiveDisplayPane()
                .getDescriptor().getResourceList().iterator();

        while (iter.hasNext()) {
            ResourcePair rPair = iter.next();

            if (rPair.getResource().getClass().equals(rsc)) {
                return rPair.getResource();
            }
        }
        return null;
    }
}
