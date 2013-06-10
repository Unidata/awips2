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
package com.raytheon.uf.viz.core.rsc.tools.action;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.tools.GenericToolsResourceData;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.IMultiPaneEditor;
import com.raytheon.viz.ui.tools.AbstractTool;

/**
 * A class which represents an action for loading a tool which is a single
 * resource(layer) onto a mapEditor. It could extend AbstractTool, except some
 * subclasses like having the mapEditor and Java doesn't do mixins.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 21, 2013       1638 mschenke    Renamed to better represent purpose
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 * @param <T>
 */
public abstract class AbstractGenericToolAction<T extends AbstractVizResource<AbstractResourceData, MapDescriptor>>
        extends AbstractTool {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractGenericToolAction.class);

    protected GenericToolsResourceData<T> data;

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.tools.AbstractTool#runTool()
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        super.execute(arg0);
        try {
            for (IDisplayPane pane : getSelectedPanes()) {
                IDescriptor desc = pane.getDescriptor();
                T t = getResource(new LoadProperties(), desc);
                if (t != null) {
                    desc.getResourceList().add(t);
                    // This is for a single resource that is referenced in
                    // a four panel pane
                    desc.getTimeMatcher().redoTimeMatching(desc);
                }
            }

            // Assume getSelectedPanes() has already tried to locate the editor.
            // Only do something when editor is found.
            if (editor != null) {
                for (IDisplayPane pane : editor.getDisplayPanes()) {
                    pane.getDescriptor().getTimeMatcher()
                            .redoTimeMatching(pane.getDescriptor());
                }

                editor.refresh();
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Failed to initalized map tool", e);
        }
        return null;
    }

    protected T getResource(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        if (data == null) {
            data = getResourceData();
        }
        return data.construct(loadProperties, descriptor);
    }

    protected abstract GenericToolsResourceData<T> getResourceData();

    protected IDisplayPane[] getSelectedPanes() {
        if (this.editor == null) {
            this.editor = EditorUtil.getActiveVizContainer();
        }

        if (this.editor == null) {
            // User does not have a display editor showing.
            return new IDisplayPane[0];
        }

        IDisplayPane[] displayPanes = editor.getDisplayPanes();

        if (editor instanceof IMultiPaneEditor) {
            IDisplayPane selected = ((IMultiPaneEditor) editor)
                    .getSelectedPane(IMultiPaneEditor.LOAD_ACTION);
            if (selected != null) {
                displayPanes = new IDisplayPane[] { selected };
            }
        }
        return displayPanes;
    }
}
