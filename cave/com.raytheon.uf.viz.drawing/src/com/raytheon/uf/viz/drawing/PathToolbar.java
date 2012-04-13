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
package com.raytheon.uf.viz.drawing;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.contexts.IContextActivation;
import org.eclipse.ui.contexts.IContextService;

import com.google.common.eventbus.AllowConcurrentEvents;
import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.icon.IconUtil;
import com.raytheon.uf.viz.drawing.DrawingLayer.LayerState;
import com.raytheon.uf.viz.drawing.actions.ClearDrawingAction;
import com.raytheon.uf.viz.drawing.actions.EraseObjectsAction;
import com.raytheon.uf.viz.drawing.actions.RedoAddAction;
import com.raytheon.uf.viz.drawing.actions.UndoAddAction;
import com.raytheon.uf.viz.drawing.events.DrawingEvent;
import com.raytheon.uf.viz.drawing.events.DrawingEventBus;
import com.raytheon.uf.viz.drawing.tools.PathDrawingTool;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.perspectives.VizPerspectiveListener;
import com.raytheon.viz.ui.tools.AbstractModalTool;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 28, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class PathToolbar extends CaveSWTDialog {
    protected static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PathToolbar.class);

    private static final String EDIT_TOOL_CATEGY = "com.raytheon.viz.ui.modalTool.nav";

    protected static AbstractModalTool lastTool = null;

    protected static PathToolbar pathToolbar;

    protected ToolBar toolbar;

    protected ToolItem drawItem;

    protected ToolItem eraserItem;

    protected ToolItem undoItem;

    protected ToolItem redoItem;

    protected ToolItem clearItem;

    private IContextActivation drawingContext;

    public static PathToolbar getToolbar() {
        if (pathToolbar == null) {
            pathToolbar = new PathToolbar(new Shell(Display.getCurrent()));
            DrawingEventBus.register(PathToolbar.getToolbar());
        }

        return pathToolbar;
    }

    /**
     * @param parentShell
     * @param swtStyle
     */
    protected PathToolbar(Shell parentShell) {
        super(parentShell, SWT.DIALOG_TRIM | CAVE.DO_NOT_BLOCK);
        setText("Drawing");
        lastTool = VizPerspectiveListener.getCurrentPerspectiveManager()
                .getToolManager().getSelectedModalTool(EDIT_TOOL_CATEGY);

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        shell.addShellListener(new ShellAdapter() {
            @Override
            public void shellClosed(ShellEvent e) {
                PathToolbar.getToolbar().getShell().removeShellListener(this);

                IContextService contextService = (IContextService) PlatformUI
                        .getWorkbench().getService(IContextService.class);
                contextService.deactivateContext(drawingContext);
                super.shellClosed(e);
            }
        });

        Composite comp = new Composite(shell, SWT.NONE);
        GridLayout layout = new GridLayout();
        comp.setLayout(layout);
        GridData data = new GridData(SWT.FILL, SWT.FILL, true, true);
        comp.setLayoutData(data);
        layout.marginHeight = 0;
        layout.marginWidth = 0;

        toolbar = new ToolBar(comp, SWT.FLAT);

        layout = new GridLayout();
        layout.marginHeight = 0;
        layout.marginWidth = 0;
        data = new GridData(SWT.FILL, SWT.FILL, true, true);
        toolbar.setLayout(layout);
        toolbar.setLayoutData(data);

        drawItem = new ToolItem(toolbar, SWT.CHECK);
        drawItem.setText("Draw");
        drawItem.setImage(IconUtil.getImageDescriptor(
                Activator.getDefault().getBundle(), "draw.gif").createImage());
        getDrawingResource().setState(LayerState.NONE);
        drawItem.setSelection(false);
        drawItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                DrawingLayer layer = getDrawingResource();
                if (layer != null) {
                    switch (layer.getState()) {
                    case DRAWING:
                        lastTool.activate();
                        layer.setState(LayerState.NONE);
                        break;
                    case ERASING:
                        layer.setState(LayerState.DRAWING);
                        eraserItem.setSelection(false);
                        break;
                    case NONE:
                        layer.setState(LayerState.DRAWING);
                        break;
                    }
                }
                updateToolbar();
            }
        });
        undoItem = new ToolItem(toolbar, SWT.FLAT);
        undoItem.setText("Undo");
        undoItem.setImage(IconUtil.getImageDescriptor(
                Activator.getDefault().getBundle(), "undo.gif").createImage());
        undoItem.setToolTipText("Ctrl+Z to Undo");
        undoItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                UndoAddAction action = new UndoAddAction();
                executeAction(action);
                updateToolbar();
            }
        });

        redoItem = new ToolItem(toolbar, SWT.FLAT);
        redoItem.setText("Redo");
        redoItem.setImage(IconUtil.getImageDescriptor(
                Activator.getDefault().getBundle(), "redo.gif").createImage());
        redoItem.setToolTipText("Ctrl+Y to Redo");
        redoItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                RedoAddAction action = new RedoAddAction();
                executeAction(action);
                updateToolbar();
            }
        });

        clearItem = new ToolItem(toolbar, SWT.FLAT);
        clearItem.setText("Clear");
        clearItem
                .setImage(IconUtil.getImageDescriptor(
                        Activator.getDefault().getBundle(), "remove.gif")
                        .createImage());
        clearItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                ClearDrawingAction action = new ClearDrawingAction();
                executeAction(action);
                updateToolbar();
            }
        });

        eraserItem = new ToolItem(toolbar, SWT.CHECK);
        eraserItem.setText("Eraser");
        eraserItem
                .setImage(IconUtil.getImageDescriptor(
                        Activator.getDefault().getBundle(), "eraser.png")
                        .createImage());
        eraserItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                DrawingLayer layer = getDrawingResource();

                // reactivate the last tool
                if (layer.getState() == LayerState.ERASING) {
                    lastTool.activate();
                } else {
                    lastTool.deactivate();
                }

                // uncheck the draw item
                drawItem.setSelection(false);

                // execute the EraseObjectsAction
                EraseObjectsAction action = new EraseObjectsAction();
                executeAction(action);
            }
        });
    }

    @Override
    protected void opened() {
        IContextService contextService = (IContextService) PlatformUI
                .getWorkbench().getService(IContextService.class);
        drawingContext = contextService
                .activateContext("com.raytheon.uf.viz.drawing.context");
        updateToolbar();
        super.opened();
    }

    private void executeAction(AbstractHandler action) {
        try {
            action.execute(null);
        } catch (ExecutionException e) {
            statusHandler.handle(Priority.ERROR, "Unable to execute action", e);
        }
    }

    @AllowConcurrentEvents
    @Subscribe
    public void handleMessage(DrawingEvent event) {
        updateToolbar();
    }

    public void updateToolbar() {
        if (this.isDisposed()) {
            return;
        }
        DrawingLayer layer = getDrawingResource();
        if (toolbar != null && !toolbar.isDisposed()) {
            if (layer.getDeletedShapes().isEmpty()
                    && layer.getWireframeShapes().isEmpty()) {
                undoItem.setEnabled(false);
                redoItem.setEnabled(false);
                clearItem.setEnabled(false);
            } else {
                clearItem.setEnabled(true);
                if (layer.getDeletedShapes().isEmpty()) {
                    redoItem.setEnabled(false);
                } else {
                    redoItem.setEnabled(true);
                }
                if (layer.getWireframeShapes().isEmpty()) {
                    undoItem.setEnabled(false);
                } else {
                    undoItem.setEnabled(true);
                }
            }
        }
    }

    protected DrawingLayer getDrawingResource() {
        AbstractEditor editor = EditorUtil
                .getActiveEditorAs(AbstractEditor.class);
        IDescriptor desc = editor.getActiveDisplayPane().getDescriptor();
        DrawingLayer layer = null;
        for (ResourcePair pair : desc.getResourceList()) {
            if (pair.getResource() instanceof DrawingLayer) {
                layer = (DrawingLayer) pair.getResource();
                break;
            }
        }
        if (layer == null) {
            PathDrawingTool tool = new PathDrawingTool();
            tool.activate();
            lastTool.deactivate();
            layer = getDrawingResource();
        }
        return layer;
    }
}
