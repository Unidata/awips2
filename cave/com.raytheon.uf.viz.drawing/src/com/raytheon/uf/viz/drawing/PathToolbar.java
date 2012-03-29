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

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.drawing.actions.ClearDrawingAction;
import com.raytheon.uf.viz.drawing.actions.EraseObjectsAction;
import com.raytheon.uf.viz.drawing.actions.RedoAddAction;
import com.raytheon.uf.viz.drawing.actions.UndoAddAction;
import com.raytheon.uf.viz.drawing.tools.PathDrawingTool;
import com.raytheon.uf.viz.drawing.tools.ToolsUtils;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.editor.ISelectedPanesChangedListener;
import com.raytheon.viz.ui.editor.VizMultiPaneEditor;

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

public class PathToolbar extends CaveSWTDialog implements
        ISelectedPanesChangedListener {

    private static PathToolbar toolbar;

    private Map<AbstractEditor, ResourcePair> layers;

    private ToolItem drawItem;

    private ToolItem eraserItem;

    public static PathToolbar getToolbar() {
        if (toolbar == null) {
            toolbar = new PathToolbar(Display.getCurrent().getActiveShell());
        }
        return toolbar;
    }

    /**
     * @param parentShell
     * @param swtStyle
     */
    protected PathToolbar(Shell parentShell) {
        super(parentShell, SWT.DIALOG_TRIM | CAVE.DO_NOT_BLOCK);
        layers = new HashMap<AbstractEditor, ResourcePair>();
        setText("Drawing");
    }

    /**
     * @param args
     */
    public static void main(String[] args) {
        PathToolbar bar = new PathToolbar(new Shell());
        bar.open();
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
        Composite comp = new Composite(shell, SWT.NONE);
        GridLayout layout = new GridLayout();
        comp.setLayout(layout);
        GridData data = new GridData(SWT.FILL, SWT.FILL, true, true);
        comp.setLayoutData(data);
        layout.marginHeight = 0;
        layout.marginWidth = 0;

        ToolBar toolbar = new ToolBar(comp, SWT.FLAT);

        layout = new GridLayout();
        layout.marginHeight = 0;
        layout.marginWidth = 0;
        data = new GridData(SWT.FILL, SWT.FILL, true, true);
        toolbar.setLayout(layout);
        toolbar.setLayoutData(data);

        drawItem = new ToolItem(toolbar, SWT.NONE);
        drawItem.setText("Draw");
        drawItem.setImage(ToolsUtils.getImageDescriptor("draw.gif")
                .createImage());

        drawItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                AbstractEditor editor = EditorUtil
                        .getActiveEditorAs(AbstractEditor.class);
                IDescriptor desc = editor.getActiveDisplayPane()
                        .getDescriptor();
                if (layers.containsKey(editor)) {
                    if (((DrawingLayer) layers.get(editor).getResource()).erase) {
                        ((DrawingLayer) layers.get(editor).getResource())
                                .setErase(false);
                        eraserItem.setSelection(false);
                    }
                }
                PathDrawingTool tool = new PathDrawingTool();
                tool.activate();
                // ((VizMultiPaneEditor) editor)
                // .addSelectedPaneChangedListener(PathToolbar
                // .getToolbar());
                for (ResourcePair pair : desc.getResourceList()) {
                    if (pair.getResource() instanceof DrawingLayer) {
                        layers.put(editor, pair);
                        // drawItem.setEnabled(false);
                    }
                }
            }
        });

        ToolItem undoItem = new ToolItem(toolbar, SWT.FLAT);
        undoItem.setText("Undo");
        undoItem.setImage(ToolsUtils.getImageDescriptor("undo.gif")
                .createImage());
        undoItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                UndoAddAction action = new UndoAddAction();
                AbstractEditor editor = EditorUtil
                        .getActiveEditorAs(AbstractEditor.class);
                if (layers.get(editor) != null) {
                    executeAction(action);
                }
            }
        });

        ToolItem redoItem = new ToolItem(toolbar, SWT.FLAT);
        redoItem.setText("Redo");
        redoItem.setImage(ToolsUtils.getImageDescriptor("redo.gif")
                .createImage());
        redoItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                RedoAddAction action = new RedoAddAction();
                AbstractEditor editor = EditorUtil
                        .getActiveEditorAs(AbstractEditor.class);
                if (layers.get(editor) != null) {
                    executeAction(action);
                }
            }
        });

        ToolItem clearItem = new ToolItem(toolbar, SWT.FLAT);
        clearItem.setText("Clear");
        clearItem.setImage(ToolsUtils.getImageDescriptor("remove.gif")
                .createImage());
        clearItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                ClearDrawingAction action = new ClearDrawingAction();
                AbstractEditor editor = EditorUtil
                        .getActiveEditorAs(AbstractEditor.class);
                if (layers.get(editor) != null) {
                    executeAction(action);
                }
            }
        });

        eraserItem = new ToolItem(toolbar, SWT.CHECK);
        eraserItem.setText("Eraser");
        eraserItem.setImage(ToolsUtils.getImageDescriptor("eraser.png")
                .createImage());
        eraserItem.setEnabled(false);
        eraserItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                EraseObjectsAction action = new EraseObjectsAction();
                AbstractEditor editor = EditorUtil
                        .getActiveEditorAs(AbstractEditor.class);
                if (layers.get(editor) != null) {
                    executeAction(action);
                }
            }
        });
    }

    private void executeAction(AbstractHandler action) {
        try {
            action.execute(null);
        } catch (ExecutionException e) {
            e.printStackTrace();
        }
    }

    /**
     * 
     */
    private void dispose() {
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.editor.ISelectedPanesChangedListener#selectedPanesChanged
     * (java.lang.String, com.raytheon.uf.viz.core.IDisplayPane[])
     */
    @Override
    public void selectedPanesChanged(String id, IDisplayPane[] pane) {
        AbstractEditor editor = EditorUtil
                .getActiveEditorAs(AbstractEditor.class);
        IDescriptor desc = editor.getActiveDisplayPane().getDescriptor();
        ((VizMultiPaneEditor) editor)
                .addSelectedPaneChangedListener(PathToolbar.getToolbar());
        boolean hasLayer = false;
        for (ResourcePair pair : desc.getResourceList()) {
            if (pair.getResource() instanceof DrawingLayer) {
                hasLayer = true;
                break;
            }
        }
        // drawItem.setEnabled(hasLayer);
    }

}
