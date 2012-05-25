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

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;

import com.raytheon.uf.viz.core.icon.IconUtil;
import com.raytheon.uf.viz.drawing.DrawingToolLayer.DrawMode;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Toolbar dialog for DrawingToolLayer
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 23, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DrawingToolbar extends CaveSWTDialog {

    private DrawingToolLayer layer;

    protected ToolItem drawItem;

    protected ToolItem eraserItem;

    protected ToolItem undoItem;

    protected ToolItem redoItem;

    protected ToolItem clearItem;

    /**
     * @param parentShell
     */
    public DrawingToolbar(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.PERSPECTIVE_INDEPENDENT
                | CAVE.DO_NOT_BLOCK);
        setText("Drawing Tool");
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

        addToolsToToolBar(toolbar);
        nullLayer();
    }

    /**
     * @param toolbar
     */
    protected void addToolsToToolBar(ToolBar toolbar) {
        // Draw button
        drawItem = new ToolItem(toolbar, SWT.CHECK);
        drawItem.setText("Draw");
        drawItem.setImage(IconUtil.getImageDescriptor(
                Activator.getDefault().getBundle(), "draw.gif").createImage());
        drawItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (drawItem.getSelection()) {
                    layer.setDrawMode(DrawMode.DRAW);
                } else if (eraserItem.getSelection() == false) {
                    layer.setDrawMode(DrawMode.NONE);
                }
                updateItemState();
            }
        });

        // Undo button
        undoItem = new ToolItem(toolbar, SWT.FLAT);
        undoItem.setText("Undo");
        undoItem.setImage(IconUtil.getImageDescriptor(
                Activator.getDefault().getBundle(), "undo.gif").createImage());
        undoItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                undo();
            }
        });

        // Redo button
        redoItem = new ToolItem(toolbar, SWT.FLAT);
        redoItem.setText("Redo");
        redoItem.setImage(IconUtil.getImageDescriptor(
                Activator.getDefault().getBundle(), "redo.gif").createImage());
        redoItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                redo();
            }
        });

        // Clear button
        clearItem = new ToolItem(toolbar, SWT.FLAT);
        clearItem.setText("Clear");
        clearItem
                .setImage(IconUtil.getImageDescriptor(
                        Activator.getDefault().getBundle(), "remove.gif")
                        .createImage());
        clearItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                clear();
            }
        });

        // Erase button
        eraserItem = new ToolItem(toolbar, SWT.CHECK);
        eraserItem.setText("Eraser");
        eraserItem
                .setImage(IconUtil.getImageDescriptor(
                        Activator.getDefault().getBundle(), "eraser.png")
                        .createImage());
        eraserItem.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (eraserItem.getSelection()) {
                    layer.setDrawMode(DrawMode.ERASE);
                } else if (drawItem.getSelection() == false) {
                    layer.setDrawMode(DrawMode.NONE);
                }
                updateItemState();
            }
        });
    }

    /**
     * Method to setup tool items when no layer is active
     */
    protected void nullLayer() {
        drawItem.setSelection(false);
        drawItem.setEnabled(false);
        undoItem.setEnabled(false);
        redoItem.setEnabled(false);
        clearItem.setEnabled(false);
        eraserItem.setEnabled(false);
        eraserItem.setSelection(false);
    }

    /**
     * Method to setup the tool items when there is a valid layer
     */
    protected void validLayer() {
        drawItem.setEnabled(true);
        undoItem.setEnabled(layer.canUndo());
        redoItem.setEnabled(layer.canRedo());
        clearItem.setEnabled(layer.canClear());
        eraserItem.setEnabled(true);
        switch (layer.getDrawMode()) {
        case DRAW:
            drawItem.setSelection(true);
            eraserItem.setSelection(false);
            break;
        case ERASE:
            drawItem.setSelection(false);
            eraserItem.setSelection(true);
            break;
        case NONE:
            drawItem.setSelection(false);
            eraserItem.setSelection(false);
            break;
        }
    }

    public void setCurrentDrawingLayer(DrawingToolLayer layer) {
        this.layer = layer;
        updateItemState();
    }

    /**
     * Update the tool item state based on the layer
     */
    protected final void updateItemState() {
        if (layer == null) {
            nullLayer();
        } else {
            validLayer();
        }
        getShell().layout();
    }

    protected void undo() {
        if (layer != null) {
            layer.undo();
            updateItemState();
        }
    }

    protected void redo() {
        if (layer != null) {
            layer.redo();
            updateItemState();
        }
    }

    protected void clear() {
        if (layer != null) {
            layer.clear();
            updateItemState();
        }
    }
}
