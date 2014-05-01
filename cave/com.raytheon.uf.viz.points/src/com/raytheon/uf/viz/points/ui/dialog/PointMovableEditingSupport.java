/*****************************************************************************************
 * COPYRIGHT (c), 2007, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/
package com.raytheon.uf.viz.points.ui.dialog;

import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.EditingSupport;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.viz.points.data.IPointNode;
import com.raytheon.uf.viz.points.data.Point;
import com.raytheon.uf.viz.points.data.PointNameChangeException;
import com.raytheon.uf.viz.points.data.PointFieldState;
import com.raytheon.uf.viz.points.ui.dialog.TriStateCellEditor.STATE;
import com.raytheon.uf.viz.points.ui.layer.PointsToolLayer;

/**
 * Class for supporting editing of the Movable column.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 31, 2012 #875       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class PointMovableEditingSupport extends EditingSupport {

    private PointsToolLayer toolLayer;

    private TriStateCellEditor cellEditor;

    public PointMovableEditingSupport(TreeViewer viewer, PointsToolLayer layer) {
        super(viewer);
        toolLayer = layer;
        cellEditor = new TriStateCellEditor(
                (Composite) viewer.getContentProvider(), SWT.READ_ONLY);
    }

    @Override
    protected CellEditor getCellEditor(Object element) {
        return cellEditor;
    }

    @Override
    protected boolean canEdit(Object element) {
        return true;
    }

    @Override
    protected Object getValue(Object element) {
        IPointNode node = (IPointNode) element;
        STATE value = STATE.GRAYED;
        switch (node.getMovable()) {
        case TRUE:
            value = STATE.SELECTED;
            break;
        case FALSE:
            value = STATE.UNSELECTED;
            break;
        case UNKNOWN:
            value = STATE.GRAYED;
            break;
        default:
            Assert.isTrue(false);
        }
        return value;
    }

    @Override
    protected void setValue(Object element, Object value) {
        IPointNode node = (IPointNode) element;
        STATE state = (STATE) value;
        PointFieldState newValue = PointFieldState.UNKNOWN;
        switch (state) {
        case SELECTED:
            newValue = PointFieldState.TRUE;
            break;
        case UNSELECTED:
            newValue = PointFieldState.FALSE;
            break;
        case GRAYED:
            newValue = PointFieldState.UNKNOWN;
            break;
        default:
            Assert.isTrue(false);
        }
        try {
            ((TreeViewer) getViewer()).getTree().setCursor(
                    Display.getCurrent().getSystemCursor(SWT.CURSOR_WAIT));
            if (!node.isGroup()) {
                Point point = (Point) node;
                point.setMovable(newValue);
                toolLayer.updatePoint(point);
            } else {
                Assert.isTrue(newValue != PointFieldState.UNKNOWN);
                toolLayer.updateChildrenMovable(node, newValue);
            }
        } catch (PointNameChangeException e) {
            // ignore as point name is not being changed ...
        }
    }
}
