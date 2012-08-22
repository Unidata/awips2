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

import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.dnd.DragSourceEvent;
import org.eclipse.swt.dnd.DragSourceListener;

import com.raytheon.uf.viz.points.data.Point;
import com.raytheon.uf.viz.points.data.PointTransfer;

/**
 * This implements a drag source listener for a Tree Viewer that contains point
 * nodes.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 06, 2012 #875       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class PointTreeDragSourceListener implements DragSourceListener {

    private final TreeViewer viewer;

    public PointTreeDragSourceListener(TreeViewer viewer) {
        this.viewer = viewer;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.swt.dnd.DragSourceListener#dragStart(org.eclipse.swt.dnd.
     * DragSourceEvent)
     */
    @Override
    public void dragStart(DragSourceEvent event) {
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.swt.dnd.DragSourceListener#dragSetData(org.eclipse.swt.dnd
     * .DragSourceEvent)
     */
    @SuppressWarnings("unchecked")
    @Override
    public void dragSetData(DragSourceEvent event) {
        if (PointTransfer.getInstance().isSupportedType(event.dataType)) {
            TreeSelection selection = (TreeSelection) viewer.getSelection();
            Point[] points = (Point[]) selection.toList().toArray(
                    new Point[selection.size()]);
            event.data = points;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.swt.dnd.DragSourceListener#dragFinished(org.eclipse.swt.dnd
     * .DragSourceEvent)
     */
    @Override
    public void dragFinished(DragSourceEvent event) {
    }
}
