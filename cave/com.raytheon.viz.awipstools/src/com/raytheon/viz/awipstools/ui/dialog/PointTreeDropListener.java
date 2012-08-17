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
package com.raytheon.viz.awipstools.ui.dialog;

import org.eclipse.jface.viewers.ViewerDropAdapter;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.dnd.TransferData;

import com.raytheon.uf.viz.points.PointsDataManager;
import com.raytheon.uf.viz.points.data.IPointNode;
import com.raytheon.uf.viz.points.data.Point;
import com.raytheon.uf.viz.points.data.PointTransfer;

/**
 * This handles moving nodes dropped onto the viewer.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 15, 2012 #875       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class PointTreeDropListener extends ViewerDropAdapter {
    private PointsDataManager manager;

    private PointsMgrDialog dialog;

    public PointTreeDropListener(PointsMgrDialog dialog) {
        super(dialog.pointsTreeViewer);
        this.dialog = dialog;
        this.manager = PointsDataManager.getInstance();
    }

    IPointNode targetNode;

    int location;

    @Override
    public void drop(DropTargetEvent event) {
        location = determineLocation(event);
        targetNode = (IPointNode) determineTarget(event);
        super.drop(event);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.ViewerDropAdapter#performDrop(java.lang.Object)
     */
    @Override
    public boolean performDrop(Object data) {
        if (data instanceof Point[]) {
            Point[] points = (Point[]) data;
            if (targetNode != null) {
                boolean state = false;
                try {
                    for (Point node : points) {
                        IPointNode destGroup = null;
                        if (location == LOCATION_ON) {
                            if (targetNode.isGroup()) {
                                destGroup = targetNode;
                            } else {
                                destGroup = manager.getParent(targetNode);
                            }
                        } else if (location != LOCATION_NONE) {
                            destGroup = manager.getParent(targetNode);
                        }
                        if (destGroup != null
                                && (manager.getParent(node)
                                        .compareTo(destGroup) != 0)
                                && !childGroupExists(destGroup, node.getName())) {
                            state = true;
                            dialog.setCursorBusy(true);
                            dialog.selectedNode = node;
                            manager.moveNode(node, destGroup);
                        }
                    }
                    return state;
                } finally {
                    targetNode = null;
                }
            }
        }
        return false;
    }

    /**
     * Determine if a child of a group node is a group node with the desired
     * name.
     * 
     * @param parent
     * @param name
     * @return true if child group node exists.
     */
    private boolean childGroupExists(IPointNode parent, String name) {
        for (IPointNode child : manager.getChildren(parent)) {
            if (child.isGroup() && name.equals(child.getName())) {
                return true;
            }
        }
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.ViewerDropAdapter#validateDrop(java.lang.Object
     * , int, org.eclipse.swt.dnd.TransferData)
     */
    @Override
    public boolean validateDrop(Object target, int operation,
            TransferData transferType) {
        return PointTransfer.getInstance().isSupportedType(transferType);
    }
}
