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

import java.util.List;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

import com.raytheon.uf.viz.points.PointsDataManager;
import com.raytheon.uf.viz.points.data.IPointNode;

/**
 * Provide the contents of a Point node.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 31, 2012 #875       rferrel     Initial creation.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class PointTreeContentProvider implements ITreeContentProvider {

    private final PointsDataManager manager = PointsDataManager.getInstance();

    @Override
    public void dispose() {
        // Nothing todo but must implement the method.
    }

    @Override
    public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
        // Nothing todo but must implement the method.
    }

    @Override
    public Object[] getElements(Object inputElement) {
        IPointNode node = (IPointNode) inputElement;
        return manager.getChildren(node).toArray(new IPointNode[0]);
    }

    @Override
    public Object[] getChildren(Object parentElement) {
        IPointNode node = (IPointNode) parentElement;
        List<IPointNode> children = manager.getChildren(node);
        return children.toArray(new IPointNode[0]);
    }

    @Override
    public Object getParent(Object element) {
        IPointNode node = (IPointNode) element;
        return manager.getParent(node);
    }

    @Override
    public boolean hasChildren(Object element) {
        IPointNode node = (IPointNode) element;
        return node.isGroup() && manager.getChildren(node).size() > 0;
    }
}
