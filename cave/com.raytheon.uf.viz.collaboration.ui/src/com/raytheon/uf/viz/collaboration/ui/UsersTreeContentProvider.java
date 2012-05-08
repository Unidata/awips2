package com.raytheon.uf.viz.collaboration.ui;

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

import org.eclipse.ecf.presence.roster.IRosterGroup;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

import com.raytheon.uf.viz.collaboration.data.CollaborationGroupContainer;
import com.raytheon.uf.viz.collaboration.data.SessionGroupContainer;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 1, 2012            rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class UsersTreeContentProvider implements ITreeContentProvider {
    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.viewers.IContentProvider#dispose()
     */
    @Override
    public void dispose() {
        // TODO Auto-generated method stub

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface
     * .viewers.Viewer, java.lang.Object, java.lang.Object)
     */
    @Override
    public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
        System.out.println("changing input...");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.ITreeContentProvider#getElements(java.lang.
     * Object)
     */
    @Override
    public Object[] getElements(Object inputElement) {
        if (inputElement instanceof IRosterGroup) {
            IRosterGroup group = (IRosterGroup) inputElement;
            if (group.getEntries() != null) {
                return group.getEntries().toArray();
            } else {
                return new Object[0];
            }
        } else if (inputElement instanceof SessionGroupContainer) {
            SessionGroupContainer group = (SessionGroupContainer) inputElement;
            return group.getObjects().toArray();
        } else if (inputElement instanceof CollaborationGroupContainer) {
            CollaborationGroupContainer cont = (CollaborationGroupContainer) inputElement;
            return cont.getObjects().toArray();
        } else {
            return new Object[0];
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.
     * Object)
     */
    @Override
    public Object[] getChildren(Object parentElement) {
        // the only things that can have children are the sessions item or the
        // groups items
        if (parentElement instanceof SessionGroupContainer) {
            SessionGroupContainer cont = (SessionGroupContainer) parentElement;
            return cont.getObjects().toArray();
        } else if (parentElement instanceof IRosterGroup) {
            IRosterGroup group = (IRosterGroup) parentElement;
            return group.getEntries().toArray();
        }
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.ITreeContentProvider#getParent(java.lang.Object
     * )
     */
    @Override
    public Object getParent(Object element) {
        // TODO Auto-generated method stub
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.viewers.ITreeContentProvider#hasChildren(java.lang.
     * Object)
     */
    @Override
    public boolean hasChildren(Object element) {
        if (element instanceof IRosterGroup) {
            IRosterGroup group = (IRosterGroup) element;
            if (group.getEntries().size() <= 0) {
                return false;
            } else {
                return true;
            }
        } else if (element instanceof SessionGroupContainer) {
            SessionGroupContainer cont = (SessionGroupContainer) element;
            if (cont.getObjects() != null && cont.getObjects().size() > 0) {
                return true;
            } else {
                return false;
            }
        } else {
            return false;
        }
    }
}
