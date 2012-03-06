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

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

import com.raytheon.uf.viz.collaboration.data.CollaborationGroup;

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
    // UsersTree rootNode;
    //
    // public UsersTreeContentProvider(UsersTree rootNode) {
    // this.rootNode = rootNode;
    // }

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
        // if (rootNode != null) {
        // if (rootNode.getChildren() != null) {
        // return rootNode.getChildren().toArray();
        // }
        // }
        // return new Object[0];
        CollaborationGroup group = (CollaborationGroup) inputElement;
        if (group.getChildren() != null) {
            return group.getChildren().toArray();
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
        // if (parentElement instanceof UsersTree) {
        // UsersTree parent = (UsersTree) parentElement;
        // List<UsersTree> children = parent.getChildren();
        // if (children != null) {
        // return children.toArray();
        // } else {
        // return new Object[0];
        // }
        // }
        // return null;
        CollaborationGroup group = (CollaborationGroup) parentElement;
        return group.getChildren().toArray();
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
        // if (element instanceof UsersTree) {
        // UsersTree elem = (UsersTree) element;
        // return elem.hasChildren();
        // }
        // return false;
        if (element instanceof CollaborationGroup) {
            CollaborationGroup group = (CollaborationGroup) element;
            if (group.getChildren().size() <= 0) {
                return false;
            } else {
                return true;
            }
        } else {
            return false;
        }
    }

    // public Object findItem(String text) {
    // UsersTree item = rootNode.findChildByText(text);
    // return item;
    // }
}
