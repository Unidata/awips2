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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.jivesoftware.smack.RosterEntry;
import org.jivesoftware.smack.RosterGroup;

import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.user.IDConverter;
import com.raytheon.uf.viz.collaboration.comm.provider.user.LocalGroups.LocalGroup;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.ui.data.CollaborationGroupContainer;
import com.raytheon.uf.viz.collaboration.ui.data.SessionGroupContainer;

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
 * Dec  6, 2013 2561       bclement    removed ECF
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class UsersTreeContentProvider implements ITreeContentProvider {

    private Viewer viewer;

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
        this.viewer = viewer;
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
        if (inputElement instanceof SessionGroupContainer) {
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
        } else if (parentElement instanceof RosterGroup) {
            RosterGroup group = (RosterGroup) parentElement;
            List<UserId> result = new ArrayList<UserId>();
            UserId localUser = CollaborationConnection.getConnection()
                    .getUser();
            Collection<RosterEntry> entries = group.getEntries();
            synchronized (entries) {
                entries = new ArrayList<RosterEntry>(entries);
            }
            for (RosterEntry entry : entries) {
                String user = entry.getUser();
                if (!localUser.isSameUser(user)) {
                    result.add(IDConverter.convertFrom(entry));
                }
            }
            return result.toArray();
        } else if (parentElement instanceof LocalGroup) {
            List<UserId> result = new ArrayList<UserId>();
            LocalGroup group = (LocalGroup) parentElement;
            UserId localUser = CollaborationConnection.getConnection()
                    .getUser();
            for (UserId user : group.getUsers()) {
                if (!localUser.isSameUser(user.getNormalizedId())) {
                    result.add(user);
                }
            }
            return result.toArray();
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
        boolean hasChildren = false;
        if (element instanceof RosterGroup) {
            RosterGroup group = (RosterGroup) element;
            UserId localUser = CollaborationConnection.getConnection()
                    .getUser();
            Collection<RosterEntry> entries = group.getEntries();
            synchronized (entries) {
                entries = new ArrayList<RosterEntry>(entries);
            }
            for (RosterEntry entry : entries) {
                String user = entry.getUser();
                if (!localUser.isSameUser(user)) {
                    hasChildren = true;
                    break;
                }
            }
        } else if (element instanceof SessionGroupContainer) {
            SessionGroupContainer cont = (SessionGroupContainer) element;
            if (cont.getObjects() != null && cont.getObjects().size() > 0) {
                hasChildren = true;
            } else {
                hasChildren = false;
            }
        } else if (element instanceof LocalGroup) {
            UserId localUser = CollaborationConnection.getConnection()
                    .getUser();
            List<String> userNames = ((LocalGroup) element).getUserNames();
            for (String userName : userNames) {
                if (!localUser.isSameUser(userName)) {
                    hasChildren = true;
                    break;
                }
            }
        }

        // need to check whether items are filtered out so we don't get
        // empty groups that have an arrow, masking that nothing is in them
        ViewerFilter[] filters = ((TreeViewer) viewer).getFilters();
        if (hasChildren && filters.length > 0) {
            Object[] children = getChildren(element);
            for (int i = 0; i < filters.length; ++i) {
                children = filters[i].filter(viewer, element, children);
                if (children.length == 0) {
                    return false;
                }
            }
            return children.length > 0;
        }
        return hasChildren;
    }
}
