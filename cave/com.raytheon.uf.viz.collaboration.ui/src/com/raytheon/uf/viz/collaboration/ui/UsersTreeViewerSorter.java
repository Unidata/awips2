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

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;

import com.raytheon.uf.viz.collaboration.data.CollaborationGroup;
import com.raytheon.uf.viz.collaboration.data.CollaborationNode;
import com.raytheon.uf.viz.collaboration.data.LoginUser;
import com.raytheon.uf.viz.collaboration.data.OrphanGroup;
import com.raytheon.uf.viz.collaboration.data.SessionGroup;

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
public class UsersTreeViewerSorter extends ViewerSorter {

    @Override
    public int compare(Viewer viewer, Object e1, Object e2) {
        if (e1 == e2) {
            return 0;
        }

        // Make login user top node
        if (e1 instanceof LoginUser) {
            return -1;
        }
        if (e2 instanceof LoginUser) {
            return 1;
        }

        // session group before all other types but login user.
        if (e1 instanceof SessionGroup) {
            if ((e2 instanceof SessionGroup) == false) {
                return -1;
            }
        } else if (e2 instanceof SessionGroup) {
            return 1;
        }

        // OrpahGroup always at the bottom
        if (e1 instanceof OrphanGroup) {
            return 1;
        }
        if (e2 instanceof OrphanGroup) {
            return -1;
        }

        // Groups before users.
        if (e1 instanceof CollaborationGroup) {
            if (!(e2 instanceof CollaborationGroup)) {
                return -1;
            }
        } else if (e1 instanceof CollaborationGroup) {
            return 1;
        }

        // Either both are groups or both are users.
        return ((CollaborationNode) e1).compareTo((CollaborationNode) e2);
    }
}
