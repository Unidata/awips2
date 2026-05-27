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
package com.raytheon.uf.viz.points.ui.dialog;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;

import com.raytheon.uf.viz.points.data.IPointNode;

/**
 * Sort point nodes placing group nodes at the top of the list.
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

public class PointTreeViewerSorter extends ViewerSorter {
    @Override
    public int compare(Viewer viewer, Object e1, Object e2) {
        if (e1 == e2) {
            return 0;
        }
        IPointNode node1 = (IPointNode) e1;
        IPointNode node2 = (IPointNode) e2;
        if (node1.isGroup()) {
            if (!node2.isGroup()) {
                return -1;
            }
        } else if (node2.isGroup()) {
            return 1;
        }

        String s1 = null;
        String s2 = null;

        if (node1.isGroup()) {
            s1 = node1.getGroup();
            s2 = node2.getGroup();
        } else {
            s1 = node1.getName();
            s2 = node2.getName();
        }

        // Both are either points or groups
        return s1.compareToIgnoreCase(s2);
    }
}
