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
package com.raytheon.uf.viz.d2d.core.time;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import com.raytheon.uf.viz.core.rsc.AbstractVizResource;

/**
 * Time matching tree, used to only re-timematch necessary resources
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 11, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class TimeMatchingTree {

    private class TimeMatchingTreeNode {

        private AbstractVizResource<?, ?> resource;

        private TimeMatchingTreeNode parent;

        private List<TimeMatchingTreeNode> children = new LinkedList<TimeMatchingTreeNode>();

        public TimeMatchingTreeNode(TimeMatchingTreeNode parent,
                AbstractVizResource<?, ?> resource) {
            this.parent = parent;
            this.resource = resource;
            if (parent != null) {
                parent.children.add(this);
            }
        }
    }

    private TimeMatchingTreeNode basisNode = new TimeMatchingTreeNode(null,
            null);

    TimeMatchingTree() {

    }

    public synchronized void setTimeMatchBasis(AbstractVizResource<?, ?> basis) {
        basisNode.resource = basis;
    }

    public synchronized void addResource(AbstractVizResource<?, ?> resource) {
        if (basisNode.resource == null) {

        }
        // find out which basisNode child resource belongs to
        TimeMatchingTreeNode node = null;
        for (TimeMatchingTreeNode tn : basisNode.children) {
            if (tn.resource.getDescriptor() == resource.getDescriptor()) {
                node = tn;
                break;
            }
        }

        if (node == null) {
            new TimeMatchingTreeNode(basisNode, resource);
        } else {
            while (node.children.size() > 0) {
                node = node.children.get(0);
            }
            new TimeMatchingTreeNode(node, resource);
        }
    }

    public synchronized void removeResource(AbstractVizResource<?, ?> resource) {
        removeResource(basisNode, resource);
    }

    private void removeResource(TimeMatchingTreeNode nodeToCheck,
            AbstractVizResource<?, ?> resourceToRemove) {
        TimeMatchingTreeNode container = null;
        for (TimeMatchingTreeNode node : nodeToCheck.children) {
            if (node.resource != null) {
                if (node.resource == resourceToRemove) {
                    container = node;
                    break;
                } else {
                    removeResource(node, resourceToRemove);
                }
            }
        }

        for (TimeMatchingTreeNode child : container.children) {
            child.parent = nodeToCheck;
        }
        if (nodeToCheck != basisNode) {
            nodeToCheck.children = container.children;
        } else {
            nodeToCheck.children.remove(container);
            container.parent.children.addAll(container.children);
        }
    }

    public synchronized void redoTimeMatching(
            List<AbstractVizResource<?, ?>> resources) {
        List<AbstractVizResource<?, ?>> copy = new ArrayList<AbstractVizResource<?, ?>>(
                resources);
        List<TimeMatchingTreeNode> startNodes = new ArrayList<TimeMatchingTree.TimeMatchingTreeNode>(
                1);
        startNodes.add(basisNode);
        redoTimeMatching(copy, startNodes);
    }

    private void redoTimeMatching(List<AbstractVizResource<?, ?>> resources,
            List<TimeMatchingTreeNode> searchNodes) {
        List<TimeMatchingTreeNode> nextLevel = new ArrayList<TimeMatchingTreeNode>();
        for (TimeMatchingTreeNode node : searchNodes) {
            if (node.resource != null && resources.contains(node.resource)) {
                // We found a match, time match at that node
                redoTimeMatching(node);
            } else {
                // no match, check for children in list
                nextLevel.addAll(node.children);
            }
        }

        if (nextLevel.size() > 0) {
            redoTimeMatching(resources, nextLevel);
        }
    }

    private void redoTimeMatching(TimeMatchingTreeNode startNode) {

    }
}
