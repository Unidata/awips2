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
package com.raytheon.uf.viz.derivparam.tree;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.derivparam.tree.LevelNode;

/**
 * 
 * The base level node for Cave, each Level Node should be able to handle time
 * queries and metadata requests. This class attempts to handle
 * caching(currently very limited) for all subclasses.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 19, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public abstract class AbstractRequestableNode extends LevelNode {

    public static class Dependency {

        public Dependency(AbstractRequestableNode node, int timeOffset) {
            super();
            this.node = node;
            this.timeOffset = timeOffset;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((node == null) ? 0 : node.hashCode());
            result = prime * result + timeOffset;
            return result;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            Dependency other = (Dependency) obj;
            if (node == null) {
                if (other.node != null)
                    return false;
            } else if (!node.equals(other.node))
                return false;
            if (timeOffset != other.timeOffset)
                return false;
            return true;
        }

        public AbstractRequestableNode node;

        public int timeOffset;
    }

    public AbstractRequestableNode() {
    }

    public AbstractRequestableNode(Level level) {
        super(level);
    }

    public AbstractRequestableNode(LevelNode that) {
        super(that);
    }

    public abstract boolean isConstant();

}
