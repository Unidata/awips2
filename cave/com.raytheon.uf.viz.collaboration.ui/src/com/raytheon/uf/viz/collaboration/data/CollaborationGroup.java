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
package com.raytheon.uf.viz.collaboration.data;

import java.util.ArrayList;
import java.util.List;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 22, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class CollaborationGroup extends CollaborationNode {
    protected boolean modifiable;

    protected List<CollaborationNode> children;

    public CollaborationGroup(String id) {
        super(id);
        children = new ArrayList<CollaborationNode>();
    }

    /**
     * @param modifiable
     *            the modifiable to set
     */
    public void setModifiable(boolean modifiable) {
        this.modifiable = modifiable;
    }

    public boolean getModifiable() {
        return modifiable;
    }

    public void addChild(CollaborationNode child) {
        // if (children == null) {
        // children = new ArrayList<CollaborationNode>();
        // }
        children.add(child);
    }

    /**
     * @return the users
     */
    public List<CollaborationNode> getChildren() {
        return children;
    }

}
