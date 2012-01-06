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
package com.raytheon.uf.common.derivparam.tree;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 11, 2009            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
@DynamicSerialize
public class SourceNode extends AbstractNode<ParameterNode> {

    @DynamicSerializeElement
    private int dt = -1;

    /**
     * @return the dt
     */
    public int getDt() {
        return dt;
    }

    /**
     * @param dt
     *            the dt to set
     */
    public void setDt(int dt) {
        this.dt = dt;
    }

    @Override
    public SourceNode clone() {
        SourceNode node = new SourceNode();
        node.setValue(this.getValue());
        node.setDt(dt);

        for (ParameterNode child : this.getChildNodes().values()) {
            node.addChildNode((ParameterNode) child.clone());
        }

        return node;
    }

}
