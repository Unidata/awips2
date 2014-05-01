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

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import com.raytheon.uf.common.serialization.ISerializableObject;
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
public abstract class AbstractNode<K extends AbstractNode<?>> implements
        Cloneable, ISerializableObject {

    @DynamicSerializeElement
    protected String value;

    @DynamicSerializeElement
    protected Map<String, K> childNodes;

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }

    public Map<String, K> getChildNodes() {
        if (childNodes == null) {
            childNodes = new HashMap<String, K>();
        }

        return childNodes;
    }

    public Set<String> getChildNodeIds() {
        return (childNodes != null ? childNodes.keySet()
                : new HashSet<String>());
    }

    public K getChildNode(String key) {
        return (childNodes != null ? childNodes.get(key) : null);
    }

    public void setChildNodes(Map<String, K> childNodes) {
        this.childNodes = childNodes;
    }

    public void addChildNode(K childNode) {
        if (childNodes == null) {
            childNodes = new HashMap<String, K>();
        }

        AbstractNode node = childNodes.get(childNode.getValue());
        if (node == null) {
            childNodes.put(childNode.getValue(), childNode);
        } else {
            node.mergeNode(childNode);
        }
    }

    public boolean containsChildNode(String key) {
        return childNodes != null && childNodes.containsKey(key);
    }

    public void mergeNode(AbstractNode<K> that) {
        if (childNodes != null) {
            Map<String, K> thatChildNodes = that.getChildNodes();

            if (thatChildNodes != null) {
                for (Entry<String, K> thatChildNode : thatChildNodes.entrySet()) {
                    AbstractNode childNode = childNodes.get(thatChildNode
                            .getKey());

                    if (childNode != null) {
                        childNode.mergeNode(thatChildNode.getValue());
                    } else {
                        childNodes.put(thatChildNode.getKey(), thatChildNode
                                .getValue());
                    }
                }
            }
        } else {
            childNodes = that.getChildNodes();
        }
    }

    @Override
    public abstract Object clone();

    @Override
    public String toString() {
        StringBuilder tmp = new StringBuilder();
        tmp.append(getClass().getSimpleName());
        tmp.append(" [");
        tmp.append(getValue());
        tmp.append(" ]\n");
        if (childNodes != null) {
            for (K child : childNodes.values()) {
                tmp.append(child.toString());
            }
        }

        return tmp.toString();
    }
}
