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

import javax.persistence.Transient;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

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
public class LevelNode extends AbstractNode<LevelNode> {

    @Transient
    protected Level level;

    public LevelNode() {
    }

    public LevelNode(LevelNode that) {
        this.value = that.value;
        this.level = that.level;

        for (LevelNode child : that.getChildNodes().values()) {
            addChildNode((LevelNode) child.clone());
        }
    }

    public LevelNode(Level level) {
        this.setLevel(level);
    }

    /**
     * @return the level
     */
    public Level getLevel() {
        if (value != null && level == null) {
            level = LevelFactory.getInstance().getLevel(value);
        }
        return level;
    }

    /**
     * @param level
     *            the level to set
     */
    public void setLevel(Level level) {
        if (level != this.level) {
            this.level = level;
            this.value = Long.toString(level.getId());
        }
    }

    @Override
    public LevelNode clone() {
        return new LevelNode(this);
    }

    @Override
    public boolean equals(Object other) {
        if (!(other instanceof LevelNode)) {
            return false;
        }
        LevelNode rOther = (LevelNode) other;
        if (value != null ? !value.equals(rOther.value) : rOther.value != null) {
            return false;
        }
        return true;
    }

    @Override
    public int hashCode() {
        int hash = 1;
        hash = hash * 31 + (value == null ? 0 : value.hashCode());
        return hash;
    }

}
