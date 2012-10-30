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
package com.raytheon.viz.grid.inv;

import java.util.Map;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;

/**
 * 
 * Unique key for grid metadata, useful in caches.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 20, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GridMapKey {

    public final String modelName;

    public final String parameter;

    public final String masterLevel;

    public final double levelone;

    public final double leveltwo;

    public GridMapKey(GridRequestableNode node) {
        this(node.getRequestConstraintMap());
    }

    public GridMapKey(Map<String, ?> map) {
        Object obj;
        obj = map.get(GridInventory.MODEL_NAME_QUERY);
        if (obj != null && obj instanceof RequestConstraint) {
            modelName = ((RequestConstraint) obj).getConstraintValue();
        } else if (obj != null) {
            modelName = obj.toString();
        } else {
            modelName = null;
        }
        obj = map.get(GridInventory.PARAMETER_QUERY);
        if (obj != null && obj instanceof RequestConstraint) {
            parameter = ((RequestConstraint) obj).getConstraintValue();
        } else if (obj != null) {
            parameter = obj.toString();
        } else {
            parameter = null;
        }
        obj = map.get(GridInventory.MASTER_LEVEL_QUERY);
        if (obj != null && obj instanceof RequestConstraint) {
            masterLevel = ((RequestConstraint) obj).getConstraintValue();
        } else if (obj != null) {
            masterLevel = obj.toString();
        } else {
            masterLevel = null;
        }
        double levelone = Level.getInvalidLevelValue();
        obj = map.get(GridInventory.LEVEL_ONE_QUERY);
        if (obj != null) {
            if (obj instanceof RequestConstraint) {
                obj = ((RequestConstraint) obj).getConstraintValue();
                if (obj != null && !obj.toString().equals("null")) {
                    levelone = Double.parseDouble(obj.toString());
                }
            } else {
                levelone = (Double) obj;
            }
        }
        this.levelone = levelone;
        double leveltwo = Level.getInvalidLevelValue();
        obj = map.get(GridInventory.LEVEL_TWO_QUERY);
        if (obj != null) {
            if (obj instanceof RequestConstraint) {
                obj = ((RequestConstraint) obj).getConstraintValue();
                if (obj != null && !obj.toString().equals("null")) {
                    leveltwo = Double.parseDouble(obj.toString());
                }
            } else {
                leveltwo = (Double) obj;
            }
        }
        this.leveltwo = leveltwo;
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
        long temp;
        temp = Double.doubleToLongBits(levelone);
        result = prime * result + (int) (temp ^ (temp >>> 32));
        temp = Double.doubleToLongBits(leveltwo);
        result = prime * result + (int) (temp ^ (temp >>> 32));
        result = prime * result
                + ((masterLevel == null) ? 0 : masterLevel.hashCode());
        result = prime * result
                + ((modelName == null) ? 0 : modelName.hashCode());
        result = prime * result
                + ((parameter == null) ? 0 : parameter.hashCode());
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
        GridMapKey other = (GridMapKey) obj;
        if (Double.doubleToLongBits(levelone) != Double
                .doubleToLongBits(other.levelone))
            return false;
        if (Double.doubleToLongBits(leveltwo) != Double
                .doubleToLongBits(other.leveltwo))
            return false;
        if (masterLevel == null) {
            if (other.masterLevel != null)
                return false;
        } else if (!masterLevel.equals(other.masterLevel))
            return false;
        if (modelName == null) {
            if (other.modelName != null)
                return false;
        } else if (!modelName.equals(other.modelName))
            return false;
        if (parameter == null) {
            if (other.parameter != null)
                return false;
        } else if (!parameter.equals(other.parameter))
            return false;
        return true;
    }

}