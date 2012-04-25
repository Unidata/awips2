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
package com.raytheon.uf.viz.remote.graphics.events.imagery;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.remote.graphics.events.AbstractDispatchingObjectEvent;
import com.raytheon.uf.viz.remote.graphics.events.IRenderEvent;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 8, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@DynamicSerialize
public class PaintImageEvent extends AbstractDispatchingObjectEvent implements
        IRenderEvent {

    @DynamicSerializeElement
    private int meshId = -1;

    @DynamicSerializeElement
    private Coordinate ul;

    @DynamicSerializeElement
    private Coordinate ur;

    @DynamicSerializeElement
    private Coordinate lr;

    @DynamicSerializeElement
    private Coordinate ll;

    /**
     * @return the meshId
     */
    public int getMeshId() {
        return meshId;
    }

    /**
     * @param meshId
     *            the meshId to set
     */
    public void setMeshId(int meshId) {
        this.meshId = meshId;
    }

    /**
     * @return the ul
     */
    public Coordinate getUl() {
        return ul;
    }

    /**
     * @param ul
     *            the ul to set
     */
    public void setUl(Coordinate ul) {
        this.ul = ul;
    }

    /**
     * @return the ur
     */
    public Coordinate getUr() {
        return ur;
    }

    /**
     * @param ur
     *            the ur to set
     */
    public void setUr(Coordinate ur) {
        this.ur = ur;
    }

    /**
     * @return the lr
     */
    public Coordinate getLr() {
        return lr;
    }

    /**
     * @param lr
     *            the lr to set
     */
    public void setLr(Coordinate lr) {
        this.lr = lr;
    }

    /**
     * @return the ll
     */
    public Coordinate getLl() {
        return ll;
    }

    /**
     * @param ll
     *            the ll to set
     */
    public void setLl(Coordinate ll) {
        this.ll = ll;
    }

    public void setPixelCoverage(PixelCoverage coverage) {
        this.ll = coverage.getLl();
        this.lr = coverage.getLr();
        this.ur = coverage.getUr();
        this.ul = coverage.getUl();
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
        PaintImageEvent other = (PaintImageEvent) obj;
        if (meshId == other.meshId && meshId != -1) {
            // If meshes are set, compare them only
            return true;
        } else if (meshId == other.meshId) {
            // Compare extents if no meshes set (-1)
            if (ll == null) {
                if (other.ll != null)
                    return false;
            } else if (!ll.equals(other.ll))
                return false;
            if (lr == null) {
                if (other.lr != null)
                    return false;
            } else if (!lr.equals(other.lr))
                return false;
            if (ul == null) {
                if (other.ul != null)
                    return false;
            } else if (!ul.equals(other.ul))
                return false;
            if (ur == null) {
                if (other.ur != null)
                    return false;
            } else if (!ur.equals(other.ur))
                return false;
        }
        return true;
    }

}
