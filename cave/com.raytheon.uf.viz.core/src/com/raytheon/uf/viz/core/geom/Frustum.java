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

package com.raytheon.uf.viz.core.geom;

import javax.vecmath.Matrix4d;
import javax.vecmath.Vector3d;
import javax.vecmath.Vector4d;

/**
 * This class defines the view frustum for 3D view areas
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  
 * </pre>
 * 
 * @author estrabal
 * @version 1
 */
public class Frustum {

    // view volume area clipping planes
    private Plane near;

    private Plane far;

    private Plane left;

    private Plane right;

    private Plane top;

    private Plane bottom;

    public Frustum() {

    }

    /**
     * Extract the frustum from the projection matrix *modelview matrix
     * 
     * @param pm
     *            projection matrix * modelview matrix
     */
    public boolean init(Matrix4d pm) {

        Vector4d row = new Vector4d();
        Vector4d temp = new Vector4d();

        pm.getRow(0, row);
        // left plane =
        pm.getRow(3, temp);
        temp.add(row);
        left = new Plane(temp, true);

        // right plane
        pm.getRow(3, temp);
        temp.sub(row);
        right = new Plane(temp, true);

        // bottom plane
        pm.getRow(1, row);
        pm.getRow(3, temp);
        temp.add(row);
        bottom = new Plane(temp, true);

        // top plane
        pm.getRow(3, temp);
        temp.sub(row);
        top = new Plane(temp, true);

        // near plane
        pm.getRow(2, row);
        pm.getRow(3, temp);
        temp.add(row);
        near = new Plane(temp, true);

        pm.getRow(3, temp);
        temp.sub(row);
        far = new Plane(temp, true);

        return true;
    }

    /**
     * Determine if the point and all points within a radius of dist are inside
     * the frustum
     * 
     * @param pnt
     * @param dist
     * @return
     */
    public boolean inside(Vector3d pnt, double dist) {

        final boolean dbg = false;

        if (this.left.distance(pnt) <= -dist) {
            if (dbg) {
                System.out.println("left dot = " + this.left.distance(pnt)
                        + " " + dist);
            }
            return false;
        }

        if (this.right.distance(pnt) <= -dist) {
            if (dbg) {
                System.out.println("right dot = " + this.right.distance(pnt)
                        + " " + dist);
            }
            return false;
        }

        if (this.top.distance(pnt) <= -dist) {
            if (dbg) {
                System.out.println("top dot = " + this.top.distance(pnt) + " "
                        + dist);
            }

            return false;
        }

        if (this.bottom.distance(pnt) <= -dist) {
            if (dbg) {
                System.out.println("bottom dot = " + this.bottom.distance(pnt)
                        + " " + dist);
            }
            return false;
        }

        if (this.near.distance(pnt) <= -dist) {
            if (dbg) {
                System.out.println("near dot = " + this.near.distance(pnt)
                        + " " + dist);
            }
            return false;
        }

        if (this.far.distance(pnt) <= -dist) {
            if (dbg) {
                System.out.println("far dot = " + this.far.distance(pnt) + " "
                        + dist);
            }
            return false;
        }

        return true;
    }

    public Plane[] getClippingPlanes() {
        return new Plane[] { left, right, top, bottom, near, far };
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#clone()
     */
    @Override
    public Object clone() {

        Frustum f = new Frustum();
        if (this.bottom != null) {
            f.bottom = (Plane) this.bottom.clone();
            f.top = (Plane) this.top.clone();
            f.right = (Plane) this.right.clone();
            f.left = (Plane) this.left.clone();
            f.near = (Plane) this.near.clone();
            f.far = (Plane) this.far.clone();
        }
        return f;
    }

}
