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

/**
 * This class defines a 2D plane
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
import javax.vecmath.Vector3d;
import javax.vecmath.Vector4d;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 	
 * 
 * </pre>
 * 
 * @author estrabal
 * @version 1.0
 */
/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 	
 * 
 * </pre>
 * 
 * @author estrabal
 * @version 1.0
 */
public class Plane {

    public double a = 0.0;

    public double b = 0.0;

    public double c = 0.0;

    public double d = 0.0;

    boolean normalized = false;

    /*
     * 
     */
    public Plane(Vector4d vec, boolean normalize) {

        a = vec.x;
        b = vec.y;
        c = vec.z;
        d = vec.w;

        if (normalize) {
            this.normalize();
        }
    }

    public Plane(double a, double b, double c, double d, boolean normalize) {
        this.a = a;
        this.b = b;
        this.c = c;
        this.d = d;
        if (normalize) {
            this.normalize();
        }
    }

    /**
     * Normalize the plane
     */
    public void normalize() {
        if (this.normalized == true) {
            return;
        }

        double mag = Math.sqrt(a * a + b * b + c * c);
        if (mag == 0) {
            mag = 1;
        }
        this.a /= mag;
        this.b /= mag;
        this.c /= mag;
        this.d /= mag;
        this.normalized = true;
    }

    // public Plane(Vector3d pnt, Vector3d normal) {
    // this.point = pnt;
    // this.normal = normal;
    // this.d = pnt.dot(normal);
    // }

    /**
     * Get the intersection of the plane and the ray
     * 
     * @param r
     * @return
     */
    public Vector3d intersection(Ray r) {

        Vector3d normal = new Vector3d(a, b, c);
        double dn = r.direction.dot(normal);
        if (dn == 0) {
            return null;
        }

        double t = this.d - (r.origin.dot(normal) / dn);
        Vector3d i = new Vector3d(r.direction);
        i.scale(t);
        i.add(r.origin);

        return i;
    }

    /**
     * Get the distance for a point to the plane
     * 
     * @param pnt
     * @return
     */
    public double distance(Vector3d pnt) {
        return pnt.x * a + pnt.y * b + pnt.z * c + d;
    }

    /**
     * Get the equation for the plane
     * 
     * @return
     */
    public double[] getEquation() {
        return new double[] { a, b, c, d };
    }

    // public String toString() {
    // return "Plane point " + this.point.toString() + " normal "
    // + this.normal.toString();
    // }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#clone()
     */
    @Override
    public Object clone() {

        Plane p = new Plane(this.a, this.b, this.c, this.d, this.normalized);

        return p;
    }
}
