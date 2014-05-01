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
package com.raytheon.viz.gfe.contours.util;

/**
 * This is a simple container class that contains a value and distance.
 * Both {@link #value} and {@link #distance} are exposed as public
 * elements, so no getters or setters are provided. It is intended for
 * passing a value/distance pair in and out of a method via the method's
 * arguments.
 * <P>
 * Usage:
 * <PRE>
 *    {@code ContourValueDistance cvd = new ContourValueDistance();}
 *    {@code // in the method...}
 *    {@code cvd.value = 100.0;}
 *    {@code cvd.distance = 50.5;}
 *    {@code // back in the calling method...}
 *    {@code float localValue = (float)cvd.value;}
 *    {@code float localDistance = (float)cvd.distance;}
 * </PRE>
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 13Mar2008    968        MW Fegan    Initial implementation.
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1.0	
 */

public class ContourValueDistance {
    /**
     * The <em>value</em> component of the distance/value pair.
     * The default value is {@code (double)0.0}.
     */
    public double value = 0.0;
    /**
     * The <em>distance</em> component of the distance/value pair.
     * The default value is {@code (double)0.0}.
     */
    public double distance = 0.0;
    /**
     * Constructor. Creates a {@code ContourValueDistance} object
     * with both {@code value} and {@code distance} set to zero.
     */
    public ContourValueDistance() {
        // intentionally empty
    }
    /**
     * Constructor. Creates a @code ContourValueDistance} object
     * with the specified {@code value} and {@code distance}.
     * @param value the value
     * @param distance the distance
     */
    public ContourValueDistance(double value, double distance) {
        this.value = value;
        this.distance = distance;
    }
    /**
     * Tests to see if this object is equal to the specified object.
     * <P>
     * Two {@code ContourValueDistance} objects are considered to be equal
     * if either of the following tests is true.
     * <OL>
     * <LI>This two objects are really the same object.
     * <LI>The two objects have equal distances and values.
     * </OL> 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object rhs) {
        if (!(rhs instanceof ContourValueDistance)) {
            return false;
        }
        ContourValueDistance cvd = (ContourValueDistance)rhs;
        if (cvd == this) {
        }
        return (this.value == cvd.value && this.distance == cvd.distance);
    }
    /**
     * Computes the gradient between this object and the specified object.
     * 
     * @param rhs the second object
     * 
     * @return the gradient
     */
    public double gradient(ContourValueDistance rhs) {
        if (this.equals(rhs)) {
            return 0;
        }
        return (double)(this.value - rhs.value) / (double)(this.distance - rhs.distance); 
    }
    /**
     * Resets attributes to original default values.
     */
    public void reset() {
        this.value = 0.0;
        this.distance = 0.0;
    }
}
