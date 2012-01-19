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

import java.io.Serializable;

/**
 * This is a port of the GFE SIRS CartCoord2D C++ class. The original
 * C++ class was implemented as a template class to allow for different
 * underlying data types. A quick search of the legacy code indicates
 * that only float were used, so this implemetation only support floats.  
 * <P>
 * Note: Since Java does not support operator override, the operator
 * overrides in the original class have been been replaced with named
 * methods. Generally, binary operations have been implemented as static
 * methods while unary operations have been implemented as instance
 * methods.
 * <P>
 * The Cartesian specific product methods (scalar product, dot product
 * and cross product) have been implemented as  implemented both as static
 * methods and instance methods. Unlike the original C++ class, the
 * accumulation operators ("+=", "-=", etc.) have been implemented as
 * chainable operations, so code similar to 
 * {@code lhs.addTo(rhs).scalarProduct(f);} are possible.
 *  
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29Feb2008    968        MW Fegan    Initial creation.
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1.0	
 */

public class CartCoord2D implements Cloneable, Comparable<CartCoord2D>, Serializable {
    private static final long serialVersionUID = 1L;

    /* the coordinates */
    /** the x coordinate */
    public float x = (float)0;
    /** the y coordinate */
    public float y = (float)0;
    
    /**
     * Constructor. Creates an empty CartCoord2D object.
     */
    public CartCoord2D() {
        /* set coordinates to safe values */
    }
    /**
     * Constructor. Creates an CartCoord2D object having specified 
     * abscissa and ordinate.
     * 
     * @param x the abscissa
     * @param y the ordinate
     */
    public CartCoord2D(float x, float y) {
        this.x = x;
        this.y = y;
    }

    /*
     * (non-Javadoc)
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return String.format("CartCoord2D[%f,%f]", this.x,this.y);
    }
   
    /* start of unary operations */
    /**
     * Implementation of the "/=" operation for CartCoord2D objects.
     * 
     * @param rhs right hand side operand
     * 
     * @return the modified CartCoord2D object
     */
    public CartCoord2D scalarQuotient(float rhs) {
        this.x /= rhs;
        this.y /= rhs;
        return this;
    }
    /**
     * Implementation of the "*=" operation for CartCoord2D objects.
     * 
     * @param rhs right hand side operand
     * 
     * @return the modified CartCoord2D object
     */
    public CartCoord2D scalarProduct(float rhs) {
        this.x *= rhs;
        this.y *= rhs;
        return this;
    }
    /**
     * Implements the "-=" operation for CartCoord2D objects.
     * 
     * @param rhs right hand side operand
     * 
     * @return the modified CartCoord2D object
     */
    public CartCoord2D subtractFrom(CartCoord2D rhs) {
        this.x -= rhs.x;
        this.y -= rhs.y;
        return this;
    }
    /**
     * Implements the "+=" operation for CartCoord2D objects.
     * 
     * @param rhs right hand side operand
     * 
     * @return the modified CartCoord2D object
     */
    public CartCoord2D addTo(CartCoord2D rhs) {
        this.x += rhs.x;
        this.y += rhs.y;
        return this;
    }
    /**
     * Swaps the abscissa and ordinate of the CartCoord2D object.
     * 
     * @return the modified CartCoord2D object
     */
    public CartCoord2D swapXY() {
        float temp = this.x;
        this.x = this.y;
        this.y = temp;
        return this;
    }
    /**
     * Implements a non-square scaling operation for CartCoord2D objects.
     * The abscissa is scaled by the x-factor, the ordinate is scaled by
     * the y-factor.
     * 
     * @param xFactor scale factor for the abscissa
     * @param yFactor scale factor for the ordinate
     * 
     * @return the modified CartCoord2D object
     */
    public CartCoord2D scale(float xFactor, float yFactor) {
        this.x *= xFactor;
        this.y *= yFactor;
        return this;
    }
    /**
     * Modifies the CartCoord2D object by setting its abscissa and ordinate
     * to the minimum corresponding values between it and the specified 
     * CartCoord2D object.
     * 
     * @param rhs the right hand side operand
     * 
     * @return the modified CartCoord2D object
     */
    public CartCoord2D setToMinOfSelfAnd(CartCoord2D rhs) {
        this.x = Math.min(this.x, rhs.x);
        this.y = Math.min(this.y, rhs.y);
        return this;
    }
    /**
     * Modifies the CartCoord2D object by setting its abscissa and ordinate
     * to the maximum corresponding values between it and the specified 
     * CartCoord2D object.
     * 
     * @param rhs the right hand side operand
     * 
     * @return the modified CartCoord2D object
     */
    public CartCoord2D setToMaxOfSelfAnd(CartCoord2D rhs) {
        this.x = Math.max(this.x, rhs.x);
        this.y = Math.max(this.y, rhs.y);
        return this;
    }
    /**
     * Computes the distance between the CartCoord2D object and the
     * specified CartCoord2D object.
     * 
     * @param rhs the right hand side operand
     * 
     * @return the distance between the CartCoord2D objects
     */
    public float distance(CartCoord2D rhs) {
        float xDelt = this.x - rhs.x;
        float ydelt = this.y - rhs.y;
        return (float)Math.sqrt(xDelt*xDelt + ydelt*ydelt);
    }
    /**
     * Computes the (scalar) cross product of a CartCoord2D object
     * with a second CartCoord2D object. The description of the 
     * comparable method from the C++ class is "Return the magnitude
     * of the cross product of two CartCoord2Ds." 
     * 
     * @param rhs the right hand side operand
     * @return the cross product
     */
    public float crossProduct(CartCoord2D rhs) {
        return (this.x * rhs.y - this.y * rhs.x);
    }
    /**
     * Computes the (scalar) dot product of a CartCoord2D object
     * with a second CartCoord2D object.
     * 
     * @param rhs the right hand side operand
     * 
     * @return the dot product
     */
    public float dotProduct(CartCoord2D rhs) {
        return (this.x * rhs.x + this.y * rhs.y);
    }
    /* end of unary operations */
    
    /* comparison operations */
    /* (non-Javadoc)
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
    public int compareTo(CartCoord2D rhs) {
        if (this.equals(rhs)) {
            return 0;
        } 
        if (this.greaterThan(rhs)) {
            return 1;
        }
        return -1;
    }
    /**
     * Determines if this CartCoord2D object is "not equal" to the 
     * specified CartCoord2D object.
     * <P>
     * The two objects are "not equal" if and only if they're abscissas
     * are different or they're ordinates are different. That is
     * <PRE>
     *    {@code a.notEqual(b)} iff {@code !a.equals(b)} 
     * </PRE>
     *
     * @param rhs the object to compare
     * 
     * @return true if the objects are "not equal"
     */
    public boolean notEqual(CartCoord2D rhs) {
        return !this.equals(rhs);
    }
    /**
     * Determines if this CartCoord2D object is "equal" to the CartCoord2D 
     * specified object.
     * <P>
     * The two objects are "equal" if and only if their abscissas have the
     * same value and their ordinates have the same value.
     *
     * @param rhs the object to compare
     * 
     * @return true if the objects are "equal"
     */
    public boolean equals(CartCoord2D rhs) {
        return (this.x == rhs.x && this.y == rhs.y);
    }
    /**
     * Determines if this CartCoord2D object is "less than" the specified
     * CartCoord2D object.
     * <P>
     * This object is is "less than" the specified object if and only if
     * it is neither "equal" to nor "greater than" the specified object.
     * 
     * @param rhs the object to compare
     * 
     * @return true is this object is "less than" the specified object
     */
    public boolean lessThan(CartCoord2D rhs) {
        return !(this.equals(rhs) || this.greaterThan(rhs));
    }
    /**
     * Determines if this CartCoord2D object is "greater than" the specified
     * CartCoord2D object.
     * <P>
     * Let x<sub>1</sub> and y<sub>1</sub> represent this object's abscissa
     *     and ordinate and x<sub>2</sub> and y<sub>2</sub> the specified
     *     object's abscissa and ordinate.
     * This object is "greater than" the specified object if and only if
     * either of the following conditions are true:
     * <P>
     * This object is "greater than" the specified object if and only if
     * either of the following conditions are true:
     * <OL>
     * <LI>x<sub>1</sub> > x<sub>2</sub>
     * <LI>x<sub>1</sub> == x<sub>2</sub> y<sub>1</sub> > y<sub>2</sub>
     * </OL>
     * In all other cases, this object is "not greater than" the specified
     * object.
     * 
     * @param rhs the object to compare
     * 
     * @return true is this object is "greater than" the specified object
     */    
    public boolean greaterThan(CartCoord2D rhs) {
        if (this.x > rhs.x) {
            return true;
        }
        if (this.x != rhs.x) {
            return false;
        }
        if (this.y > rhs.y) {
            return true;
        }
        return false;
    }
    /* end of comparison operations */
    
    /* start of binary operations */
    /**
     * Computes the vector sum of the two CartCoord2D objects.
     * 
     * @param lhs first addend
     * @param rhs second addend
     * 
     * @return the vector sum of the two objects
     */
    public static CartCoord2D add(CartCoord2D lhs, CartCoord2D rhs) {
        
        try {
            CartCoord2D retVal = (CartCoord2D) lhs.clone();
            retVal.addTo(rhs);
            return retVal;
        } catch (Exception e) {
            return null;
        }
    }
    /**
     * Computes the vector difference of the two CartCoord2D objects.
     * 
     * @param lhs object to subtract from
     * @param rhs object to subtract
     * 
     * @return the vector difference of the objects
     */
    public static CartCoord2D subtract(CartCoord2D lhs, CartCoord2D rhs) {
        
        try {
            CartCoord2D retVal = (CartCoord2D) lhs.clone();
            retVal.subtractFrom(rhs);
            return retVal;
        } catch (Exception e) {
            return null;
        }
    }
    /**
     * Computes the scalar product of a CartCoord2D object and a scalar.
     * 
     * @param lhs the coordinate object
     * @param rhs the scalar
     * 
     * @return the scalar product
     */
    public static CartCoord2D multiply(CartCoord2D lhs, float rhs) {
        
        try {
            CartCoord2D retVal = (CartCoord2D) lhs.clone();
            retVal.scalarProduct(rhs);
            return retVal;
        } catch (Exception e) {
            return null;
        }
    }
    /**
     * Computes the scalar quotient of a CartCoord2D coordinate object
     * and a scalar.
     * 
     * @param lhs the CartCoord2D object
     * @param rhs the scalar
     * 
     * @return the scalar quotient
     */
    public static CartCoord2D divide(CartCoord2D lhs, float rhs) {
        
        try {
            CartCoord2D retVal = (CartCoord2D) lhs.clone();
            retVal.scalarQuotient(rhs);
            return retVal;
        } catch (Exception e) {
            return null;
        }
    }
    /**
     * Computes the (scalar) cross product product between two CartCoord2D 
     * objects. The description of this method from the C++ class is "Return
     * the magnitude of the cross product of two CartCoord2Ds."
     * 
     * @param lhs the left hand side operand
     * @param rhs the right hand side operand
     * 
     * @return the cross product
     */
    public static float crossProduct(CartCoord2D lhs, CartCoord2D rhs) {
        return lhs.crossProduct(rhs);
    }
    /**
     * Computes the (scalar) dot product product between two CartCoord2D 
     * objects.
     * 
     * @param lhs the left hand side operand
     * @param rhs the right hand side operand
     * 
     * @return the dot product
     */
    public static float dotProduct(CartCoord2D lhs, CartCoord2D rhs) {
        return lhs.dotProduct(rhs);
    }
    /**
     * Computes the distance between the specified CartCoord2D objects.
     * 
     * @param lhs the left hand side operand
     * @param rhs the right hand side operand
     * 
     * @return the distance between the CartCoord2D objects
     */
    public static float distance(CartCoord2D lhs, CartCoord2D rhs) {
        return lhs.distance(rhs);
    }
    /* end of binary operations */
}
