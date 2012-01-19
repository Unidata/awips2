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

package com.raytheon.edex.uengine.util;

/**
 * Represents a two dimensional vector.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 10Nov2006    TO4         MW Fegan    Iniitial creation.
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1
 */

public class Vector {
    /**
     * Initialization constant. Indicates the vector is being initialized using
     * U,V coordinates 
     */
    public static final int VECTOR_UV = 0;
    /**
     * Initialization constant. Indicated the vector is being initialized using
     * &rho;,&theta; (rho,theta) coordinates.
     */
    public static final int VECTOR_RT = 1;
    /*
     * the vector components
     */
    private float vectorU = 0;
    private float vectorV = 0;
    private float vectorRho = 0;
    private float vectorTheta = 0;
    
    /*
     * constants for vector resolution
     */
    private static final double RDNTODEG = 57.2957795;
    private static final double DEG_HALF_PI = 180.0;
    private static final double R_EQ_ZERO = 1.0E-10;

    /**
     * Constructor. Creates a vector from the specified coordinates. The coorinate
     * system, rectangular or polar, is determined by the {@code system} argument.
     * 
     * @param system determines the coordinate system of the vector. Allowed values
     *               are {@link #VECTOR_RT} or {@link #VECTOR_UV}.
     * @param abscissa the first coordinate (U or &rho;)
     * @param ordinate the second coordinate (V or &theta)
     */
    public Vector(int system, float abscissa, float ordinate) {
        createVector(system,abscissa, ordinate);
    }
    /**
     * Constructor. Creates a vector from the specified coordinates. The vector is
     * assumed to be in rectangular coordinates.
     * 
     * @param abscissa the first coordinate
     * @param ordinate the second coordinate
     */
    public Vector (float abscissa, float ordinate) {
        createVector(VECTOR_UV,abscissa, ordinate);
    }
    /**
     * Resolves the vector using the specified coordinate system. 
     *  
     * @param system determines the coordinate system for resolving the vector. 
     *               Allowed values are {@link #VECTOR_RT} or {@link #VECTOR_UV}.
     */
    public void resolve(int system) {
        if(system == VECTOR_UV) {
            resolveUV();
        } else if (system == VECTOR_RT) {
            resolveRT();
        } else {
            clear();
        }
    }
    /**
     * Resets the vector to the initial state. All components are set to {@code (float)0}.
     *
     */
    public void clear() {
        vectorRho = (float)0;
        vectorTheta = (float)0;
        vectorU = (float)0;
        vectorV = (float)0;
    }
    /*
     * helper methods
     */
    /**
     * Populates the vector. Missing components are computed from the coordinates provided.
     * 
     * @param system system flag. Must be{@link #VECTOR_RT} or {@link #VECTOR_UV}.
     * @param abscissa the first coordinate (&rho; or U)
     * @param ordinate the first coordinate (&theta; or V)
     */
    private void createVector(int system, float abscissa, float ordinate) {
        if (system == VECTOR_UV) {
            vectorU = abscissa;
            vectorV = ordinate;
            resolveUV();
        } else if (system == VECTOR_RT) {
            vectorRho = abscissa;
            vectorTheta = ordinate;
            resolveRT();
        }
    }
    /**
     * Resolves the vector when U,V coordinates have been set. The &rho; and &theta;
     * coordinates are set as a result. 
     */
    private void resolveUV() {
        this.vectorRho = magnitude(this.vectorU,this.vectorV);
        this.vectorTheta = direction(this.vectorU,this.vectorV);
    }
    /**
     * Resolves the vector when &rho;,&theta; coordinates have been set. The U and V
     * coordinates are set as a result.
     *
     */
    private void resolveRT() {
        this.vectorU = this.vectorRho * (float)Math.cos((double)this.vectorTheta);
        this.vectorV = this.vectorRho * (float)Math.sin((double)this.vectorTheta);
    }
    /**
     * Comuptes the magnitude of the specified vector.
     *  
     * @param u the "horizontal" component.
     * @param v the "vertical" component.
     * 
     * @return the magnitude of the vector.
     */
    private float magnitude(float u, float v) {
        return (float)Math.sqrt(u * u + v * v);
    }
    
    /**
     * computes the direction of the specified vector. The direction is returned
     * as degrees on the interval 0 &le; direction &le; 360.
     *  
     * @param u the "horizontal" component.
     * @param v the "vertical" component.
     * 
     * @return the direction
     */
    private float direction(float u, float v) {
       float xspd = (float)Math.sqrt(u * u + v * v);
       float dir;
       if (xspd < R_EQ_ZERO) {
           dir = (float)0.0;
       } else {
           dir = (float)(Math.atan2(u, v) * RDNTODEG  + DEG_HALF_PI + 1.0E-3) ;  
       }
       return dir;
    }
    /*
     * Accessors
     */
    
    /**
     * @return the vector magnitude
     */
    public float getMagnitude() {
        return vectorRho;
    }
    /**
     * @param magnitude the magnitude to set
     */
    public void setMagnitude(float magnitude) {
        this.vectorRho = magnitude;
    }
    /**
     * @return the vector direction
     */
    public float getDirection() {
        return vectorTheta;
    }
    /**
     * @param direction the vector direction to set
     */
    public void setDirection(float direction) {
        this.vectorTheta = direction;
    }
    /**
     * @return the vectorU
     */
    public float getVectorU() {
        return vectorU;
    }
    /**
     * @param vectorU the vectorU to set
     */
    public void setVectorU(float vectorU) {
        this.vectorU = vectorU;
    }
    /**
     * @return the vectorV
     */
    public float getVectorV() {
        return vectorV;
    }
    /**
     * @param vectorV the vectorV to set
     */
    public void setVectorV(float vectorV) {
        this.vectorV = vectorV;
    }
    
}
