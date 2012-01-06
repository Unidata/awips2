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
 * Contains mathematics routines intended primarily for GRIB record manipulations.
 * This class is intended to support the &mu;Engine's Jython scripting tags.
 * <P>
 * <b>Usage:</B>
 * <PRE>
 *   {@literal <mathScript using="gribout">}
 *      {@literal <input name="u" value="uwind" />}
 *      {@literal <input name="v" value="vwind" />}
 *      {@literal <param name="red" value="2.0" />}
 *      {@literal <param name="yellow" value="1.0" />}
 *      {@literal <param name="green" value="0.0" />}
 *      {@literal <![CDATA[}
 *   {@literal import com.raytheon.edex.uengine.util.jython as utilities}
 *   {@literal utils = utilities.JMath()}
 *   {@literal STDOUT = utils.awndspd(u,v)}
 *   {@literal STDOUT = utils.astoplight(STDOUT,red,yellow,green)}
 *   {@literal ]]>}
 *   {@literal </mathScript>}
 * </PRE>
 * This is an example of using the {@code <mathScript />} &mu;Engine task to
 * perform a wind speed analysis on previously retrieved grib records and then
 * producing a red, yellow, green stop light chart from the wind speeds.
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 06Nov2006    TO4         MW Fegan    Initial creation.
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1
 */

public class JMath {
    
    /*
     * constants.
     */
    private static final float NULL = (float)9999;

    /**
     * Contructor.
     *
     */
    public JMath() {
        super();
    }
    /**
     * Converts a grid to a stoplight chart. This is a demo function that uses
     * a decile based formula for the coloring.
     * <PRE>
     * {@literal decile     0      1      2      3      4      5      6      7      8      9}
     * {@literal color     red  yellow  green  green  green  green  green  green  yellow  red}
     * </PRE>
     * @param data GRIB record to be used for the chart.
     * @param red grib value for "red"
     * @param yellow grib value for "yellow"
     * @param green grib value for "green"
     * 
     * @return the transformed grib record.
     */
    public float[] astoplight(float[] data,
                              float red, 
                              float yellow, 
                              float green) {
        return astoplight(data,red,yellow,green,yellow,red);
    }
    /**
     * Converts a grid to a stoplight chart. This is a demo function that uses
     * a decile based formula for the coloring.
     * <PRE>
     * {@literal decile     0      1      2      3      4      5      6      7      8      9}
     * {@literal region     I     II     III    III    III    III    III    III    IV      V}
     * </PRE>
     * 
     * @param data GRIB record to be used for the chart.
     * @param clr0 new grib value for region I
     * @param clr1 new grib value for region II
     * @param clr2 new grib value for region III
     * @param clr3 new grib value for region IV
     * @param clr4 new grib value for region V
     * 
     * @return the transformed grib record.
     */
    public float[] astoplight(float[] data,
                              float clr0, 
                              float clr1, 
                              float clr2, 
                              float clr3, 
                              float clr4) {
        float min = amin(data);
        float max = amax(data);
        float rng = max - min;
        float p = rng / (float)10.0;
        float p1 = min + p;
        float p2 = p1 + p;
        float p3 = p2 + (float)6 * p;
        float p4 = p3 + p;
        float[] retVal = new float[data.length];
        for (int i = 0; i < data.length; i++) {
            float z = data[i];
            if (z == NULL) {
                retVal[i] = clr0;
            } else if (z < p1) {
                retVal[i] = clr0;
            } else if (z < p2) {
                retVal[i] = clr1;
            } else if (z < p3) {
                retVal[i] = clr2;
            } else if (z < p4) {
                retVal[i] = clr3;
            } else {
                retVal[i] = clr4;
            }
        }
        return retVal;
        
    }
    /**
     * Creates a {@code float[]} by taking the maximum value of corresponding elements
     * of the two {@code float[]} input array. That is, c<sub>i</sub> = max(a<sub>i</sub>,
     * b<sub>i</sub>).
     * 
     * @param a the first array.
     * @param b the recond array.
     * 
     * @return the max array.
     */
    public float[] amax(float[] a, float[] b) {
        if(a.length != b.length) {
            return (float[])null;
        }
        float[] retVal = new float[a.length];
        for (int i = 0; i < a.length;i++) {
            retVal[i] = Math.max(a[i], b[i]);
        }
        return retVal;
    }
    /**
     * Creates an {@code int[]} by taking the maximum value of corresponding elements
     * of the two {@code int[]} input array. That is, c<sub>i</sub> = max(a<sub>i</sub>,
     * b<sub>i</sub>).
     * 
     * @param a the first array.
     * @param b the recond array.
     * 
     * @return the max array.
     */
    public int[] amax(int[] a, int[] b) {
        if(a.length != b.length) {
            return (int[])null;
        }
        int[] retVal = new int[a.length];
        for (int i = 0; i < a.length;i++) {
            retVal[i] = Math.max(a[i], b[i]);
        }
        return retVal;
    }
    /**
     * Creates a {@code float[]} by taking the average of the correcponding elements of
     * the two {@code float[]} inputs. That is, c<sub>i</sub> = (a<sub>i</sub> +
     * b<sub>i</sub>) / 2.0.
     * 
     * @param a the first array.
     * @param b the recond array.
     * 
     * @return the mean array.
     */
    public float[] amean(float[] a, float[] b) {
        if(a.length != b.length) {
            return (float[])null;
        }
        float[] retVal = new float[a.length];
        for (int i = 0; i < a.length;i++) {
            retVal[i] = (a[i] + b[i]) / (float)2;
        }
        return retVal;
    }
    /**
     * Creates an {@code int[]} by taking the average of the corresponding elements of
     * the two {@code int[]} inputs. That is, c<sub>i</sub> = (a<sub>i</sub> +
     * b<sub>i</sub>) / 2.
     * 
     * @param a the first array.
     * @param b the recond array.
     * 
     * @return the mean array.
     */
    public int[] amean(int[] a, int[] b) {
        if(a.length != b.length) {
            return (int[])null;
        }
        int[] retVal = new int[a.length];
        for (int i = 0; i < a.length;i++) {
            retVal[i] = (a[i] + b[i]) / 2;
        }
        return retVal;
    }
    /**
     * Performs the wind speed computation for a pair of wind-u, wind-v buffers.
     * 
     * @param u the wind-u buffer.
     * @param v the wind-v buffer.
     * 
     * @return a buffer containing the wind speeds.
     */
    public float[] awndspd(float[] u, float[] v) {
        if(u.length != v.length) {
            return (float[])null;
        }
        float[] retVal = new float[u.length];
        for (int i = 0; i < u.length; i++) {
            Vector vector = new Vector(Vector.VECTOR_UV,u[i],v[i]);
            retVal[i] = vector.getMagnitude();
        }
        return retVal;
    }
    /**
     * Performs the wind direction computation for a pair of wind-u, wind-v buffers.
     * 
     * @param u the wind-u buffer.
     * @param v the wind-v buffer.
     * 
     * @return a buffer containing the wind speeds.
     */
    public float[] awnddir(float[] u, float[] v) {
        if(u.length != v.length) {
            return (float[])null;
        }
        float[] retVal = new float[u.length];
        for (int i = 0; i < u.length; i++) {
            Vector vector = new Vector(Vector.VECTOR_UV,u[i],v[i]);
            retVal[i] = vector.getDirection();
        }
        return retVal;        
    }
    /**
     * finds the minimum value of a {@code float[]} array.
     * 
     * @param a the buffer containing the values to process.
     * 
     * @return the minimum.
     */
    public float amin(float[] a) {
        float retVal = a[0];
        for (float f : a) {
            if(f < retVal) {
                retVal = f;
            }
        }
        return retVal;
    }
    /**
     * finds the minimum value of a {@code int[]} array.
     * 
     * @param a the buffer containing the values to process.
     * 
     * @return the minimum.
     */
    public int amin(int[] a) {
        int retVal = a[0];
        for (int f : a) {
            if(f < retVal) {
                retVal = f;
            }
        }
        return retVal;
    }
    /**
     * finds the maximum value of a {@code float[]} array.
     * 
     * @param a the buffer containing the values to process.
     * 
     * @return the maximum.
     */
    public float amax(float[] a) {
        float retVal = a[0];
        for (float f : a) {
            if(f > retVal) {
                retVal = f;
            }
        }
        return retVal;
    }
    /**
     * finds the maximum value of a {@code int[]} array.
     * 
     * @param a the buffer containing the values to process.
     * 
     * @return the maximum.
     */
    public int amax(int[] a) {
        int retVal = a[0];
        for (int f : a) {
            if(f > retVal) {
                retVal = f;
            }
        }
        return retVal;
    }
}
