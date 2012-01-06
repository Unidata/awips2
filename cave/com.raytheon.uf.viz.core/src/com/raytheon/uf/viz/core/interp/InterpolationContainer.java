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
package com.raytheon.uf.viz.core.interp;

import jep.INumpyable;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 8, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class InterpolationContainer implements INumpyable {

    private float[] xValues;

    private float[] yValues;

    private float[] zValues;

    private float[] xSpace;

    private float[] ySpace;

    private float[] interpValues;

    public InterpolationContainer(float[] xValues, float[] yValues,
            float[] zValues) {
        this.xValues = xValues;
        this.yValues = yValues;
        this.zValues = zValues;
    }

    /*
     * (non-Javadoc)
     * 
     * @see jep.INumpyable#getNumPy()
     */
    @Override
    public Object[] getNumPy() {
        // todo
        return new Object[] { xValues, yValues, zValues };
    }

    /*
     * (non-Javadoc)
     * 
     * @see jep.INumpyable#getNumpyX()
     */
    @Override
    public int getNumpyX() {
        return xValues.length;
    }

    /*
     * (non-Javadoc)
     * 
     * @see jep.INumpyable#getNumpyY()
     */
    @Override
    public int getNumpyY() {
        return 1;
    }

    public void setResults(float[] XI, float[] YI, float[] ZI) {
        this.xSpace = XI;
        this.ySpace = YI;
        this.interpValues = ZI;
    }

    public float[] getXSpace() {
        return xSpace;
    }

    public float[] getYSpace() {
        return ySpace;
    }

    public float[] getInterpValues() {
        return interpValues;
    }

    public float[] getXValues() {
        return xValues;
    }

    public float[] getYValues() {
        return yValues;
    }

    public float[] getZValues() {
        return zValues;
    }

    public void setXValues(float[] values) {
        xValues = values;
    }

    public void setYValues(float[] values) {
        yValues = values;
    }

    public void setZValues(float[] values) {
        zValues = values;
    }
}
