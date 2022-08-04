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
package com.raytheon.uf.common.dataplugin.radar.level3;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * LinkedVectors are used within {@link SymbologyPacket}s to represent a single
 * graphical line in a specific color.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#  Engineer    Description
 * ------------ -------- ----------- --------------------------
 * Sep 14, 2010           mnash       Initial creation
 * Jun 04, 2014  3232     bsteffen    Remove ISerializableObject
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */
@DynamicSerialize
public class LinkedVector {
    @DynamicSerializeElement
    protected int theColor;

    @DynamicSerializeElement
    public int i1;

    @DynamicSerializeElement
    public int j1;

    @DynamicSerializeElement
    public int i2;

    @DynamicSerializeElement
    public int j2;

    /**
     * @return the i1
     */
    public int getI1() {
        return i1;
    }

    /**
     * @param i1
     *            the i1 to set
     */
    public void setI1(int i1) {
        this.i1 = i1;
    }

    /**
     * @return the j1
     */
    public int getJ1() {
        return j1;
    }

    /**
     * @param j1
     *            the j1 to set
     */
    public void setJ1(int j1) {
        this.j1 = j1;
    }

    /**
     * @return the i2
     */
    public int getI2() {
        return i2;
    }

    /**
     * @param i2
     *            the i2 to set
     */
    public void setI2(int i2) {
        this.i2 = i2;
    }

    /**
     * @return the j2
     */
    public int getJ2() {
        return j2;
    }

    /**
     * @param j2
     *            the j2 to set
     */
    public void setJ2(int j2) {
        this.j2 = j2;
    }

    /**
     * @return the theColor
     */
    public int getTheColor() {
        return theColor;
    }

    /**
     * @param theColor
     *            the theColor to set
     */
    public void setTheColor(int theColor) {
        this.theColor = theColor;
    }

    @Override
    public String toString() {
        // TODO
        return String.format("(%d,%d), (%d,%d)", i1, j1, i2, j2);
    }
}
