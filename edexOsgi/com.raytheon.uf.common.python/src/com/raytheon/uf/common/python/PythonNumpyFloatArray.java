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
package com.raytheon.uf.common.python;

import jep.INumpyable;

/**
 * Simple wrapper of data to send to python
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 10, 2011            rjpeter     Initial creation
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class PythonNumpyFloatArray implements INumpyable {

    private float[] messageData;

    private int nx;

    private int ny;

    public PythonNumpyFloatArray(float[] data, int nx, int ny) {
        this.messageData = data;
        this.nx = nx;
        this.ny = ny;
    }

    /*
     * (non-Javadoc)
     * 
     * @see jep.INumpyable#getNumPy()
     */
    @Override
    public Object[] getNumPy() {
        return new Object[] { messageData };
    }

    /*
     * (non-Javadoc)
     * 
     * @see jep.INumpyable#getNumpyX()
     */
    @Override
    public int getNumpyX() {
        return nx;
    }

    /*
     * (non-Javadoc)
     * 
     * @see jep.INumpyable#getNumpyY()
     */
    @Override
    public int getNumpyY() {
        return ny;
    }

}
