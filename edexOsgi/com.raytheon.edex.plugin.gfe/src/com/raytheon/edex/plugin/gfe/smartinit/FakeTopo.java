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
package com.raytheon.edex.plugin.gfe.smartinit;

import jep.INumpyable;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * May 1, 2008				njensen	Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class FakeTopo implements INumpyable {

    // TODO need real topo databases!

    private static final int x = 145;

    private static final int y = 145;

    /*
     * (non-Javadoc)
     * 
     * @see jep.INumpyable#getNumPy()
     */
    @Override
    public Object[] getNumPy() {
        return new Object[] { new float[x * y] };
    }

    /*
     * (non-Javadoc)
     * 
     * @see jep.INumpyable#getNumpyX()
     */
    @Override
    public int getNumpyX() {
        return x;
    }

    /*
     * (non-Javadoc)
     * 
     * @see jep.INumpyable#getNumpyY()
     */
    @Override
    public int getNumpyY() {
        return y;
    }

}
