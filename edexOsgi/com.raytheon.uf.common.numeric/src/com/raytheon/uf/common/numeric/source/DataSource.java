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
package com.raytheon.uf.common.numeric.source;

/**
 * An abstraction for reading numeric data from a 2D grid of numeric data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jun 18, 2012           bsteffen    Initial creation
 * Mar 07, 2014  2791     bsteffen    Move to numeric plugin.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public interface DataSource {

    /**
     * Get a data value at the specified location. If the coordiantes are
     * outside the valid range of data Double.NaN should be returned without an
     * exception.
     * 
     * @param x
     *            x coordiante of data
     * @param y
     *            y coordiante of data
     * @return a data value
     */
    public double getDataValue(int x, int y);

}
