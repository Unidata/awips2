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

package com.raytheon.viz.core.graphing.rsc;

import com.raytheon.viz.core.graphing.axis.IAxis;

/**
 * Interface for xy resources
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 5, 2007             njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 */
public interface IXYResource {

    /**
     * Builds an x axis dependent on the number of graphs on a display
     * 
     * @param numberOfGraphs
     *            number of graphs on a display
     * @return an x axis
     */
    public IAxis buildXAxis(int numberOfGraphs);

    /**
     * Builds a y axis dependent on the number of graphs on a display
     * 
     * @param numberOfGraphs
     *            number of graphs on a display
     * @return a y axis
     */
    public IAxis buildYAxis(int numberOfGraphs);

    /**
     * Gets the minimum data value of the resource
     * 
     * @return the smallest data value
     */
    public double getMinDataValue();

    /**
     * Gets the maximum data value of the resource
     * 
     * @return the largest data value
     */
    public double getMaxDataValue();

    /**
     * Returns whether or not the data is at zero
     * 
     * @return if the data is at zero
     */
    public boolean isDataAtZero();

}
