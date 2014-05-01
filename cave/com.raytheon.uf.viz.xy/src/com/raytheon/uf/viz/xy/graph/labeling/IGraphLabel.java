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
package com.raytheon.uf.viz.xy.graph.labeling;

import com.raytheon.uf.viz.core.rsc.AbstractVizResource;

/**
 * label to be displayed on the graph, toLabelString() will be called on this
 * method to get the text, the Discrete value is used to determine differences
 * between labels
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 30, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public interface IGraphLabel<T> {

    /** The delimiter used to parse the label */
    public static final String DELIMETER = "\n";

    /**
     * A numerical value representing the label
     * 
     * @return the value
     */
    public double getDiscreteValue();

    /**
     * The text to display on the graph
     * 
     * @return the text
     */
    public String toLabelString();

    /**
     * Return the underlying object representing the label
     * 
     * @return
     */
    public T getUnderlyingObject();

    /**
     * Get the resource associated with the label
     * 
     * @return
     */
    public AbstractVizResource<?, ?> getResource();

    /**
     * Set the resource associated with the color
     * 
     * @param resource
     */
    public void setResource(AbstractVizResource<?, ?> resource);
}
