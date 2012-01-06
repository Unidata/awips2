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
package com.raytheon.uf.viz.core.legend;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.ResourcePair;

/**
 * Interface that defines the format for legends
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/17/2008              chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public interface ILegendDecorator {

    /*
     * Simple class that represents one line on the legend
     */
    public static class LegendEntry {

        public IFont font;

        public LegendData[] legendParts = new LegendData[1];

    }

    /**
     * Simple data structure containing a label, the color and the resource
     * represented
     * 
     */
    public static class LegendData {
        public RGB color;

        public String label;

        public ResourcePair resource;
    }

    /**
     * Return the legend data back to the user
     * 
     * @param descriptor
     *            the descriptor
     * @return the legend data
     */
    public abstract LegendEntry[] getLegendData(IDescriptor descriptor);

}
