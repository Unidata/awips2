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
package com.raytheon.viz.grid.rsc;

import javax.measure.unit.Unit;

import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.viz.core.rsc.VizGroupResourceData;

/**
 * FFG Grid Name Generator.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 29, 2011            mpduff      Initial creation
 * Sep 19, 2012  1162      mpudff      Protect against null pointer
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0
 */

public class FfgGridNameGenerator extends GridNameGenerator {

    public FfgGridNameGenerator() {
    }
    /*
     * (non-Javadoc)
     *
     * @see
     * com.raytheon.viz.grid.rsc.GridNameGenerator#getName(com.raytheon.uf.viz
     * .core.rsc.AbstractVizResource)
     */
    @Override
    public String getName(AbstractVizResource<?, ?> absResource) {
        Unit<?> unitObj = ((FFGVizGroupResource) absResource).getCapability(ColorMapCapability.class).getColorMapParameters().getDisplayUnit();
        if (unitObj != null) {
            return ((VizGroupResourceData) absResource.getResourceData()).getName() + "(" + unitObj.toString() + ")";
        }

        return ((VizGroupResourceData) absResource.getResourceData()).getName();
    }

}
