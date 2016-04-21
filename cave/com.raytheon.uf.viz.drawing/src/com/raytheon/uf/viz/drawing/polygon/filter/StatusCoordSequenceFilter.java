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
package com.raytheon.uf.viz.drawing.polygon.filter;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.vividsolutions.jts.geom.CoordinateSequenceFilter;

/**
 * An incomplete CoordinateSequenceFilter that is associated with an
 * AbstractVizResource and reports on the status of an apply operation.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 26, 2015  3974      njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public abstract class StatusCoordSequenceFilter implements
        CoordinateSequenceFilter {

    private final AbstractVizResource<?, ?> rsc;

    protected boolean valid = true;

    protected StatusCoordSequenceFilter(AbstractVizResource<?, ?> rsc) {
        this.rsc = rsc;
    }

    protected IDisplayPaneContainer getContainer() {
        return rsc.getResourceContainer();
    }

    @Override
    public boolean isGeometryChanged() {
        return true;
    }

    @Override
    public boolean isDone() {
        // exit before completion if invalid point was encountered
        return !valid;
    }

    /**
     * Resets the state of the filter and returns the state of the previous time
     * the filter was applied to a geometry. If true, the filter completed
     * successfully. If false, the filter had an issue and the modified polygon
     * should not be trusted.
     * 
     * @return the state of the previous run of this filter
     */
    public boolean success() {
        boolean temp = valid;
        valid = true;
        return temp;
    }

}
