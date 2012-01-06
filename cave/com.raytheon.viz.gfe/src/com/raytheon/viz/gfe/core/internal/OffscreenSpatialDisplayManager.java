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
package com.raytheon.viz.gfe.core.internal;

import org.eclipse.ui.IWorkbenchWindow;

import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.parm.Parm;

/**
 * Spatial display manager for working offscreen without a display, i.e. with
 * IFP Image
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 30, 2009            njensen     Initial creation
 * Apr 09, 2009 1288       rjpeter     Added new method stubs for ISpatialDisplayManager.
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class OffscreenSpatialDisplayManager extends
        AbstractSpatialDisplayManager {

    private MapDescriptor descriptor;

    public OffscreenSpatialDisplayManager(IWorkbenchWindow window,
            DataManager mgr) {
        super(mgr);
    }

    @Override
    protected MapDescriptor[] getDescriptors() {
        return new MapDescriptor[] { descriptor };
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.ISpatialDisplayManager#getGlobalTimeRange()
     */
    @Override
    public TimeRange getGlobalTimeRange() {
        // TODO Auto-generated method stub
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.ISpatialDisplayManager#toggleVisibility(com
     * .raytheon .viz.gfe.core.parm.Parm)
     */
    @Override
    public void toggleVisibility(Parm parm) {
        // TODO Auto-generated method stub

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.ISpatialDisplayManager#setGlobalTimeRange(com
     * .raytheon.uf.common.time.TimeRange)
     */
    @Override
    public void setGlobalTimeRange(TimeRange timeRange) {
        // TODO Auto-generated method stub

    }

    public void setDescriptor(MapDescriptor desc) {
        descriptor = desc;
    }

}
