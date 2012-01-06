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
package com.raytheon.uf.viz.d2d.core.sampling;

import java.util.ArrayList;

import org.eclipse.jface.action.IMenuManager;

import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.d2d.core.sampling.CloudHeightResourceData.ICloudHeightAlgorithm;
import com.raytheon.viz.ui.cmenu.IContextMenuContributor;

/**
 * Cloud height resource, uses the ICloudHeightAlgorithm to compute and sample
 * cloud height
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 18, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class CloudHeightResource extends
        AbstractVizResource<CloudHeightResourceData, IMapDescriptor> implements
        IContextMenuContributor {

    private ICloudHeightAlgorithm algorithm;

    /**
     * @param resourceData
     * @param loadProperties
     */
    protected CloudHeightResource(CloudHeightResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        algorithm = resourceData.getAlgorithm();
        dataTimes = new ArrayList<DataTime>();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.cmenu.IContextMenuContributor#addContextMenuItems
     * (org.eclipse.jface.action.IMenuManager, int, int)
     */
    @Override
    public void addContextMenuItems(IMenuManager menuManager, int x, int y) {
        if (algorithm.isEnabled(descriptor)) {
            algorithm.addContextMenuItems(descriptor, menuManager, x, y);
        }
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        try {
            if (algorithm.isEnabled(descriptor)) {
                return algorithm.inspect(this, coord.asLatLon());
            }
        } catch (Exception e) {
            CloudHeightResourceData.handler.handle(Priority.PROBLEM,
                    "Error converting coordinate to lat/lon", e);
        }
        return null;
    }

    @Override
    public DataTime[] getDataTimes() {
        return algorithm.getDataTimes();
    }

    @Override
    public String getName() {
        return "Cloud Height";
    }

    public ICloudHeightAlgorithm getAlgorithm() {
        return algorithm;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#disposeInternal()
     */
    @Override
    protected void disposeInternal() {
        algorithm.dispose();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#paintInternal(com.raytheon
     * .uf.viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#initInternal(com.raytheon
     * .uf.viz.core.IGraphicsTarget)
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {

    }
}
