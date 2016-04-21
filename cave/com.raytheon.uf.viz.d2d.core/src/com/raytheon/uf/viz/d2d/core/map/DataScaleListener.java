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
package com.raytheon.uf.viz.d2d.core.map;

import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceList.AddListener;
import com.raytheon.uf.viz.core.rsc.ResourceList.RemoveListener;
import com.raytheon.uf.viz.core.rsc.capabilities.BlendableCapability;
import com.raytheon.uf.viz.d2d.core.DataScale;
import com.raytheon.viz.core.rsc.BestResResource;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Resource add/remove listener for doing data scale changing with the data
 * scale set in the options menu
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 20, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DataScaleListener implements AddListener, RemoveListener {

    private ResourcePair scaleResource;

    private D2DMapRenderableDisplay renderableDisplay;

    public DataScaleListener(D2DMapRenderableDisplay display) {
        this.renderableDisplay = display;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.ResourceList.AddListener#notifyAdd(com.raytheon
     * .uf.viz.core.drawables.ResourcePair)
     */
    @Override
    public void notifyAdd(ResourcePair rp) throws VizException {
        if (isDataScale() && scaleResource == null) {
            checkDataScaleResource(null, rp);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.ResourceList.RemoveListener#notifyRemove
     * (com.raytheon.uf.viz.core.drawables.ResourcePair)
     */
    @Override
    public void notifyRemove(ResourcePair rp) throws VizException {
        if (scaleResource != null && rp == scaleResource) {
            scaleResource = null;
            if (isDataScale()) {
                checkDataScaleResourceList(null, renderableDisplay
                        .getDescriptor().getResourceList());
            }
        }
    }

    /**
     * Checks the DataScale class for data scaled selected on the active window.
     * Must be run on UI Thread
     * 
     * @return
     */
    private boolean isDataScale() {
        return DataScale.isDataScale();
    }

    /**
     * Checks for IDataScaleInterface on the resource
     * 
     * @param parent
     * @param rp
     * @return
     * @throws VizException
     */
    private boolean checkDataScaleResource(ResourcePair parent, ResourcePair rp)
            throws VizException {
        if (rp.getResource() != null) {
            parent = parent == null ? rp : parent;
            if (rp.getResource() instanceof IDataScaleResource) {
                scaleResource = parent;

                IDataScaleResource dsr = (IDataScaleResource) rp.getResource();
                Coordinate centerPoint = dsr.getCenterLocation();
                if (centerPoint != null) {
                    CoordinateReferenceSystem crs = MapUtil
                            .constructStereographic(MapUtil.AWIPS_EARTH_RADIUS,
                                    MapUtil.AWIPS_EARTH_RADIUS, centerPoint.y,
                                    centerPoint.x);
                    GridGeometry2D gridGeom = MapDescriptor.createGridGeometry(
                            crs, centerPoint, 500000, 500000);
                    renderableDisplay.getDescriptor().setGridGeometry(gridGeom);
                    renderableDisplay.setScaleOnNextPaint(true);
                    return true;
                }
            } else if (rp.getResource()
                    .hasCapability(BlendableCapability.class)) {
                return checkDataScaleResourceList(parent, rp.getResource()
                        .getCapability(BlendableCapability.class)
                        .getResourceList());
            } else if (rp.getResource() instanceof IResourceGroup) {
                return checkDataScaleResourceList(parent, ((IResourceGroup) rp
                        .getResource()).getResourceList());
            } else if (rp.getResource() instanceof BestResResource) {
                return checkDataScaleResourceList(parent, ((BestResResource) rp
                        .getResource()).getResourceList());
            }
        }
        return false;
    }

    private boolean checkDataScaleResourceList(ResourcePair parent,
            ResourceList list) throws VizException {
        boolean rval = false;
        for (ResourcePair pair : list) {
            if (checkDataScaleResource(parent, pair)) {
                rval = true;
                break;
            }
        }
        return rval;
    }
}
