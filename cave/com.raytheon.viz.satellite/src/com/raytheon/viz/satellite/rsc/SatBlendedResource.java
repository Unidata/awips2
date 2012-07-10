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
package com.raytheon.viz.satellite.rsc;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.drawables.ext.IMosaicImageExtension;
import com.raytheon.uf.viz.core.drawables.ext.IMosaicImageExtension.IMosaicImage;
import com.raytheon.uf.viz.core.drawables.ext.IMosaicOrderedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IRefreshListener;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 18, 2009     2032   jsanchez    Initial Creation.
 *                                      Updated inspect to display a single value.
 * Mar 17, 2009      800   jsanchez    Avoided displaying unnecessary 0.0.
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */

public class SatBlendedResource extends
        AbstractVizResource<SatBlendedResourceData, MapDescriptor> implements
        IResourceGroup, IRefreshListener, IResourceDataChanged {

    private IMosaicImage mosaicImage = null;

    private IExtent lastExtent = null;

    private List<DataTime> lastTimes = null;

    private PixelCoverage imageCoverage = null;

    public SatBlendedResource(SatBlendedResourceData data, LoadProperties props) {
        super(data, props);
        dataTimes = new ArrayList<DataTime>();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#dispose()
     */
    @Override
    protected void disposeInternal() {
        for (ResourcePair rp : this.resourceData.getResourceList()) {
            rp.getResource().dispose();
        }
        disposeImage();
        resourceData.removeChangeListener(this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#init(com.raytheon.uf
     * .viz.core.IGraphicsTarget)
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        resourceData.addChangeListener(this);
        for (ResourcePair rp : getResourceList()) {
            AbstractVizResource<?, ?> rsc = rp.getResource();
            rsc.init(target);
            rsc.registerListener(new IRefreshListener() {
                @Override
                public void refresh() {
                    issueRefresh();
                }
            });
        }
        // make sure we get notified when this resource or any children
        // refreshes.
        this.registerListener(this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#setDescriptor(com.raytheon
     * .uf.viz.core.drawables.IDescriptor)
     */
    @SuppressWarnings("unchecked")
    @Override
    public void setDescriptor(MapDescriptor descriptor) {
        super.setDescriptor(descriptor);
        for (ResourcePair rp : getResourceList()) {
            AbstractVizResource rsc = rp.getResource();
            rsc.setDescriptor(descriptor);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.IRenderable#paint(com.raytheon.uf.
     * viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        ColorMapParameters params = getCapability(ColorMapCapability.class)
                .getColorMapParameters();

        // If first paint, initialize and wait for next paint
        if (mosaicImage == null) {
            initImage(target, paintProps, params);
        } else if (Arrays.equals(
                new int[] { mosaicImage.getWidth(), mosaicImage.getHeight() },
                new int[] { paintProps.getCanvasBounds().width,
                        paintProps.getCanvasBounds().height }) == false) {
            // If Window size changed, recreate the off screen buffer
            disposeImage();
            initImage(target, paintProps, params);
        }

        List<DataTime> rscTimes = new ArrayList<DataTime>();
        for (ResourcePair rp : getResourceList()) {
            AbstractVizResource<?, ?> rsc = rp.getResource();
            if (rsc != null) {
                rscTimes.add(descriptor.getTimeForResource(rsc));
            }
        }

        IExtent extent = paintProps.getView().getExtent().clone();
        if (extent.equals(lastExtent) == false
                || rscTimes.equals(lastTimes) == false) {
            lastTimes = rscTimes;
            lastExtent = extent;
            List<DrawableImage> images = new ArrayList<DrawableImage>();

            for (ResourcePair rp : getResourceList()) {
                AbstractVizResource<?, ?> rsc = rp.getResource();
                DataTime time = paintProps.getFramesInfo().getTimeForResource(
                        rsc);
                if (rsc != null && time != null) {
                    SatResource sr = (SatResource) rsc;
                    DataTime timeForRsc = paintProps.getFramesInfo()
                            .getTimeForResource(rsc);
                    PaintProperties rscProps = new PaintProperties(paintProps);
                    rscProps.setDataTime(timeForRsc);
                    rscProps.setAlpha(1.0f);
                    List<DrawableImage> rscImages = sr.getImages(target,
                            rscProps);
                    for (DrawableImage di : rscImages) {
                        if (di != null && di.getImage() != null
                                && di.getCoverage() != null
                                && di.getCoverage().getMesh() != null) {
                            // If image is ready to go, add
                            images.add(di);
                        }
                    }
                }
            }

            mosaicImage.setImagesToMosaic(images
                    .toArray(new DrawableImage[images.size()]));
            mosaicImage.setImageExtent(extent);

            Coordinate ul = new Coordinate(extent.getMinX(), extent.getMaxY());
            Coordinate ur = new Coordinate(extent.getMaxX(), extent.getMaxY());
            Coordinate lr = new Coordinate(extent.getMaxX(), extent.getMinY());
            Coordinate ll = new Coordinate(extent.getMinX(), extent.getMinY());

            imageCoverage = new PixelCoverage(ul, ur, lr, ll);
        }

        mosaicImage.setContrast(getCapability(ImagingCapability.class)
                .getContrast());
        mosaicImage.setBrightness(getCapability(ImagingCapability.class)
                .getBrightness());

        target.drawRaster(mosaicImage, imageCoverage, paintProps);
    }

    private void initImage(IGraphicsTarget target, PaintProperties paintProps,
            ColorMapParameters params) throws VizException {
        IMosaicImageExtension ext = target
                .getExtension(IMosaicOrderedImageExtension.class);
        if (ext == null) {
            // This could return about any mosaicing algorithm but it is better
            // than drawing nothing
            ext = target.getExtension(IMosaicImageExtension.class);
        }
        // Construct texture for mosaicing
        mosaicImage = ext.initializeRaster(
                new int[] { paintProps.getCanvasBounds().width,
                        paintProps.getCanvasBounds().height }, paintProps
                        .getView().getExtent(), params);
    }

    private void disposeImage() {
        // Dispose of all data, offscreen texture
        if (mosaicImage != null) {
            mosaicImage.dispose();
            mosaicImage = null;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#project(org.opengis.
     * referencing.crs.CoordinateReferenceSystem)
     */
    @Override
    public void project(CoordinateReferenceSystem mapData) throws VizException {
        for (ResourcePair rp : this.resourceData.resourceList) {
            AbstractVizResource<?, ?> rsc = rp.getResource();
            rsc.project(mapData);
        }
        refresh();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.capabilities.IInspectableResource#inspect(com
     * .vividsolutions.jts.geom.Coordinate)
     */
    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        String inspectString = "NO DATA";
        Double inspectValue = null;
        ResourceList list = getResourceList();
        for (int i = list.size() - 1; i >= 0; --i) {
            AbstractVizResource<?, ?> rsc = list.get(i).getResource();
            Map<String, Object> dataMap = rsc.interrogate(coord);
            if (dataMap.get(SatResource.RAW_VALUE) instanceof Double) {
                Double value = (Double) dataMap.get(SatResource.RAW_VALUE);
                if (value.isNaN()) {
                    // This one is no good, skip it.
                    continue;
                }
                if (inspectValue == null || inspectValue == 0.0) {
                    // Either there is no previous value or it was zero, so
                    // try this one.
                    inspectValue = value;
                    inspectString = rsc.inspect(coord);
                }
            }
        }
        return inspectString;
    }

    @Override
    public ResourceList getResourceList() {
        return this.resourceData.getResourceList();
    }

    @Override
    public void refresh() {
        lastExtent = null;
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        refresh();
    }
}
