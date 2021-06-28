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
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.measure.Measure;

import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelCoverage;
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
import com.raytheon.uf.viz.core.rsc.interrogation.Interrogatable;
import com.raytheon.uf.viz.core.rsc.interrogation.InterrogateMap;
import com.raytheon.uf.viz.core.rsc.interrogation.InterrogationKey;
import com.raytheon.uf.viz.core.rsc.interrogation.Interrogator;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Displays multiple satellite resources in a single resource. Uses graphics
 * mosaicing to combine images so that alhpa blending correctly treats multiple
 * images as a single layer when applying the alpha.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Feb 18, 2009  2032     jsanchez  Initial Creation. Updated inspect to display
 *                                  a single value.
 * Mar 17, 2009  800      jsanchez  Avoided displaying unnecessary 0.0.
 * Jul 31, 2013  2190     mschenke  Removed arbitrary check for 0.0 and instead
 *                                  only check for NaN.  SatResource handles
 *                                  fill values and returns NaN now
 * Nov 18, 2013  2544     bsteffen  Override recycleInternal
 * Nov 20, 2013  2492     bsteffen  Update inspect to use Measure objects
 * Oct 27, 2014  3681     bsteffen  Implement Interrogatable
 * Nov 28, 2017  5863     bsteffen  Change dataTimes to a NavigableSet
 * 
 * </pre>
 * 
 * @author jsanchez
 */
public class SatBlendedResource extends
        AbstractVizResource<SatBlendedResourceData, MapDescriptor> implements
        IResourceGroup, IRefreshListener, IResourceDataChanged, Interrogatable {

    private IMosaicImage mosaicImage = null;

    private IExtent lastExtent = null;

    private List<DataTime> lastTimes = null;

    private PixelCoverage imageCoverage = null;

    public SatBlendedResource(SatBlendedResourceData data,
            LoadProperties props) {
        super(data, props, false);
    }

    @Override
    protected void disposeInternal() {
        for (ResourcePair rp : this.resourceData.getResourceList()) {
            rp.getResource().dispose();
        }
        disposeImage();
        resourceData.removeChangeListener(this);
    }

    @Override
    protected void recycleInternal() {
        for (ResourcePair rp : this.resourceData.getResourceList()) {
            rp.getResource().recycle();
        }
        disposeImage();
    }

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

    @SuppressWarnings("unchecked")
    @Override
    public void setDescriptor(MapDescriptor descriptor) {
        super.setDescriptor(descriptor);
        for (ResourcePair rp : getResourceList()) {
            AbstractVizResource rsc = rp.getResource();
            rsc.setDescriptor(descriptor);
        }
    }

    private List<DrawableImage> images = new ArrayList<>();
    @Override
    /*
     * For the mac build, skip the use of the mosaicImage completely
     * and simply draw the individual SatResource images.
     * (non-Javadoc)
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#paintInternal(com.raytheon.uf.viz.core.IGraphicsTarget, com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        
        PaintProperties rscProps = new PaintProperties(paintProps);
        
        List<DataTime> rscTimes = new ArrayList<>();
        for (ResourcePair rp : getResourceList()) {
            AbstractVizResource<?, ?> rsc = rp.getResource();
            if (rsc != null) {
                rscTimes.add(descriptor.getTimeForResource(rsc));
            }
        }

        IExtent extent = paintProps.getView().getExtent().clone();
        if (!extent.equals(lastExtent) || !rscTimes.equals(lastTimes)) {
            lastTimes = rscTimes;
            lastExtent = extent;
            List<DrawableImage> tempImages = new ArrayList<>();

            for (ResourcePair rp : getResourceList()) {
                AbstractVizResource<?, ?> rsc = rp.getResource();
                DataTime time = paintProps.getFramesInfo()
                        .getTimeForResource(rsc);
                if (rsc != null && time != null) {
                    SatResource sr = (SatResource) rsc;
                    DataTime timeForRsc = paintProps.getFramesInfo()
                            .getTimeForResource(rsc);
                    rscProps.setDataTime(timeForRsc);
                    rscProps.setAlpha(1.0f);
                    List<DrawableImage> rscImages = sr.getImages(target,
                            rscProps);
                    
                    for (DrawableImage di : rscImages) {
                        if (di != null && di.getImage() != null
                                && di.getCoverage() != null
                                && di.getCoverage().getMesh() != null) {
                        	
                            // If image is ready to go, add
                            tempImages.add(di);
                        }
                    }
                }
            }
            
            if(tempImages.size()>0){
            	images = new ArrayList(tempImages);
            }

        }

        if(images.size()>0){
        	target.drawRasters(rscProps, images.toArray(new DrawableImage[0]));
        }
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
                        paintProps.getCanvasBounds().height },
                paintProps.getView().getExtent(), params);
    }

    private void disposeImage() {
        // Dispose of all data, offscreen texture
        if (mosaicImage != null) {
            mosaicImage.dispose();
            mosaicImage = null;
        }
    }

    @Override
    public void project(CoordinateReferenceSystem mapData) throws VizException {
        for (ResourcePair rp : this.resourceData.resourceList) {
            AbstractVizResource<?, ?> rsc = rp.getResource();
            rsc.project(mapData);
        }
        refresh();
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        String inspectString = "NO DATA";
        ResourceList list = getResourceList();
        for (int i = list.size() - 1; i >= 0; --i) {
            AbstractVizResource<?, ?> rsc = list.get(i).getResource();
            Map<String, Object> dataMap = rsc.interrogate(coord);
            Measure<?, ?> value = (Measure<?, ?>) dataMap
                    .get(SatResource.SATELLITE_DATA_INTERROGATE_ID);
            if (value != null && value.getValue() instanceof Number) {
                double measuredValue = ((Number) value.getValue())
                        .doubleValue();
                if (!Double.isNaN(measuredValue)) {
                    // use this resource
                    inspectString = rsc.inspect(coord);
                    break;
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

    @Override
    public Set<InterrogationKey<?>> getInterrogationKeys() {
        Set<InterrogationKey<?>> set = new HashSet<>();
        List<Interrogatable> resourceList = getResourceList()
                .getResourcesByTypeAsType(Interrogatable.class);
        for (Interrogatable resource : resourceList) {
            set.addAll(resource.getInterrogationKeys());
        }
        return set;
    }

    @Override
    public InterrogateMap interrogate(ReferencedCoordinate coordinate,
            DataTime time, InterrogationKey<?>... keys) {
        if (!Arrays.asList(keys).contains(Interrogator.VALUE)) {
            keys = Arrays.copyOf(keys, keys.length + 1);
            keys[keys.length - 1] = Interrogator.VALUE;
        }
        List<Interrogatable> list = getResourceList()
                .getResourcesByTypeAsType(Interrogatable.class);
        Collections.reverse(list);
        for (Interrogatable resource : list) {
            InterrogateMap result = resource.interrogate(coordinate, time,
                    keys);
            Measure<? extends Number, ?> value = result.get(Interrogator.VALUE);
            if (value != null) {
                double quantity = value.getValue().doubleValue();
                if (!Double.isNaN(quantity)) {
                    return result;
                }
            }
        }
        return new InterrogateMap();
    }
}
