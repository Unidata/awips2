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
package com.raytheon.uf.viz.core.maps.display;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource.ResourceStatus;
import com.raytheon.uf.viz.core.rsc.GenericResourceData;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.core.rsc.legend.DefaultLegendResource;
import com.raytheon.uf.viz.core.rsc.sampling.SamplingResource;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 4, 2009            njensen     Initial creation
 * Mar 3, 2009	 	2032  jsanchez	  Set the paintProps in the rsc.
 * Oct 28, 2009     2354  bsteffen    Moved logic for handling IMiddleClickCapableResource to the input handler so it uses configurable mouse preferences
 * Jul 20, 2010     6187  bkowal      The alpha level will always be reset for every
 *                                    resource when the paint method is called now.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class MapRenderableDisplay extends AbstractRenderableDisplay implements
        ISerializableObject {

    // height in

    /** Center point of the map */
    @XmlAttribute
    protected double[] mapCenter;

    /**
     * The zoom level last used
     */
    @XmlAttribute
    protected double zoomLevel = 1.0f;

    public MapRenderableDisplay() {
        super();
    }

    public MapRenderableDisplay(IMapDescriptor desc) {
        this();
        setDescriptor(desc);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay#calcPixelExtent
     * (org.eclipse.swt.graphics.Rectangle)
     */
    @Override
    public void calcPixelExtent(Rectangle clientArea) {

        // adjust pixel extent to client area
        getView().scaleToClientArea(clientArea, getDimensions());

        // adjust pixel extent for desired map center and zoom level
        if (mapCenter != null) {
            recenter(mapCenter);
        }

        getView().zoom(zoomLevel);

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay#paint(com
     * .raytheon.uf.viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        super.paint(target, paintProps);
        float zoomLevel = paintProps.getZoomLevel();
        LoopProperties loopProperties = paintProps.getLoopProperties();
        this.zoomLevel = zoomLevel;

        // If no loop properties, use the default values
        if (loopProperties == null) {
            loopProperties = new LoopProperties();
        }

        // Calculate the new map center
        this.mapCenter = descriptor.pixelToWorld(paintProps.getView()
                .getExtent().getCenter());

        target.setupClippingPlane(getMapExtent());
        paintProps.setClippingPane(getMapExtent());

        int displayWidth = (int) (((MapDescriptor) descriptor).getMapWidth() * zoomLevel);

        List<ResourcePair> renderingList = new ArrayList<ResourcePair>(
                descriptor.getResourceList());

        for (ResourcePair pair : renderingList) {
            AbstractVizResource<?, ?> rsc = pair.getResource();
            if (rsc == null) {
                continue;
            }

            // ResourceProperties properties = pair.getProperties();

            // if ((rsc.getStatus() == ResourceStatus.NEW || properties
            // .isDisplayable(displayWidth))
            // && (!properties.isBlinking() || getCurrentBlinkState())) {
            if (shouldDisplay(pair, displayWidth)) {
                // always reset the alpha
                paintProps.setAlpha(1.0f);
                if (rsc.hasCapability(ImagingCapability.class)) {
                    paintProps.setAlpha(rsc.getCapability(
                            ImagingCapability.class).getAlpha());
                }

                paintProps = calcPaintDataTime(paintProps, rsc);
                try {
                    rsc.paint(target, paintProps);
                } catch (Throwable e) {
                    pair.getProperties().setVisible(false);
                    throw new VizException("Paint error: " + e.getMessage()
                            + ":: The resource has been disabled.", e);
                }
            }
        }
        target.clearClippingPlane();
    }

    protected boolean shouldDisplay(ResourcePair pair, int displayWidth) {
        AbstractVizResource<?, ?> rsc = pair.getResource();
        ResourceProperties properties = pair.getProperties();

        if (rsc == null) {
            return false;
        }

        ResourceStatus status = rsc.getStatus();

        if (status == ResourceStatus.DISPOSED) {
            return false;
        }

        boolean doNotDrawBecauseOfBlinking = false;
        if (properties.isBlinking()) {
            if (!rsc.hasCapability(ColorMapCapability.class)) {
                // Not a colormapped image...
                doNotDrawBecauseOfBlinking = !getCurrentBlinkState();
            } else {
                ColorMapParameters params = rsc.getCapability(
                        ColorMapCapability.class).getColorMapParameters();
                params.setUseMask(!getCurrentBlinkState());
                // notify the resource it is blinking.
                rsc.issueRefresh();
            }
        }

        if (!doNotDrawBecauseOfBlinking) {
            if (pair.getResource() instanceof IResourceGroup) {
                for (ResourcePair rp : ((IResourceGroup) pair.getResource())
                        .getResourceList()) {
                    doNotDrawBecauseOfBlinking &= shouldDisplay(rp,
                            displayWidth);
                }
            }
        }

        boolean drawBecauseItsNew = status == ResourceStatus.NEW;

        if (!drawBecauseItsNew) {
            if (pair.getResource() instanceof IResourceGroup) {
                for (ResourcePair rp : ((IResourceGroup) pair.getResource())
                        .getResourceList()) {
                    if (rp.getResource() != null) {
                        drawBecauseItsNew |= rp.getResource().getStatus() == ResourceStatus.NEW;
                    }
                }
            }
        }

        return (drawBecauseItsNew || properties.isDisplayable(displayWidth))
                && !doNotDrawBecauseOfBlinking;
    }

    public void setMapCenter(double[] mapCenter) {
        this.mapCenter = mapCenter;
    }

    public void setZoomLevel(double zoomLevel) {
        this.zoomLevel = zoomLevel;
    }

    public IExtent getMapExtent() {
        return new PixelExtent(descriptor.getGridGeometry().getGridRange());
    }

    public boolean handleClick(int x, int y, int button, IGraphicsTarget target)
            throws VizException {
        // This logic has been moved to MouseInspectAdapter
        return false;
    }

    @Override
    protected void customizeResourceList(ResourceList resourceList) {
        resourceList.add(ResourcePair
                .constructSystemResourcePair(new GenericResourceData(
                        DefaultLegendResource.class)));
        resourceList.add(ResourcePair
                .constructSystemResourcePair(new GenericResourceData(
                        SamplingResource.class)));
    }

    @Override
    public void recenter(double[] center) {
        setMapCenter(center);
        super.recenter(center);
    }

    @Override
    public void zoom(double zoomLevel) {
        setZoomLevel(zoomLevel);
        super.zoom(zoomLevel);
    }

    @Override
    public void scaleAndBias(double factor, double screenX, double screenY,
            IGraphicsTarget target) {
        super.scaleAndBias(factor, screenX, screenY, target);
        setZoomLevel(recalcZoomLevel(getDimensions()));
        setMapCenter(descriptor.pixelToWorld(getView().getExtent().getCenter()));
    }

    @Override
    public void shiftExtent(double[] startScreen, double[] endScreen,
            IGraphicsTarget target) {
        super.shiftExtent(startScreen, endScreen, target);
        setMapCenter(descriptor.pixelToWorld(getView().getExtent().getCenter()));
    }

    @Override
    public void setExtent(IExtent pe) {
        super.setExtent(pe);
        setZoomLevel(recalcZoomLevel(getDimensions()));
        setMapCenter(descriptor.pixelToWorld(getView().getExtent().getCenter()));
    }

}
