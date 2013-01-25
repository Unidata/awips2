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
package com.raytheon.uf.viz.core.drawables;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBException;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.AbstractGraphicsFactoryAdapter;
import com.raytheon.uf.viz.core.GraphicsFactory;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IView;
import com.raytheon.uf.viz.core.VizConstants;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.preferences.ColorFactory;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IRefreshListener;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.ResourceList.AddListener;
import com.raytheon.uf.viz.core.rsc.ResourceList.RemoveListener;

/**
 * 
 * Abstract renderable display class, implements common functionality between
 * all IRenderableDisplays. Note: classes extending this class should be away
 * that cloneDisplay/createNewDisplay will not work properly unless the
 * extending class has the annotation XmlRootElement above the class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 06, 2009            bgonzale     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public abstract class AbstractRenderableDisplay implements IRenderableDisplay {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractRenderableDisplay.class);

    private static RGB BACKGROUND_COLOR = null;

    protected static RGB getStartingBackgroundColor() {
        if (BACKGROUND_COLOR == null) {
            if (PlatformUI.isWorkbenchRunning()) {
                BACKGROUND_COLOR = ColorFactory.getInstance().getColor(
                        "com.raytheon.uf.viz.core.backgroundColor");
            } else {
                return new RGB(0, 0, 0);
            }
        }
        return BACKGROUND_COLOR;
    }

    public static final int CURSOR_HEIGHT = 18; // guesstimate of cursor

    protected RGB backgroundColor;

    /** The view area */
    private IView view;

    protected Rectangle canvasBounds;

    protected AbstractDescriptor descriptor;

    protected IGraphicsTarget initializedTarget;

    protected IDisplayPaneContainer container;

    /** The blink interval in milliseconds. */
    long blinkInterval = 500;

    /** The last blink time in computer epoch milliseconds */
    long timeLastBlink;

    boolean currentBlinkState;

    protected RenderableDisplayListener listener;

    private boolean swapping = false;

    private Map<String, Object> globals = new HashMap<String, Object>();

    private AbstractGraphicsFactoryAdapter graphicsAdapter;

    public AbstractRenderableDisplay() {
        super();
        this.listener = new RenderableDisplayListener();
        backgroundColor = getStartingBackgroundColor();
        setGraphicsAdapter(GraphicsFactory.getGraphicsAdapter());
    }

    public AbstractRenderableDisplay(IExtent extent, IDescriptor descriptor) {
        this();
        this.setDescriptor(descriptor);
        this.view.setExtent(extent);
    }

    @Override
    public void dispose() {
        if (this.descriptor != null) {
            descriptor.getResourceList().clear();
            this.descriptor.getResourceList().removePostAddListener(
                    this.listener);
            this.descriptor.getResourceList().removePostRemoveListener(
                    this.listener);
        }
        this.initializedTarget = null;
    }

    public IExtent getExtent() {
        return this.view.getExtent();
    }

    public int getWorldHeight() {
        return descriptor.getGridGeometry().getGridRange().getHigh(1) + 1;
    }

    public int getWorldWidth() {
        return descriptor.getGridGeometry().getGridRange().getHigh(0) + 1;
    }

    @Override
    public void recenter(double[] center) {

        try {

            double[] p2 = descriptor.worldToPixel(center);

            // move into the view
            double[] curCenter = getView().getExtent().getCenter();

            double deltaX = (p2[0] - curCenter[0]);
            double deltaY = (p2[1] - curCenter[1]);

            getView().getExtent().shift(deltaX, deltaY);

        } catch (Exception e) {
            statusHandler.handle(Priority.SIGNIFICANT,
                    "Unable to transform map center to display coordinates", e);
        }
    }

    @Override
    public void calcPixelExtent(Rectangle clientArea) {
        view.scaleToClientArea(clientArea, getDimensions());
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.drawables.IRenderableDisplay#isBlinking()
     */
    @Override
    public boolean isBlinking() {
        if (blinkInterval == 0.0) {
            return false;
        }
        for (ResourcePair rp : this.descriptor.getResourceList()) {
            if (rp.getProperties().isBlinking()) {
                return true;
            } else if (rp.getResource() instanceof IResourceGroup) {
                for (ResourcePair rp2 : ((IResourceGroup) rp.getResource())
                        .getResourceList()) {
                    if (rp2.getProperties().isBlinking()) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    @Override
    public boolean isBlinkStateChanged() {
        return (System.currentTimeMillis() - this.timeLastBlink) > blinkInterval;
    }

    @Override
    public IDescriptor getDescriptor() {
        return descriptor;
    }

    /**
     * This setter is needed for JAXB
     * 
     * @param ad
     */
    protected void setAbstractDescriptor(AbstractDescriptor ad) {
        this.setDescriptor(ad);
    }

    @XmlElement(name = "descriptor")
    protected AbstractDescriptor getAbstractDescriptor() {
        return this.descriptor;
    }

    @Override
    public void setDescriptor(IDescriptor desc) {
        if (this.descriptor != null) {
            this.descriptor.getResourceList().removePostAddListener(
                    this.listener);
            this.descriptor.getResourceList().removePostRemoveListener(
                    this.listener);
        }

        this.descriptor = (AbstractDescriptor) desc;
        this.descriptor.getResourceList().addPostAddListener(this.listener);
        this.descriptor.getResourceList().addPostRemoveListener(this.listener);
        this.descriptor.setRenderableDisplay(this);

        customizeResourceList(this.descriptor.getResourceList());
    }

    /**
     * Customize the resource list by adding any renderable display specific
     * resources or listeners. Called right before
     * AbstractRenderableDisplay.setDescriptor finishes. Custom resources should
     * be constructed before being added. It is only recommended to add SYSTEM
     * resources in this method, non system resources such as maps and products
     * should be constructed after the fact due to serialization issues.
     * 
     * @param resourceList
     */
    protected void customizeResourceList(ResourceList resourceList) {

    }

    @Override
    public int[] getDimensions() {
        return new int[] { getWorldWidth(), getWorldHeight(), 0 };
    }

    /**
     * @return the view
     */
    public IView getView() {
        return view;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.IRenderableDisplay#gridToScreen(double
     * [], com.raytheon.uf.viz.core.IGraphicsTarget)
     */
    @Override
    public double[] gridToScreen(double[] grid, IGraphicsTarget target) {
        return this.view.gridToScreen(grid, target);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.IRenderableDisplay#recalcZoomLevel
     * (int[])
     */
    @Override
    public double recalcZoomLevel(int[] dimensions) {
        return this.view.recalcZoomLevel(dimensions);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.IRenderableDisplay#scaleAndBias(double
     * , double, double, com.raytheon.uf.viz.core.IGraphicsTarget)
     */
    @Override
    public void scaleAndBias(double factor, double screenX, double screenY,
            IGraphicsTarget target) {
        this.view.scaleAndBias(factor, screenX, screenY, target);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.IRenderableDisplay#scaleToClientArea
     * (org.eclipse.swt.graphics.Rectangle,
     * com.raytheon.uf.viz.core.drawables.IRenderableDisplay)
     */
    @Override
    public void scaleToClientArea(Rectangle clientArea) {
        this.view.scaleToClientArea(clientArea, getDimensions());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.IRenderableDisplay#screenToGrid(double
     * , double, double, com.raytheon.uf.viz.core.IGraphicsTarget)
     */
    @Override
    public double[] screenToGrid(double x, double y, double depth,
            IGraphicsTarget target) {
        return this.view.screenToGrid(x, y, depth, target);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.IRenderableDisplay#setBounds(org.eclipse
     * .swt.graphics.Rectangle)
     */
    @Override
    public void setBounds(Rectangle bounds) {
        this.canvasBounds = bounds;
    }

    @Override
    public Rectangle getBounds() {
        return canvasBounds;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.IRenderableDisplay#setExtent(com.raytheon
     * .uf.viz.core.IExtent)
     */
    @Override
    public void setExtent(IExtent pe) {
        this.view.setExtent(pe);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.IRenderableDisplay#shiftExtent(double
     * [], double[], com.raytheon.uf.viz.core.IGraphicsTarget)
     */
    @Override
    public void shiftExtent(double[] startScreen, double[] endScreen,
            IGraphicsTarget target) {
        this.view.shiftExtent(startScreen, endScreen, target);
    }

    /**
     * Get Zoom
     * 
     * @return zoom
     */
    public double getZoom() {
        return this.view.getZoom();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.drawables.IRenderableDisplay#zoom(double)
     */
    @Override
    public void zoom(double zoomLevel) {
        this.view.zoom(zoomLevel);
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
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        target.setBackgroundColor(backgroundColor);
    }

    public void setup(IGraphicsTarget target) {
        this.initializedTarget = target;
        this.view.setupView(target);
    }

    protected PaintProperties calcPaintDataTime(PaintProperties paintProps,
            AbstractVizResource<?, ?> rsc) {
        paintProps.setDataTime(descriptor.getTimeForResource(rsc));
        return paintProps;
    }

    protected class RenderableDisplayListener implements AddListener,
            RemoveListener, IRefreshListener {

        @Override
        public void notifyAdd(ResourcePair rp) throws VizException {
            rp.getResource().registerListener(this);
            if (AbstractRenderableDisplay.this.initializedTarget != null) {
                AbstractRenderableDisplay.this.initializedTarget
                        .setNeedsRefresh(true);
            }
        }

        @Override
        public void notifyRemove(ResourcePair rp) throws VizException {

            if (rp.getResource() != null) {
                rp.getResource().unregisterListener(this);
            }

            if (AbstractRenderableDisplay.this.initializedTarget != null) {
                AbstractRenderableDisplay.this.initializedTarget
                        .setNeedsRefresh(true);
            }
        }

        @Override
        public void refresh() {
            if (AbstractRenderableDisplay.this.initializedTarget != null) {
                AbstractRenderableDisplay.this.initializedTarget
                        .setNeedsRefresh(true);
            }
        }

    }

    /*
     * Calculate and return the current boolean blink state.
     * 
     * @return true if current blink is on; false otherwise.
     */
    protected boolean getCurrentBlinkState() {
        if (blinkInterval <= 0) {
            return true;
        }
        if ((System.currentTimeMillis() - this.timeLastBlink) > blinkInterval) {
            currentBlinkState = !currentBlinkState;
            timeLastBlink = System.currentTimeMillis();
        }
        return currentBlinkState;
    }

    @Override
    public void clear() {

    }

    @Override
    public RGB getBackgroundColor() {
        return backgroundColor;
    }

    @Override
    public void setBackgroundColor(RGB backgroundColor) {
        this.backgroundColor = backgroundColor;
    }

    @Override
    public IRenderableDisplay createNewDisplay() {
        try {
            AbstractRenderableDisplay clonedDisplay = (AbstractRenderableDisplay) SerializationUtil
                    .unmarshalFromXml(SerializationUtil.marshalToXml(this));
            List<ResourcePair> rscsToRemove = new ArrayList<ResourcePair>();
            for (ResourcePair rp : clonedDisplay.getDescriptor()
                    .getResourceList()) {
                // Remove any non system resources or map resources
                if (!(rp.getProperties().isMapLayer() || rp.getProperties()
                        .isSystemResource())) {
                    rscsToRemove.add(rp);
                }
            }
            for (ResourcePair rp : rscsToRemove) {
                clonedDisplay.getDescriptor().getResourceList().remove(rp);
            }
            clonedDisplay.setExtent(this.getExtent().clone());
            return clonedDisplay;
        } catch (JAXBException e) {
            e.printStackTrace();
        }
        return null;
    }

    public AbstractRenderableDisplay cloneDisplay() {
        try {
            AbstractRenderableDisplay clonedDisplay = (AbstractRenderableDisplay) SerializationUtil
                    .unmarshalFromXml(SerializationUtil.marshalToXml(this));
            if (getExtent() != null) {
                clonedDisplay.setExtent(this.getExtent().clone());
            }
            return clonedDisplay;
        } catch (JAXBException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error cloning renderable display", e);
        }
        return null;
    }

    public void setSwapping(boolean swapping) {
        this.swapping = swapping;
    }

    public boolean isSwapping() {
        return this.swapping;
    }

    public Map<String, Object> getGlobalsMap() {
        globals.put(VizConstants.FRAME_COUNT_ID, getDescriptor()
                .getFramesInfo().getFrameCount());
        return globals;
    }

    protected long getBlinkInterval() {
        return blinkInterval;
    }

    protected void setBlinkInterval(long blinkInterval) {
        this.blinkInterval = blinkInterval;
    }

    @Override
    public IDisplayPaneContainer getContainer() {
        return container;
    }

    @Override
    public void setContainer(IDisplayPaneContainer container) {
        this.container = container;
    }

    @Override
    public void refresh() {
        listener.refresh();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.IRenderableDisplay#getGraphicsAdapter
     * ()
     */
    @Override
    public AbstractGraphicsFactoryAdapter getGraphicsAdapter() {
        return graphicsAdapter;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.IRenderableDisplay#setGraphicsAdapter
     * (com.raytheon.uf.viz.core.AbstractGraphicsFactoryAdapter)
     */
    @Override
    public void setGraphicsAdapter(AbstractGraphicsFactoryAdapter adapter) {
        if (this.graphicsAdapter != adapter) {
            this.graphicsAdapter = adapter;
            this.view = adapter.constructView();
        }
    }

}
