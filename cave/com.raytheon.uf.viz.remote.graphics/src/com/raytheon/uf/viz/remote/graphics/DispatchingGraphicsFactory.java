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
package com.raytheon.uf.viz.remote.graphics;

import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.opengis.coverage.grid.GridEnvelope;

import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.AbstractGraphicsFactoryAdapter;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IView;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.remote.graphics.objects.ViewWrapper;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Graphics factory for creating graphics objects that forward (dispatch)
 * important events for remote processing
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 5, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DispatchingGraphicsFactory extends AbstractGraphicsFactoryAdapter {

    private AbstractGraphicsFactoryAdapter delegate;

    private Dispatcher dispatcher;

    private DispatchingGraphicsFactory(AbstractGraphicsFactoryAdapter delegate,
            Dispatcher dispatcher) {
        this.delegate = delegate;
        this.dispatcher = dispatcher;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.AbstractGraphicsFactoryAdapter#constructView()
     */
    @Override
    public IView constructView() {
        return new ViewWrapper(delegate.constructView());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.AbstractGraphicsFactoryAdapter#constructTarget
     * (org.eclipse.swt.widgets.Canvas, float, float)
     */
    @Override
    public IGraphicsTarget constructTarget(Canvas canvas, float width,
            float height) throws VizException {
        return new DispatchGraphicsTarget(delegate.constructTarget(canvas,
                width, height), canvas, dispatcher);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.AbstractGraphicsFactoryAdapter#constructExtent
     * (com.vividsolutions.jts.geom.Coordinate[])
     */
    @Override
    public IExtent constructExtent(Coordinate[] coords) throws VizException {
        return delegate.constructExtent(coords);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.AbstractGraphicsFactoryAdapter#constructExtent
     * (double, double, double, double)
     */
    @Override
    public IExtent constructExtent(double aMinX, double aMaxX, double aMinY,
            double aMaxY) throws VizException {
        return delegate.constructExtent(aMinX, aMaxX, aMinY, aMaxY);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.AbstractGraphicsFactoryAdapter#constructExtent
     * (org.eclipse.swt.graphics.Rectangle)
     */
    @Override
    public IExtent constructExtent(Rectangle rect) throws VizException {
        return delegate.constructExtent(rect);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.AbstractGraphicsFactoryAdapter#constructExtent
     * (org.opengis.coverage.grid.GridEnvelope)
     */
    @Override
    public IExtent constructExtent(GridEnvelope range) throws VizException {
        return delegate.constructExtent(range);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.AbstractGraphicsFactoryAdapter#constrcutCanvas
     * (org.eclipse.swt.widgets.Composite)
     */
    @Override
    public Canvas constrcutCanvas(Composite canvasComp) throws VizException {
        return delegate.constrcutCanvas(canvasComp);
    }

    /**
     * Injects the DispatchingGraphicsFactory into the given display
     * 
     * @param display
     * @param factory
     * @throws InstantiationException
     */
    public static boolean injectRemoteFunctionality(IDisplayPane pane,
            DispatcherFactory factory) throws InstantiationException {
        boolean injected = false;
        IRenderableDisplay display = pane.getRenderableDisplay();
        AbstractGraphicsFactoryAdapter currAdapter = display
                .getGraphicsAdapter();
        if (currAdapter instanceof DispatchingGraphicsFactory == false) {
            // Wrap the graphics adapter in dispatching one
            display.setGraphicsAdapter(new DispatchingGraphicsFactory(
                    currAdapter, factory.createNewDispatcher(display)));
            refreshPane(pane);
            injected = true;
        }
        return injected;
    }

    /**
     * Extracts the DispatchingGraphicsFactory from the given renderable display
     * 
     * @param display
     */
    public static boolean extractRemoteFunctionality(IDisplayPane pane) {
        boolean extracted = false;
        IRenderableDisplay display = pane.getRenderableDisplay();
        AbstractGraphicsFactoryAdapter adapter = display.getGraphicsAdapter();
        if (adapter instanceof DispatchingGraphicsFactory) {
            AbstractGraphicsFactoryAdapter wrapped = ((DispatchingGraphicsFactory) adapter).delegate;
            display.setGraphicsAdapter(wrapped);
            refreshPane(pane);
            extracted = true;
        }
        return extracted;
    }

    /**
     * Refresh the IDisplayPane and recycle the resources on the pane. Should be
     * executed after graphics injection
     * 
     * @param pane
     */
    private static void refreshPane(IDisplayPane pane) {
        IRenderableDisplay display = pane.getRenderableDisplay();
        IDescriptor descriptor = display.getDescriptor();
        // Force resetting of the pane's display
        pane.setRenderableDisplay(null);
        pane.setRenderableDisplay(display);

        display.setup(pane.getTarget());

        for (ResourcePair rp : descriptor.getResourceList()) {
            if (rp.getResource() != null) {
                rp.getResource().recycle();
            }
        }

        if (descriptor.getTimeMatcher() != null) {
            try {
                descriptor.getTimeMatcher().redoTimeMatching(descriptor);
            } catch (VizException e) {
                Activator.statusHandler.handle(Priority.PROBLEM,
                        "Error redoing time matching", e);
            }
        }

        pane.refresh();
    }
}
