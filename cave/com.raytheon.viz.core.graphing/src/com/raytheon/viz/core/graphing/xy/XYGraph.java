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

package com.raytheon.viz.core.graphing.xy;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.viz.core.ColorUtil;
import com.raytheon.viz.core.graphing.GraphProperties;
import com.raytheon.viz.core.graphing.IGraph;
import com.raytheon.viz.core.graphing.axis.IAxis;
import com.raytheon.viz.core.graphing.rsc.IXYResource;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Graph descriptor for xy graphs
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 21, 2007            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 */
public class XYGraph implements IRenderable, IGraph {

    protected IExtent worldExtent;

    protected IAxis domainAxis;

    protected IAxis rangeAxis;

    protected boolean shown = true;

    protected RGB color = ColorUtil.DEFAULT_ITEM_COLOR;

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.drawables.IRenderableDisplay#paint(com.raytheon
     * .viz.core.IGraphicsTarget, float, float)
     */
    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        GraphProperties graphProps = (GraphProperties) paintProps;
        graphProps.setGraph(this);
        domainAxis.paint(target, graphProps);
        rangeAxis.paint(target, graphProps);
    }

    public void init(IXYResource aRsc, int numberOfGraphs) {
        rangeAxis = aRsc.buildYAxis(numberOfGraphs);
        domainAxis = aRsc.buildXAxis(numberOfGraphs);
        if (aRsc instanceof AbstractVizResource<?, ?>) {
            AbstractVizResource<?, ?> vrsc = (AbstractVizResource<?, ?>) aRsc;
            if (vrsc.getCapabilities().hasCapability(ColorableCapability.class)) {
                color = vrsc.getCapability(ColorableCapability.class)
                        .getColor();
                domainAxis.setColor(color);
                rangeAxis.setColor(color);
            }
        }
    }

    public void init(IAxis aDomainAxis, IAxis aRangeAxis, RGB aColor,
            int numberOfGraphs) {
        domainAxis = aDomainAxis;
        rangeAxis = aRangeAxis;
        if (aColor != null) {
            color = aColor;
            domainAxis.setColor(color);
            rangeAxis.setColor(color);
        }
    }

    @Override
    public IAxis getDomainAxis() {
        return domainAxis;
    }

    @Override
    public IAxis getRangeAxis() {
        return rangeAxis;
    }

    @Override
    public boolean isShown() {
        return shown;
    }

    @Override
    public void setShown(boolean aShown) {
        shown = aShown;
    }

    @Override
    public boolean zoomGraph(double x, double y, double zoomLevel) {
        boolean rangeZoom = rangeAxis.zoom(x, y, zoomLevel);
        boolean domainZoom = domainAxis.zoom(x, y, zoomLevel);
        // TODO if one zoomed and the other didn't, revert the one that did
        // or come up with alternate solution

        return (rangeZoom && domainZoom);
    }

    @Override
    public void updateLabeling(int numberOfGraphs) {
        rangeAxis.updateLabeling(numberOfGraphs);
        domainAxis.updateLabeling(numberOfGraphs);
    }

    @Override
    public double[] translateCoordToValue(Coordinate aCoordinate) {
        double xRange = Math.abs(domainAxis.getMaxVal()
                - domainAxis.getMinVal());
        double xMin = domainAxis.getMinVal();
        if (domainAxis.getMinVal() > domainAxis.getMaxVal()) {
            xMin = domainAxis.getMaxVal();
        }
        double x = xMin
                + ((xRange * (aCoordinate.x - domainAxis.getGraphArea().x) / domainAxis
                        .getGraphArea().width));
        double yRange = Math.abs(rangeAxis.getMaxVal() - rangeAxis.getMinVal());
        double yMin = rangeAxis.getMinVal();
        if (rangeAxis.getMinVal() > rangeAxis.getMaxVal()) {
            yMin = rangeAxis.getMaxVal();
        }
        double y = yMin
                + ((yRange * (aCoordinate.y - rangeAxis.getGraphArea().y) / rangeAxis
                        .getGraphArea().height));
        return new double[] { x, y };
    }

    /**
     * @return the worldExtent
     */
    public IExtent getWorldExtent() {
        return worldExtent;
    }

    /**
     * @param worldExtent
     *            the worldExtent to set
     */
    public void setWorldExtent(IExtent worldExtent) {
        this.worldExtent = worldExtent;
    }

    /**
     * @param domainAxis
     *            the domainAxis to set
     */
    public void setDomainAxis(IAxis domainAxis) {
        this.domainAxis = domainAxis;
    }

    /**
     * @param rangeAxis
     *            the rangeAxis to set
     */
    public void setRangeAxis(IAxis rangeAxis) {
        this.rangeAxis = rangeAxis;
    }

}
