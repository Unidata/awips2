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
package com.raytheon.uf.viz.xy.varheight.graph;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.xy.graph.AbstractGraph;
import com.raytheon.uf.viz.xy.graph.IGraph;
import com.raytheon.uf.viz.xy.graph.XyGraphDescriptor;
import com.raytheon.uf.viz.xy.graph.axis.GraphAxis;
import com.raytheon.uf.viz.xy.graph.axis.IAxis;
import com.raytheon.uf.viz.xy.graph.axis.LinearAxisPlacer;
import com.raytheon.uf.viz.xy.graph.axis.LogarithmicAxisPlacer;
import com.raytheon.uf.viz.xy.graph.labeling.IGraphLabel;
import com.raytheon.uf.viz.xy.map.rsc.IGraphableResource;
import com.raytheon.uf.viz.xy.varheight.display.VarHeightDescriptor;
import com.raytheon.uf.viz.xy.varheight.rsc.VarHeightResource;
import com.raytheon.viz.core.slice.request.HeightScale;
import com.raytheon.viz.core.slice.request.HeightScale.ScaleType;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * The background graph for a var height display
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 03, 2010            bsteffen    Initial creation
 * Feb 10, 2011 8244       bkowal      replaced deprecrated method calls;
 *                                     magnitude influences axis label font.
 * Jun 18, 2014 3242       njensen     Replaced deprecated calls
 * Mar 02, 2015 4189       nabowle     Prevent NPE when panning. Copy
 *                                     graphResource in paintUnits() to prevent
 *                                     ConcurrentModification in a single thread.
 * Jul 21, 2015 4220       mapeters    Reset zoomHandler when constructing this graph
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class VarHeightGraph extends AbstractGraph {

    protected static final DecimalFormat df = new DecimalFormat("#.0###");

    private double zoomLevel = 1;

    /**
     * @param descriptor
     */
    public VarHeightGraph(XyGraphDescriptor descriptor) {
        super(descriptor);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.xy.graph.AbstractGraph#canHandleResoruce(com.raytheon
     * .uf.viz.xy.map.rsc.IGraphableResource)
     */
    @Override
    protected boolean canHandleResoruce(IGraphableResource<?, ?> rsc) {
        return rsc instanceof VarHeightResource;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.xy.graph.AbstractGraph#constructVirtualExtent()
     */
    @Override
    protected void constructVirtualExtent() {
        this.zoomLevel = 1;
        if (zoomHandler != null) {
            zoomHandler.reset();
        }
        double[] minMaxX = new double[2];
        ArrayList<IGraphLabel<Double>> xLabels = new ArrayList<IGraphLabel<Double>>();
        getRangeData(xLabels, new ArrayList<IGraphLabel<Double>>());
        minMaxX[0] = xLabels.get(0).getDiscreteValue();
        minMaxX[1] = xLabels.get(xLabels.size() - 1).getDiscreteValue();
        normalizeAxis(minMaxX);
        HeightScale heightScale = ((VarHeightDescriptor) descriptor)
                .getHeightScale();
        if (heightScale.getScale() == ScaleType.LOG) {
            xAxisPlacer = new LogarithmicAxisPlacer(graphExtent.getHeight(),
                    heightScale.getMinVal(), heightScale.getMaxVal());
        } else {
            xAxisPlacer = new LinearAxisPlacer(graphExtent.getHeight(),
                    heightScale.getMinVal(), heightScale.getMaxVal());
        }
        yAxisPlacer = new LinearAxisPlacer(graphExtent.getWidth(), minMaxX[0],
                minMaxX[1]);
        updateVirtualExtent();
        newResources = false;

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.xy.graph.AbstractGraph#createAxes()
     */
    @Override
    protected void createAxes() {
        yAxisPlacer.setPixelWidth(graphExtent.getWidth());

        createHeightAxis(((VarHeightDescriptor) descriptor).getHeightScale(),
                zoomLevel);

        // Place them
        double minX = graphExtent.getMinX();
        double maxY = graphExtent.getMaxY();
        double minY = graphExtent.getMinY();

        // Create the Axis if they do not exist
        if (yAxes.length == 0) {
            yAxes = new IAxis[11];
            for (int i = 0; i < yAxes.length; ++i) {
                yAxes[i] = new GraphAxis();
                yAxes[i].setLineStyle(LineStyle.DASHED_LARGE);
                yAxes[i].setDrawAxis(true);
            }
        }

        // Update the values
        double inc = yAxisPlacer.getDataWidth() / 10;
        double val = Math.floor(yAxisPlacer.getMinDataValue() / inc) * inc;

        for (int i = 0; i < yAxes.length; i++) {
            yAxes[i].setDiscreteValue(val + inc * i);
        }

        // Place the data axes
        double[] offsets = yAxisPlacer.placeAxes(yAxes);

        for (int i = 0; i < offsets.length; ++i) {
            double offset = offsets[i];
            yAxes[i].setStartLoc(new Coordinate(minX + offset, minY, 0));
            yAxes[i].setEndLoc(new Coordinate(minX + offset, maxY, 0));
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.xy.graph.AbstractGraph#paintTitles(com.raytheon.uf
     * .viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintTitles(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        if (titleFont == null) {
            titleFont = target.initializeFont((String) null, 11.0f,
                    new IFont.Style[] { IFont.Style.BOLD });
        }

        RGB titleColor = descriptor.getGraphResource()
                .getCapability(ColorableCapability.class).getColor();
        String title = ((VarHeightDescriptor) descriptor).getHeightScale()
                .getUnit();

        paintYTitle(target, paintProps, title, titleColor, 0);

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.xy.graph.AbstractGraph#paintUnits(com.raytheon.uf
     * .viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintUnits(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        if (unitsFont == null) {
            unitsFont = target.initializeFont((String) null, 10.0f,
                    new IFont.Style[] {});
        }
        paintHeightUnits(target, paintProps);

        List<DrawableString> strings = new ArrayList<DrawableString>();

        RGB rcsColor = null;
        /*
         * iterate on a copy of graphResource since descriptor.getGraph() may
         * modify graphResource.
         */
        List<IGraphableResource<?, ?>> copy = new ArrayList<>(graphResource);
        for (IGraphableResource<?, ?> grsc : copy) {
            Set<IGraph> otherGraphs = new HashSet<IGraph>();
            for (ResourcePair rp : descriptor.getResourceList()) {
                if (rp.getResource() instanceof IGraphableResource<?, ?>) {
                    IGraph graph = descriptor
                            .getGraph((IGraphableResource<?, ?>) rp
                                    .getResource());
                    if (graph == this) {
                        break;
                    } else {
                        otherGraphs.add(graph);
                    }
                }
            }
            int index = otherGraphs.size();
            VarHeightResource rsc = (VarHeightResource) grsc;
            if (descriptor.getResourceList().getProperties(rsc) != null
                    && descriptor.getResourceList().getProperties(rsc)
                            .isVisible()) {
                rcsColor = rsc.getCapability(ColorableCapability.class)
                        .getColor();

                for (IAxis axis : yAxes) {
                    Coordinate coord = axis.getCoordinates()[1];
                    String value = df.format(axis.getDiscreteValue());
                    if (coord.x < graphExtent.getMinX() - 1) {
                        continue;
                    } else if (coord.x > graphExtent.getMaxX() + 1) {
                        continue;
                    }
                    DrawableString parameters = new DrawableString("", rcsColor);
                    parameters.font = unitsFont;
                    parameters.addTextStyle(TextStyle.DROP_SHADOW);
                    parameters.horizontalAlignment = HorizontalAlignment.CENTER;
                    parameters.verticallAlignment = VerticalAlignment.TOP;
                    parameters.magnification = this.currentMagnification;

                    parameters.setText(value, rcsColor);
                    parameters.setCoordinates(coord.x, coord.y + index
                            * unitsFont.getFontSize() * 1.25, coord.z);
                    strings.add(parameters);
                }
            }
        }
        target.drawStrings(strings);
    }

    @Override
    public void zoom(int index, Coordinate gridCoord) {
        this.zoomLevel = index;
        yAxisPlacer.zoom(gridCoord.x - graphExtent.getMinX(), index);
        xAxisPlacer.zoom(graphExtent.getMaxY() - gridCoord.y, index);
        double inc = yAxisPlacer.getDataWidth() / 10;
        double newMin = (int) (yAxisPlacer.getMinDataValue() / inc) * inc;
        yAxisPlacer.pan(yAxisPlacer.getPixelLoc(newMin));
        updateVirtualExtent();
    }

    @Override
    public void pan(double xDist, double yDist, boolean panning) {
        if (yAxisPlacer == null || xAxisPlacer == null) {
            return;
        }
        yAxisPlacer.pan(xDist);
        xAxisPlacer.pan(-yDist);
        if (!panning) {
            double inc = yAxisPlacer.getDataWidth() / 10;
            double newMin = Math.round(yAxisPlacer.getMinDataValue() / inc)
                    * inc;
            yAxisPlacer.pan(yAxisPlacer.getPixelLoc(newMin));
        }
        updateVirtualExtent();
    }
}
