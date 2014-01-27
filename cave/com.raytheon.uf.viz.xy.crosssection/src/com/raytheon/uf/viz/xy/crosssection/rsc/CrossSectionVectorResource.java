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
package com.raytheon.uf.viz.xy.crosssection.rsc;

import java.util.ArrayList;

import org.eclipse.swt.graphics.RGB;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.style.ParamLevelMatchCriteria;
import com.raytheon.uf.common.style.StyleException;
import com.raytheon.uf.common.style.StyleManager;
import com.raytheon.uf.common.style.StyleRule;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DisplayTypeCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.uf.viz.xy.crosssection.adapter.AbstractCrossSectionAdapter;
import com.raytheon.viz.core.contours.util.VectorGraphicsConfig;
import com.raytheon.viz.core.contours.util.VectorGraphicsRenderable;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Creates a vector resource on a cross section display.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jun 15, 2010            bsteffen     Initial creation
 * Feb 14, 2011 8244       bkowal       enabled magnification capability.
 * Sep 23, 2013  2363     bsteffen    Add more vector configuration options.
 * Dec 11, 2013 DR 16795   D. Friedman  Transform pixel coordinate in inspect
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class CrossSectionVectorResource extends AbstractCrossSectionResource {

    /* Unknown source, provides acceptable density. */
    private static final int VECTOR_SPACING = 60;

    /**
     * @param data
     * @param props
     * @param adapter
     */
    public CrossSectionVectorResource(CrossSectionResourceData data,
            LoadProperties props, AbstractCrossSectionAdapter<?> adapter) {
        super(data, props, adapter);
        ParamLevelMatchCriteria match = new ParamLevelMatchCriteria();
        match.setLevel(null);
        ArrayList<String> paramList = new ArrayList<String>();
        paramList.add(resourceData.getParameter());
        match.setParameterName(paramList);
        StyleRule sr = null;
        try {
            sr = StyleManager.getInstance().getStyleRule(
                    StyleManager.StyleType.ARROW, match);
        } catch (StyleException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        if (sr != null) {
            prefs = sr.getPreferences();
        }
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        super.paintInternal(target, paintProps);

        Double magnification = getCapability(MagnificationCapability.class)
                .getMagnification();

        DataTime time = paintProps.getDataTime();
        if (sliceMap.get(time) == null) {
            return;
        }
        float[] uData = sliceMap.get(time).get(2);
        float[] vData = sliceMap.get(time).get(3);

        double density = getCapability(DensityCapability.class).getDensity();
        RGB color = getCapability(ColorableCapability.class).getColor();

        IExtent graphArea = descriptor.getGraph(this).getExtent();
        if (graphArea == null) {
            return;
        }
        double width = graphArea.getWidth()
                / geometry.getGridRange2D().getMaxX();
        double height = graphArea.getHeight()
                / geometry.getGridRange2D().getMaxY();
        PaintProperties imagePaintProperties = new PaintProperties(paintProps);
        imagePaintProperties.setAlpha(1.0f);
        double ratio = paintProps.getView().getExtent().getWidth()
                / paintProps.getCanvasBounds().width;
        int spacing = (int) ((geometry.getGridRange2D().getMaxX()
                * VECTOR_SPACING * ratio / Math.min(2.0, density) * magnification)
                / paintProps.getCanvasBounds().width + 1);
        IExtent viewableArea = paintProps.getView().getExtent()
                .intersection(graphArea);

        VectorGraphicsConfig config = new VectorGraphicsConfig();
        config.setSizeScaler(magnification * ratio);
        VectorGraphicsRenderable vgr = new VectorGraphicsRenderable(descriptor,
                target, config);

        for (int i = spacing / 2; i < geometry.getGridRange2D().getMaxX(); i += spacing) {
            for (int j = spacing / 2; j < geometry.getGridRange2D().getMaxY(); j += spacing) {
                double screenX = graphArea.getMinX() + (i + 0.5) * width;
                double screenY = graphArea.getMinY() + graphArea.getHeight()
                        - (j + 0.5) * height;
                if (!viewableArea.contains(new double[] { screenX, screenY })) {
                    continue;
                }
                int index = j * (int) geometry.getGridRange2D().getMaxX() + i;
                float uudd = uData[index];
                float vvff = vData[index];
                if (uudd <= -9999 || vvff <= -9999) {
                    continue;
                }
                double spd = Math.hypot(uudd, vvff);
                double dir = Math.atan2(-uudd, -vvff);
                Coordinate plotLoc = new Coordinate(screenX, screenY);
                if (getCapability(DisplayTypeCapability.class).getDisplayType() == DisplayType.ARROW) {
                    vgr.paintArrow(plotLoc, spd, dir);
                } else {
                    vgr.paintBarb(plotLoc, spd, dir);
                }
            }
        }
        vgr.setColor(color);
        vgr.setLineWidth(getCapability(OutlineCapability.class)
                .getOutlineWidth());
        vgr.setLineStyle(getCapability(OutlineCapability.class).getLineStyle());
        vgr.paint(target);
        vgr.dispose();

    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        String s = null;
        Coordinate c = coord.getObject();
        DataTime time = descriptor.getTimeForResource(this);
        double[] values = descriptor.pixelToWorld(new double[] { c.x, c.y });

        // if geometry has not been created yet dont sample
        if (geometry == null) {
            return null;
        }
        try {
            MathTransform transform = geometry.getGridToCRS().inverse();
            double[] result = new double[2];
            transform.transform(values, 0, result, 0, 1);
            int x = (int) Math.round(result[0]);
            int y = (int) Math.round(result[1]);
            if (x > -1 && x < geometry.getGridRange().getSpan(0) && y > -1
                    && y < geometry.getGridRange().getSpan(1)
                    && sliceMap.get(time) != null) {
                int index = y * geometry.getGridRange().getSpan(0) + x;
                float[] ufd = sliceMap.get(time).get(2);
                float[] vfd = sliceMap.get(time).get(3);

                double val = Math.hypot(ufd[index], vfd[index]);
                ColorMapParameters colorMapParams = getCapability(
                        ColorMapCapability.class).getColorMapParameters();
                if (colorMapParams != null) {
                    val = colorMapParams.getDataToDisplayConverter().convert(
                            val);
                }
                s = sampleFormat.format(val);
            }
        } catch (Exception e) {
            throw new VizException("Error sampling cross section.", e);
        }
        if (s == null) {
            return s;
        }
        return s + getUnitString();
    }

}
