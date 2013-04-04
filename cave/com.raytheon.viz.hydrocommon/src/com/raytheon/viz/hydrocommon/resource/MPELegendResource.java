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
package com.raytheon.viz.hydrocommon.resource;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.viz.core.DrawableColorMap;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters.LabelEntry;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.GenericResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.sampling.ISamplingResource;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;

/**
 * Displays the MPE Legend.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 19, 2008            randerso     Initial creation
 * Feb 14, 2013 1616       bsteffen    Add option for interpolation of colormap
 *                                     parameters, disable colormap interpolation
 *                                     by default.
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class MPELegendResource extends
        AbstractVizResource<GenericResourceData, IDescriptor> implements
        ISamplingResource {

    private double scale;

    private double textSpace;

    private double width;

    private AbstractVizResource<?, ?> rsc;

    private boolean sampling = false;

    public MPELegendResource(GenericResourceData rscData,
            LoadProperties loadProps) {
        super(rscData, loadProps);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#dispose()
     */
    @Override
    protected void disposeInternal() {
        // TODO Auto-generated method stub

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
        // TODO Auto-generated method stub

    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        IFont font = target.getDefaultFont();
        IExtent screenExtent = paintProps.getView().getExtent();

        scale = (screenExtent.getHeight() / paintProps.getCanvasBounds().height);

        DrawableString pa = new DrawableString("0", new RGB(255, 255, 255));
        pa.font = font;
        double textHeight = target.getStringsBounds(pa).getHeight() * scale;
        double padding = 3 * scale;
        textSpace = textHeight + padding;
        double yMax = screenExtent.getMaxY();
        // PixelExtent pe = new PixelExtent(screenExtent.getMinX(),
        // screenExtent.getMaxX(), screenExtent.getMinY(),
        // screenExtent.getMinY() + screenExtent.getMaxY());
        // this.setExtent(pe);
        target.clearClippingPlane();

        yMax = drawMpeLegend(target, paintProps.getAlpha(),
                screenExtent.getMinX(), screenExtent.getMaxX(), yMax,
                textHeight, padding);

        target.setupClippingPlane(screenExtent);
    }

    private double drawMpeLegend(IGraphicsTarget target, float alpha,
            double xMin, double xMax, double yMax, double textHeight,
            double padding) throws VizException {
        double legendHeight = 0;
        HydroDisplayManager dispMan = HydroDisplayManager.getInstance();
        rsc = dispMan.getDisplayedResource();

        if (rsc == null) {
            for (ResourcePair pair : descriptor.getResourceList()) {
                if ((pair.getResource() instanceof XmrgResource)
                        || (pair.getResource() instanceof TimeLapseResource)
                        || (pair.getResource() instanceof MeanArealPrecipResource)) {
                    if (!rsc.getStatus().equals(ResourceStatus.INITIALIZED)) {
                        rsc.init(null);
                    }
                }
            }
        }

        if (rsc != null) {
            if (rsc.getStatus().equals(ResourceStatus.INITIALIZED)) {

                double cmapHeight = textHeight * 1.25;

                legendHeight = cmapHeight + 3.0 * textSpace + 2.0 * padding;
                double y1 = yMax - legendHeight;

                DrawableColorMap cmap = new DrawableColorMap(rsc.getCapability(
                        ColorMapCapability.class).getColorMapParameters());
                cmap.alpha = alpha;
                IColorMap cm = cmap.getColorMapParams().getColorMap();

                width = (xMax - xMin) / 30 * cm.getSize();
                PixelExtent legendExtent = new PixelExtent(xMin, xMin + width,
                        y1, yMax);
                target.drawShadedRect(legendExtent, new RGB(0, 0, 0), alpha,
                        null);
                int offset = 0;
                RGB textColor = new RGB(255, 255, 255);
                DrawableString strings = new DrawableString("", textColor);
                strings.font = target.getDefaultFont();

                int i = 0;
                for (LabelEntry entry : rsc
                        .getCapability(ColorMapCapability.class)
                        .getColorMapParameters().getLabels()) {

                    if (entry.getText().length() > 10) {
                        break;
                    } else {
                        double cbarSize = (width / cm.getSize());
                        double xLoc = xMin + offset + (cbarSize * i);
                        strings.setText(entry.getText(), textColor);
                        strings.horizontalAlignment = HorizontalAlignment.CENTER;
                        strings.verticallAlignment = VerticalAlignment.TOP;
                        strings.setCoordinates(xLoc, y1);
                        target.drawStrings(strings);
                    }
                    i++;
                }

                y1 += textSpace;
                cmap.extent = new PixelExtent(xMin, xMin + width, y1, y1
                        + cmapHeight);
                target.drawColorRamp(cmap);
                y1 += cmapHeight;
                strings.setText(rsc.getName(), textColor);
                double xLoc = xMin + padding;
                strings.horizontalAlignment = HorizontalAlignment.LEFT;
                strings.verticallAlignment = VerticalAlignment.TOP;
                strings.setCoordinates(xLoc, y1);
                target.drawStrings(strings);

                textColor = null;
            }
        }
        return yMax - legendHeight;
    }

    @Override
    public String getName() {
        return "";
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.sampling.ISamplingResource#isSampling()
     */
    @Override
    public boolean isSampling() {
        return sampling;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.sampling.ISamplingResource#setSampling(boolean)
     */
    @Override
    public void setSampling(boolean sampling) {
        this.sampling = sampling;
    }

}
