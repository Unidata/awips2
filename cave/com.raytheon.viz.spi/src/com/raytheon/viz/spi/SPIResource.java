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

package com.raytheon.viz.spi;

import java.awt.geom.Rectangle2D;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.eclipse.swt.graphics.RGB;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.PointStyle;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.maps.rsc.StyledMapResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.LabelableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.PointCapability;
import com.raytheon.viz.pointdata.StaticPlotInfoPV;
import com.raytheon.viz.pointdata.StaticPlotInfoPV.SPIEntry;

/**
 * Reads in a "D2D"-native SPI resource
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#     Engineer    Description
 * ------------  ----------  ----------- --------------------------
 * 1/10/08       562         bphillip    Initial Creation.
 * 8/04/14       3489        mapeters    Updated deprecated getStringBounds() calls.
 * 08/21/2014   #3459        randerso    Restructured Map resource class hierarchy
 * Nov 05, 2015 #5070        randerso    Moved label font management up to AbstractMapResource
 * 
 * </pre>
 * 
 * @author bphillip
 * 
 */
public class SPIResource extends
        StyledMapResource<SPIResourceData, MapDescriptor> {

    /** The line color */
    private static final RGB DEFAULT_COLOR = new RGB(155, 155, 155);

    private int maxLen = 0;

    HashMap<String, SPIEntry> entries;

    private int pixelSizeHint = 90;

    public int getPixelSizeHint() {
        return pixelSizeHint;
    }

    public void setPixelSizeHint(int pixelSizeHint) {
        this.pixelSizeHint = pixelSizeHint;
    }

    public SPIResource(SPIResourceData data, LoadProperties props) {
        super(data, props);
        if (!this.getCapabilities().hasCapability(ColorableCapability.class)) {
            getCapability(ColorableCapability.class).setColor(DEFAULT_COLOR);
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        super.initInternal(target);
        getCapability(LabelableCapability.class).setAvailableLabelFields(
                "Label");
        getCapability(LabelableCapability.class).setLabelField("Label");

        File file = new File(resourceData.getFilename());
        if (!file.isAbsolute()) {
            file = PathManagerFactory.getPathManager().getStaticFile(
                    resourceData.getFilename());
        }
        if ((file == null) || (file.exists() == false)) {
            throw new VizException("Could not find spi file",
                    new FileNotFoundException(resourceData.getFilename()));
        }

        entries = StaticPlotInfoPV.readStaticPlotInfoPV(file.getAbsolutePath(),
                true).getSpiList();
        String key = null;
        for (Iterator<String> iterator = entries.keySet().iterator(); iterator
                .hasNext();) {
            key = iterator.next();
            if (key.length() > maxLen) {
                maxLen = key.length();
            }

        }
        project(this.descriptor.getCRS());
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        int displayWidth = (int) (this.descriptor.getMapWidth() * paintProps
                .getZoomLevel());

        double metersPerPixel = displayWidth
                / paintProps.getCanvasBounds().width;

        double screenToWorldRatio = paintProps.getCanvasBounds().width
                / paintProps.getView().getExtent().getWidth();

        DrawableString n = new DrawableString("N");
        n.font = getFont(target);
        Rectangle2D charSize = target.getStringsBounds(n);
        double charWidth = charSize.getWidth();
        double charHeight = charSize.getHeight();

        float magnification = getCapability(MagnificationCapability.class)
                .getMagnification().floatValue();
        double displayHintSize = this.pixelSizeHint * magnification;
        double minSepDist = (displayHintSize * (metersPerPixel / 1000.0))
                / getCapability(DensityCapability.class).getDensity();

        double offsetX = charWidth / 2.0 / screenToWorldRatio;
        double offsetY = charHeight / screenToWorldRatio;
        HorizontalAlignment align = HorizontalAlignment.LEFT;

        PointStyle pointStyle = getCapability(PointCapability.class)
                .getPointStyle();
        if (pointStyle.equals(PointStyle.NONE)) {
            offsetX = 0;
            offsetY = 0;
            align = HorizontalAlignment.CENTER;
        }
        offsetX += getCapability(LabelableCapability.class).getxOffset()
                / screenToWorldRatio;
        offsetY -= getCapability(LabelableCapability.class).getyOffset()
                / screenToWorldRatio;

        boolean isLabeled = getCapability(LabelableCapability.class)
                .getLabelField() != null;

        String key = null;
        SPIEntry entry = null;
        List<DrawableString> strings = new ArrayList<DrawableString>();
        List<double[]> points = new ArrayList<double[]>();
        RGB color = getCapability(ColorableCapability.class).getColor();
        for (Iterator<String> iterator = entries.keySet().iterator(); iterator
                .hasNext();) {
            key = iterator.next();
            entry = entries.get(key);

            if ((entry.pixel != null)
                    && paintProps.getView().isVisible(entry.pixel)
                    && (entry.distance >= minSepDist)) {
                points.add(entry.pixel);
                if (isLabeled && (magnification > 0.0)) {
                    DrawableString string = new DrawableString(key, color);
                    string.font = getFont(target);
                    string.setCoordinates(entry.pixel[0] + offsetX,
                            entry.pixel[1] + offsetY, 0.0);
                    string.horizontalAlignment = align;
                    strings.add(string);
                }
            }
        }
        target.drawStrings(strings);
        target.drawPoints(points, color, pointStyle, 1.0f);
    }

    @Override
    public void project(CoordinateReferenceSystem mapData) throws VizException {

        if (entries != null) {
            for (SPIEntry entry : entries.values()) {
                entry.pixel = this.descriptor.worldToPixel(new double[] {
                        entry.latlon.x, entry.latlon.y });
            }
        }
    }

}
