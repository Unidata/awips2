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

package com.raytheon.viz.lpi;

import java.awt.geom.Rectangle2D;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

import org.eclipse.swt.graphics.RGB;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.PointStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.maps.rsc.AbstractMapResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.LabelableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.PointCapability;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Reads in a "D2D"-native LPI resource
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date          Ticket#     Engineer    Description
 *    ------------	----------	-----------	--------------------------
 *    9/17/07                   randerso    Initial Creation.
 * 
 * </pre>
 * 
 * @author randerso
 * 
 */
public class LPIResource extends
        AbstractMapResource<LPIResourceData, IMapDescriptor> implements
        IResourceDataChanged {

    /** Whether the resource is ready to be drawn */
    private boolean ready = false;

    private List<LPIPoint> points;

    private int maxLen = 0;

    private int pixelSizeHint = 90;

    private IFont font;

    private class LPIPoint {
        public Coordinate latLon;

        public double[] pixel;

        public double dist;

        public String label;

        LPIPoint() {
            latLon = new Coordinate();
        }
    }

    protected LPIResource(LPIResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        resourceData.addChangeListener(this);
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type == ChangeType.CAPABILITY) {
            if (object instanceof MagnificationCapability) {
                if (font != null) {
                    font.dispose();
                    font = null;
                }
            }
        }
        this.issueRefresh();
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.core.rsc.IVizResource#init(com.raytheon.viz.core.
     * IGraphicsTarget)
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        super.initInternal(target);
        try {
            getCapability(LabelableCapability.class).setAvailableLabelFields(
                    "Label");
            getCapability(LabelableCapability.class).setLabelField("Label");

            String filename = resourceData.getFilename();
            File file = new File(filename);
            if (!file.isAbsolute()) {
                filename = FileUtil.join(VizApp.getMapsDir(), filename);
                file = PathManagerFactory.getPathManager().getStaticFile(
                        filename);
            }
            if (file == null || file.exists() == false) {
                throw new VizException("Could not find lpi file",
                        new FileNotFoundException(filename));
            }

            points = new ArrayList<LPIPoint>();
            BufferedReader in = new BufferedReader(new FileReader(file));

            String s = in.readLine();
            while (s != null) {
                LPIPoint p = readPoint(s);
                if (p != null) {
                    points.add(p);
                }
                s = in.readLine();
            }
            in.close();

        } catch (FileNotFoundException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        project(this.descriptor.getCRS());

        ready = true;
    }

    public LPIPoint readPoint(String s) throws IOException {

        Scanner in = new Scanner(s);

        LPIPoint p = this.new LPIPoint();

        if (!in.hasNextDouble()) {
            return null;
        }
        p.latLon.y = in.nextDouble();
        if (!in.hasNextDouble()) {
            return null;
        }
        p.latLon.x = in.nextDouble();

        if (!in.hasNextDouble()) {
            return null;
        }
        p.dist = in.nextDouble();

        if (!in.hasNext()) {
            return null;
        }
        p.label = in.findInLine("[^\\|]*").trim();
        if (p.label.length() > maxLen) {
            maxLen = p.label.length();
        }

        return p;
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
        if (ready) {
            int displayWidth = (int) (this.descriptor.getMapWidth() * paintProps
                    .getZoomLevel());

            double metersPerPixel = displayWidth
                    / paintProps.getCanvasBounds().width;

            double magnification = getCapability(MagnificationCapability.class)
                    .getMagnification();

            if (this.font == null) {
                font = target.initializeFont(target.getDefaultFont()
                        .getFontName(), (float) (10 * magnification), null);
            }

            double displayHintSize = this.pixelSizeHint * magnification;
            double minSepDist = (displayHintSize * (metersPerPixel / 1000.0))
                    / getCapability(DensityCapability.class).getDensity();

            Rectangle2D charSize = target.getStringBounds(font, "N");
            double charWidth = charSize.getWidth();
            double charHeight = charSize.getHeight();

            double screenToWorldRatio = paintProps.getCanvasBounds().width
                    / paintProps.getView().getExtent().getWidth();

            RGB color = getCapability(ColorableCapability.class).getColor();
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

            List<DrawableString> strings = new ArrayList<DrawableString>();
            List<double[]> pointList = new ArrayList<double[]>();
            for (LPIPoint p : points) {
                if (p.pixel == null) {
                    continue;
                }

                if ((paintProps.getView().isVisible(p.pixel))
                        && (p.dist >= minSepDist)) {

                    pointList.add(p.pixel);

                    if (isLabeled && (magnification > 0.0)) {
                        DrawableString string = new DrawableString(p.label,
                                color);
                        string.font = font;
                        string.setCoordinates(p.pixel[0] + offsetX, p.pixel[1]
                                + offsetX);
                        string.horizontalAlignment = align;
                        string.textStyle = TextStyle.WORD_WRAP;
                        strings.add(string);
                    }
                }
            }

            target.drawStrings(strings);
            target.drawPoints(pointList, color, pointStyle, 1.0f);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#dispose()
     */
    @Override
    protected void disposeInternal() {
        resourceData.removeChangeListener(this);
        if (font != null) {
            font.dispose();
            font = null;
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
        if (points != null) {
            for (LPIPoint p : points) {
                p.pixel = this.descriptor.worldToPixel(new double[] {
                        p.latLon.x, p.latLon.y });
            }
        }
    }

    public int getPixelSizeHint() {
        return pixelSizeHint;
    }

    public void setPixelSizeHint(int pixelSizeHint) {
        this.pixelSizeHint = pixelSizeHint;
    }

}
