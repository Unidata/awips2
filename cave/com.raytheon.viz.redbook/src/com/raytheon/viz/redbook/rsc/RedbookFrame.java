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
package com.raytheon.viz.redbook.rsc;

import java.awt.geom.Rectangle2D;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.ProjectedCRS;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.redbook.blocks.Block_004_016;
import com.raytheon.uf.common.dataplugin.redbook.blocks.DefaultBlock;
import com.raytheon.uf.common.dataplugin.redbook.blocks.RedbookBlockBuilder;
import com.raytheon.uf.common.dataplugin.redbook.blocks.RedbookBlockHeader;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.ICanvasRenderingExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.viz.core.rsc.jts.JTSCompiler;
import com.raytheon.viz.redbook.RedbookWMOMap;
import com.raytheon.viz.redbook.blocks.AbstractTextBlock;
import com.raytheon.viz.redbook.blocks.AlphaNumBlock;
import com.raytheon.viz.redbook.blocks.PlotDataBlock;
import com.raytheon.viz.redbook.blocks.PlotParametersBlock;
import com.raytheon.viz.redbook.blocks.RedbookProjectionBlock;
import com.raytheon.viz.redbook.blocks.ShortLongVectorsBlock;
import com.raytheon.viz.redbook.blocks.TextBlock;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Displays a frame of redbook data
 * 
 * Redbook data is presented as a ByteBuffer to this class, and then decomposed
 * into blocks. The blocks are then parsed for data and renderable parts are
 * then converted to JTS geometry objects that are rendered using the
 * JTSCompiler.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * May 29, 2008 1162        chammack    Initial creation
 * Jan 28, 2010 4224        M. Huang    Added Line Style, Line Width menu choice
 * Apr 29, 2013 1958        bgonzale    New class RedbookBlockHeader.
 * May 21, 2013 2001        njensen     Fixed error handling
 * Jul 19, 2013 16401       D. Friedman Fix unknown block processing.
 * Mar 13, 2014 2907        njensen     split edex.redbook plugin into common
 *                                      and edex redbook plugins
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public class RedbookFrame implements IRenderable {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RedbookFrame.class);

    protected JTSCompiler compiler;

    protected IWireframeShape wireframeShape;

    protected IMapDescriptor descriptor;

    protected final List<TextBlock> textBlocks;

    protected static final String MODE_KEY_FMT = "%03d_%03d";

    protected static final int MIN_REMAINING = 4;

    protected DataTime dataTime;

    protected ByteDataRecord byteDataRecord;

    protected RedbookResource redbookResource;

    public RedbookFrame(RedbookResource redbookResource,
            IMapDescriptor descriptor, ByteDataRecord byteDataRecord) {
        this.redbookResource = redbookResource;
        this.descriptor = descriptor;
        this.textBlocks = new ArrayList<TextBlock>();
        this.byteDataRecord = byteDataRecord;
    }

    public RedbookStatus init(IGraphicsTarget target) {
        this.wireframeShape = target.createWireframeShape(false, descriptor);
        this.compiler = new JTSCompiler(null, wireframeShape, this.descriptor);
        return loadRedbook(this.byteDataRecord);
    }

    public boolean hasInited() {
        return (this.compiler != null);
    }

    public void deInit() {
        if (this.wireframeShape != null) {
            this.wireframeShape.dispose();
        }
        this.textBlocks.clear();
        this.compiler = null;
    }

    private RedbookStatus loadRedbook(ByteDataRecord bdr) {

        RedbookStatus status = new RedbookStatus();
        RedbookLegend legend = new RedbookLegend();
        ArrayList<AbstractTextBlock> parsedTextBlocks = new ArrayList<AbstractTextBlock>();

        synchronized (this) {
            try {
                byte[] d = bdr.getByteData();

                ByteBuffer dataBuf = ByteBuffer.wrap(d);

                int n = 0;
                int m = 0;

                MathTransform mt = null;

                while (dataBuf.hasRemaining()) {

                    RedbookBlockHeader header = RedbookBlockBuilder
                            .getHeader(dataBuf);
                    String currBlock = header.blockFactoryKey;

                    if (currBlock.equals("005_002")) {
                        // Block that describes plot data
                        PlotDataBlock pdb = new PlotDataBlock(header, dataBuf,
                                mt, m, n);
                        parsedTextBlocks.add(pdb);
                    } else if (currBlock.equals("005_001")) {
                        // Block that describes alphanumeric data
                        AlphaNumBlock pdb = new AlphaNumBlock(header, dataBuf,
                                mt, m, n);
                        parsedTextBlocks.add(pdb);
                    } else if (currBlock.equals("004_005")) {
                        // Block that describes the relative short/long format
                        ShortLongVectorsBlock vb = new ShortLongVectorsBlock(
                                header, dataBuf, mt, m, n, legend);
                        try {
                            this.compiler.handle(vb.getGeometry());
                        } catch (VizException e) {
                            statusHandler.error(
                                    "Error during rendering of redbook", e);
                            status.vectorRenderingWarning = true;
                        }
                    } else if (currBlock.equals("001_004")) {
                        // Block that describes the plot parameters
                        new PlotParametersBlock(header, dataBuf);
                        // Currently, this is not used in rendering
                    } else if (currBlock.equals("004_017")) {
                        // Block that describes the projection
                        RedbookProjectionBlock vb = new RedbookProjectionBlock(
                                header, dataBuf);

                        String customProjection = null;
                        try {
                            RequestConstraint wmo = redbookResource
                                    .getResourceData().getMetadataMap()
                                    .get("wmoTTAAii");
                            if (wmo != null) {
                                RedbookWMOMap.Info info = RedbookWMOMap.load().mapping
                                        .get(wmo.getConstraintValue());
                                if (info != null) {
                                    customProjection = info.projection; // may
                                    // be
                                    // null
                                }
                            }
                        } catch (Exception e) {
                            statusHandler.error(
                                    "Error rendering redbook product", e);
                        }

                        if (customProjection == null) {
                            mt = vb.getMathTransform(m, n);
                        } else {
                            ArrayList<Float> flt = new ArrayList<Float>();
                            StringTokenizer t = new StringTokenizer(
                                    customProjection);
                            t.nextToken(); // skip 'mercator' for now
                            while (t.hasMoreTokens()) {
                                flt.add(Float.parseFloat(t.nextToken()));
                            }
                            ProjectedCRS crs = MapUtil.constructMercator(
                                    MapUtil.AWIPS_EARTH_RADIUS,
                                    MapUtil.AWIPS_EARTH_RADIUS, flt.get(0),
                                    flt.get(1));
                            float llLat = flt.get(2);
                            float llLon = flt.get(3);
                            float urLat = flt.get(4);
                            float urLon = flt.get(5);
                            MathTransform toProj = MapUtil
                                    .getTransformFromLatLon(crs);

                            double[] ll = new double[2];
                            double[] ur = new double[2];
                            toProj.transform(new double[] { llLon, llLat }, 0,
                                    ll, 0, 1);
                            toProj.transform(new double[] { urLon, urLat }, 0,
                                    ur, 0, 1);

                            GeneralEnvelope env = new GeneralEnvelope(2);
                            env.setCoordinateReferenceSystem(crs);
                            env.setRange(0, ll[0], ur[0]);
                            env.setRange(1, ll[1], ur[1]);

                            GridGeometry2D gg = new GridGeometry2D(
                                    new GeneralGridEnvelope(new int[] { 0, 0 },
                                            new int[] { m, n }, false), env);

                            MathTransform mt2 = gg
                                    .getGridToCRS(PixelInCell.CELL_CENTER);

                            DefaultMathTransformFactory dmtf = new DefaultMathTransformFactory();
                            mt = dmtf.createConcatenatedTransform(mt2,
                                    toProj.inverse());
                        }
                    } else if (currBlock.equals("004_016")) {
                        // Block that describes the plot space
                        Block_004_016 vb = new Block_004_016(header, dataBuf);

                        // Store off the pixel space
                        m = vb.getRefM2coord();
                        n = vb.getRefN2coord();

                        legend.setMaxRefY(n);
                    } else if (currBlock.equals("001_001")
                            || currBlock.equals("001_006")
                            || currBlock.equals("001_002")
                            || currBlock.startsWith("002_")) {

                        // Recognized blocks that we don't do anything with
                        new DefaultBlock(header, dataBuf);

                    } else {

                        DefaultBlock block = new DefaultBlock(header, dataBuf);
                        if (!RedbookBlockHeader.DEFAULT.equals(currBlock)) {
                            status.unhandledPackets = true;
                            statusHandler
                                    .debug("Unhandled redbook packet: (mode="
                                            + block.getMode() + ", submode="
                                            + block.getSubMode() + ")");
                        }
                    }

                }

            } catch (TransformException e) {
                statusHandler
                        .error("Error rendering redbook product due to an error setting up the map projection.",
                                e);
            } catch (FactoryException e) {
                statusHandler
                        .error("Error rendering redbook product due to an error setting up the map projection.",
                                e);
            }

        }

        for (AbstractTextBlock atb : parsedTextBlocks) {
            TextBlock tb = atb.determineTextBlockType(legend);
            if (tb != null) {
                this.textBlocks.add(tb);
            }
        }

        return status;

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.drawables.IRenderable#paint(com.raytheon.viz.core
     * .IGraphicsTarget, com.raytheon.viz.core.drawables.PaintProperties)
     */
    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {

        target.drawWireframeShape(this.wireframeShape, this.redbookResource
                .getCapability(ColorableCapability.class).getColor(),
                this.redbookResource.getCapability(OutlineCapability.class)
                        .getOutlineWidth(),
                this.redbookResource.getCapability(OutlineCapability.class)
                        .getLineStyle());

        synchronized (this) {
            IExtent pe = paintProps.getView().getExtent();
            IFont font = this.redbookResource.getRenderingFont();
            Rectangle2D cellSize = target.getStringBounds(font, "M");
            boolean clipped = true;
            double xRatio = paintProps.getView().getExtent().getWidth()
                    / paintProps.getCanvasBounds().width;
            double yRatio = paintProps.getView().getExtent().getHeight()
                    / paintProps.getCanvasBounds().height;
            double xCellSize = cellSize.getWidth() * xRatio * 1.2;
            double yCellSize = cellSize.getHeight() * yRatio * 1.2;
            int legendLine = 0;

            for (TextBlock tb : this.textBlocks) {
                double xOrigin;
                double yOrigin;
                IGraphicsTarget.VerticalAlignment vAlign;

                if (tb.isLegend) {
                    // Left edge with some padding
                    xOrigin = 2;
                    // n lines down from top edge
                    yOrigin = yCellSize * legendLine / yRatio;
                    ++legendLine;
                    vAlign = IGraphicsTarget.VerticalAlignment.TOP;

                    if (clipped) {
                        // Clear the clipping pane so the legend text is not
                        // cropped
                        target.clearClippingPlane();
                        clipped = false;
                    }
                } else {
                    double[] world = this.descriptor.worldToPixel(new double[] {
                            tb.location.x, tb.location.y });
                    if (!pe.contains(world)) {
                        continue;
                    }
                    xOrigin = world[0];
                    yOrigin = world[1];

                    if (tb.offset != null) {
                        /*
                         * Apply offsets. AWIPS 1 uses
                         * 
                         * delta*fggetsiz()/9
                         * 
                         * Where fggetsiz() is 8 at the default magnification
                         * level. This applies to both the x and y direction.
                         * Note that this is an integer calculation so an offset
                         * of +/-1 normally does nothing.
                         * 
                         * The default AWIPS 2 character width is currently 9 so
                         * the character size is modified to get a similar
                         * affect. Also, the calculation is different for x and
                         * y because the AWIPS 2 font is not as square as AWIPS
                         * 1.
                         */
                        xOrigin += (tb.offset[0] * (cellSize.getWidth() - 1) / 9)
                                * xRatio;
                        // Note: y is flipped
                        yOrigin -= (tb.offset[1] * (cellSize.getHeight() - 1) / 9)
                                * yRatio;
                    }

                    vAlign = IGraphicsTarget.VerticalAlignment.BOTTOM;

                    if (!clipped) {
                        // Reset the clipping pane
                        target.setupClippingPlane(new PixelExtent(
                                this.descriptor.getGridGeometry()
                                        .getGridRange()));
                        clipped = true;
                    }
                }

                double x = xOrigin;
                double y = yOrigin;

                boolean symbolFont = false;

                String text = tb.text;
                char[] characters = text.toCharArray();
                int iStart = -1;
                int nCharsOnLine = 0;

                for (int i = 0; i < characters.length; ++i) {
                    char c = characters[i];
                    /*
                     * Cannot just test for >= 32. The weather symbols font
                     * makes use of the low ASCII characters.
                     */
                    if (c < 8 || (c > 13 && c != 17 && c != 18)) {
                        if (iStart == -1) {
                            iStart = i;
                        }
                    } else {
                        if (iStart != -1) {
                            String seg = text.substring(iStart, i);
                            if (!symbolFont) {
                                /*
                                 * Prevent NUL from displaying as a box or other
                                 * default glyph.
                                 */
                                seg = seg.replace('\u0000', ' ');
                            }

                            if (tb.isLegend) {
                                // The value of 50 is taken from AWIPS-1
                                if ((nCharsOnLine + seg.length()) > 50) {
                                    x = xOrigin;
                                    y += yCellSize / yRatio;
                                    nCharsOnLine = 0;
                                    ++legendLine;
                                }
                                nCharsOnLine += seg.length();
                            }

                            x += drawString(target, seg, x, y, symbolFont,
                                    true, vAlign == VerticalAlignment.TOP,
                                    xRatio, yRatio, paintProps, tb.isLegend);

                            iStart = -1;
                        }

                        if (c == 17) {
                            symbolFont = false;
                        } else if (c == 18) {
                            symbolFont = true;
                        }

                        /*
                         * AWIPS 1 ignores cursor movement in the legend, but
                         * may still break lines at cursor movement characters.
                         * See nCharsOnLine above.
                         */

                        if (!tb.isLegend) {
                            switch (c) {
                            case 8:
                                x -= xCellSize;
                                break;
                            case 9:
                                x += xCellSize;
                                break;
                            case 10:
                                y += yCellSize;
                                break;
                            case 11:
                                y -= yCellSize;
                                break;
                            case 12:
                                /*
                                 * This is documented as "half line" in the
                                 * AWIPS 1 code, but it appears to be 1.5 lines.
                                 */
                                y += yCellSize * 1.5;
                                break;
                            case 13:
                                x = xOrigin;
                                y += yCellSize;
                                break;
                            }
                        }
                    }
                }

                if (iStart != -1) {
                    String seg = text.substring(iStart);
                    if (!symbolFont) {
                        seg = seg.replace('\u0000', ' '); // Prevent NUL from
                        // display as a box or
                        // other default glyph
                    }

                    if (tb.isLegend) {
                        // The value of 50 is taken from AWIPS-1
                        if ((nCharsOnLine + seg.length()) > 50) {
                            x = xOrigin;
                            y += yCellSize / yRatio;
                            nCharsOnLine = 0;
                            ++legendLine;
                        }
                        nCharsOnLine += seg.length();
                    }

                    drawString(target, seg, x, y, symbolFont, true,
                            vAlign == VerticalAlignment.TOP, xRatio, yRatio,
                            paintProps, tb.isLegend);
                }
            }

            if (!clipped) {
                // Reset the clipping pane
                target.setupClippingPlane(new PixelExtent(this.descriptor
                        .getGridGeometry().getGridRange()));
                clipped = true;
            }
        }
    }

    private double drawString(IGraphicsTarget target, String s, double x,
            double y, boolean symbols, boolean blanked, boolean top,
            double xRatio, double yRatio, PaintProperties paintProps,
            boolean isLegend) throws VizException {

        double magnification = redbookResource.getMagnification();

        if (!symbols) {
            IFont font = redbookResource.getRenderingFont();
            DrawableString dstring = new DrawableString(s, this.redbookResource
                    .getCapability(ColorableCapability.class).getColor());
            dstring.setCoordinates(x, y);
            dstring.font = font;
            dstring.horizontalAlignment = HorizontalAlignment.LEFT;
            dstring.verticallAlignment = top ? VerticalAlignment.TOP
                    : VerticalAlignment.BOTTOM;
            Rectangle2D bounds = target.getStringsBounds(dstring);
            if (blanked) {
                dstring.textStyle = TextStyle.BLANKED;
            }
            if (isLegend) {
                target.getExtension(ICanvasRenderingExtension.class)
                        .drawStrings(paintProps, dstring);
            } else {
                target.drawStrings(dstring);

            }
            return bounds.getWidth() * xRatio;
        } else {
            double width = s.length() * 12 * xRatio * magnification;
            double yext = y + 12 * yRatio * magnification * (top ? 1 : -1);
            if (blanked) {
                PixelExtent pe = new PixelExtent(x, x + width,
                        Math.min(y, yext), Math.max(y, yext));
                target.drawShadedRect(pe, new RGB(0, 0, 0),
                        paintProps.getAlpha(), null);
            }
            for (char c2 : s.toCharArray()) {
                IImage image = redbookResource.getWxSymbols().getImage(
                        target,
                        this.redbookResource.getCapability(
                                ColorableCapability.class).getColor(), c2);
                if (image != null) {
                    /*
                     * double dy = 0; if (vAlign == IGraphicsTarget
                     * .VerticalAlignment.BOTTOM) dy = - 12 yRatio;
                     */
                    double dy = 0;
                    if (!top) {
                        dy = -12 * yRatio * magnification;
                    }

                    // image.getWidth() and getHeight() may
                    // not return the correct values
                    Coordinate ul = new Coordinate(x, y + dy);
                    Coordinate ur = new Coordinate(x + image.getWidth()
                            * xRatio * magnification, y + dy);
                    Coordinate lr = new Coordinate(ur.x, y + image.getHeight()
                            * yRatio * magnification + dy);
                    Coordinate ll = new Coordinate(x, lr.y);
                    PixelCoverage extent = new PixelCoverage(ul, ur, lr, ll);
                    target.drawRaster(image, extent, paintProps);
                }
                x += 12 * xRatio * magnification;
            }
            return width;
        }
    }

    public void dispose() {
        if (this.wireframeShape != null) {
            this.wireframeShape.dispose();
        }
    }

    public DataTime getDataTime() {
        return this.dataTime;
    }

    public void setDescriptor(IMapDescriptor descriptor) {
        this.descriptor = descriptor;
    }

    /**
     * Status object that allowed rendering to continue, but notifies the user
     * of potential problems.
     */
    public class RedbookStatus {
        public boolean vectorRenderingWarning;

        public boolean unhandledPackets;

    }
}
