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

package com.raytheon.viz.drawing;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.apache.batik.bridge.BridgeContext;
import org.apache.batik.bridge.GVTBuilder;
import org.apache.batik.bridge.UserAgentAdapter;
import org.apache.batik.bridge.ViewBox;
import org.apache.batik.dom.svg.SAXSVGDocumentFactory;
import org.apache.batik.gvt.GraphicsNode;
import org.apache.batik.util.XMLResourceDescriptor;
import org.eclipse.swt.graphics.RGB;
import org.jivesoftware.smack.XMPPException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.svg.SVGDocument;

import com.raytheon.edex.msg.collaboration.DrawingChangeMessage;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.data.prep.IODataPreparer;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.adapter.CoordConverter;
import com.raytheon.viz.drawing.collaboration.CollaborationManager;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Polygon;

import fsl.tools.scribble.Front;

/**
 * Implements a basic drawing layer
 * 
 * @author chammack
 * 
 */
public class DrawingLayer extends
        AbstractVizResource<AbstractResourceData, MapDescriptor> {

    protected Geometry primitiveShape;

    protected List<Geometry> geometries;

    protected List<Geometry> tempGeometries;

    protected IShadedShape shadedShape;

    protected boolean hasDrawnOutline;

    protected boolean hasDrawnShaded;

    protected IGraphicsTarget target;

    protected List<FrontBundle> fronts;

    protected boolean frontNeedsRefresh = true;

    protected Map<File, IImage> imageMap;

    protected Map<Coordinate, IImage> coordinateMap;

    protected Map<Integer, List<Coordinate>> symbolMap;

    protected Map<String, Object> uuidMap;

    protected Map<Object, String> reverseUUIDMap;

    protected Map<Coordinate, String> stringMap;

    public class FrontBundle {
        public Front front;

        public IWireframeShape wireframe;

        public IShadedShape shaded;

        public double lastZoomLevel;
    }

    public DrawingLayer() {
        super(null, new LoadProperties());
        imageMap = new HashMap<File, IImage>();
        coordinateMap = new HashMap<Coordinate, IImage>();
        symbolMap = new HashMap<Integer, List<Coordinate>>();
        uuidMap = new HashMap<String, Object>();
        reverseUUIDMap = new HashMap<Object, String>();
        stringMap = new HashMap<Coordinate, String>();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#getName()
     */
    public String getName() {
        return "DrawingLayer";
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
        this.geometries = new ArrayList<Geometry>();
        this.tempGeometries = new ArrayList<Geometry>();

        this.target = target;
        fronts = new ArrayList<FrontBundle>();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.IVizResource#isApplicable(com.raytheon.viz.
     * core.PixelExtent)
     */
    public boolean isApplicable(PixelExtent extent) {
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.core.rsc.IVizResource#paint(com.raytheon.viz.core.
     * IGraphicsTarget, com.raytheon.viz.core.PixelExtent, double, float)
     */
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        float zoomLevel = paintProps.getZoomLevel();

        if (primitiveShape != null) {
            if (primitiveShape instanceof LineString) {
                drawLinePrimitive(primitiveShape, target, new RGB(255, 255, 0));

            } else if (primitiveShape instanceof Polygon) {
                drawRectPrimitive(primitiveShape, target, new RGB(255, 255, 0));
            }
        }

        if (hasDrawnOutline) {
            for (Geometry g : this.geometries)
                if (g instanceof LineString)
                    drawLinePrimitive(g, target, new RGB(1.0f, 1.0f, 1.0f));
                else if (g instanceof Polygon)
                    drawRectPrimitive(g, target, new RGB(1.0f, 1.0f, 1.0f));
        }
        if (hasDrawnShaded) {
            if (shadedShape != null)
                target.drawShadedShape(shadedShape, 1.0f);
        }

        for (Geometry g : this.tempGeometries)
            if (g instanceof LineString)
                drawLinePrimitive(g, target, new RGB(1.0f, 1.0f, 1.0f));
            else if (g instanceof Polygon)
                drawRectPrimitive(g, target, new RGB(1.0f, 1.0f, 1.0f));

        if (fronts != null) {
            for (FrontBundle frontB : fronts) {
                Front front = frontB.front;

                if (frontB.lastZoomLevel != zoomLevel || frontNeedsRefresh
                        || frontB.wireframe == null || frontB.shaded == null) {
                    if (frontB.wireframe == null)
                        frontB.wireframe = target.createWireframeShape(true,
                                descriptor);
                    else
                        frontB.wireframe.reset();

                    if (frontB.shaded == null)
                        frontB.shaded = target.createShadedShape(true,
                                descriptor, false);
                    else
                        frontB.shaded.reset();

                    CoordConverter cc = new CoordConverter(
                            getResourceContainer());
                    front.flushDisplayCoordinates();
                    frontB.lastZoomLevel = zoomLevel;
                    front.createShape(target, cc);
                    front.prepareShape(frontB.wireframe, frontB.shaded, cc);
                    frontNeedsRefresh = false;
                }

                Color frontColor = front.getColor();
                RGB color = new RGB(frontColor.getRed(), frontColor.getGreen(),
                        frontColor.getBlue());

                target.drawWireframeShape(frontB.wireframe, color, 1.0f);
                target.drawShadedShape(frontB.shaded, 1.0f);

            }
        }

        if (coordinateMap != null) {
            Iterator<Coordinate> coordIterator = coordinateMap.keySet()
                    .iterator();
            while (coordIterator.hasNext()) {
                Coordinate key = coordIterator.next();
                IImage img = coordinateMap.get(key);
                // Calc stamp area

                float offset = 2.5f * zoomLevel;

                Envelope env = new Envelope(key.x - offset, key.x + offset,
                        key.y - offset, key.y + offset);

                PixelCoverage pc = this.descriptor.worldToPixel(env);

                target.drawRaster(img, pc, paintProps);

            }
        }

        if (this.stringMap != null) {
            Iterator<Coordinate> coordIterator = this.stringMap.keySet()
                    .iterator();
            while (coordIterator.hasNext()) {
                Coordinate key = coordIterator.next();
                String string = this.stringMap.get(key);

                double[] out = this.descriptor.worldToPixel(new double[] {
                        key.x, key.y });
                target.drawString(null, string, out[0], out[1], 0.0,
                        IGraphicsTarget.TextStyle.NORMAL, new RGB(255, 0, 0),
                        HorizontalAlignment.LEFT, null);

            }
        }

    }

    private void drawRectPrimitive(Geometry shape, IGraphicsTarget target,
            RGB color) throws VizException {
        Envelope rect = ((Polygon) shape).getEnvelopeInternal();

        PixelCoverage coverage = this.descriptor.worldToPixel(rect);

        target.drawRect(new PixelExtent(coverage.getMinX(), coverage.getMaxX(),
                coverage.getMinY(), coverage.getMaxY()), color, 1.0f, 1.0);
    }

    private void drawLinePrimitive(Geometry shape, IGraphicsTarget target,
            RGB color) throws VizException {
        LineString line = (LineString) shape;

        int pts = line.getNumPoints();

        for (int i = 1; i < pts; i++) {
            double[] p1 = this.descriptor
                    .worldToPixel(new double[] { line.getPointN(i - 1).getX(),
                            line.getPointN(i - 1).getY() });
            double[] p2 = this.descriptor.worldToPixel(new double[] {
                    line.getPointN(i).getX(), line.getPointN(i).getY() });

            if (p1.length == 3 && p1[2] != 0.0 && p2[2] != 0.0) {
                target.drawLine(p1[0], p1[1], p1[2], p2[0], p2[1], p2[2],
                        color, 1.0f);
            } else {
                target.drawLine(p1[0], p1[1], 0.0, p2[0], p2[1], 0.0, color,
                        1.0f);
            }
        }
    }

    /**
     * Draw a line using pixel coordinates
     * 
     * UUID is optional, and generally should be null
     * 
     * @param line
     * @param isFinal
     * @param uuid
     */
    public void drawLine(LineString line, boolean isFinal, String uuid) {
        if (!isFinal) {
            this.primitiveShape = line;
        } else {
            this.primitiveShape = null;

            this.geometries.add(line);
            this.hasDrawnOutline = true;

            if (CollaborationManager.isRunning() && uuid == null) {
                if (uuid == null)
                    uuid = UUID.randomUUID().toString();

                String msg = null;

                if (line.getNumPoints() == 2) {
                    msg = DrawingIO.buildLineChangeMessage(line, uuid,
                            DrawingChangeMessage.STATE_CREATE);
                } else
                    msg = DrawingIO.buildPathChangeMessage(line, uuid,
                            DrawingChangeMessage.STATE_CREATE);
                try {
                    CollaborationManager.getInstance().sendCollabMessage(msg);
                } catch (XMPPException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }

            if (uuid == null)
                uuid = UUID.randomUUID().toString();

            this.uuidMap.put(uuid, line);
            this.reverseUUIDMap.put(line, uuid);
        }
    }

    public void drawTempLine(LineString line, boolean isFinal) {
        if (!isFinal) {
            this.primitiveShape = line;
        } else {

            this.primitiveShape = null;

            this.tempGeometries.add(line);
            this.hasDrawnOutline = true;
        }
    }

    /**
     * Draw a rectangle using pixel space
     * 
     * UUID is optional, and should generally be null
     * 
     * @param rect
     * @param isFinal
     * @param uuid
     */
    public void drawRect(Polygon rect, boolean isFinal, String uuid) {
        if (!isFinal) {
            primitiveShape = rect;
        } else {
            primitiveShape = null;

            this.geometries.add(rect);
            this.hasDrawnOutline = true;

            if (CollaborationManager.isRunning() && uuid == null) {
                if (uuid == null)
                    uuid = UUID.randomUUID().toString();

                String msg = DrawingIO.buildRectChangeMessage(rect, uuid,
                        DrawingChangeMessage.STATE_CREATE);
                try {
                    CollaborationManager.getInstance().sendCollabMessage(msg);
                } catch (XMPPException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }

            if (uuid == null)
                uuid = UUID.randomUUID().toString();

            this.uuidMap.put(uuid, rect);
            this.reverseUUIDMap.put(rect, uuid);
        }
    }

    public void reset() {
        this.hasDrawnOutline = false;
        this.hasDrawnShaded = false;

        this.geometries.clear();
        this.reverseUUIDMap.clear();
        this.uuidMap.clear();

        for (FrontBundle f : fronts) {
            if (f.shaded != null)
                f.shaded.dispose();
            if (f.wireframe != null)
                f.wireframe.dispose();
        }

        Iterator<IImage> images = coordinateMap.values().iterator();
        while (images.hasNext()) {
            images.next().dispose();
        }

        this.coordinateMap.clear();
        this.imageMap.clear();
        this.fronts.clear();
        this.stringMap.clear();
    }

    public void resetTemp() {
        this.tempGeometries.clear();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#dispose()
     */
    @Override
    protected void disposeInternal() {
        if (this.geometries != null) {
            this.geometries.clear();
        }

        if (this.tempGeometries != null) {
            this.tempGeometries.clear();
        }

        for (FrontBundle f : this.fronts) {
            if (f.shaded != null)
                f.shaded.dispose();
            if (f.wireframe != null)
                f.wireframe.dispose();
        }
        this.fronts.clear();

        Iterator<IImage> images = this.coordinateMap.values().iterator();
        while (images.hasNext()) {
            images.next().dispose();
        }

        this.coordinateMap.clear();
        this.imageMap.clear();
        this.fronts.clear();

        CollaborationManager mgr = CollaborationManager.getInstance();

        if (mgr != null) {
            mgr.leave();
        }

    }

    public void addShadedShape(IShadedShape ss) {
        if (shadedShape != null)
            shadedShape.dispose();
        shadedShape = ss;
        hasDrawnShaded = true;
    }

    public void addSymbol(Coordinate coord, int i, boolean propogate,
            String uuid) {
        File f = new File(VizApp.getMapsDir() + "/" + i + ".svg");

        IImage image = imageMap.get(f);

        if (image == null) {
            String parser = XMLResourceDescriptor.getXMLParserClassName();
            SAXSVGDocumentFactory factory = new SAXSVGDocumentFactory(parser);
            Document document = null;
            try {
                document = factory.createDocument(f.toURI().toURL().toString());
            } catch (MalformedURLException e1) {
                // TODO Auto-generated catch block
                e1.printStackTrace();
            } catch (IOException e1) {
                // TODO Auto-generated catch block
                e1.printStackTrace();
            }

            UserAgentAdapter userAgentAdapter = new UserAgentAdapter();
            // userAgentAdapter.setTransform(arg0)
            BridgeContext bridgeContext = new BridgeContext(userAgentAdapter);
            GVTBuilder builder = new GVTBuilder();

            GraphicsNode theGraphicsNode = builder.build(bridgeContext,
                    document);
            BufferedImage bufferedImage = new BufferedImage(400, 400,
                    BufferedImage.TYPE_4BYTE_ABGR);
            Graphics2D g2d = bufferedImage.createGraphics();

            // For a smooth graphic with no jagged edges or rastorized look.
            g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                    RenderingHints.VALUE_ANTIALIAS_ON);
            g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION,
                    RenderingHints.VALUE_INTERPOLATION_BILINEAR);

            // Scale image to desired size
            Element elt = ((SVGDocument) document).getRootElement();
            AffineTransform usr2dev = ViewBox.getViewTransform(null, elt, 400,
                    400);
            g2d.transform(usr2dev);

            theGraphicsNode.paint(g2d);

            // Cleanup and return image
            g2d.dispose();

            image = target.initializeRaster(new IODataPreparer(bufferedImage,
                    "Symbol", 0), null);

            imageMap.put(f, image);

        }

        List<Coordinate> cList = symbolMap.get(i);
        if (cList == null)
            cList = new ArrayList<Coordinate>();
        cList.add(coord);
        symbolMap.put(i, cList);

        coordinateMap.put(coord, image);

        if (CollaborationManager.isRunning() && propogate) {
            if (uuid == null)
                uuid = UUID.randomUUID().toString();

            String msg = DrawingIO.buildSymbolChangeMessage(i, coord, uuid,
                    DrawingChangeMessage.STATE_CREATE);
            try {
                CollaborationManager.getInstance().sendCollabMessage(msg);
            } catch (XMPPException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
        if (uuid == null)
            uuid = UUID.randomUUID().toString();

        uuidMap.put(uuid, coord);
        reverseUUIDMap.put(coord, uuid);

    }

    public void addFront(Front f, boolean propogate, String uuid) {
        FrontBundle fb = new FrontBundle();
        fb.front = f;
        fronts.add(fb);
        frontNeedsRefresh = true;
        resetTemp();

        if (propogate == false) {
            // CoordConverter converter = new CoordConverter(editor);
            // f.smooth(null, converter);
        }

        if (uuid == null)
            uuid = UUID.randomUUID().toString();

        this.uuidMap.put(uuid, f);
        this.reverseUUIDMap.put(f, uuid);

        if (CollaborationManager.isRunning() && propogate) {
            String msg = DrawingIO.buildFrontChangeMessage(f, uuid,
                    DrawingChangeMessage.STATE_CREATE);
            try {
                CollaborationManager.getInstance().sendCollabMessage(msg);
            } catch (XMPPException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }

        uuidMap.put(uuid, fb);
        reverseUUIDMap.put(fb, uuid);

    }

    protected Map<Integer, List<Coordinate>> getSymbolMap() {
        return symbolMap;
    }

    protected List<Front> getFronts() {
        List<Front> frontList = new ArrayList<Front>();
        for (FrontBundle fb : fronts)
            frontList.add(fb.front);

        return frontList;
    }

    public void removeObjectByUUID(String uuid) {
        Object obj = uuidMap.get(uuid);

        if (obj instanceof Geometry) {
            geometries.remove(obj);
        } else if (obj instanceof FrontBundle) {
            fronts.remove(obj);
        } else if (obj instanceof Coordinate) {
            this.coordinateMap.remove(obj);
            this.stringMap.remove(obj);
        }

        uuidMap.remove(uuid);
        reverseUUIDMap.remove(obj);

    }

    /**
     * Add Text
     * 
     * @param text
     *            the text to add
     * @param c
     *            the starting point of the text
     * @param uuid
     *            the uuid
     */
    public void addText(String text, Coordinate c, String uuid) {

        this.stringMap.put(c, text);

        if (uuid == null)
            uuid = UUID.randomUUID().toString();

        this.uuidMap.put(uuid, c);
        this.reverseUUIDMap.put(c, uuid);

    }

    public void removeObjectAt(Coordinate c) {
        GeometryFactory gf = new GeometryFactory();
        Coordinate c1 = new Coordinate(c.x - 0.25, c.y - 0.25);
        Coordinate c2 = new Coordinate(c.x + 0.25, c.y + 0.25);
        Coordinate c3 = new Coordinate(c.x - 0.25, c.y + 0.25);
        Coordinate c4 = new Coordinate(c.x + 0.25, c.y - 0.25);

        LinearRing lr = gf.createLinearRing(new Coordinate[] { c1, c3, c2, c4,
                c1 });
        Polygon p = gf.createPolygon(lr, null);

        Envelope env = new Envelope(c1, c2);

        // Check symbols first
        String msg = null;
        for (Coordinate coord : this.coordinateMap.keySet()) {
            if (env.contains(coord)) {
                this.coordinateMap.remove(coord);

                String uuid = reverseUUIDMap.remove(coord);
                msg = DrawingIO.buildSymbolChangeMessage(0, null, uuid,
                        DrawingChangeMessage.STATE_DELETE);
                break;
            }
        }

        if (msg == null) {
            for (Geometry g : this.geometries) {
                if (p.intersects(g)) {
                    this.geometries.remove(g);
                    String uuid = reverseUUIDMap.remove(g);
                    msg = DrawingIO.buildLineChangeMessage(null, uuid,
                            DrawingChangeMessage.STATE_DELETE);
                    break;
                }
            }
        }

        if (msg == null) {
            for (FrontBundle fb : this.fronts) {
                int pts = fb.front.getNumPoints();
                Coordinate[] coords = new Coordinate[pts];
                for (int i = 0; i < pts; i++) {
                    coords[i] = fb.front.getPoint(i);
                }

                LineString ls = gf.createLineString(coords);

                if (p.intersects(ls)) {
                    fb.shaded.dispose();
                    fb.wireframe.dispose();

                    this.fronts.remove(fb);
                    String uuid = reverseUUIDMap.remove(fb);
                    msg = DrawingIO.buildFrontChangeMessage(null, uuid,
                            DrawingChangeMessage.STATE_DELETE);
                    break;
                }
            }
        }

        if (msg != null && CollaborationManager.isRunning()) {
            try {
                CollaborationManager.getInstance().sendCollabMessage(msg);
            } catch (XMPPException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }

    }

}
