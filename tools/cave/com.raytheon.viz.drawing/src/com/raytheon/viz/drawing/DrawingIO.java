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

//import java.io.StringReader;
import java.sql.Date;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import javax.xml.bind.JAXBException;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;

import com.raytheon.edex.msg.CoordinateXML;
import com.raytheon.edex.msg.collaboration.CollaborationObject;
import com.raytheon.edex.msg.collaboration.DrawingChangeMessage;
import com.raytheon.edex.msg.collaboration.DrawingState;
import com.raytheon.edex.msg.collaboration.Line;
import com.raytheon.edex.msg.collaboration.Path;
import com.raytheon.edex.msg.collaboration.Rectangle;
import com.raytheon.edex.msg.collaboration.Symbol;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.viz.adapter.NullCoordConverter;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Polygon;

import fsl.tools.glyph.FreehandPath;
import fsl.tools.glyph.Glyph;
import fsl.tools.scribble.ColdFront;
import fsl.tools.scribble.Front;
import fsl.tools.scribble.StationaryFront;
import fsl.tools.scribble.WarmFront;

/**
 * Provides support to turn drawing objects into XML
 * 
 * <pre>
 * 
 *     SOFTWARE HISTORY
 *   
 *     Date         Ticket#     Engineer    Description
 *     ------------ ----------  ----------- --------------------------
 *     Nov 27, 2006             chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class DrawingIO {
    private static NullCoordConverter ncc = new NullCoordConverter(null);

    public static void handleDrawingChange(DrawingLayer layer, String message) {
        // try {
        // StringReader sr = new StringReader(message);
        // IBindingFactory bfact = BindingDirectory
        // .getFactory(DrawingChangeMessage.class);
        // IUnmarshallingContext mctx = bfact.createUnmarshallingContext();
        // Object o = mctx.unmarshalDocument(sr);

        try {
            // System.out.println(">>>>>>>>>>>>> : \n" + message);
            Object o = SerializationUtil.unmarshalFromXml(message);
            DrawingChangeMessage dmsg = (DrawingChangeMessage) o;
            handleDrawingChange(layer, dmsg);
        } catch (JAXBException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        // DrawingChangeMessage dmsg = (DrawingChangeMessage) o;
        // handleDrawingChange(layer, dmsg);

        // } catch (JiBXException e) {
        // // TODO Auto-generated catch block
        // e.printStackTrace();
        // }
    }

    public static void handleDrawingChange(DrawingLayer layer,
            DrawingChangeMessage msg) {

        CollaborationObject obj = msg.getObject();
        if (msg.getState() == DrawingChangeMessage.STATE_CREATE) {
            if (obj instanceof com.raytheon.edex.msg.collaboration.Front) {
                Front f = createFront((com.raytheon.edex.msg.collaboration.Front) obj);
                if (f == null)
                    return;
                layer.addFront(f, false, obj.getId());
            } else if (obj instanceof Symbol) {
                Symbol s = (Symbol) obj;
                CoordinateXML c = s.getCenterPoint();
                layer.addSymbol(new Coordinate(c.getX(), c.getY()),
                        s.getSymbolId(), false, obj.getId());
            } else if (obj instanceof Line) {
                Line l = (Line) obj;
                Coordinate[] c = new Coordinate[2];
                c[0] = l.getPoint1().getCoordinate();
                c[1] = l.getPoint2().getCoordinate();
                GeometryFactory gf = new GeometryFactory();
                layer.drawLine(gf.createLineString(c), true, obj.getId());
            } else if (obj instanceof Path) {
                Path p = (Path) obj;
                CoordinateXML[] cXML = p.getPoints();
                Coordinate[] c = new Coordinate[cXML.length];
                for (int i = 0; i < cXML.length; i++) {
                    c[i] = cXML[i].getCoordinate();
                }

                GeometryFactory gf = new GeometryFactory();
                layer.drawLine(gf.createLineString(c), true, obj.getId());

            } else if (obj instanceof Rectangle) {
                Rectangle rect = (Rectangle) obj;
                Coordinate lr = rect.getLowerRight().getCoordinate();
                Coordinate ul = rect.getUpperLeft().getCoordinate();

                Coordinate ur = new Coordinate(lr.x, ul.y);
                Coordinate ll = new Coordinate(ul.x, lr.y);

                GeometryFactory gf = new GeometryFactory();
                LinearRing ring = gf.createLinearRing(new Coordinate[] { ul,
                        ur, lr, ll, ul });
                Polygon poly = gf.createPolygon(ring, null);
                layer.drawRect(poly, true, obj.getId());

            }
        } else if (msg.getState() == DrawingChangeMessage.STATE_DELETE) {
            String uuid = msg.getObject().getId();
            layer.removeObjectByUUID(uuid);
        }

        layer.issueRefresh();
    }

    private static Front createFront(com.raytheon.edex.msg.collaboration.Front f) {
        Front newFront = null;
        if (f.getType().equals(
                com.raytheon.edex.msg.collaboration.Front.FRONT_COLD)) {
            newFront = new ColdFront(Glyph.ALL_FRAMES, 1,
                    FreehandPath.SMOOTHING_BICUBIC, 24.0, 12.0, false, false);
        } else if (f.getType().equals(
                com.raytheon.edex.msg.collaboration.Front.FRONT_WARM)) {
            newFront = new WarmFront(Glyph.ALL_FRAMES, 1,
                    FreehandPath.SMOOTHING_BICUBIC, 24.0, 12.0, false, false);
        } else if (f.getType().equals(
                com.raytheon.edex.msg.collaboration.Front.FRONT_STATIONARY)) {
            newFront = new StationaryFront(Glyph.ALL_FRAMES, 1,
                    FreehandPath.SMOOTHING_BICUBIC, 24.0, 12.0, false, false);
        }

        if (newFront == null)
            return null;
        Event e = new Event();
        boolean started = false;
        for (CoordinateXML cxml : f.getPoints()) {

            e.type = SWT.MouseDown;
            e.x = (int) (cxml.getX() * 1000);
            e.y = (int) (cxml.getY() * 1000);
            if (!started) {
                e.type = SWT.MouseDown;
                newFront.startEdit(Front.FINISH_CREATION, e, null, ncc);
                started = true;
            } else {
                e.type = SWT.MouseMove;
                newFront.edit(e, null, ncc);
            }
        }

        newFront.finishEdit(null, ncc);
        return newFront;
    }

    public static String getDrawingStateXML(DrawingLayer layer) {
        ArrayList<CollaborationObject> list = new ArrayList<CollaborationObject>();

        DrawingState drawingState = new DrawingState();
        List<Front> fronts = layer.getFronts();
        Map<Integer, List<Coordinate>> symbolMap = layer.getSymbolMap();

        for (int i = 0; i < fronts.size(); i++) {
            com.raytheon.edex.msg.collaboration.Front frontXML = new com.raytheon.edex.msg.collaboration.Front();

            Front f = fronts.get(i);

            if (f instanceof ColdFront)
                frontXML.setType(com.raytheon.edex.msg.collaboration.Front.FRONT_COLD);
            else if (f instanceof WarmFront)
                frontXML.setType(com.raytheon.edex.msg.collaboration.Front.FRONT_WARM);
            else if (f instanceof StationaryFront)
                frontXML.setType(com.raytheon.edex.msg.collaboration.Front.FRONT_STATIONARY);

            int numPoints = f.getNumPoints();

            CoordinateXML[] cxml = new CoordinateXML[numPoints];

            for (int j = 0; j < numPoints; j++) {
                cxml[j] = new CoordinateXML();
                Coordinate p = f.getPoint(j);
                cxml[j].setX(p.x);
                cxml[j].setY(p.y);
            }

            frontXML.setId(UUID.randomUUID().toString());
            frontXML.setPoints(cxml);
            list.add(frontXML);
        }

        Set<Integer> keySet = symbolMap.keySet();
        Iterator<Integer> keyIterator = keySet.iterator();
        while (keyIterator.hasNext()) {
            Integer key = keyIterator.next();

            List<Coordinate> c = symbolMap.get(key);

            for (int j = 0; j < c.size(); j++) {
                Symbol symbol = new Symbol();
                symbol.setSymbolId(key);
                Coordinate coordinateItem = c.get(j);
                CoordinateXML cxml = new CoordinateXML();
                cxml.setX(coordinateItem.x);
                cxml.setY(coordinateItem.y);
                symbol.setCenterPoint(cxml);
                symbol.setId(UUID.randomUUID().toString());
                list.add(symbol);
            }

        }

        CollaborationObject[] objs = list.toArray(new CollaborationObject[list
                .size()]);

        drawingState.setObjects(objs);

        CoordinateXML pt = new CoordinateXML();
        drawingState.setLrScreenPosition(pt);
        drawingState.setUlScreenPosition(pt);

        // try {
        // StringWriter sw = new StringWriter();
        //
        // IBindingFactory bfact = BindingDirectory.getFactory(drawingState
        // .getClass());
        // IMarshallingContext mctx = bfact.createMarshallingContext();
        // mctx.setIndent(3, "\n", ' ');
        // mctx.marshalDocument(drawingState, null, null, sw);
        //
        // return sw.toString();
        // } catch (JiBXException e) {
        // // TODO Auto-generated catch block
        // e.printStackTrace();
        // }

        return null;

    }

    public static String buildFrontChangeMessage(Front f, String uuid, int state) {

        DrawingChangeMessage drawingChange = new DrawingChangeMessage();

        com.raytheon.edex.msg.collaboration.Front frontXML = new com.raytheon.edex.msg.collaboration.Front();

        if (f != null) {
            if (f instanceof ColdFront)
                frontXML.setType(com.raytheon.edex.msg.collaboration.Front.FRONT_COLD);
            else if (f instanceof WarmFront)
                frontXML.setType(com.raytheon.edex.msg.collaboration.Front.FRONT_WARM);
            else if (f instanceof StationaryFront)
                frontXML.setType(com.raytheon.edex.msg.collaboration.Front.FRONT_STATIONARY);

            int numPoints = f.getNumPoints();

            CoordinateXML[] cxml = new CoordinateXML[numPoints];

            for (int j = 0; j < numPoints; j++) {
                cxml[j] = new CoordinateXML();
                Coordinate p = f.getPoint(j);
                cxml[j].setX(p.x);
                cxml[j].setY(p.y);
            }
            frontXML.setPoints(cxml);
        }

        frontXML.setId(uuid);

        return marshallMessage(state, drawingChange, frontXML);
    }

    public static String buildSymbolChangeMessage(int id, Coordinate c,
            String uuid, int state) {
        DrawingChangeMessage drawingChange = new DrawingChangeMessage();
        Symbol symbol = null;

        symbol = new Symbol();
        symbol.setId(uuid);
        if (c != null) {
            symbol.setCenterPoint(new CoordinateXML(c));
            symbol.setSymbolId(id);
        }

        return marshallMessage(state, drawingChange, symbol);
    }

    public static String buildLineChangeMessage(LineString line, String uuid,
            int state) {

        DrawingChangeMessage drawingChange = new DrawingChangeMessage();

        Line lineXML = new Line();
        lineXML.setId(uuid);
        if (line != null) {
            Coordinate[] c = line.getCoordinates();
            lineXML.setPoint1(new CoordinateXML(c[0]));
            lineXML.setPoint2(new CoordinateXML(c[1]));
        }
        return marshallMessage(state, drawingChange, lineXML);
    }

    public static String buildPathChangeMessage(LineString lines, String uuid,
            int state) {

        DrawingChangeMessage drawingChange = new DrawingChangeMessage();

        Path path = new Path();
        path.setId(uuid);

        if (lines != null) {
            Coordinate[] c = lines.getCoordinates();
            CoordinateXML[] cXML = new CoordinateXML[c.length];

            for (int i = 0; i < c.length; i++) {
                cXML[i] = new CoordinateXML(c[i]);
            }
            path.setPoints(cXML);
        }

        return marshallMessage(state, drawingChange, path);
    }

    public static String buildRectChangeMessage(Polygon poly, String uuid,
            int state) {

        DrawingChangeMessage drawingChange = new DrawingChangeMessage();

        Rectangle rect = new Rectangle();
        rect.setId(uuid);

        if (poly != null) {
            Envelope env = poly.getEnvelopeInternal();
            Coordinate ul = new Coordinate(env.getMinX(), env.getMaxY());
            Coordinate lr = new Coordinate(env.getMaxX(), env.getMinY());
            rect.setUpperLeft(new CoordinateXML(ul));
            rect.setLowerRight(new CoordinateXML(lr));
        }

        return marshallMessage(state, drawingChange, rect);
    }

    private static String marshallMessage(int state,
            DrawingChangeMessage drawingChange, CollaborationObject obj) {
        String mess;
        mess = null;
        drawingChange.setDate(new Date(System.currentTimeMillis()));
        drawingChange.setObject(obj);
        drawingChange.setState(state);

        try {

            mess = SerializationUtil.marshalToXml(drawingChange);
            // StringWriter sw = new StringWriter();
            // SerializationUtil.marshalToXml(drawingChange);
            // System.out.println("==========: \n" + mess);
        } catch (JAXBException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        // IBindingFactory bfact = BindingDirectory.getFactory(drawingChange
        // .getClass());
        // IMarshallingContext mctx = bfact.createMarshallingContext();
        //
        // mctx.setIndent(3, "\n", ' ');
        // mctx.marshalDocument(drawingChange, null, null, sw);

        // return sw.toString();
        // } catch (JiBXException e) {
        // // TODO Auto-generated catch block
        // e.printStackTrace();
        // }

        return mess;
    }
}