/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/diclosure is restricted by U.S. law. Dissemination
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
package com.raytheon.uf.common.serialization.adapters;

import java.io.DataOutput;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.MultiLineString;
import com.vividsolutions.jts.geom.MultiPoint;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.io.WKBConstants;

/**
 * Class for serializing geometries in FloatWKB format. FloatWKB is a format
 * based off of but completely incompatible with WKB. The only difference from
 * WKB is that instead of writing every coordinate as a 8 byte double it is
 * written as a 4 byte float. This cuts the size of objects in half while
 * decreasing the precision.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 26, 2013 1954       bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class FloatWKBWriter {

    public void writeGeometry(Geometry geom, OutputStream out)
            throws IOException {
        writeGeometry(geom, (DataOutput) new DataOutputStream(out));
    }

    private void writeGeometry(Geometry geom, DataOutput d)
            throws IOException {
        d.write(WKBConstants.wkbXDR);
        if (geom instanceof Point)
            writePoint((Point) geom, d);
        else if (geom instanceof LineString)
            writeLineString((LineString) geom, d);
        else if (geom instanceof Polygon)
            writePolygon((Polygon) geom, d);
        else if (geom instanceof MultiPoint)
            writeMultiPoint((MultiPoint) geom, d);
        else if (geom instanceof MultiLineString)
            writeMultiLineString((MultiLineString) geom, d);
        else if (geom instanceof MultiPolygon)
            writeMultiPolygon((MultiPolygon) geom, d);
        else if (geom instanceof GeometryCollection)
            writeGeometryCollection((GeometryCollection) geom, d);
        else {
            throw new IOException("Unknown Geometry type: "
                    + geom.getClass().getSimpleName());
        }
    }

    private void writePoint(Point p, DataOutput d) throws IOException {
        d.writeInt(WKBConstants.wkbPoint);
        writeCoordinate(p.getCoordinate(), d);
    }

    private void writeLineString(LineString ls, DataOutput d)
            throws IOException {
        d.writeInt(WKBConstants.wkbLineString);
        writeCoordinates(ls.getCoordinates(), d);
    }

    private void writePolygon(Polygon polygon, DataOutput d) throws IOException {
        d.writeInt(WKBConstants.wkbPolygon);
        d.writeInt(polygon.getNumInteriorRing() + 1);
        writeCoordinates(polygon.getExteriorRing().getCoordinates(), d);
        for (int i = 0; i < polygon.getNumInteriorRing(); i += 1) {
            writeCoordinates(polygon.getInteriorRingN(i).getCoordinates(), d);
        }
    }

    private void writeMultiPoint(MultiPoint mp, DataOutput d)
            throws IOException {
        d.writeInt(WKBConstants.wkbMultiPoint);
        writeMultiGeometry(mp, d);
    }

    private void writeMultiLineString(MultiLineString mls, DataOutput d)
            throws IOException {
        d.writeInt(WKBConstants.wkbMultiLineString);
        writeMultiGeometry(mls, d);
    }

    private void writeMultiPolygon(MultiPolygon mp, DataOutput d)
            throws IOException {
        d.writeInt(WKBConstants.wkbMultiPolygon);
        writeMultiGeometry(mp, d);
    }

    private void writeGeometryCollection(GeometryCollection gc, DataOutput d)
            throws IOException {
        d.writeInt(WKBConstants.wkbGeometryCollection);
        writeMultiGeometry(gc, d);
    }

    private void writeMultiGeometry(GeometryCollection gc, DataOutput d)
            throws IOException {
        d.writeInt(gc.getNumGeometries());
        for (int i = 0; i < gc.getNumGeometries(); i++) {
            writeGeometry(gc.getGeometryN(i), d);
        }
    }

    private void writeCoordinates(Coordinate[] coordinates, DataOutput d)
            throws IOException {
        d.writeInt(coordinates.length);
        for (Coordinate c : coordinates) {
            writeCoordinate(c, d);
        }
    }

    private void writeCoordinate(Coordinate c, DataOutput d) throws IOException {
        d.writeFloat((float) c.x);
        d.writeFloat((float) c.y);
    }
}
