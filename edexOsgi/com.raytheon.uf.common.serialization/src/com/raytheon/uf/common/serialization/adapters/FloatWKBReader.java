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

import java.io.DataInput;
import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Array;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.MultiLineString;
import com.vividsolutions.jts.geom.MultiPoint;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.io.WKBConstants;

/**
 * Class for deserializing geometries in FloatWKB format. FloatWKB is a format
 * based off of but completely incompatible with WKB. The only difference from
 * WKB is that instead of reading every coordinate as a 8 byte double it is read
 * as a 4 byte float. This cuts the size of objects in half while decreasing the
 * precision.
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
public class FloatWKBReader {

    private final GeometryFactory factory;

    public FloatWKBReader(GeometryFactory factory) {
        this.factory = factory;
    }

    public Geometry readGeometry(InputStream in) throws IOException {
        return readGeometry((DataInput) new DataInputStream(in));
    }

    private Geometry readGeometry(DataInput di) throws IOException {
        byte byteOrder = di.readByte();
        if (byteOrder == WKBConstants.wkbNDR) {
            throw new IOException(
                    "WKBF does not currently support little endian");
        }
        int type = di.readInt();
        switch (type) {
        case WKBConstants.wkbPoint:
            return readPoint(di);
        case WKBConstants.wkbLineString:
            return readLineString(di);
        case WKBConstants.wkbPolygon:
            return readPolygon(di);
        case WKBConstants.wkbMultiPoint:
            return readMultiPoint(di);
        case WKBConstants.wkbMultiLineString:
            return readMultiLineString(di);
        case WKBConstants.wkbMultiPolygon:
            return readMultiPolygon(di);
        case WKBConstants.wkbGeometryCollection:
            return readGeometryCollection(di);
        }
        // If the geometry contains three ordinates or a SRID it will also end
        // up here
        throw new IOException("Unknown WKB type " + (type & 0xff));
    }

    private Point readPoint(DataInput di) throws IOException {
        return factory.createPoint(readCoordinate(di));
    }

    private LineString readLineString(DataInput di) throws IOException {
        return factory.createLineString(readCoordinates(di));
    }

    private Polygon readPolygon(DataInput di) throws IOException {
        int size = di.readInt();
        LinearRing shell = null;
        LinearRing[] holes = null;
        shell = factory.createLinearRing(readCoordinates(di));
        if (size > 1) {
            holes = new LinearRing[size - 1];
            for (int i = 1; i < size; i += 1) {
                holes[i - 1] = factory.createLinearRing(readCoordinates(di));
            }
        }
        return factory.createPolygon(shell, holes);
    }

    private MultiPoint readMultiPoint(DataInput di) throws IOException {
        return factory.createMultiPoint(readMultiGeometry(di, Point.class));
    }

    private MultiLineString readMultiLineString(DataInput di)
            throws IOException {
        return factory.createMultiLineString(readMultiGeometry(di,
                LineString.class));
    }

    private MultiPolygon readMultiPolygon(DataInput di) throws IOException {
        return factory.createMultiPolygon(readMultiGeometry(di, Polygon.class));
    }

    private GeometryCollection readGeometryCollection(DataInput di)
            throws IOException {
        return factory.createGeometryCollection(readMultiGeometry(di,
                Geometry.class));
    }

    private <T extends Geometry> T[] readMultiGeometry(DataInput di,
            Class<T> geomType) throws IOException {
        int size = di.readInt();
        @SuppressWarnings("unchecked")
        T[] geoms = (T[]) Array.newInstance(geomType, size);
        for (int i = 0; i < size; i++) {
            Geometry g = readGeometry(di);
            if (geomType.isInstance(g)) {
                geoms[i] = geomType.cast(g);
            } else {
                throw new IOException("Expected a "
                        + geomType.getClass().getSimpleName()
                        + " but recieved a " + g.getClass().getSimpleName());
            }
        }
        return geoms;
    }

    private Coordinate[] readCoordinates(DataInput di) throws IOException {
        int size = di.readInt();
        Coordinate[] coordinates = new Coordinate[size];
        for (int i = 0; i < size; i += 1) {
            coordinates[i] = readCoordinate(di);
        }
        return coordinates;
    }

    private Coordinate readCoordinate(DataInput di) throws IOException {
        return new Coordinate(di.readFloat(), di.readFloat());
    }

}
