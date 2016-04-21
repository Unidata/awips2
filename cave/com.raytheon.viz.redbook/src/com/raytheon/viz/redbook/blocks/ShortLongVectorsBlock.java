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
package com.raytheon.viz.redbook.blocks;

import java.util.ArrayList;
import java.util.List;

import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.redbook.blocks.RedbookBlock;
import com.raytheon.uf.common.dataplugin.redbook.blocks.RedbookBlockHeader;
import com.raytheon.viz.redbook.rsc.RedbookLegend;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;

/**
 * Implements the Redbook relative short and long vector block
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * May 22, 2008 1162        chammack    Initial creation
 * Apr 29, 2013 1958        bgonzale    New class RedbookBlockHeader.
 * Mar 13, 2014 2907        njensen     split edex.redbook plugin into common
 *                                      and edex redbook plugins
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class ShortLongVectorsBlock extends RedbookBlock {

    protected Geometry geometry;

    public ShortLongVectorsBlock(RedbookBlockHeader header,
            java.nio.ByteBuffer data, MathTransform mt, int maxX, int maxY,
            RedbookLegend legend) {
        super(header, data);

        List<Geometry> geometries = new ArrayList<Geometry>();
        List<Coordinate> coords = new ArrayList<Coordinate>();

        int length = getLength();

        int posX = (data.getShort());
        int posY = maxY - (data.getShort());

        double[] out = new double[2];
        try {
            mt.transform(new double[] { posX, posY }, 0, out, 0, 1);
        } catch (TransformException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        coords.add(new Coordinate(out[0], out[1]));

        int consumed = 8;
        GeometryFactory gf = new GeometryFactory();
        int lastX = posX;
        int lastY = posY;
        boolean firstPoint = true;
        int flagB = 0;

        while (consumed < (length * 2)) {

            short i1 = (data.getShort());

            int flagA = (i1 & 0x8000);
            if (flagA == 0) {
                // LONG FORMAT (Two shorts):
                // 1st short: 0XXA AAAA AAAA AAAA
                // 2nd short: 0XBA AAAA AAAA AAAA
                // Where B = beam format, A=twos complement integer
                short i2 = (data.getShort());
                int m = (i1 << 19) >> 19;
                int n = (i2 << 19) >> 19;

                flagB = (i2 & 0x2000);
                if (flagB > 0 && !firstPoint) {
                    if (coords.size() > 1) {
                        LineString lineString = gf.createLineString(coords
                                .toArray(new Coordinate[coords.size()]));
                        geometries.add(lineString);

                    }
                    coords.clear();
                }

                int curX = m + lastX;
                int curY = lastY - n;
                try {
                    mt.transform(new double[] { curX, curY }, 0, out, 0, 1);
                } catch (TransformException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
                if (curX != lastX || curY != lastY)
                    coords.add(new Coordinate(out[0], out[1]));
                else
                    geometries.add(gf
                            .createPoint(new Coordinate(out[0], out[1])));
                lastX = curX;
                lastY = curY;
                legend.addCoordinate(curX, curY);

                consumed += 4;
            } else {

                // SHORT FORMAT (One Short)
                // 1AAA AAAA BAAA AAAA
                // Where B = beam format, A=twos complement integer

                int m = (i1 << 17) >> 25;
                int n = (i1 << 25) >> 25;
                flagB = i1 & 0x0080;
                if (flagB > 0 && !firstPoint) {
                    if (coords.size() > 1) {
                        LineString lineString = gf.createLineString(coords
                                .toArray(new Coordinate[coords.size()]));
                        geometries.add(lineString);
                    }
                    coords.clear();
                }
                int curX = m + lastX;
                int curY = lastY - n;
                try {
                    mt.transform(new double[] { curX, curY }, 0, out, 0, 1);
                } catch (TransformException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }

                if (curX != lastX || curY != lastY)
                    coords.add(new Coordinate(out[0], out[1]));
                else
                    geometries.add(gf
                            .createPoint(new Coordinate(out[0], out[1])));
                lastX = curX;
                lastY = curY;
                legend.addCoordinate(curX, curY);

                consumed += 2;
            }
            firstPoint = false;

        }

        if (coords.size() > 1 && flagB == 0) {
            LineString lineString = gf.createLineString(coords
                    .toArray(new Coordinate[coords.size()]));
            geometries.add(lineString);
        }
        this.geometry = gf.createGeometryCollection(geometries
                .toArray(new Geometry[geometries.size()]));

    }

    /**
     * @return the lineString
     */
    public Geometry getGeometry() {
        return geometry;
    }

}