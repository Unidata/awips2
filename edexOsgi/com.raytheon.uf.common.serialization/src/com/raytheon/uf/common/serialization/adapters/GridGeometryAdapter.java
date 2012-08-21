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
package com.raytheon.uf.common.serialization.adapters;

import javax.xml.bind.annotation.adapters.XmlAdapter;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridEnvelope2D;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.referencing.CRS;
import org.opengis.coverage.grid.GridEnvelope;
import org.opengis.geometry.Envelope;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.serialization.IDeserializationContext;
import com.raytheon.uf.common.serialization.ISerializationContext;
import com.raytheon.uf.common.serialization.ISerializationTypeAdapter;
import com.raytheon.uf.common.serialization.SerializationException;

/**
 * Marshals GeneralGridGeometry to a class that is easily JAXB'd
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 11, 2009            chammack     Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class GridGeometryAdapter extends
        XmlAdapter<GridGeometrySerialized, GeneralGridGeometry> implements
        ISerializationTypeAdapter<GeneralGridGeometry> {

    @Override
    public GridGeometrySerialized marshal(GeneralGridGeometry v)
            throws Exception {
        GridGeometrySerialized ggs = new GridGeometrySerialized();
        ggs.rangeX = new Integer[] { v.getGridRange().getLow(0),
                v.getGridRange().getHigh(0) };
        ggs.rangeY = new Integer[] { v.getGridRange().getLow(1),
                v.getGridRange().getHigh(1) };

        ggs.CRS = v.getCoordinateReferenceSystem().toWKT();
        ggs.envelopeMinX = v.getEnvelope().getMinimum(0);
        ggs.envelopeMinY = v.getEnvelope().getMinimum(1);
        ggs.envelopeMaxX = v.getEnvelope().getMaximum(0);
        ggs.envelopeMaxY = v.getEnvelope().getMaximum(1);

        if (v.getGridRange().getDimension() == 3
                && v.getEnvelope().getDimension() == 3) {
            ggs.rangeZ = new Integer[] { v.getGridRange().getLow(2),
                    v.getGridRange().getHigh(2) };
            ggs.envelopeMinZ = v.getEnvelope().getMinimum(2);
            ggs.envelopeMaxZ = v.getEnvelope().getMaximum(2);
        }

        return ggs;
    }

    @Override
    public GeneralGridGeometry unmarshal(GridGeometrySerialized v)
            throws Exception {
        CoordinateReferenceSystem crs = CRS.parseWKT(v.CRS);
        GeneralEnvelope env = new GeneralEnvelope(crs);
        env.setRange(0, v.envelopeMinX, v.envelopeMaxX);
        env.setRange(1, v.envelopeMinY, v.envelopeMaxY);

        GeneralGridGeometry ggg = null;
        final int gridX = v.rangeX[1];
        final int gridY = v.rangeY[1];

        if (v.envelopeMinZ == null && v.envelopeMaxZ == null
                && v.rangeZ == null) {
            GridEnvelope2D ge = new GridEnvelope2D(v.rangeX[0], v.rangeY[0],
                    v.rangeX[1] - v.rangeX[0] + 1, v.rangeY[1] - v.rangeY[0]
                            + 1);
            ggg = new GeneralGridGeometry(ge, env);
        } else {
            final int gridZ = v.rangeZ[1];

            env.setRange(2, v.envelopeMinZ, v.envelopeMaxZ);
            GeneralGridEnvelope gge = new GeneralGridEnvelope(new int[] {
                    gridX / -2, gridY / -2, gridZ / -2 }, new int[] {
                    gridX / 2, gridY / 2, gridZ / 2 }, false);
            ggg = new GeneralGridGeometry(gge, env);
        }

        return ggg;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.ISerializationTypeAdapter#serialize
     * (com.raytheon.uf.common.serialization.ISerializationContext,
     * java.lang.Object)
     */
    @Override
    public void serialize(ISerializationContext serializer,
            GeneralGridGeometry object) throws SerializationException {
        int numDims = object.getDimension();
        GridEnvelope range = object.getGridRange();
        Envelope env = object.getEnvelope();

        serializer.writeString(object.getCoordinateReferenceSystem().toWKT());
        serializer.writeI32(numDims);
        for (int i = 0; i < numDims; ++i) {
            serializer.writeI32(range.getLow(i));
            serializer.writeI32(range.getHigh(i));
            serializer.writeDouble(env.getMinimum(i));
            serializer.writeDouble(env.getMaximum(i));
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.ISerializationTypeAdapter#deserialize
     * (com.raytheon.uf.common.serialization.IDeserializationContext)
     */
    @Override
    public GeneralGridGeometry deserialize(IDeserializationContext deserializer)
            throws SerializationException {
        try {
            CoordinateReferenceSystem crs = CRS.parseWKT(deserializer
                    .readString());
            int numDims = deserializer.readI32();
            GeneralEnvelope env = new GeneralEnvelope(crs);
            int[] lowRange = new int[numDims];
            int[] highRange = new int[numDims];
            for (int i = 0; i < numDims; ++i) {
                lowRange[i] = deserializer.readI32();
                highRange[i] = deserializer.readI32();
                env.setRange(i, deserializer.readDouble(),
                        deserializer.readDouble());
            }

            return new GeneralGridGeometry(new GeneralGridEnvelope(lowRange,
                    highRange, false), env);
        } catch (FactoryException e) {
            throw new SerializationException(
                    "Error deserializing GeneralGridGeometry, could not read CRS",
                    e);
        }
    }
}
