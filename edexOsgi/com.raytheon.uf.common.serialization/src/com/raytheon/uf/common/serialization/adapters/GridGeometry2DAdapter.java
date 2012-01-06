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

import org.geotools.coverage.grid.GridEnvelope2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.Envelope2D;
import org.geotools.referencing.CRS;
import org.opengis.geometry.Envelope;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.serialization.IDeserializationContext;
import com.raytheon.uf.common.serialization.ISerializationContext;
import com.raytheon.uf.common.serialization.ISerializationTypeAdapter;
import com.raytheon.uf.common.serialization.SerializationException;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 5, 2010            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class GridGeometry2DAdapter implements
        ISerializationTypeAdapter<GridGeometry2D> {

    @Override
    public GridGeometry2D deserialize(IDeserializationContext deserializer)
            throws SerializationException {

        try {
            int x = deserializer.readI32();
            int y = deserializer.readI32();
            int width = deserializer.readI32();
            int height = deserializer.readI32();
            GridEnvelope2D gridRange = new GridEnvelope2D(x, y, width, height);

            CoordinateReferenceSystem crs = CRS.parseWKT(deserializer
                    .readString());

            double dx = deserializer.readDouble();
            double dy = deserializer.readDouble();
            double dw = deserializer.readDouble();
            double dh = deserializer.readDouble();
            Envelope envelope = new Envelope2D(crs, dx, dy, dw, dh);

            GridGeometry2D gridGeom = new GridGeometry2D(gridRange, envelope);
            return gridGeom;
        } catch (Throwable e) {
            throw new SerializationException(
                    "Error deserializing GridGeomtry2D", e);
        }
    }

    @Override
    public void serialize(ISerializationContext serializer,
            GridGeometry2D gridGeom) throws SerializationException {
        serializer.writeI32(gridGeom.getGridRange2D().x);
        serializer.writeI32(gridGeom.getGridRange2D().y);
        serializer.writeI32(gridGeom.getGridRange2D().width);
        serializer.writeI32(gridGeom.getGridRange2D().height);

        serializer.writeString(gridGeom.getCoordinateReferenceSystem().toWKT());
        serializer.writeDouble(gridGeom.getEnvelope().getMinimum(0));
        serializer.writeDouble(gridGeom.getEnvelope().getMinimum(1));
        serializer.writeDouble(gridGeom.getEnvelope().getSpan(0));
        serializer.writeDouble(gridGeom.getEnvelope().getSpan(1));
    }

}
