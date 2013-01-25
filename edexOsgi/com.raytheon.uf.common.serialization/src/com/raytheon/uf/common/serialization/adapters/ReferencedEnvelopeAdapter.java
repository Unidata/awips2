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

import org.geotools.geometry.jts.ReferencedEnvelope;
import org.geotools.referencing.CRS;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.serialization.IDeserializationContext;
import com.raytheon.uf.common.serialization.ISerializationContext;
import com.raytheon.uf.common.serialization.ISerializationTypeAdapter;
import com.raytheon.uf.common.serialization.SerializationException;

/**
 * 
 * Adapter for serialization and JAXBing ReferencedEnvelope objects.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 4, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class ReferencedEnvelopeAdapter extends
        XmlAdapter<ReferencedEnvelopeSerialized, ReferencedEnvelope> implements
        ISerializationTypeAdapter<ReferencedEnvelope> {

    @Override
    public ReferencedEnvelope deserialize(IDeserializationContext deserializer)
            throws SerializationException {
        CoordinateReferenceSystem crs;
        try {
            crs = CRS.parseWKT(deserializer.readString());

            return new ReferencedEnvelope(deserializer.readDouble(),
                    deserializer.readDouble(), deserializer.readDouble(),
                    deserializer.readDouble(), crs);
        } catch (FactoryException e) {
            throw new SerializationException(
                    "Error deserializing ReferencedEnvelope, could not read CRS",
                    e);
        }
    }

    @Override
    public void serialize(ISerializationContext serializer,
            ReferencedEnvelope object) throws SerializationException {
        serializer.writeString(object.getCoordinateReferenceSystem().toWKT());
        serializer.writeDouble(object.getMinX());
        serializer.writeDouble(object.getMaxX());
        serializer.writeDouble(object.getMinY());
        serializer.writeDouble(object.getMaxY());
    }

    @Override
    public ReferencedEnvelope unmarshal(ReferencedEnvelopeSerialized v)
            throws Exception {
        return new ReferencedEnvelope(v.minX, v.maxX, v.minY, v.maxY,
                CRS.parseWKT(v.crs));
    }

    @Override
    public ReferencedEnvelopeSerialized marshal(ReferencedEnvelope v)
            throws Exception {
        if (v == null) {
            return null;
        }
        ReferencedEnvelopeSerialized r = new ReferencedEnvelopeSerialized();
        r.crs = v.getCoordinateReferenceSystem().toWKT();
        r.minX = v.getMinX();
        r.maxX = v.getMaxX();
        r.minY = v.getMinY();
        r.maxY = v.getMaxY();
        return r;
    }

}
