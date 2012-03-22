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

import com.raytheon.uf.common.serialization.IDeserializationContext;
import com.raytheon.uf.common.serialization.ISerializationContext;
import com.raytheon.uf.common.serialization.ISerializationTypeAdapter;
import com.raytheon.uf.common.serialization.SerializationException;
import com.vividsolutions.jts.geom.Envelope;

/**
 * Serialization adapter for JTS Geometries
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Aug 20, 2008             njensen Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class JTSEnvelopeAdapter extends XmlAdapter<String, Envelope> implements
        ISerializationTypeAdapter<Envelope> {

    private static final String SEPARATOR = ",";

    @Override
    public Envelope deserialize(IDeserializationContext deserializer)
            throws SerializationException {
        return new Envelope(deserializer.readDouble(),
                deserializer.readDouble(), deserializer.readDouble(),
                deserializer.readDouble());
    }

    @Override
    public void serialize(ISerializationContext serializer, Envelope object)
            throws SerializationException {
        serializer.writeDouble(object.getMinX());
        serializer.writeDouble(object.getMaxX());
        serializer.writeDouble(object.getMinY());
        serializer.writeDouble(object.getMaxY());
    }

    @Override
    public Envelope unmarshal(String v) throws Exception {
        String[] split = v.split(SEPARATOR);
        double minX = Double.valueOf(split[0]);
        double maxX = Double.valueOf(split[1]);
        double minY = Double.valueOf(split[2]);
        double maxY = Double.valueOf(split[3]);
        return new Envelope(minX, maxX, minY, maxY);
    }

    @Override
    public String marshal(Envelope v) throws Exception {
        StringBuffer sb = new StringBuffer();
        sb.append(v.getMinX());
        sb.append(SEPARATOR);
        sb.append(v.getMaxX());
        sb.append(SEPARATOR);
        sb.append(v.getMinY());
        sb.append(SEPARATOR);
        sb.append(v.getMaxY());
        return sb.toString();
    }

}
