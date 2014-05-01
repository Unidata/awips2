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
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKTReader;

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

public class JTSGeometryAdapter extends XmlAdapter<String, Geometry> implements
        ISerializationTypeAdapter<Geometry> {

    @Override
    public String marshal(Geometry g) throws Exception {
        return g.toText();
    }

    @Override
    public Geometry unmarshal(String wkt) throws Exception {
        Geometry geom = null;
        try {
            geom = new WKTReader().read(wkt);
        } catch (ParseException e) {
            throw new SerializationException("Error parsing wkt", e);
        }
        return geom;
    }

    @Override
    public Geometry deserialize(IDeserializationContext deserializer)
            throws SerializationException {
        String wkt = deserializer.readString();
        Geometry geom = null;
        try {
            geom = new WKTReader().read(wkt);
        } catch (ParseException e) {
            throw new SerializationException("Error parsing wkt", e);
        }
        return geom;
    }

    @Override
    public void serialize(ISerializationContext serializer, Geometry object)
            throws SerializationException {
        serializer.writeString(object.toText());
    }

}
