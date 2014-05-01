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

import com.raytheon.uf.common.serialization.IDeserializationContext;
import com.raytheon.uf.common.serialization.ISerializationContext;
import com.raytheon.uf.common.serialization.ISerializationTypeAdapter;
import com.raytheon.uf.common.serialization.SerializationException;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKBReader;
import com.vividsolutions.jts.io.WKBWriter;

/**
 * Serializes a geometry in a binary format
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Aug 11, 2008             chammack    Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class GeometryTypeAdapter implements ISerializationTypeAdapter<Geometry> {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.esb.serialize.ISerializationFactory#deserialize(com
     * .raytheon.edex.esb.serialize.ISerializer)
     */
    @Override
    public Geometry deserialize(IDeserializationContext serializer)
            throws SerializationException {

        byte[] data = serializer.readBinary();
        if (data.length == 0) {
            return null;
        }

        try {
            WKBReader parser = new WKBReader();
            Geometry geom = parser.read(data);
            return geom;
        } catch (RuntimeException e) {
            e.printStackTrace();
            System.out.println("Bad data: " + data);
            return null;
        } catch (ParseException e) {
            System.out.println("Bad data, unable to parse: " + data);
            return null;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.edex.esb.serialize.ISerializationFactory#serialize(com.raytheon
     * .edex.esb.serialize.ISerializer, java.lang.Object)
     */
    @Override
    public void serialize(ISerializationContext serializer, Geometry object)
            throws SerializationException {
        byte[] data = null;
        if (object == null) {
            data = new byte[0];
        } else {
            WKBWriter writer = new WKBWriter();
            data = writer.write(object);
        }

        serializer.writeBinary(data);
    }

}
