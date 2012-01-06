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
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Serialization adapter for Coordinates
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Aug 20, 2008				njensen	Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class CoordAdapter extends XmlAdapter<String, Coordinate> implements
        ISerializationTypeAdapter<Coordinate> {

    public CoordAdapter() {
        super();
    }

    @Override
    public String marshal(Coordinate coord) throws Exception {
        StringBuffer buf = new StringBuffer();
        buf.append(Double.toString(coord.x)).append(",").append(
                Double.toString(coord.y));
        return buf.toString();
    }

    @Override
    public Coordinate unmarshal(String v) throws Exception {
        Coordinate retVal = null;
        if (v != null) {
            String[] tokens = v.split(",");
            retVal = new Coordinate(Double.valueOf(tokens[0]), Double
                    .valueOf(tokens[1]));
        }
        return retVal;
    }

    @Override
    public Coordinate deserialize(IDeserializationContext deserializer)
            throws SerializationException {
        double x = deserializer.readDouble();
        double y = deserializer.readDouble();

        return new Coordinate(x, y);
    }

    @Override
    public void serialize(ISerializationContext serializer, Coordinate object)
            throws SerializationException {
        serializer.writeDouble(object.x);
        serializer.writeDouble(object.y);
    }

}
