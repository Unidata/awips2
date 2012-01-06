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

import java.awt.Point;

import javax.xml.bind.annotation.adapters.XmlAdapter;

import com.raytheon.uf.common.serialization.IDeserializationContext;
import com.raytheon.uf.common.serialization.ISerializationContext;
import com.raytheon.uf.common.serialization.ISerializationTypeAdapter;
import com.raytheon.uf.common.serialization.SerializationException;

/**
 * Serialization adapter for Points
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

public class PointAdapter extends XmlAdapter<String, Point> implements
        ISerializationTypeAdapter<Point> {

    @Override
    public String marshal(Point coord) throws Exception {
        StringBuffer buf = new StringBuffer();
        buf.append(Double.toString(coord.x)).append(",").append(
                Double.toString(coord.y));
        return buf.toString();
    }

    @Override
    public Point unmarshal(String v) throws Exception {
        Point retVal = null;
        if (v != null) {
            String[] tokens = v.split(",");
            retVal = new Point(Integer.valueOf(tokens[0]), Integer
                    .valueOf(tokens[1]));
        }
        return retVal;
    }

    @Override
    public Point deserialize(IDeserializationContext deserializer)
            throws SerializationException {
        int x = deserializer.readI32();
        int y = deserializer.readI32();

        return new Point(x, y);
    }

    @Override
    public void serialize(ISerializationContext serializer, Point object)
            throws SerializationException {
        serializer.writeI32(object.x);
        serializer.writeI32(object.y);
    }

}
