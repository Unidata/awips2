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
package com.raytheon.uf.viz.remote.graphics.adapters;

import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.common.serialization.IDeserializationContext;
import com.raytheon.uf.common.serialization.ISerializationContext;
import com.raytheon.uf.common.serialization.ISerializationTypeAdapter;
import com.raytheon.uf.common.serialization.SerializationException;

/**
 * Adapter for swt Rectangle object
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 14, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class RectangleAdapter implements ISerializationTypeAdapter<Rectangle> {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.ISerializationTypeAdapter#serialize
     * (com.raytheon.uf.common.serialization.ISerializationContext,
     * java.lang.Object)
     */
    @Override
    public void serialize(ISerializationContext serializer, Rectangle object)
            throws SerializationException {
        serializer.writeI32(object.x);
        serializer.writeI32(object.y);
        serializer.writeI32(object.width);
        serializer.writeI32(object.height);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.ISerializationTypeAdapter#deserialize
     * (com.raytheon.uf.common.serialization.IDeserializationContext)
     */
    @Override
    public Rectangle deserialize(IDeserializationContext deserializer)
            throws SerializationException {
        return new Rectangle(deserializer.readI32(), deserializer.readI32(),
                deserializer.readI32(), deserializer.readI32());
    }

}
