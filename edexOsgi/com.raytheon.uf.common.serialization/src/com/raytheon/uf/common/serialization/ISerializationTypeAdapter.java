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
package com.raytheon.uf.common.serialization;

/**
 * Describes the interface for a serialization type adapter.
 * 
 * Adapters are necessary for serializing types that do not fit the traditional
 * Java Bean model, or are third party classes. Note: implementing classes need
 * to be thread safe.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Aug 7, 2008				chammack	Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public interface ISerializationTypeAdapter<T> {

    /**
     * Serialize an object
     * 
     * @param serializer
     *            the serializer
     * @param object
     *            the object
     * 
     * @throws SerializationException
     *             if an error occurs
     */
    public void serialize(ISerializationContext serializer, T object)
            throws SerializationException;

    /**
     * Deserialize an object
     * 
     * @param serializer
     *            the deserializer
     * @return the object
     * @throws SerializationException
     *             if an error occurs during serialization
     */
    public T deserialize(IDeserializationContext deserializer)
            throws SerializationException;
}
