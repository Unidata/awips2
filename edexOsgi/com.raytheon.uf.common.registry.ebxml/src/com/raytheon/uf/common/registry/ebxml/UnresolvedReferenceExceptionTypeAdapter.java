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
package com.raytheon.uf.common.registry.ebxml;

import java.util.List;

import com.raytheon.uf.common.serialization.IDeserializationContext;
import com.raytheon.uf.common.serialization.ISerializationContext;
import com.raytheon.uf.common.serialization.ISerializationTypeAdapter;
import com.raytheon.uf.common.serialization.SerializationException;

/**
 * Dynamic serialization adapter for {@link UnresolvedReferenceException}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 20, 2012 1187       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class UnresolvedReferenceExceptionTypeAdapter implements
        ISerializationTypeAdapter<UnresolvedReferenceException> {

    /**
     * {@inheritDoc}
     */
    @Override
    public void serialize(ISerializationContext serializer,
            UnresolvedReferenceException object) throws SerializationException {
        serializer.writeObject(object.getMessage());
        serializer.writeObject(object.getObjectReferenceIds());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public UnresolvedReferenceException deserialize(
            IDeserializationContext deserializer) throws SerializationException {
        String message = (String) deserializer.readObject();
        @SuppressWarnings("unchecked")
        List<String> ids = (List<String>) deserializer.readObject();

        return new UnresolvedReferenceException(message, ids);
    }

}
