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
package oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4;

import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryExceptionType;

import com.raytheon.uf.common.serialization.IDeserializationContext;
import com.raytheon.uf.common.serialization.ISerializationContext;
import com.raytheon.uf.common.serialization.ISerializationTypeAdapter;
import com.raytheon.uf.common.serialization.SerializationException;

/**
 * {@link ISerializationTypeAdapter} for {@link MsgRegistryException}.
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

public class MsgRegistryExceptionTypeAdapter implements
        ISerializationTypeAdapter<MsgRegistryException> {

    /**
     * {@inheritDoc}
     */
    @Override
    public void serialize(ISerializationContext serializer,
            MsgRegistryException object) throws SerializationException {
        serializer.writeObject(object.getFaultInfo());
        serializer.writeString(object.getMessage());
        serializer.writeObject(object.getCause());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public MsgRegistryException deserialize(IDeserializationContext deserializer)
            throws SerializationException {
        RegistryExceptionType exceptionType = (RegistryExceptionType) deserializer
                .readObject();
        String message = deserializer.readString();
        Throwable cause = (Throwable) deserializer.readObject();

        return new MsgRegistryException(message, exceptionType, cause);
    }

}
