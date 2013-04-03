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
package com.raytheon.uf.common.datadelivery.request;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.datadelivery.request.DataDeliveryPermissionsContainer.DataDeliveryPermissionsContainerTypeAdapter;
import com.raytheon.uf.common.serialization.IDeserializationContext;
import com.raytheon.uf.common.serialization.ISerializationContext;
import com.raytheon.uf.common.serialization.ISerializationTypeAdapter;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeTypeAdapter;

/**
 * Used to hide direct access to permission collections in
 * {@link DataDeliveryAuthRequest} from client-code.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 3, 2012  1241      djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@DynamicSerializeTypeAdapter(factory = DataDeliveryPermissionsContainerTypeAdapter.class)
public class DataDeliveryPermissionsContainer {

    public static class DataDeliveryPermissionsContainerTypeAdapter implements
            ISerializationTypeAdapter<DataDeliveryPermissionsContainer> {

        /**
         * {@inheritDoc}
         */
        @Override
        public void serialize(ISerializationContext serializer,
                DataDeliveryPermissionsContainer object)
                throws SerializationException {
            serializer.writeObject(object.authorizedList);
            serializer.writeObject(object.requestList);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        @SuppressWarnings("unchecked")
        public DataDeliveryPermissionsContainer deserialize(
                IDeserializationContext deserializer)
                throws SerializationException {
            DataDeliveryPermissionsContainer container = new DataDeliveryPermissionsContainer();
            container.authorizedList
                    .addAll((List<DataDeliveryPermission>) deserializer
                    .readObject());
            container.requestList
                    .addAll((List<DataDeliveryPermission>) deserializer
                    .readObject());

            return container;
        }

    }

    /**
     * List of permissions to check.
     */
    @DynamicSerializeElement
    private final List<DataDeliveryPermission> requestList = new ArrayList<DataDeliveryPermission>();

    /**
     * List of permissions that were authorized.
     */
    @DynamicSerializeElement
    private final List<DataDeliveryPermission> authorizedList = new ArrayList<DataDeliveryPermission>();

    /**
     * @param permission
     */
    public void addRequestedPermission(DataDeliveryPermission permission) {
        requestList.add(permission);
    }

    /**
     * @param permission
     * @return
     */
    public boolean contains(DataDeliveryPermission permission) {
        return authorizedList.contains(permission);
    }

    /**
     * @param permission
     */
    public void addAuthorized(DataDeliveryPermission permission) {
        this.authorizedList.add(permission);
    }

    /**
     * @return
     */
    public List<DataDeliveryPermission> getRequestedPermissions() {
        return requestList;
    }
}
