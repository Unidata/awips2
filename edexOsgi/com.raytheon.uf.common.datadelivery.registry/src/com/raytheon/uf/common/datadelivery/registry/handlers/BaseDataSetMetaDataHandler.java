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
package com.raytheon.uf.common.datadelivery.registry.handlers;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.ebxml.DataSetMetaDataDatesQuery;
import com.raytheon.uf.common.datadelivery.registry.ebxml.DataSetMetaDataFilterableQuery;
import com.raytheon.uf.common.registry.RegistryManager;
import com.raytheon.uf.common.registry.RegistryQueryResponse;
import com.raytheon.uf.common.registry.handler.BaseRegistryObjectHandler;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.time.util.ImmutableDate;

/**
 * DataSetMetaData registry handler.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 03, 2012 1241      djohnson     Initial creation
 * Oct 17, 2012 0726      djohnson     Move in {@link #getByDataSet}.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public abstract class BaseDataSetMetaDataHandler<T extends DataSetMetaData, QUERY extends DataSetMetaDataFilterableQuery<T>>
        extends BaseRegistryObjectHandler<T, QUERY> implements
        IBaseDataSetMetaDataHandler<T> {

    /**
     * {@inheritDoc}
     */
    @Override
    public Set<ImmutableDate> getDatesForDataSet(
            String dataSetName, String providerName)
            throws RegistryHandlerException {
        DataSetMetaDataDatesQuery query = new DataSetMetaDataDatesQuery();
        query.setDataSetName(dataSetName);
        query.setProviderName(providerName);

        RegistryQueryResponse<ImmutableDate> response = RegistryManager
                .getRegistyObjects(query);

        checkResponse(response, "getDatesForDataSet");

        return new HashSet<ImmutableDate>(response.getResults());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<T> getByDataSet(String dataSetName,
            String providerName) throws RegistryHandlerException {
        QUERY query = getQuery();
        query.setDataSetName(dataSetName);
        query.setProviderName(providerName);

        RegistryQueryResponse<T> response = RegistryManager
                .getRegistyObjects(query);

        checkResponse(response, "getByDataSet");

        return response.getResults();
    }
}
