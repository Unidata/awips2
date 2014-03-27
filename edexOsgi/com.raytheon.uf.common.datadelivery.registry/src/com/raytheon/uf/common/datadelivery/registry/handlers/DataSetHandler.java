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

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;

import org.geotools.geometry.jts.ReferencedEnvelope;

import com.raytheon.uf.common.datadelivery.registry.DataSet;
import com.raytheon.uf.common.datadelivery.registry.Parameter;
import com.raytheon.uf.common.datadelivery.registry.DataLevelType.LevelType;
import com.raytheon.uf.common.datadelivery.registry.ebxml.DataSetQuery;
import com.raytheon.uf.common.datadelivery.registry.ebxml.DataSetWithFiltersQuery;
import com.raytheon.uf.common.registry.RegistryQueryResponse;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.registry.ebxml.UnresolvedReferenceException;
import com.raytheon.uf.common.registry.handler.BaseRegistryObjectHandler;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.CollectionUtil;

/**
 * DataSet registry handler.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 3, 2012  1241      djohnson     Initial creation
 * Nov 19, 2012 1166      djohnson     Clean up JAXB representation of registry objects.
 * Dec 10, 2012 1259      bsteffen     Switch Data Delivery from LatLon to referenced envelopes.
 * Jun 04, 2013  223      mpduff       Added datatype to the filter.
 * Jun 24, 2013 2106      djohnson     Now composes a registryHandler.
 * Oct 09, 2013 2267      bgonzale     Fix Collection cast to List error.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class DataSetHandler extends
        BaseRegistryObjectHandler<DataSet, DataSetQuery> implements
        IDataSetHandler {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(DataSetHandler.class);

    /**
     * {@inheritDoc}
     */
    @Override
    public void store(DataSet obj) throws RegistryHandlerException {
        try {
            super.store(obj);
        } catch (RegistryHandlerException e) {
            boolean tryAgain = handleUnresolvedReferences(obj, e);

            if (tryAgain) {
                try {
                    super.store(obj);
                } catch (RegistryHandlerException e1) {
                    throw e1;
                }
            } else {
                throw e;
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void update(DataSet obj) throws RegistryHandlerException {
        try {
            super.update(obj);
        } catch (RegistryHandlerException e) {
            boolean tryAgain = handleUnresolvedReferences(obj, e);

            if (tryAgain) {
                try {
                    super.update(obj);
                } catch (RegistryHandlerException e1) {
                    throw e1;
                }
            } else {
                throw e;
            }
        }
    }

    /**
     * Attempts to persist any unresolved referenced parameter objects.
     * 
     * @param obj
     *            the object we are attempting to store
     * @param e
     *            the exception that occurred
     * @return true if another attempt should be made
     */
    private boolean handleUnresolvedReferences(DataSet obj,
            RegistryHandlerException e) {
        boolean restore = false;

        // Check to see if the Failure was because of a missing
        // parameter
        final Throwable cause = e.getCause();
        if (cause instanceof MsgRegistryException) {

            Throwable u = cause.getCause();

            if (u instanceof UnresolvedReferenceException) {

                List<String> ids = ((UnresolvedReferenceException) u)
                        .getObjectReferenceIds();
                // Get the ObjectReferenceId from the Exception and
                // lookup
                // the missing Parameter Object

                // Need to remap Parameters so that they can be
                // referenced by
                // their RegistryObject ids...
                Map<String, Parameter> remap = new HashMap<String, Parameter>();
                for (Parameter parm : (Collection<Parameter>) obj
                        .getParameters().values()) {
                    try {
                        remap.put(RegistryUtil.getRegistryObjectKey(parm), parm);
                    } catch (Throwable e1) {
                        // If an Exception is thrown, abandon the
                        // store...
                        return false;
                    }
                }

                for (String id : ids) {

                    statusHandler.info("Unresolved parameter reference [" + id
                            + "]");
                    Parameter p = remap.get(id);
                    if (p != null) {
                        // TODO: This has to be revisited for
                        // non-gridded data...
                        try {
                            DataDeliveryHandlers.getParameterHandler()
                                    .update(p);
                        } catch (RegistryHandlerException e1) {
                            return false;
                        }
                    } else {
                        statusHandler
                                .error("Failed retrieval of parameter id ["
                                        + id + "]");
                    }
                }

                // If we get to here, let's try storing it again...
                restore = true;
            }
        }

        return restore;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Class<DataSet> getRegistryObjectClass() {
        return DataSet.class;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected DataSetQuery getQuery() {
        return new DataSetQuery();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public DataSet getByNameAndProvider(String name, String providerName)
            throws RegistryHandlerException {
        DataSetQuery query = new DataSetQuery();
        query.setDataSetName(name);
        query.setProviderName(providerName);

        RegistryQueryResponse<DataSet> response = registryHandler
                .getObjects(query);

        checkResponse(response, "getByNameAndProvider");

        return response.getSingleResult();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<DataSet> getByFilters(List<String> providers,
            List<String> dataSetNames, Set<LevelType> levels,
            List<String> parameterNames, List<String> dataTypes,
            ReferencedEnvelope envelope) throws RegistryHandlerException {
        DataSetWithFiltersQuery query = new DataSetWithFiltersQuery();
        if (!CollectionUtil.isNullOrEmpty(providers)) {
            query.setProviderNames(providers);
        }

        if (!CollectionUtil.isNullOrEmpty(dataSetNames)) {
            query.setDataSetNames(dataSetNames);
        }

        if (!CollectionUtil.isNullOrEmpty(levels)) {
            query.setLevels(levels);
        }

        if (!CollectionUtil.isNullOrEmpty(parameterNames)) {
            query.setParameterNames(parameterNames);
        }

        if (!CollectionUtil.isNullOrEmpty(dataTypes)) {
            query.setDataSetTypes(dataTypes);
        }

        if (envelope != null && !envelope.isEmpty()) {
            query.setEnvelope(envelope);
        }

        RegistryQueryResponse<DataSet> response = registryHandler
                .getObjects(query);

        checkResponse(response, "getByFilters");

        return response.getResults();
    }

}
