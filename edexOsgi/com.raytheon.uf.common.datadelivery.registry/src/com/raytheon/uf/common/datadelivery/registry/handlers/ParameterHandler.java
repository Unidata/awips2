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

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;

import com.raytheon.uf.common.datadelivery.registry.Parameter;
import com.raytheon.uf.common.datadelivery.registry.ParameterLevel;
import com.raytheon.uf.common.datadelivery.registry.ebxml.DataLevelTypeDescriptionQuery;
import com.raytheon.uf.common.datadelivery.registry.ebxml.ParameterNameQuery;
import com.raytheon.uf.common.datadelivery.registry.ebxml.ParameterQuery;
import com.raytheon.uf.common.registry.RegistryManager;
import com.raytheon.uf.common.registry.RegistryQueryResponse;
import com.raytheon.uf.common.registry.ebxml.UnresolvedReferenceException;
import com.raytheon.uf.common.registry.handler.BaseRegistryObjectHandler;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Parameter registry handler.
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

public class ParameterHandler extends
        BaseRegistryObjectHandler<Parameter, ParameterQuery> implements
        IParameterHandler {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ParameterHandler.class);

    private static final String SUCCESSFULLY_STORED_PARAMETER = "Successfully stored parameter [%s]";

    private static final String SUCCESSFULLY_UPDATED_PARAMETER = "Successfully updated parameter [%s]";

    private static final String FAILED_UPDATING_PARAMETER = "Failed updating parameter [%s]";

    private static final String FAILED_STORING_PARAMETER = "Failed storing parameter [%s]";

    /**
     * Stores a parameter object to the registry. If necessary, also store the
     * ParameterLevel Objects needed to successfully store the Parameter Object.
     */
    @Override
    public void store(Parameter obj) throws RegistryHandlerException {
        final String parameterName = obj.getName();
        try {
            super.store(obj);

            statusHandler.info(String.format(SUCCESSFULLY_STORED_PARAMETER,
                    parameterName));
        } catch (RegistryHandlerException e) {
            boolean tryAgain = handleUnresolvedReferences(obj, e);

            if (tryAgain) {
                try {
                    super.store(obj);

                    statusHandler.info(String.format(
                            SUCCESSFULLY_STORED_PARAMETER, parameterName));
                } catch (RegistryHandlerException e1) {
                    statusHandler.error(String.format(FAILED_STORING_PARAMETER,
                            parameterName), e1);
                    throw e1;
                }
            } else {
                statusHandler.error(
                        String.format(FAILED_STORING_PARAMETER, parameterName),
                        e);
            }
        }
    }

    /**
     * Updates a parameter object to the registry. If necessary, also store the
     * ParameterLevel Objects needed to successfully store the Parameter Object.
     */
    @Override
    public void update(Parameter obj) throws RegistryHandlerException {
        final String parameterName = obj.getName();
        try {
            super.update(obj);

            statusHandler.info(String.format(SUCCESSFULLY_UPDATED_PARAMETER,
                    parameterName));
        } catch (RegistryHandlerException e) {
            boolean tryAgain = handleUnresolvedReferences(obj, e);

            if (tryAgain) {
                try {
                    super.update(obj);

                    statusHandler.info(String.format(
                            SUCCESSFULLY_UPDATED_PARAMETER, parameterName));
                } catch (RegistryHandlerException e1) {
                    statusHandler.error(String.format(
                            FAILED_UPDATING_PARAMETER, parameterName), e1);
                }
            } else {
                statusHandler
                        .error(String.format(FAILED_UPDATING_PARAMETER,
                                parameterName), e);
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<String> getDataLevelTypeDescriptions(List<String> dataTypes)
            throws RegistryHandlerException {
        DataLevelTypeDescriptionQuery query = new DataLevelTypeDescriptionQuery();
        query.setDataTypes(dataTypes);

        RegistryQueryResponse<String> response = RegistryManager
                .getRegistyObjects(query);
        checkResponse(response, "getDataLevelTypeDescriptions");

        return response.getResults();
    }

    private boolean handleUnresolvedReferences(Parameter obj,
            RegistryHandlerException e) {
        boolean restore = false;

        // Check to see if the Failure was because of a missing
        // ParameterLevel

        Throwable cause = e.getCause();
        if (cause instanceof MsgRegistryException) {
            Throwable u = cause.getCause();
            if (u instanceof UnresolvedReferenceException) {
                List<String> ids = ((UnresolvedReferenceException) u)
                        .getObjectReferenceIds();
                // Get the ObjectReferenceId from the Exception and
                // create
                // the necessary ParameterLevel Object
                for (String id : ids) {
                    String[] parts = id.split("-");
                    ParameterLevel pl = new ParameterLevel();
                    pl.setLevelId(Integer.parseInt(parts[0]));
                    pl.setLevelValue(Double.parseDouble(parts[1]));
                    try {
                        DataDeliveryHandlers.getParameterLevelHandler().update(
                                pl);
                        statusHandler
                                .info("Successfully stored ParameterLevel "
                                        + id);
                        restore = true;
                    } catch (RegistryHandlerException e1) {
                        statusHandler.error("Failed to store ParameterLevel "
                                + id);
                    }
                }

                // If we get this far, let's attempt a restore...
                restore = true;
            }
        }

        return restore;

    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Class<Parameter> getRegistryObjectClass() {
        return Parameter.class;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected ParameterQuery getQuery() {
        return new ParameterQuery();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Set<String> getNamesByDataTypes(List<String> dataTypes)
            throws RegistryHandlerException {

        ParameterNameQuery query = new ParameterNameQuery();
        query.setDataTypes(dataTypes);

        RegistryQueryResponse<String> response = RegistryManager
                .getRegistyObjects(query);

        checkResponse(response, "getNamesByDataTypes");

        return new HashSet<String>(response.getResults());
    }
}
