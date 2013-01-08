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

import com.raytheon.uf.common.registry.OperationStatus;
import com.raytheon.uf.common.registry.RegistryQuery;
import com.raytheon.uf.common.registry.RegistryQueryResponse;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.LifecycleManager;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.QueryManager;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.RemoveObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.SubmitObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.UpdateObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseType;

/**
 * Extends {@link FactoryRegistryHandler} to allow it to be testable.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 20, 2012 0743       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class TestableFactoryRegistryHandler extends FactoryRegistryHandler
        implements LifecycleManagerFactory, QueryManagerFactory,
        RegistryTxManager, QueryManager, LifecycleManager {
    public TestableFactoryRegistryHandler() {
        setLcmFactory(this);
        setQueryFactory(this);
        setTxManager(this);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public RegistryQueryResponse getObjects(RegistryQuery query) {
        RegistryQueryResponse response = new RegistryQueryResponse();
        response.setStatus(OperationStatus.SUCCESS);

        return response;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public TxManager getTxManager() {
        return new TxManager() {
            @Override
            public void startTransaction() throws Exception {
            }

            @Override
            public void closeTransaction() {
            }
        };
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public QueryManager getQueryManager() {
        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public LifecycleManager getLifeCycleManager() {
        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public QueryResponse executeQuery(QueryRequest partQueryRequest)
            throws MsgRegistryException {
        return getQueryResponse();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public RegistryResponseType removeObjects(
            RemoveObjectsRequest partRemoveObjectsRequest)
            throws MsgRegistryException {
        return getQueryResponse();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public RegistryResponseType submitObjects(
            SubmitObjectsRequest partSubmitObjectsRequest)
            throws MsgRegistryException {
        return getQueryResponse();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public RegistryResponseType updateObjects(
            UpdateObjectsRequest partUpdateObjectsRequest)
            throws MsgRegistryException {
        return getQueryResponse();
    }

    /**
     * @return
     */
    private QueryResponse getQueryResponse() {
        QueryResponse response = new QueryResponse();
        response.setStatus(RegistryUtil.RESPONSE_SUCCESS);
        return response;
    }
}
