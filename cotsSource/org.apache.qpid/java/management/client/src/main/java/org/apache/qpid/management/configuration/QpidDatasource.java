/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *
 */
package org.apache.qpid.management.configuration;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import org.apache.commons.pool.BasePoolableObjectFactory;
import org.apache.commons.pool.ObjectPool;
import org.apache.commons.pool.impl.GenericObjectPool;
import org.apache.commons.pool.impl.GenericObjectPoolFactory;
import org.apache.qpid.management.Messages;
import org.apache.qpid.transport.Connection;
import org.apache.qpid.transport.ConnectionException;
import org.apache.qpid.transport.util.Logger;

/**
 * Qpid datasource.
 * Basically it is a connection pool manager used for optimizing broker connections usage.
 *
 * @author Andrea Gazzarini
 */
public final class QpidDatasource
{
    private final static Logger LOGGER = Logger.get(QpidDatasource.class);

    /**
     * A connection decorator used for adding pool interaction behaviour to an existing connection.
     *
     * @author Andrea Gazzarini
     */
    class PooledConnection extends Connection
    {
        private final UUID _brokerId;
        private boolean _valid;

        /**
         * Builds a new decorator with the given connection.
         *
         * @param brokerId the broker identifier.
         */
        private PooledConnection(UUID brokerId)
        {
            this._brokerId = brokerId;
            _valid = true;
        }

        /**
         * Returns true if the underlying connection is still valid and can be used.
         *
         * @return true if the underlying connection is still valid and can be used.
         */
        boolean isValid()
        {
            return _valid;
        }

        void reallyClose()
        {
            super.close();
        }

        /**
         * Returns the connection to the pool. That is, marks this connections as available.
         * After that, this connection will be available for further operations.
         */
        public void close()
        {
            try
            {
                pools.get(_brokerId).returnObject(this);
                
                LOGGER.debug(Messages.QMAN_200006_QPID_CONNECTION_RELEASED, this);
            }
            catch (Exception e)
            {
                throw new ConnectionException(e);
            }
        }

        public void exception(Throwable t)
        {
            //super.exception(t);
            _valid = false;
        }
    }

    /**
     * This is the connection factory, that is, the factory used to manage the lifecycle (create, validate & destroy) of
     * the broker connection(s).
     *
     * @author Andrea Gazzarini
     */
    class QpidConnectionFactory extends BasePoolableObjectFactory
    {
        private final BrokerConnectionData _connectionData;
        private final UUID _brokerId;

        /**
         * Builds a new connection factory with the given parameters.
         *
         * @param brokerId the broker identifier.
         * @param connectionData the connecton data.
         */
        private QpidConnectionFactory(UUID brokerId, BrokerConnectionData connectionData)
        {
            this._connectionData = connectionData;
            this._brokerId = brokerId;
        }

        /**
         * Creates a new underlying connection.
         */
        @Override
        public Connection makeObject () throws Exception
        {
        	PooledConnection connection = new PooledConnection(_brokerId);
            connection.connect(
                    _connectionData.getHost(),
                    _connectionData.getPort(),
                    _connectionData.getVirtualHost(),
                    _connectionData.getUsername(),
                    _connectionData.getPassword(),
		    false);
            return connection;
        }

        /**
         * Validates the underlying connection.
         */
        @Override
        public boolean validateObject (Object obj)
        {
            PooledConnection connection = (PooledConnection) obj;
            boolean isValid = connection.isValid();

            LOGGER.debug(Messages.QMAN_200007_TEST_CONNECTION_ON_RESERVE,isValid);
            
            return isValid;
        }

        /**
         * Closes the underlying connection.
         */
        @Override
        public void destroyObject (Object obj) throws Exception
        {
            try
            {
                PooledConnection connection = (PooledConnection) obj;
                connection.reallyClose();
                
                LOGGER.debug(Messages.QMAN_200008_CONNECTION_DESTROYED);
            } catch (Exception exception)
            {
                LOGGER.debug(exception, Messages.QMAN_200009_CONNECTION_DESTROY_FAILURE);
            }
        }
    }

    // Singleton instance.
    private static QpidDatasource instance = new QpidDatasource();

    // Each entry contains a connection pool for a specific broker.
    private Map<UUID, ObjectPool> pools = new HashMap<UUID, ObjectPool>();

    // Private constructor.
    private QpidDatasource()
    {
    }

    /**
     * Gets an available connection from the pool of the given broker.
     *
     * @param brokerId the broker identifier.
     * @return a valid connection to the broker associated with the given identifier.
     */
    public Connection getConnection(UUID brokerId) throws Exception
    {
        return (Connection) pools.get(brokerId).borrowObject();
    }

    /**
     * Entry point method for retrieving the singleton instance of this datasource.
     *
     * @return the qpid datasource singleton instance.
     */
    public static QpidDatasource getInstance()
    {
        return instance;
    }

    /**
     * Adds a connection pool to this datasource.
     * 
     * @param brokerId the broker identifier that will be associated with the new connection pool.
     * @param connectionData the broker connection data.
     * @throws Exception when the pool cannot be created.
     */
    void addConnectionPool(UUID brokerId,BrokerConnectionData connectionData) throws Exception 
    {
        GenericObjectPoolFactory factory = new GenericObjectPoolFactory(
                new QpidConnectionFactory(brokerId,connectionData),
                connectionData.getMaxPoolCapacity(),
                GenericObjectPool.WHEN_EXHAUSTED_BLOCK,
                connectionData.getMaxWaitTimeout(),-1,
                true,
                false);
    
        ObjectPool pool = factory.createPool();

        // Open connections at startup according to initial capacity param value.
        int howManyConnectionAtStartup = connectionData.getInitialPoolCapacity(); 
        Object [] openStartupList = new Object[howManyConnectionAtStartup];
        
        // Open...
        for (int index  = 0; index < howManyConnectionAtStartup; index++)
        {
            openStartupList[index] = pool.borrowObject();
        }
        
        // ...and immediately return them to pool. In this way the pooled connection has been opened.
        for (int index = 0; index < howManyConnectionAtStartup; index++)
        {
            pool.returnObject(openStartupList[index]);
        }

        pools.put(brokerId,pool);
    }
}
