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
package com.raytheon.uf.common.dataaccess;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import com.raytheon.uf.common.dataaccess.exception.DataFactoryNotFoundException;

/**
 * Registry containing the support for different datatypes in the Data Access
 * Framework. The registry maps a datatype name/key to a request type
 * (IGridRequest vs IGeometryRequest) and the corresponding factory to support
 * that request type.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 10, 2012            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class DataFactoryRegistry {

    private Map<String, Map<Class<IDataRequest<?>>, IDataFactory<?, ?>>> datatypeMap;

    private static DataFactoryRegistry instance;

    /**
     * Constructor
     */
    private DataFactoryRegistry() {
        datatypeMap = new HashMap<String, Map<Class<IDataRequest<?>>, IDataFactory<?, ?>>>();
    }

    /**
     * Returns the instance of the DataFactoryRegistry
     * 
     * @return
     */
    public static DataFactoryRegistry getInstance() {
        if (instance == null) {
            instance = new DataFactoryRegistry();
        }
        return instance;
    }

    /**
     * Registers a datatype name with a request type and the supporting factory.
     * Should only be called from spring.
     * 
     * @param datatype
     *            the datatype name that will become the key for requests'
     *            datatypes
     * @param requestType
     *            IGridRequest vs IGeometryRequest
     * @param factory
     *            the factory that will support requests of this datatype and
     *            type
     */
    public IDataFactory<?, ?> register(String datatype,
            Class<IDataRequest<?>> requestType, IDataFactory<?, ?> factory) {
        Map<Class<IDataRequest<?>>, IDataFactory<?, ?>> requestTypeMap = datatypeMap
                .get(datatype);
        if (requestTypeMap == null) {
            requestTypeMap = new HashMap<Class<IDataRequest<?>>, IDataFactory<?, ?>>();
            datatypeMap.put(datatype, requestTypeMap);
        }
        requestTypeMap.put(requestType, factory);
        return factory;
    }

    /**
     * Returns the factory that should process the request. Will never return
     * null and will instead throw an exception if no registered factories
     * match.
     * 
     * @param request
     *            the request to find a matching factory for
     * @return the factory that is registered to process the request
     * @throws DataFactoryNotFoundException
     * @throws IllegalArgumentException
     */
    @SuppressWarnings("unchecked")
    public <R extends IDataRequest<D>, D extends IData> IDataFactory<R, D> getFactory(
            R request) {
        String datatype = request.getDatatype();
        if (datatype != null) {
            Map<Class<IDataRequest<?>>, IDataFactory<?, ?>> requestTypeMap = datatypeMap
                    .get(datatype);
            if (requestTypeMap != null) {
                IDataFactory<?, ?> factory = null;
                for (Entry<Class<IDataRequest<?>>, IDataFactory<?, ?>> entry : requestTypeMap
                        .entrySet()) {
                    if (entry.getKey().isInstance(request)) {
                        factory = entry.getValue();
                        break;
                    }
                }
                if (factory != null) {
                    return (IDataFactory<R, D>) factory;
                } else {
                    throw new DataFactoryNotFoundException(
                            "No data access support for requests of datatype "
                                    + datatype + " and type "
                                    + request.getClass());
                }
            } else {
                throw new DataFactoryNotFoundException(
                        "No data access support registered to datatype key: "
                                + datatype);
            }
        } else {
            throw new IllegalArgumentException(
                    "Request must have a datatype set");
        }

    }
}
