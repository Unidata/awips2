package com.raytheon.uf.common.registry;

import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;

import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * 
 * Interface for querying the registry for Objects.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 28, 2012            jspinks     Initial creation
 * Aug 02, 2012 955        djohnson    Type-safe registry query/responses.
 * Aug 20, 2012 0743       djohnson    Finish making registry type-safe.
 * 
 * </pre>
 * 
 * @author jspinks
 * @version 1.0
 * 
 * @see RegistryManager
 */
public interface RegistryQuery<T> extends IServerRequest {

    /**
     * The registry object type.
     * 
     * @return the class object representing the items in the registry.
     */
    public Class<?> getObjectType();

    /**
     * Determine the Class associated with the results to retrieve from the
     * registry.
     * 
     * @return The typed Class Object to use to assign the runtime type of
     *         Objects retrieved from the registry.
     */
    public Class<T> getResultType();

    /**
     * Querying the registry requires the use of a QueryRequest Object.
     * This base Object supports different types of queries.  This method 
     * provides the RegistryManager a means to determine the query type 
     * that should be used in conjunction with the slots provided by
     * the getSlots() method to produce the correct query to locate 
     * registry Objects. 
     *  
     * @return The type of query to perform. 
     */
    public abstract String getQueryType();

    /**
     * Querying the registry requires the use of a QueryRequest Object.
     * This Object queries the registry based on the slots add to the
     * query.  This method provides an abstraction of what slots get
     * added to the QueryRequest Object.
     *  
     * @return The slots to add to a QueryRequest to find the desired
     *         registry Objects. 
     */
    public List<SlotType> getSlots();

    /**
     * Retrieves the results of the {@link RegistryQuery} in a type-safe manner.
     * This method should be used rather than explicitly casting in the calling
     * code.
     * 
     * @param response
     *            the response
     * @return the results
     */
    public List<T> getResults(RegistryResponse<T> response);

    /**
     * Retrieves the result of the {@link RegistryQuery} in a type-safe manner.
     * This method should be used rather than explicitly casting in the calling
     * code.
     * 
     * @return the single result
     */
    public T getSingleResult(RegistryResponse<T> response);
}
