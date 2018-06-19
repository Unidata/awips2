package com.raytheon.uf.common.registry;

import java.util.List;

import com.raytheon.uf.common.auth.req.AbstractPrivilegedRequest;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Container Class for Thrift communication with the Registry.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 28, 2012 428        jspinks     Initial creation
 * Jun 21, 2012 736        djohnson    Add thrift serialization annotations.
 * 8/3/2012     724        bphillip    Modified to make into a privileged request
 * Aug 20, 2012 0743       djohnson    Finish making registry type-safe.
 * Sep 14, 2012 1169       djohnson    Rename UPDATE to STORE_OR_REPLACE.
 * 
 * </pre>
 * 
 * @author jspinks
 * @version 1.0
 * 
 * @see ThriftRegistryManager
 */
@DynamicSerialize
public class IRegistryRequest<T> extends AbstractPrivilegedRequest {

    public static enum Action {
        QUERY, REMOVE, STORE_OR_REPLACE, STORE;
    }

    @DynamicSerializeElement
    private RegistryQuery<T> query;

    @DynamicSerializeElement
    private Action action;

    @DynamicSerializeElement
    private String username;

    @DynamicSerializeElement
    private List<T> objects;

    /**
     * A setter for the query attribute.
     * 
     * @param update
     *            The value to set the query attribute to.
     */
    public void setQuery(RegistryQuery<T> query) {
        this.query = query;
    }

    /**
     * A getter for the query attribute.
     * 
     * @return The value of the query attribute.
     */
    public RegistryQuery<T> getQuery() {
        return query;
    }

    /**
     * @return the action
     */
    public Action getAction() {
        return action;
    }

    /**
     * @param action
     *            the action to set
     */
    public void setAction(Action action) {
        this.action = action;
    }

    /**
     * @return the username
     */
    public String getUsername() {
        return username;
    }

    /**
     * @param username
     */
    public void setUsername(String username) {
        this.username = username;
    }

    /**
     * @return the registryObjects
     */
    public List<T> getObjects() {
        return objects;
    }

    /**
     * @param objects
     */
    public void setObjects(List<T> objects) {
        this.objects = objects;
    }
}
