package com.raytheon.uf.common.datadelivery.registry.ebxml;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.datadelivery.registry.Provider;
import com.raytheon.uf.common.registry.ebxml.AdhocRegistryQuery;
import com.raytheon.uf.common.registry.ebxml.StringAttribute;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Extension of the AdhocQuery registry query. This implementation searches the
 * registry for Provider Objects that satisfy the values added with the various
 * set methods.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 28, 2012            jspinks     Initial creation
 * Jun 21, 2012 736        djohnson    Add thrift serialization annotations.
 * Aug 02, 2012 955        djohnson    Add generics and results retrieval to registry queries.
 * 
 * </pre>
 * 
 * @author jspinks
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class ProviderQuery extends AdhocRegistryQuery<Provider> {

    /**
     * Set the Class used when Objects of this query are returned.
     * 
     * @return The typed Class for the objects retrieved. 
     *
     */
    @Override
    public Class<Provider> getObjectType() {
        return Provider.class;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Class<Provider> getResultType() {
        return Provider.class;
    }

    /**
     * A setter for the queryable attribute providerNames equals a single String value.
     * Using this setter will equate to an HQL "equals" query against the specified column name.
     *
     * @param providerNames
     *        The value of the providerNames attribute to search for.
     */
    public void setProviderName(String providerName) {
        setAttribute("name", new StringAttribute(providerName));
    }

    /**
     * A setter for the queryable attribute providerName is like a String value.
     * Using this setter will equate to an HQL "like" query against the specified column name.
     * 
     * @param providerName
     *        The HQL compliant like value to use to query dataSetType attribute.
     */
    public void setProviderNameLike(String providerName) {
        setAttribute("name", new StringAttribute(providerName, true));
    }
    
    /**
     * A setter for the queryable attribute providerName equals a List of String values.
     * Using this setter will equate to an HQL "in list" query against the specified column name.
     * 
     * @param providerNames
     *        The values of the providerName attribute to search for.
     */
    public void setProviderNames(List<String> providerNames) {
        setAttribute("name", new StringAttribute(providerNames));
    }
    
}