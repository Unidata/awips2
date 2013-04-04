package com.raytheon.uf.common.datadelivery.registry.ebxml;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.registry.ebxml.AdhocRegistryQuery;
import com.raytheon.uf.common.registry.ebxml.StringAttribute;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Extension of the AdhocQuery registry query. This implementation searches the
 * registry for DataSet filterable objects that satisfy the values added with
 * the various set methods.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 28, 2012 356        jspinks     Initial creation
 * May 15, 2012 455        jspinks     Updated with parameters slot.
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
public abstract class DataSetFilterableQuery<RETURN_TYPE, REGISTRY_TYPE>
        extends AdhocRegistryQuery<RETURN_TYPE> {
    
    /**
     * {@inheritDoc}
     */
    @Override
    public abstract Class<REGISTRY_TYPE> getObjectType();

    /**
     * A setter for the queryable attribute CollectionName equals a single String value.
     * Using this setter will equate to an HQL "equals" query against the specified column name.
     *
     * @param collectionName
     *        The value of the collectionName attribute to search for.
     */
    public void setCollectionName(String collectionName) {
        setAttribute("collectionName", new StringAttribute(collectionName));
    }
    
    /**
     * A setter for the queryable attribute collectionName is like a String value.
     * Using this setter will equate to an HQL "like" query against the specified column name.
     * 
     * @param collectionName
     *        The HQL compliant like value to use to query collectionName attribute.
     */
    public void setCollectionNameLike(String collectionName) {
        setAttribute("collectionName", new StringAttribute(collectionName, true));
    }

    /**
     * A setter for the queryable attribute CollectionName equals a List of String values.
     * Using this setter will equate to an HQL "in list" query against the specified column name.
     * 
     * @param collectionNames
     *        The values of the collectionName attribute to search for.
     */
    public void setCollectionNames(List<String> collectionNames) {
        setAttribute("collectionName", new StringAttribute(collectionNames));
    }

    /**
     * A setter for the queryable attribute dataSetDescription is like a String value.
     * Using this setter will equate to an HQL "like" query against the specified column name.
     * 
     * @param dataSetDescription
     *        The HQL compliant like value to use to query dataSetDescription attribute.
     */
    public void setDataSetDescriptionLike(String dataSetDescription) {
        setAttribute("dataSetDescription", new StringAttribute(dataSetDescription, true));
    }
    
    /**
     * A setter for the queryable attribute dataSetName equals a single String value.
     * Using this setter will equate to an HQL "equals" query against the specified column name.
     *
     * @param collectionName
     *        The value of the dataSetName attribute to search for.
     */
    public void setDataSetName(String dataSetName) {
        setAttribute("dataSetName", new StringAttribute(dataSetName));
    }
    
    /**
     * A setter for the queryable attribute dataSetName is like a String value.
     * Using this setter will equate to an HQL "like" query against the specified column name.
     * 
     * @param dataSetName
     *        The HQL compliant like value to use to query dataSetName attribute.
     */
    public void setDataSetNameLike(String dataSetName) {
        setAttribute("dataSetName", new StringAttribute(dataSetName, true));
    }
    
    /**
     * A setter for the queryable attribute dataSetName equals a List of String values.
     * Using this setter will equate to an HQL "in list" query against the specified column name.
     * 
     * @param dataSetNames
     *        The values of the dataSetName attribute to search for.
     */
    public void setDataSetNames(List<String> dataSetNames) {
        setAttribute("dataSetName", new StringAttribute(dataSetNames));
    }
    
    /**
     * A setter for the queryable attribute dataSetType equals a single String value.
     * Using this setter will equate to an HQL "equals" query against the specified column name.
     *
     * @param dataSetType
     *        The value of the dataSetType attribute to search for.
     */
    public void setDataSetType(String dataSetType) {
        setAttribute("dataSetType", new StringAttribute(dataSetType));
    }
    

    /**
     * A setter for the queryable attribute dataSetType is like a String value.
     * Using this setter will equate to an HQL "like" query against the specified column name.
     * 
     * @param dataSetType
     *        The HQL compliant like value to use to query dataSetType attribute.
     */
    public void setDataSetTypeLike(String dataSetType) {
        setAttribute("dataSetType", new StringAttribute(dataSetType, true));
    }
    
    /**
     * A setter for the queryable attribute dataSetType equals a List of String values.
     * Using this setter will equate to an HQL "in list" query against the specified column name.
     * 
     * @param dataSetTypes
     *        The values of the dataSetType attribute to search for.
     */
    public void setDataSetTypes(List<String> dataSetTypes) {
        setAttribute("dataSetType", new StringAttribute(dataSetTypes));
    }
    
    /**
     * A setter for the queryable attribute providerName equals a single String value.
     * Using this setter will equate to an HQL "equals" query against the specified column name.
     *
     * @param collectionName
     *        The value of the providerName attribute to search for.
     */
    public void setProviderName(String providerName) {
        setAttribute("providerName", new StringAttribute(providerName));
    }

    
    /**
     * A setter for the queryable attribute providerName is like a String value.
     * Using this setter will equate to an HQL "like" query against the specified column name.
     * 
     * @param providerName
     *        The HQL compliant like value to use to query providerName attribute.
     */
    public void setProviderNameLike(String providerName) {
        setAttribute("providerName", new StringAttribute(providerName, true));
    }
    
    /**
     * A setter for the queryable attribute providerName equals a List of String values.
     * Using this setter will equate to an HQL "in list" query against the specified column name.
     * 
     * @param collectionNames
     *        The values of the providerName attribute to search for.
     */
    public void setProviderNames(List<String> providerNames) {
        setAttribute("providerName", new StringAttribute(providerNames));
    }
    
    /**
     * A setter for the queryable attribute providerName equals a List of String values.
     * Using this setter will equate to an HQL "in list" query against the specified column name.
     * 
     * @param collectionNames
     *        The values of the providerName attribute to search for.
     */
    public void setParameterNames(List<String> parameterNames) {
        StringAttribute sa = new StringAttribute(parameterNames);
        sa.setCollection(true);
        setAttribute("parameters", sa);
    }
    
}