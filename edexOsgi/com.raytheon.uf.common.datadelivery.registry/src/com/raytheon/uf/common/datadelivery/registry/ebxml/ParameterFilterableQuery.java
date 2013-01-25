package com.raytheon.uf.common.datadelivery.registry.ebxml;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.datadelivery.registry.Parameter;
import com.raytheon.uf.common.registry.ebxml.AdhocRegistryQuery;
import com.raytheon.uf.common.registry.ebxml.StringAttribute;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Extension of the AdhocQuery registry query. This implementation searches the
 * registry for Parameter Objects that satisfy the values added with the various
 * set methods.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 28, 2012 356        jspinks     Initial creation
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
public abstract class ParameterFilterableQuery<T> extends AdhocRegistryQuery<T> {

    /**
     * {@inheritDoc}
     */
    @Override
    public Class<Parameter> getObjectType() {
        return Parameter.class;
    }

    /**
     * A setter for the queryable attribute ParameterName equals a single String
     * value. Using this setter will equate to an HQL "equals" query against the
     * specified column name.
     * 
     * @param parameterName
     *            The value of the parameterName attribute to search for.
     */
    public void setParameterName(String parameterName) {
        setAttribute("name", new StringAttribute(parameterName));
    }
    
    /**
     * A setter for the queryable attribute parameterName is like a String value.
     * Using this setter will equate to an HQL "like" query against the specified column name.
     * 
     * @param parameterName
     *        The HQL compliant like value to use to query parameterName attribute.
     */
    public void setParameterNameLike(String parameterName) {
        setAttribute("name", new StringAttribute(parameterName, true));
    }

    /**
     * A setter for the queryable attribute ParameterName equals a List of String values.
     * Using this setter will equate to an HQL "in list" query against the specified column name.
     * 
     * @param parameterNames
     *        The values of the parameterName attribute to search for.
     */
    public void setParameterNames(List<String> parameterNames) {
        setAttribute("name", new StringAttribute(parameterNames));
    }

    /**
     * A setter for the queryable attribute dataType equals a single String value.
     * Using this setter will equate to an HQL "equals" query against the specified column name.
     *
     * @param parameterName
     *        The value of the dataType attribute to search for.
     */
    public void setDataType(String dataType) {
        setAttribute("dataType", new StringAttribute(dataType));
    }
    
    /**
     * A setter for the queryable attribute dataType is like a String value.
     * Using this setter will equate to an HQL "like" query against the specified column name.
     * 
     * @param dataType
     *        The HQL compliant like value to use to query dataType attribute.
     */
    public void setDataTypeLike(String dataType) {
        setAttribute("dataType", new StringAttribute(dataType, true));
    }
    
    /**
     * A setter for the queryable attribute dataType equals a List of String values.
     * Using this setter will equate to an HQL "in list" query against the specified column name.
     * 
     * @param dataTypes
     *        The values of the dataType attribute to search for.
     */
    public void setDataTypes(List<String> dataTypes) {
        setAttribute("dataType", new StringAttribute(dataTypes));
    }
}