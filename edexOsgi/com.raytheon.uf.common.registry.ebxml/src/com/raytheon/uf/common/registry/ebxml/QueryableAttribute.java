package com.raytheon.uf.common.registry.ebxml;

import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.CollectionValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotValueType;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * A container Class for a query information for a queryable attribute of an
 * arbitrary registry Object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 28, 2012 356        jspinks     Initial creation
 * May 15, 2012 455        jspinks     Updated for collection slot types.
 * Jun 21, 2012 736        djohnson    Add thrift serialization annotations.
 * 
 * </pre>
 * 
 * @author jspinks
 * @version 1.0
 * @param <T>
 */
@DynamicSerialize
public abstract class QueryableAttribute<T> {
   
    @DynamicSerializeElement
    protected boolean collection = false;

    @DynamicSerializeElement
    protected boolean like;

    @DynamicSerializeElement
    protected T value;

    @DynamicSerializeElement
    protected List<T> values;

    /**
     * Default Constructor required for serialization.
     */
    public QueryableAttribute() {

    }

    /**
     * Create a QueryableAttribute with an implied 'in list' relation.
     * 
     * @param values
     *            The values to query for.
     */
    public QueryableAttribute(List<T> values) {
        this.values = values;
    }
    
    /**
     * Create a QueryableAttribute with an implied equals relation.
     * 
     * @param value
     *        The value to query for.
     */
    public QueryableAttribute(T value) {
        this.value = value;
    }
    
    /**
     * Create a QueryableAttribute with an optional 'like' relation.
     * 
     * @param value
     *        The value to query for.
     */
    public QueryableAttribute(T value, boolean isLike) {
        this.value = value;
        this.like = isLike;
    }

    
    /**
     * Return the formatted text of this QueryableAttribute appropriate
     * for the options specified when created.  This includes text, numeric,
     * list, like and other formatting. 
     * 
     * @return The formatted value for this QueryableAttribute.
     */
    public abstract String getQueryValue();
    
    /**
     * Return the type of slot used to contain the value.  For instance
     * if the attribute is a String attribute, then 'stringValue' should
     * be returned. If the attribute is an integer, then 'integerValue'
     * should be returned.  
     * 
     * @return The type of value attribute the slot for this QueryableAttribute contains.
     * 
     * @see SlotValueType
     */
    public abstract String getSlotValueType();

    /**
     * Is this attribute a <code>CollectionValueType</code> attribute. 
     * 
     * @return Whether or not this attribute is a collection type.
     * 
     * @see CollectionValueType
     */
    public boolean isCollection() {
        return collection;
    }

    /**
     * Set the isCollection attribute for this QueryableAttribute.
     * 
     * @param isCollection
     *        The value to set for the isCollection attribute.
     */
    public void setCollection(boolean isCollection) {
        this.collection = isCollection;
    }

    /**
     * @return the like
     */
    public boolean isLike() {
        return like;
    }

    /**
     * @param like
     *            the like to set
     */
    public void setLike(boolean like) {
        this.like = like;
    }

    /**
     * @return the value
     */
    public T getValue() {
        return value;
    }

    /**
     * @param value
     *            the value to set
     */
    public void setValue(T value) {
        this.value = value;
    }

    /**
     * @return the values
     */
    public List<T> getValues() {
        return values;
    }

    /**
     * @param values
     *            the values to set
     */
    public void setValues(List<T> values) {
        this.values = values;
    }
}