package com.raytheon.uf.common.registry.ebxml;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;

import com.raytheon.uf.common.registry.BaseQuery;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Base implementation of the AdhocQuery registry query. This implementation
 * uses AdhocQuery and the Hiberate Query Language (HQL) to search for objects
 * stored in the registry.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 28, 2012 356        jspinks     Initial creation
 * May 15, 2012 455        jspinks     Added support for collection slots.
 * Jun 21, 2012 736        djohnson    Add thrift serialization annotations.
 * Aug 02, 2012 955        djohnson    Add generics and results retrieval to registry queries.
 * </pre>
 * 
 * @author jspinks
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public abstract class AdhocRegistryQuery<T> extends BaseQuery<T> {

    private static String SLOT_CRITERIA_STRING_EQUALS = "slot%1$d.name='%2$s' and slot%1$d.slotValue.%3$s = %4$s and ";

    private static String SLOT_CRITERIA_STRING_IN = "slot%1$d.name='%2$s' and slot%1$d.slotValue.%3$s in (%4$s) and ";
    private static String SLOT_CRITERIA_STRING_LIKE = "slot%1$d.name='%2$s' and slot%1$d.slotValue.%3$s like %4$s and ";
    
    private static String COLLECTION_CRITERIA_STRING_EQUALS = "slot%1$d.name='%2$s' and collectionJoin%1$d.%3$s = %4$s and ";
    private static String COLLECTION_CRITERIA_STRING_IN = "slot%1$d.name='%2$s' and collectionJoin%1$d.%3$s in (%4$s) and ";
    private static String COLLECTION_CRITERIA_STRING_LIKE = "slot%1$d.name='%2$s' and collectionJoin%1$d.%3$s like %4$s and ";
    
    protected static String FIND_OBJECTS_QUERY = "from RegistryObjectType as obj ";

    protected static String OBJECT_TYPE_CRITERIA = " where obj.objectType='%1$s' and ";

    protected static String SLOT_JOIN_CLAUSE = " inner join obj.slot as slot%1$d ";
    
    protected static String COLLECTION_JOIN_CLAUSE = 
        " inner join obj.slot as slot%1$d " +
        " left join slot%1$d.slotValue.collectionValue collectionJoin%1$d ";
    
    /**
     * Create the HQL for a QueryableAttribute. 
     * 
     * @param attribute
     *        The QueryableAttribute to use.
     *        
     * @param columnName
     *        The slot name that is to be queried against.
     *        
     * @param slotNumber
     *        The join number of this criteria. Multiple slot criteria require the joining of 
     *        the slot table to aggregate the results properly.
     *        
     * @return The formatted line of HQL.
     */
    @SuppressWarnings("rawtypes")
    protected static String formatCriteria(QueryableAttribute attribute, String columnName, int slotNumber) {
        String criteria;

        if (attribute.collection) {
            
        if (attribute.like) {
             criteria = String.format(COLLECTION_CRITERIA_STRING_LIKE, slotNumber, columnName, attribute.getSlotValueType(), attribute.getQueryValue());
         }
         else if (attribute.values != null) {
             criteria = String.format(COLLECTION_CRITERIA_STRING_IN, slotNumber, columnName, attribute.getSlotValueType(), attribute.getQueryValue());
         } else {
            criteria = String.format(COLLECTION_CRITERIA_STRING_EQUALS, slotNumber, columnName, attribute.getSlotValueType(), attribute.getQueryValue());
         }
        
        } else {

        if (attribute.like) {
             criteria = String.format(SLOT_CRITERIA_STRING_LIKE, slotNumber, columnName, attribute.getSlotValueType(), attribute.getQueryValue());
         }
         else if (attribute.values != null) {
             criteria = String.format(SLOT_CRITERIA_STRING_IN, slotNumber, columnName, attribute.getSlotValueType(), attribute.getQueryValue());
         } else {
            criteria = String.format(SLOT_CRITERIA_STRING_EQUALS, slotNumber, columnName, attribute.getSlotValueType(), attribute.getQueryValue());
         }
        }
        return criteria;
                 
     }
    
    @DynamicSerializeElement
    protected Map<String, QueryableAttribute<?>> attributes = new HashMap<String, QueryableAttribute<?>>();
    
    /**
     * Querying the registry requires the use of a QueryRequest Object.
     * This base Object supports different types of queries.  This method 
     * provides the RegistryManager a means to determine the query type 
     * that should be used in conjunction with the slots provided by
     * the getSlots() method to produce the correct query to locate 
     * registry Objects. 
     *  
     * @return The constant "AdhocQuery" to mark queries generated using this
     *         Class as adhoc. 
     */
    @Override
    public String getQueryType() {
        return RegistryUtil.QUERY_TYPE_ADHOC;
    }
    
    /**
     * Querying the registry requires the use of a QueryRequest Object.
     * This Object queries the registry based on the slots add to the
     * query.  This method provides the slots necessary to execute an
     * adhoc query using HQL.
     *  
     * @return The slots to add to a QueryRequest to find the desired
     *         registry Objects. 
     */
    @Override
    public List<SlotType> getSlots() {
        List<SlotType> slots = new ArrayList<SlotType>();
        slots.add(RegistryUtil.newStringSlot("queryLanguage", "HQL"));
        slots.add(RegistryUtil.newStringSlot("queryExpression", getHQL()));
        return slots;
    }
    
    /**
     * Create the HQL String used to query the registry.
     * 
     * @return The HQL query.
     */
    @SuppressWarnings("rawtypes")
    protected String getHQL() {
        
        // Build the HQL query..
        StringBuilder sb = new StringBuilder(FIND_OBJECTS_QUERY);
        List<String> criterias = new ArrayList<String>();

        criterias.add(String.format(OBJECT_TYPE_CRITERIA, RegistryUtil.getObjectType(getObjectType())));

        for (String columnName : attributes.keySet()) {
            QueryableAttribute attribute = attributes.get(columnName);
            
            if (attribute.isCollection()) {
                sb.append(String.format(COLLECTION_JOIN_CLAUSE, criterias.size()));
            }
            else {
                sb.append(String.format(SLOT_JOIN_CLAUSE, criterias.size()));
            }
            criterias.add(formatCriteria(attribute, columnName, criterias.size()));
        }
        
        // Add the where clauses...
        for (String criteria : criterias) {
            sb.append(criteria);
        }

        // Remove the last 'and' from the query...
        String tmp = sb.toString();
        return tmp.substring(0, tmp.length() - 4);
    }

    /**
     * Set attributes to use to query against. 
     * 
     * @param columnName
     *        The name of the attribute to query against.
     *        
     * @param attribute
     *        The QueryAttribute to use to generate the HQL needed to find
     *        the desired registry Objects.
     */
    @SuppressWarnings("rawtypes")
    protected
    void setAttribute(String columnName, QueryableAttribute attribute) {
        attributes.put(columnName, attribute);
    }

    /**
     * @return the attributes
     */
    public Map<String, QueryableAttribute<?>> getAttributes() {
        return attributes;
    }

    /**
     * @param attributes
     *            the attributes to set
     */
    public void setAttributes(Map<String, QueryableAttribute<?>> attributes) {
        this.attributes = attributes;
    }
}
