package com.raytheon.uf.common.registry.ebxml;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;

import com.raytheon.uf.common.registry.BaseQuery;
import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.common.registry.constants.QueryLanguages;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Base implementation of the AdhocQuery registry query. This implementation
 * uses AdhocQuery and the Hiberate Query Language (HQL) to search for objects
 * stored in the registry.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Mar 28, 2012  356      jspinks   Initial creation
 * May 15, 2012  455      jspinks   Added support for collection slots.
 * Jun 21, 2012  736      djohnson  Add thrift serialization annotations.
 * Aug 02, 2012  955      djohnson  Add generics and results retrieval to
 *                                  registry queries.
 * Apr 09, 2013  1802     bphillip  Modified to use constants in constants
 *                                  package instead of RegistryUtil
 * Oct 10, 2013  1683     bphillip  Using correct id value for the HQL query
 *                                  language
 * May 20, 2016  5659     dhladky   Change queries to intersect and not return
 *                                  product of all slots.
 * Aug 10, 2016  5659     rjpeter   Update collection query to not be cartesian product
 * 
 * </pre>
 * 
 * @author jspinks
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public abstract class AdhocRegistryQuery<T> extends BaseQuery<T> {

    protected static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AdhocRegistryQuery.class);

    protected static String FIND_OBJECTS_QUERY = "select obj from RegistryObjectType as obj ";

    protected static String OBJECT_TYPE_CRITERIA = "where obj.objectType='%1$s' and ";

    protected static String SLOT_CRITERIA_CLAUSE = "obj.id in (select slot%1$s.parent_id from SlotType slot%1$s inner join slot%1$s.slotValue value%1$s where slot%1$s.name = '%2$s' and value%1$s.%3$s %4$s) and ";

    protected static String COLLECTION_CRITERIA_CLAUSE = "obj.id in (select slot%1$s.parent_id from SlotType slot%1$s inner join slot%1$s.slotValue value%1$s inner join value%1$s.collectionValue cvalue%1$s where slot%1$s.name = '%2$s' and cvalue%1$s.%3$s %4$s) and ";

    /**
     * Create the HQL for a QueryableAttribute.
     * 
     * @param attribute
     *            The QueryableAttribute to use.
     * 
     * @param columnName
     *            The slot name that is to be queried against.
     * @param slotNumber
     *            The join number of this criteria. Multiple slot criteria
     *            require the joining of the slot table to aggregate the results
     *            properly.
     * @return The formatted line of HQL.
     */
    @SuppressWarnings("rawtypes")
    protected static String formatCriteria(QueryableAttribute attribute,
            String columnName, int slotNumber) {
        String criteria = SLOT_CRITERIA_CLAUSE;
        String queryType = "= " + attribute.getQueryValue();

        if (attribute.like) {
            queryType = "like " + attribute.getQueryValue();
        } else if (attribute.values != null) {
            queryType = "in (" + attribute.getQueryValue() + ")";
        }

        if (attribute.collection) {
            criteria = COLLECTION_CRITERIA_CLAUSE;
        }

        return String.format(criteria, slotNumber, columnName,
                attribute.getSlotValueType(), queryType);
    }

    @DynamicSerializeElement
    protected Map<String, QueryableAttribute<?>> attributes = new HashMap<>();

    /**
     * Querying the registry requires the use of a QueryRequest Object. This
     * base Object supports different types of queries. This method provides the
     * RegistryManager a means to determine the query type that should be used
     * in conjunction with the slots provided by the getSlots() method to
     * produce the correct query to locate registry Objects.
     * 
     * @return The constant "AdhocQuery" to mark queries generated using this
     *         Class as adhoc.
     */
    @Override
    public String getQueryType() {
        return CanonicalQueryTypes.ADHOC_QUERY;
    }

    /**
     * Querying the registry requires the use of a QueryRequest Object. This
     * Object queries the registry based on the slots add to the query. This
     * method provides the slots necessary to execute an adhoc query using HQL.
     * 
     * @return The slots to add to a QueryRequest to find the desired registry
     *         Objects.
     */
    @Override
    public List<SlotType> getSlots() {
        List<SlotType> slots = new ArrayList<>();
        slots.add(RegistryUtil.newStringSlot("queryLanguage",
                QueryLanguages.HQL));
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

        // Add the where clauses...
        sb.append(String.format(OBJECT_TYPE_CRITERIA,
                RegistryUtil.getObjectType(getObjectType())));

        int i = 1;
        for (String columnName : attributes.keySet()) {
            QueryableAttribute attribute = attributes.get(columnName);
            sb.append(formatCriteria(attribute, columnName, i++));
        }

        // Remove the last 'and' from the query...
        String query = sb.substring(0, sb.length() - 4);

        if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
            statusHandler.debug("Registry HQL: " + query);
        }

        return query;
    }

    /**
     * Set attributes to use to query against.
     * 
     * @param columnName
     *            The name of the attribute to query against.
     * 
     * @param attribute
     *            The QueryAttribute to use to generate the HQL needed to find
     *            the desired registry Objects.
     */
    @SuppressWarnings("rawtypes")
    protected void setAttribute(String columnName, QueryableAttribute attribute) {
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
