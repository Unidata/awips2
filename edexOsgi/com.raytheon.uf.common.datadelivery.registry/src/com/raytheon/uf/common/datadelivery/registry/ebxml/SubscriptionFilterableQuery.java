package com.raytheon.uf.common.datadelivery.registry.ebxml;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.Subscription.SubscriptionState;
import com.raytheon.uf.common.registry.ebxml.AdhocRegistryQuery;
import com.raytheon.uf.common.registry.ebxml.CalendarAttribute;
import com.raytheon.uf.common.registry.ebxml.StringAttribute;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Extension of the AdhocQuery registry query. This implementation searches the
 * registry for Subscription Objects that satisfy the values added with the
 * various set methods.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 28, 2012 356        jspinks     Initial creation
 * Jun 08, 2012 XXX        djohnson    Fix criteria for object type.
 * Jun 21, 2012 736        djohnson    Add thrift serialization annotations.
 * Aug 02, 2012 955        djohnson    Add generics and results retrieval to registry queries.
 * Oct 03, 2012 1241       djohnson    Move query parameters in from SubscriptionQuery.
 * Oct 10, 2012 0726       djohnson    Add {@link #setActive(boolean)}.
 * Feb 20, 2013 1543       djohnson    Add ability to filter on routes.
 * May 28, 2013 1650       djohnson    More information when failing to schedule.
 * Sep 04, 2013 2330       bgonzale    OfficeIds attribute is a collection.
 * Jan 14, 2014 2459       mpduff      Change to query for ON subscription state.
 * 
 * </pre>
 * 
 * @author jspinks
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public abstract class SubscriptionFilterableQuery<T> extends
        AdhocRegistryQuery<T> {

    /**
     * A setter for the queryable attribute dataSetName equals a single String
     * value. Using this setter will equate to an HQL "equals" query against the
     * specified column name.
     * 
     * @param collectionName
     *            The value of the dataSetName attribute to search for.
     */
    public void setDataSetName(String dataSetName) {
        setAttribute("dataSetName", new StringAttribute(dataSetName));
    }

    /**
     * A setter for the queryable attribute time equals a single String value.
     * Using this setter will equate to an HQL "equals" query against the
     * specified column name.
     * 
     * @param collectionName
     *            The value of the time attribute to search for.
     */
    public void setMatureTime(Calendar matureTime) {
        CalendarAttribute ca = new CalendarAttribute(matureTime);
        ca.setCollection(true);
        setAttribute("time", ca);
    }

    /**
     * A setter for the queryable attribute name equals a single String value.
     * Using this setter will equate to an HQL "equals" query against the
     * specified column name.
     * 
     * @param collectionName
     *            The value of the name attribute to search for.
     */
    public void setName(String name) {
        setAttribute(Subscription.NAME_SLOT, new StringAttribute(name));
    }

    /**
     * A setter for the queryable attribute name is like a String value. Using
     * this setter will equate to an HQL "like" query against the specified
     * column name.
     * 
     * @param name
     *            The HQL compliant like value to use to query name attribute.
     */
    public void setNameLike(String name) {
        setAttribute(Subscription.NAME_SLOT, new StringAttribute(name, true));
    }

    /**
     * A setter for the queryable attribute providerName equals a List of String
     * values. Using this setter will equate to an HQL "in list" query against
     * the specified column name.
     * 
     * @param collectionNames
     *            The values of the name attribute to search for.
     */
    public void setNames(Collection<String> names) {
        setAttribute(Subscription.NAME_SLOT, new StringAttribute(
                new ArrayList<String>(names)));
    }

    /**
     * A setter for the queryable attribute owner equals a single String value.
     * Using this setter will equate to an HQL "equals" query against the
     * specified column name.
     * 
     * @param collectionName
     *            The value of the owner attribute to search for.
     */
    public void setOwner(String owner) {
        setAttribute("owner", new StringAttribute(owner));
    }

    /**
     * A setter for the queryable attribute groupName equals a single String
     * value. Using this setter will equate to an HQL "equals" query against the
     * specified column name.
     * 
     * @param groupName
     *            The value of the groupName attribute to search for.
     */
    public void setGroupName(String groupName) {
        setAttribute("groupName", new StringAttribute(groupName));
    }

    /**
     * A setter for the queryable attribute officeId equals a single String
     * value. Using this setter will equate to an HQL "equals" query against the
     * specified column name.
     * 
     * @param officeId
     *            The value of the officeId attribute to search for.
     */
    public void setOfficeId(String officeId) {
        List<String> officeIdList = new ArrayList<String>();
        officeIdList.add(officeId);
        StringAttribute stringAtt = new StringAttribute(officeIdList);
        stringAtt.setCollection(true);
        setAttribute("officeIDs", stringAtt);
    }

    /**
     * A setter for the queryable attribute active equals a single String value.
     * Using this setter will equate to an HQL "equals" query against the
     * specified column name.
     * 
     * @param active
     *            The value of the active attribute to search for.
     */
    public void setActive(boolean active) {
        setAttribute(Subscription.SUBSCRIPTION_STATE_SLOT, new StringAttribute(
                SubscriptionState.ON.name()));
    }

    /**
     * A setter for the queryable attribute providerName equals a single String
     * value. Using this setter will equate to an HQL "equals" query against the
     * specified column name.
     * 
     * @param collectionName
     *            The value of the providerName attribute to search for.
     */
    public void setProviderName(String providerName) {
        setAttribute("provider", new StringAttribute(providerName));
    }

    /**
     * A setter for the queryable attribute providerName is like a String value.
     * Using this setter will equate to an HQL "like" query against the
     * specified column name.
     * 
     * @param providerName
     *            The HQL compliant like value to use to query providerName
     *            attribute.
     */
    public void setProviderNameLike(String providerName) {
        setAttribute("provider", new StringAttribute(providerName, true));
    }

    /**
     * A setter for the queryable attribute providerName equals a List of String
     * values. Using this setter will equate to an HQL "in list" query against
     * the specified column name.
     * 
     * @param collectionNames
     *            The values of the providerName attribute to search for.
     */
    public void setProviderNames(List<String> providerNames) {
        setAttribute("provider", new StringAttribute(providerNames));
    }

    /**
     * Set the routes to match.
     * 
     * @param routes
     *            the routes
     */
    public void setRoutes(List<Network> routes) {
        List<String> enumValues = new ArrayList<String>(routes.size());
        for (Network route : routes) {
            enumValues.add(route.toString());
        }
        setAttribute(Subscription.ROUTE_SLOT, new StringAttribute(enumValues));
    }
}
