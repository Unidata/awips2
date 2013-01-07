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
package com.raytheon.uf.common.datadelivery.registry.ebxml;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.registry.ebxml.AdhocRegistryQuery;
import com.raytheon.uf.common.registry.ebxml.IntegerAttribute;
import com.raytheon.uf.common.registry.ebxml.StringAttribute;
import com.raytheon.uf.common.time.util.ImmutableDate;

/**
 * A query that allows filtering on {@link DataSetMetaData} objects.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 15, 2012 0743       djohnson     Moved filterable methods here.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public abstract class DataSetMetaDataFilterableQuery<RETURN_TYPE>
        extends AdhocRegistryQuery<RETURN_TYPE> {

    /**
     * {@inheritDoc}
     */
    @Override
    public Class<DataSetMetaData> getObjectType() {
        return DataSetMetaData.class;
    }

    /**
     * A setter for the queryable attribute dataSetName equals a single String
     * value. Using this setter will equate to an HQL "equals" query against the
     * specified column name.
     * 
     * @param collectionName
     *            The value of the dataSetName attribute to search for.
     */
    public void setDataSetName(String dataSetName) {
        setAttribute(DataSetMetaData.DATA_SET_NAME_SLOT,
                new StringAttribute(dataSetName));
    }

    /**
     * A setter for the queryable attribute dataSetName is like a String value.
     * Using this setter will equate to an HQL "like" query against the
     * specified column name.
     * 
     * @param dataSetName
     *            The HQL compliant like value to use to query dataSetName
     *            attribute.
     */
    public void setDataSetNameLike(String dataSetName) {
        setAttribute(DataSetMetaData.DATA_SET_NAME_SLOT,
                new StringAttribute(dataSetName, true));
    }

    /**
     * A setter for the queryable attribute dataSetName equals a List of String
     * values. Using this setter will equate to an HQL "in list" query against
     * the specified column name.
     * 
     * @param dataSetNames
     *            The values of the dataSetName attribute to search for.
     */
    public void setDataSetNames(List<String> dataSetNames) {
        setAttribute(DataSetMetaData.DATA_SET_NAME_SLOT,
                new StringAttribute(dataSetNames));
    }

    /**
     * A setter for the queryable attribute providerName equals a single String
     * value. Using this setter will equate to an HQL "equals" query against the
     * specified column name.
     * 
     * @param providerName
     *            The value of the providerName attribute to search for.
     */
    public void setProviderName(String providerName) {
        setAttribute(DataSetMetaData.PROVIDER_NAME_SLOT, new StringAttribute(
                providerName));
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
        setAttribute(DataSetMetaData.PROVIDER_NAME_SLOT, new StringAttribute(
                providerName, true));
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
        setAttribute(DataSetMetaData.PROVIDER_NAME_SLOT, new StringAttribute(
                providerNames));
    }

    /**
     * A setter for the queryable attribute date. Using this setter will equate
     * to an HQL "equals" query against the specified column name.
     * 
     * @param immutableDate
     *            The value of the date attribute to search for.
     */
    public void setDate(ImmutableDate immutableDate) {
        setAttribute(DataSetMetaData.DATE_SLOT, new IntegerAttribute(
                immutableDate.getTime()));
    }

    /**
     * A setter for the queryable attribute date. Using this setter will equate
     * to an HQL "in list" query against the specified column name.
     * 
     * @param dates
     *            The values of the date attribute to search for.
     */
    public void setDates(List<ImmutableDate> dates) {
        List<BigInteger> values = new ArrayList<BigInteger>(dates.size());
        for (ImmutableDate date : dates) {
            values.add(BigInteger.valueOf(date.getTime()));
        }
        setAttribute(DataSetMetaData.DATE_SLOT, new IntegerAttribute(values));
    }
}
