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
package com.raytheon.uf.viz.datadelivery.browser;

import com.raytheon.uf.common.datadelivery.registry.DataSet;
import com.raytheon.uf.viz.datadelivery.common.ui.ISortTable;
import com.raytheon.uf.viz.datadelivery.common.ui.ITableData;
import com.raytheon.uf.viz.datadelivery.common.ui.SortImages.SortDirection;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils.BrowserColumnNames;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 12, 2012            lvenable     Initial creation
 * Jun 07, 2012   687      lvenable     Table data refactor.
 * Aug 06, 2012   955      djohnson     Use {@link DataSet}.
 * Aug 10, 2012   1022     djohnson     Use GriddedDataSet.
 * Aug 22, 2012   0743     djohnson     Convert back to DataSet.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */

public class BrowserTableRowData implements ITableData<BrowserTableRowData> {

    /**
     * Dataset name.
     */
    private String dataSetName = "";

    /**
     * Provider name.
     */
    private String providerName = "";

    /**
     * Subscription name.
     */
    private String subscriptionName = "";

    /**
     * Dataset metadata.
     */
    private DataSet dataSet;

    /**
     * Sort callback.
     */
    private ISortTable sortCallback;

    /**
     * Constructor
     */
    public BrowserTableRowData() {

    }

    /**
     * Constructor
     * 
     * @param dataSetName
     *            Dataset name.
     * @param providerName
     *            Provider name.
     * @param subscriptionName
     *            Subscription name.
     */
    public BrowserTableRowData(String dataSetName, String providerName, String subscriptionName,
 DataSet dataSet) {
        this.dataSetName = dataSetName;
        this.providerName = providerName;
        this.subscriptionName = subscriptionName;
        this.dataSet = dataSet;
    }

    /**
     * Get the dataset name.
     * 
     * @return The dataset name.
     */
    public String getDataSetName() {
        return dataSetName;
    }

    /**
     * Set the dataset name.
     * 
     * @param dataSetName
     *            The dataset name.
     */
    public void setDataSetName(String dataSetName) {
        this.dataSetName = dataSetName;
    }

    /**
     * Get the provider name.
     * 
     * @return The provider name.
     */
    public String getProviderName() {
        return providerName;
    }

    /**
     * Set the provider name.
     * 
     * @param providerName
     *            The provider name.
     */
    public void setProviderName(String providerName) {
        this.providerName = providerName;
    }

    /**
     * Get the subscription name.
     * 
     * @return The subscription name.
     */
    public String getSubscriptionName() {
        return subscriptionName;
    }

    /**
     * Set the subscription name.
     * 
     * @param subscriptionName
     *            The subscription name.
     */
    public void setSubscriptionName(String subscriptionName) {
        this.subscriptionName = subscriptionName;
    }

    /**
     * Get the dataset meta data.
     * 
     * @return The dataset meta data.
     */
    public DataSet getDataset() {
        return dataSet;
    }

    public void setDataset(DataSet datasetMetaData) {
        this.dataSet = datasetMetaData;
    }

    @Override
    public void setSortCallback(ISortTable sortCallback) {
        this.sortCallback = sortCallback;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
    public int compareTo(BrowserTableRowData otherObj) {
        int returnValue = 0;

        String selectedColumn = "";
        SortDirection direction = SortDirection.ASCENDING;

        if (sortCallback != null) {
            selectedColumn = sortCallback.getSortColumnText();
            direction = sortCallback.getSortDirection();
        }

        String sortValue = this.getSortValue(selectedColumn);

        returnValue = sortValue.toUpperCase().compareTo(otherObj.getSortValue(selectedColumn).toUpperCase());

        if (direction == SortDirection.DESCENDING) {
            returnValue *= -1;
        }

        return returnValue;
    }

    /**
     * Get the sort value.
     * 
     * @param columnName
     *            Name of the column.
     * @return The string to sort on.
     */
    private String getSortValue(String columnName) {
        if (columnName.equals(BrowserColumnNames.NAME.getColumnName())) {
            return this.dataSetName;
        }
        else if (columnName.equals(BrowserColumnNames.PROVIDER.getColumnName())) {
            return this.providerName;
        }
        else if (columnName.equals(BrowserColumnNames.SUBSCRIPTION.getColumnName())) {
            return this.subscriptionName;
        }
        return "";
    }
}
