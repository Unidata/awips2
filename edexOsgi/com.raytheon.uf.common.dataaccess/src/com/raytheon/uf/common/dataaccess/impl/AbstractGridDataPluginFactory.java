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
package com.raytheon.uf.common.dataaccess.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.dataaccess.IDataRequest;
import com.raytheon.uf.common.dataaccess.exception.DataRetrievalException;
import com.raytheon.uf.common.dataaccess.grid.IGridData;
import com.raytheon.uf.common.dataaccess.util.PDOUtil;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.IDataRecord;

/**
 * An abstract factory for getting grid data from plugins that use
 * PluginDataObject.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 17, 2013            bsteffen     Initial creation
 * Feb 14, 2013 1614       bsteffen    Refactor data access framework to use
 *                                     single request.
 * Jan 14, 2014 2667       mnash       Remove getGeometryData methods
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public abstract class AbstractGridDataPluginFactory extends
        AbstractDataPluginFactory {

    /**
     * Executes the provided DbQueryRequest and returns an array of IGridData
     * 
     * @param request
     *            the original grid request
     * @param dbQueryRequest
     *            the db query request to execute
     * @return an array of IGridData
     */
    protected IGridData[] getGridData(IDataRequest request,
            DbQueryResponse dbQueryResponse) {

        List<IGridData> gridData = new ArrayList<IGridData>();
        for (Map<String, Object> resultMap : dbQueryResponse.getResults()) {
            if (resultMap.containsKey(null) == false) {
                throw new DataRetrievalException(
                        "The results of the DbQueryRequest do not consist of PluginDataObject objects as expected.");
            }
            if ((resultMap.get(null) instanceof PluginDataObject) == false) {
                throw new DataRetrievalException(
                        "The objects returned by the DbQueryRequest are not of type PluginDataObject as expected.");
            }

            PluginDataObject pdo = (PluginDataObject) resultMap.get(null);

            IDataRecord dataRecord = getDataRecord(pdo);

            /*
             * Extract the grid geometry.
             */
            GridGeometry2D gridGeometry = getGridGeometry(pdo);

            IGridData defaultGridData = null;
            defaultGridData = this.constructGridDataResponse(request, pdo,
                    gridGeometry, dataRecord);

            gridData.add(defaultGridData);
        }

        return gridData.toArray(new IGridData[gridData.size()]);
    }

    protected IDataRecord getDataRecord(PluginDataObject pdo) {
        try {
            return PDOUtil.getDataRecord(pdo, "Data", Request.ALL);
        } catch (Exception e) {
            e.printStackTrace();
            throw new DataRetrievalException(
                    "Failed to retrieve the IDataRecord for PluginDataObject: "
                            + pdo.toString(), e);
        }
    }

    protected GridGeometry2D getGridGeometry(PluginDataObject pdo) {
        return PDOUtil.retrieveGeometry(pdo);
    }

    /**
     * Builds an IGridData with the information that is supplied
     * 
     * @param request
     *            the original grid request
     * @param pdo
     *            a record that was retrieved from the database
     * @param gridGeometry
     *            the geometry extracted from the pdo
     * @param dataRecord
     *            the raw data
     * @return the IGridData that was constructed
     */
    protected abstract IGridData constructGridDataResponse(
            IDataRequest request, PluginDataObject pdo,
            GridGeometry2D gridGeometry, IDataRecord dataRecord);

}
