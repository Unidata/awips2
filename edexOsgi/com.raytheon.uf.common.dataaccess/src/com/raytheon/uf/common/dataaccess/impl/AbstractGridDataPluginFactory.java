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

import org.geotools.coverage.grid.GridEnvelope2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.geometry.Envelope;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataaccess.exception.DataRetrievalException;
import com.raytheon.uf.common.dataaccess.grid.IGridData;
import com.raytheon.uf.common.dataaccess.grid.IGridRequest;
import com.raytheon.uf.common.dataaccess.util.PDOUtil;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.Request.Type;
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
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public abstract class AbstractGridDataPluginFactory extends
        AbstractDataPluginFactory<IGridRequest, IGridData> {

    /**
     * Executes the provided DbQueryRequest and returns an array of IGridData
     * 
     * @param request
     *            the original grid request
     * @param dbQueryRequest
     *            the db query request to execute
     * @return an array of IGridData
     */
    protected IGridData[] getData(IGridRequest request,
            DbQueryResponse dbQueryResponse) {

        List<IGridData> gridData = new ArrayList<IGridData>();
        for (Map<String, Object> resultMap : dbQueryResponse.getResults()) {
            if (resultMap.containsKey(null) == false) {
                throw new DataRetrievalException(
                        "The results of the DbQueryRequest do not consist of PluginDataObject objects as expected.");
            }
            if ((resultMap.get(null) instanceof PluginDataObject) == false) {
                throw new DataRetrievalException(
                        "The PluginDataObject objects returned by the DbQueryRequest are not of type SatelliteRecord as expected.");
            }

            PluginDataObject pdo = (PluginDataObject) resultMap
                    .get(null);

            IDataRecord dataRecord = null;

            try {
                dataRecord = PDOUtil.getDataRecords(pdo, "Data",
                        request.getStorageRequest());
            } catch (Exception e) {
                e.printStackTrace();
                throw new DataRetrievalException(
                        "Failed to retrieve the IDataRecord for PluginDataObject: "
                                + pdo.toString(), e);
            }

            /*
             * Extract the grid geometry.
             */
            GridGeometry2D gridGeometry = PDOUtil.retrieveGeometry(pdo);

            gridGeometry = trimGridGeometryToRequest(gridGeometry,
                    request.getStorageRequest());

            IGridData defaultGridData = null;
            defaultGridData = this.constructGridDataResponse(request, pdo,
                    gridGeometry, dataRecord);

            gridData.add(defaultGridData);
        }

        return gridData.toArray(new IGridData[gridData.size()]);
    }

    /**
     * Given a full PDO grid geometry and the request used this will determine
     * the geometry that describes the requested area. For null or ALL this
     * returns the full geometry, for SLAB requests this will create a subset
     * geometry describing the slab and for all other types of requests this
     * returns null.
     * 
     * @param gridGeom
     *            - full dataset geometry
     * @param storageRequest
     * @return for null or ALL this returns the full geometry, for SLAB requests
     *         this will create a subset geometry describing the slab and for
     *         all other types of requests this returns null.
     */
    protected GridGeometry2D trimGridGeometryToRequest(GridGeometry2D gridGeom,
            Request storageRequest) {
        if (storageRequest == null || storageRequest.getType() == Type.ALL) {
            return gridGeom;
        } else if (storageRequest.getType() == Type.SLAB) {
            int[] min = storageRequest.getMinIndexForSlab();
            int[] max = storageRequest.getMaxIndexForSlab();
            GridEnvelope2D range = new GridEnvelope2D(min[0], min[1], max[0]
                    - min[0], max[1] - min[1]);
            try {
                Envelope env = gridGeom.gridToWorld(range);
                return new GridGeometry2D(range, env);
            } catch (TransformException e) {
                throw new DataRetrievalException(e);
            }
        } else {
            // point, and line requests can't easily be described by a grid
            // geometry. Theoretically if there is one point or if the lines are
            // evenly spaced it might be possible or lines could be described by
            // a nonlinear geometry, but as of now there are no plans to use the
            // api for anything this exciting.
            return null;
        }
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
            IGridRequest request, PluginDataObject pdo,
            GridGeometry2D gridGeometry, IDataRecord dataRecord);

}
