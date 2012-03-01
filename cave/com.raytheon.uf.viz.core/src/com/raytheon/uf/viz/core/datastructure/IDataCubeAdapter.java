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
package com.raytheon.uf.viz.core.datastructure;

import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.TimeQueryRequest;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * The IDataCubeAdapter interface allows a class, once it implements this
 * interface, to be used by the DataCubeContainer. This generalizes the
 * requesting of data and allows for derived data to be requested just like
 * regular data. This interface supports both grid based requests and point data
 * requests. Those desiring to write to this interface can look at
 * GribDataCubeAdapter and PointDataCubeAdapter, respectively, for examples of
 * how this is done.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 16, 2009            brockwoo     Initial creation
 * 
 * </pre>
 * 
 * @author brockwoo
 * @version 1.0
 */

public interface IDataCubeAdapter {

    /**
     * Returns a list of plugins supported by the adapter.
     * 
     * @return An array of strings representing the names of the plugins
     *         supported by the adapter
     */
    public String[] getSupportedPlugins();

    /**
     * A bulk implementation of the timeQuery method, this should return the
     * same result as if timeQuery was called seperatly for each request
     * 
     * @param queryParams
     *            a map of request contraints
     * @return
     */
    public List<List<DataTime>> timeQuery(List<TimeQueryRequest> requests)
            throws VizException;

    /**
     * Returns a point data container which contains the parameters specified.
     * This array can include
     * 
     * @param plugin
     *            the name of the point data plugin
     * @param parameters
     *            an array of the requested parameters
     * @param queryParams
     *            query information for that data
     * @return a point data container with both base and derived parameters
     * @throws VizException
     */
    public PointDataContainer getPoints(String plugin, String[] parameters,
            Map<String, RequestConstraint> queryParams) throws VizException;

    /**
     * Returns a point data container which contains the parameters specified.
     * This array can include
     * 
     * @param plugin
     *            the name of the point data plugin
     * @param parameters
     *            an array of the requested parameters
     * @param queryParams
     *            query information for that data
     * @param levelKey
     *            the level to retrieve the points at.
     * @return a point data container with both base and derived parameters
     * @throws VizException
     */
    public PointDataContainer getPoints(String plugin, String[] parameters,
            String levelKey, Map<String, RequestConstraint> queryParams)
            throws VizException;

    /**
     * A call to getRecord will perform the specified derived parameter
     * calculation. Presently, this method expects the PluginDataObject passed
     * in to contain a DerivedParameteRequest as the message for that PDO.
     * 
     * @param obj
     *            the PluginDataObject that contains the info for the derived
     *            parameter
     * @return an array of IDataRecords that contain the calculated values
     * @throws VizDataCubeException
     */
    public IDataRecord[] getRecord(PluginDataObject obj)
            throws VizDataCubeException;

    public IDataRecord[] getRecord(PluginDataObject obj, Request req,
            String dataset) throws VizDataCubeException;

    public void getRecords(List<PluginDataObject> objs, Request req,
            String dataset) throws VizDataCubeException;

    /**
     * Builds a list of responses for the specified LayerPropterty. This
     * includes looking at the derived parameter library and determining if any
     * derived parameter satisfies the request. The list should contain the
     * record type the is expected by the resource. For example, a GribRecord
     * would be returned from a GRIB IDataCubeAdapter instance.
     * 
     * @param property
     *            the layer property
     * @return A list of records that are expected by the resource
     * @throws VizException
     */
    public List<Object> getData(LayerProperty property, int timeOut)
            throws VizException;

    /**
     * If the inventory for a particular data type is large (for example, Grib),
     * a call to this method should get a copy of that data type's inventory
     * and, if possible, determine was derived parameters are available for the
     * base parameters available for that data type.
     * 
     * @param derParLibrary
     *            The derived parameter library
     */
    public void initInventory();

    /**
     * Returns an instance of that adapter's inventory. This can be any type of
     * object.
     * 
     * @return an instance of the adapter's inventory
     */
    public Object getInventory();

    /**
     * If these constraints link to any derived products return a list of
     * constraints that when updated will trigger an update of the derived
     * product
     * 
     * @param constraints
     * @return
     */
    public List<Map<String, RequestConstraint>> getBaseUpdateConstraints(
            Map<String, RequestConstraint> constraints);
}
