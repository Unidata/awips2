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
package com.raytheon.uf.common.dataaccess;

import com.raytheon.uf.common.dataaccess.exception.DataFactoryNotFoundException;
import com.raytheon.uf.common.dataaccess.exception.TimeAgnosticDataException;
import com.raytheon.uf.common.dataaccess.exception.UnsupportedOutputTypeException;
import com.raytheon.uf.common.dataaccess.geom.IGeometryData;
import com.raytheon.uf.common.dataaccess.grid.IGridData;
import com.raytheon.uf.common.dataaccess.impl.DefaultDataRequest;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;

/**
 * The Data Access Layer is the published API for getting data through the Data
 * Access Framework. Code from other components should go through these methods
 * to retrieve the data. All methods may potentially throw
 * UnsupportedOperationException or IllegalArgumentException dependent on how
 * much support has been provided per datatype.
 * 
 * The implementation of this class is a retrieval of the corresponding factory
 * and then delegating the processing to that factory. All the generics that
 * exist in this implementation are to save the caller from the hassle of
 * casting what is returned.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 24, 2012            njensen     Initial creation
 * Feb 14, 2013 1614       bsteffen    Refactor data access framework to use
 *                                     single request.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class DataAccessLayer {

    /**
     * Gets the times of available data to the request
     * 
     * @param request
     *            the request to find available times for
     * @return the available times that match the request
     * @throws TimeAgnosticDataException
     */
    public static DataTime[] getAvailableTimes(IDataRequest request) {
        IDataFactory factory = getFactory(request);
        return factory.getAvailableTimes(request);
    }

    /**
     * Gets the times of available data to the request with a BinOffset applied
     * 
     * @param request
     *            the request to find available times for
     * @param binOffset
     *            the BinOffset to apply
     * @return the available times with the bin offset applied that match the
     *         request
     * @throws TimeAgnosticDataException
     */
    public static DataTime[] getAvailableTimes(IDataRequest request,
            BinOffset binOffset) {
        IDataFactory factory = getFactory(request);
        return factory.getAvailableTimes(request, binOffset);
    }

    /**
     * Gets the data that matches the request at the specified times. If data is
     * time agnostic then simply don't pass in any DataTimes, for example
     * DataAccessLayer.getGridData(R)
     * 
     * @param request
     *            the request to get data for
     * @param times
     *            the times to get data for
     * @return the data that matches the request and times
     * @throws UnsupportedOutputTypeException
     *             if the factory for this datatype cannot produce IGridData
     */
    public static IGridData[] getGridData(IDataRequest request,
            DataTime... times) throws UnsupportedOutputTypeException {
        IDataFactory factory = getFactory(request);
        return factory.getGridData(request, times);
    }

    /**
     * Gets the data that matches the request within the time range
     * 
     * @param request
     *            the request to get data for
     * @param timeRange
     *            the time range to get data for
     * @return the data that matches the request and time range
     * @throws UnsupportedOutputTypeException
     *             if the factory for this datatype cannot produce IGeometryData
     */
    public static IGeometryData[] getGeometryData(IDataRequest request,
            TimeRange timeRange) throws UnsupportedOutputTypeException {
        IDataFactory factory = getFactory(request);
        return factory.getGeometryData(request, timeRange);
    }

    /**
     * Gets the data that matches the request at the specified times. If data is
     * time agnostic then simply don't pass in any DataTimes, for example
     * DataAccessLayer.getGeometryData(R)
     * 
     * @param request
     *            the request to get data for
     * @param times
     *            the times to get data for
     * @return the data that matches the request and times
     * @throws UnsupportedOutputTypeException
     *             if the factory for this datatype cannot produce IGeometryData
     */
    public static IGeometryData[] getGeometryData(IDataRequest request,
            DataTime... times) throws UnsupportedOutputTypeException {
        IDataFactory factory = getFactory(request);
        return factory.getGeometryData(request, times);
    }

    /**
     * Gets the data that matches the request within the time range
     * 
     * @param request
     *            the request to get data for
     * @param timeRange
     *            the time range to get data for
     * @return the data that matches the request and time range
     * @throws UnsupportedOutputTypeException
     *             if the factory for this datatype cannot produce IGridData
     */
    public static IGridData[] getGridData(IDataRequest request,
            TimeRange timeRange) throws UnsupportedOutputTypeException {
        IDataFactory factory = getFactory(request);
        return factory.getGridData(request, timeRange);
    }

    /**
     * Gets the available location names that match the request without actually
     * requesting the data.
     * 
     * @param request
     * @return the available location names if the data was requested
     */
    public static String[] getAvailableLocationNames(IDataRequest request) {
        IDataFactory factory = getFactory(request);
        return factory.getAvailableLocationNames(request);
    }

    /**
     * Shortcut to creating a new request
     * 
     * @return
     */
    public static IDataRequest newDataRequest() {
        return new DefaultDataRequest();
    }

    /**
     * TODO: contemplate making this public to allow for special case handling
     * Returns the factory that should process the request. Will never return
     * null, will instead throw exceptions.
     * 
     * @param request
     * @return the factory that matches the request
     * @throws DataFactoryNotFoundException
     */
    private static IDataFactory getFactory(IDataRequest request) {
        return DataFactoryRegistry.getInstance().getFactory(request);
    }

}
