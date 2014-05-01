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

import com.raytheon.uf.common.dataaccess.IDataRequest;
import com.raytheon.uf.common.dataaccess.exception.TimeAgnosticDataException;
import com.raytheon.uf.common.dataaccess.geom.IGeometryData;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;

/**
 * Abstracts the retrieval of time agnostic geometry data by building on and/or
 * extending AbstractGeometryDatabaseFactory.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 29, 2013            bkowal     Initial creation
 * Feb 14, 2013 1614       bsteffen    Refactor data access framework to use
 *                                     single request.
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

public abstract class AbstractGeometryTimeAgnosticDatabaseFactory extends
        AbstractGeometryDatabaseFactory {

    /**
     * Constructor
     * 
     * @param databaseName
     *            the name of the database to execute queries against
     * @param requiredIdentifiers
     *            the identifiers that need to be included in the request
     *            (ifdef)
     */
    public AbstractGeometryTimeAgnosticDatabaseFactory(String databaseName,
            String[] requiredIdentifiers) {
        super(databaseName, requiredIdentifiers);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.dataaccess.IDataFactory#getAvailableTimes(com.
     * raytheon.uf.common.dataaccess.IDataRequest)
     */
    @Override
    public DataTime[] getAvailableTimes(IDataRequest request)
            throws TimeAgnosticDataException {
        throw new TimeAgnosticDataException(this.buildExceptionMessage(request));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.dataaccess.IDataFactory#getData(com.raytheon.uf
     * .common.dataaccess.IDataRequest, com.raytheon.uf.common.time.DataTime[])
     */
    @Override
    public IGeometryData[] getGeometryData(IDataRequest request,
            DataTime... times) {
        return this.getData(request);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.dataaccess.IDataFactory#getData(com.raytheon.uf
     * .common.dataaccess.IDataRequest, com.raytheon.uf.common.time.TimeRange)
     */
    @Override
    public IGeometryData[] getGeometryData(IDataRequest request,
            TimeRange timeRange) {
        return this.getData(request);
    }

    /**
     * Retrieves data in a time agnostic way.
     * 
     * @param request the original request that we are processing
     * @return an array of IGeometryData
     */
    protected IGeometryData[] getData(IDataRequest request) {
        return super.executeDataQuery(this.assembleGetData(request), request);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.dataaccess.IDataFactory#getAvailableTimes(com.
     * raytheon.uf.common.dataaccess.IDataRequest,
     * com.raytheon.uf.common.time.BinOffset)
     */
    @Override
    public DataTime[] getAvailableTimes(IDataRequest request,
            BinOffset binOffset) throws TimeAgnosticDataException {
        throw new TimeAgnosticDataException(this.buildExceptionMessage(request));
    }

    /**
     * Constructs the message that will be included in the TimeAgnosticException
     * 
     * @param request the original request that we are processing
     * @return the constructed exception message
     */
    private String buildExceptionMessage(IDataRequest request) {
        StringBuilder stringBuilder = new StringBuilder(
                "This operation is unsupported for data type: ");
        stringBuilder.append(request.getDatatype());

        return stringBuilder.toString();
    }

    /**
     * Builds a time agnostic version of the query that will be used to retrieve data from the database.
     * 
     * @param request the original request that we are processing
     * @return the query
     */
    protected abstract String assembleGetData(IDataRequest request);

    /**
     * The following methods are no longer applicable to us.
     * 
     * Should we be throwing an exception
     */
    @Override
    protected String assembleGetTimes(IDataRequest request) {
        return null;
    }

    @Override
    protected String assembleGetTimes(IDataRequest request,
            BinOffset binOffset) {
        return null;
    }

    @Override
    protected String assembleGetData(IDataRequest request,
            DataTime... times) {
        return null;
    }

    @Override
    protected String assembleGetData(IDataRequest request,
            TimeRange timeRange) {
        return null;
    }
}