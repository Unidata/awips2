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
package com.raytheon.uf.common.hydro.dataaccess;

import java.util.Date;
import java.util.Map;

import com.raytheon.uf.common.dataaccess.IDataRequest;
import com.raytheon.uf.common.dataaccess.exception.IncompatibleRequestException;
import com.raytheon.uf.common.dataaccess.geom.IGeometryData;
import com.raytheon.uf.common.dataaccess.impl.AbstractGeometryDatabaseFactory;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;

/**
 * A data factory for getting data from the IHFS database. Requires that a
 * request have a table identifier that corresponds to the table it should
 * retrieve data from. Only works against tables that follow the SHEF PEDTSEP
 * pattern.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Nov 13, 2012           njensen     Initial creation
 * Jan 30, 2012  1551     bkowal      Refactored
 * Jan 31, 2012  1555     bkowal      Modification based on existing hydro code
 * Feb 14, 2013  1614     bsteffen    Refactor data access framework to use
 *                                    single request.
 * Mar 03, 2014  2673     bsteffen    Add ability to query only ref times.
 * Jul 14, 2014  3184     njensen     Overrode getAvailableLevels()
 * Jul 30, 2014  3184     njensen     Added optional identifiers
 * Feb 03, 2015  4009     mapeters    Moved getAvailableLevels() override to super
 * Mar 18, 2015  4227     mapeters    Removed getAvailableTimes(), assembleGetTimes()
 *                                    taking BinOffsets.
 * Aug 05, 2015  4486     rjpeter     Changed Timestamp to Date.
 * Oct 05, 2016  5926     dgilling    Rewrite assembleGetAvailableLocationNames
 *                                    to use all request parameters.
 * </pre>
 * 
 * @author njensen
 */

public class HydroGeometryFactory extends AbstractGeometryDatabaseFactory {

    // TODO always require at least one PE
    // TODO possibly take care of it for them and add value
    // TODO potentially limit big requests so that if too big of a result set,
    // throw error to
    // let the user know they need to reduce amount of data they request
    // test out on live data how slow it gets to determine max number
    // TODO add support for envelopes bounding the request
    private static final String[] REQUIRED = { HydroQueryAssembler.TABLE };

    private static final String IHFS_DATABASE = "ihfs";

    private final GeometryFactory gisFactory = new GeometryFactory();

    public HydroGeometryFactory() {
        super(IHFS_DATABASE, REQUIRED, new String[] { COL_NAME_OPTION });
    }

    @Override
    protected IGeometryData makeGeometry(Object[] data, String[] paramNames,
            Map<String, Object> attrs) {

        // order is lid, producttime, lat, lon, other params
        String lid = (String) data[0];
        Date date = (Date) data[1];
        double lat = (Double) data[2];
        double lon = (Double) data[3];
        /*
         * Assuming that this applies to all ihfs data Refer to GageData.java
         * 
         * method: setLon
         */
        if (lon > 0) {
            lon *= -1;
        }

        // intentionally setting level as null until hydrologists determine
        // something better
        return super.buildGeometryData(new DataTime(date), null,
                gisFactory.createPoint(new Coordinate(lon, lat)), lid, attrs, 4,
                data, paramNames);
    }

    @Override
    protected String assembleGetTimes(IDataRequest request,
            boolean refTimeOnly) {
        return HydroQueryAssembler.assembleGetTimes(request);
    }

    @Override
    protected String assembleGetData(IDataRequest request, DataTime... times) {
        return HydroQueryAssembler.assembleGetData(request, times);
    }

    @Override
    protected String assembleGetData(IDataRequest request,
            TimeRange timeRange) {
        return HydroQueryAssembler.assembleGetData(request, timeRange);
    }

    @Override
    protected String assembleGetAvailableLocationNames(IDataRequest request) {
        if (isLocationQuerySupported(request)) {
            return HydroQueryAssembler.assembleGetLocationNames(
                    extractTableName(request), request);
        }

        throw new IncompatibleRequestException(
                "Geometry data cannot be retrieved from table "
                        + extractTableName(request)
                        + " as it lacks a supported concept of location.");
    }

    /**
     * Determines whether the specified {@link IDataRequest} is being made
     * against a table that is location aware--that is, a table for which we can
     * return location information like latitude and longitude.
     * 
     * @param request
     *            the request that to process
     * @return whether or not this request is being for a table that is location
     *         aware.
     */
    private boolean isLocationQuerySupported(IDataRequest request) {
        /*
         * TODO: Support radid? Not all hydro db tables use lid for their
         * location id.
         */
        return executeGetColumnNames(
                new String[] { HydroQueryAssembler.LID_COL }, request)
                        .contains(HydroQueryAssembler.LID_COL);
    }
}