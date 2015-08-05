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
package com.raytheon.uf.common.dataplugin.warning.dataaccess;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.EnumSet;
import java.util.Map;

import com.raytheon.uf.common.dataaccess.IDataRequest;
import com.raytheon.uf.common.dataaccess.exception.DataRetrievalException;
import com.raytheon.uf.common.dataaccess.geom.IGeometryData;
import com.raytheon.uf.common.dataaccess.impl.AbstractGeometryDatabaseFactory;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.DataTime.FLAG;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKBReader;

/**
 * A data factory for retrieving warning/practicewarning data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Mar 18, 2015  4227     mapeters    Initial creation.
 * Aug 05, 2015  4486     rjpeter     Changed Timestamp to Date.
 * </pre>
 * 
 * @author mapeters
 * @version 1.0
 */

public class WarningGeometryFactory extends AbstractGeometryDatabaseFactory {

    private static final String REF_TIME = "reftime";

    private static final String FCST_TIME = "forecasttime";

    private static final String UTIL_FLAGS = "utilityflags";

    private static final String RANGE_END = "rangeend";

    private static final String RANGE_START = "rangestart";

    private static final String GEOM = "geometry";

    private static final String OFFICE_ID = "officeid";

    private static final String[] notParameters = new String[] { FCST_TIME,
            REF_TIME, UTIL_FLAGS, RANGE_END, RANGE_START, GEOM, OFFICE_ID,
            "datauri", "ufn" };

    public WarningGeometryFactory() {
        super("metadata", null, new String[] { COL_NAME_OPTION });
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.dataaccess.impl.AbstractDataFactory#
     * getAvailableParameters(com.raytheon.uf.common.dataaccess.IDataRequest)
     */
    @Override
    public String[] getAvailableParameters(IDataRequest request) {
        String[] columnsArray = super.getAvailableParameters(request);
        ArrayList<String> columnsList = new ArrayList<>(columnsArray.length);
        columnsList.addAll(Arrays.asList(columnsArray));
        for (String column : notParameters) {
            columnsList.remove(column);
        }
        return columnsList.toArray(new String[0]);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.dataaccess.impl.AbstractGeometryDatabaseFactory
     * #assembleGetAvailableParameters
     * (com.raytheon.uf.common.dataaccess.IDataRequest)
     */
    @Override
    protected String assembleGetAvailableParameters(IDataRequest request) {
        return "select column_name from information_schema.columns where table_name = '"
                + request.getDatatype() + "';";
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.dataaccess.impl.AbstractGeometryDatabaseFactory
     * #assembleGetTimes(com.raytheon.uf.common.dataaccess.IDataRequest,
     * boolean)
     */
    @Override
    protected String assembleGetTimes(IDataRequest request, boolean refTimeOnly) {
        StringBuilder sb = new StringBuilder();
        sb.append("select distinct ").append(REF_TIME);
        if (!refTimeOnly) {
            for (String column : new String[] { FCST_TIME, RANGE_START,
                    RANGE_END, UTIL_FLAGS }) {
                sb.append(", ").append(column);
            }
        }
        sb.append(" from ").append(request.getDatatype()).append(";");
        return sb.toString();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.dataaccess.impl.AbstractGeometryDatabaseFactory
     * #assembleGetData(com.raytheon.uf.common.dataaccess.IDataRequest,
     * com.raytheon.uf.common.time.DataTime[])
     */
    @Override
    protected String assembleGetData(IDataRequest request,
            DataTime... dataTimes) {
        return assembleGetData(request, buildDataTimesConstraint(dataTimes));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.dataaccess.impl.AbstractGeometryDatabaseFactory
     * #assembleGetData(com.raytheon.uf.common.dataaccess.IDataRequest,
     * com.raytheon.uf.common.time.TimeRange)
     */
    @Override
    protected String assembleGetData(IDataRequest request, TimeRange timeRange) {
        return assembleGetData(request, buildTimeRangeConstraint(timeRange));
    }

    private String assembleGetData(IDataRequest request, String timeConstraint) {
        StringBuilder sqlQuery = new StringBuilder("select ").append(REF_TIME);
        for (String column : new String[] { FCST_TIME, RANGE_START, RANGE_END,
                UTIL_FLAGS, "AsBinary(" + GEOM + ")", OFFICE_ID }) {
            sqlQuery.append(", ").append(column);
        }

        String[] params = request.getParameters();
        for (String p : params) {
            sqlQuery.append(", ").append(p);
        }

        sqlQuery.append(" from ").append(request.getDatatype());

        String keyWord = " where ";
        for (Map.Entry<String, Object> entry : request.getIdentifiers()
                .entrySet()) {
            String key = entry.getKey();
            String value = (String) entry.getValue();
            sqlQuery.append(keyWord).append(key).append(" = '").append(value)
                    .append("'");
            keyWord = " and ";
        }
        if (!timeConstraint.trim().isEmpty()) {
            sqlQuery.append(keyWord).append(timeConstraint);
        }
        sqlQuery.append(";");

        return sqlQuery.toString();
    }

    private static String buildDataTimesConstraint(DataTime... dataTimes) {
        StringBuilder dataTimesConstraint = new StringBuilder();
        if ((dataTimes != null) && (dataTimes.length > 0)) {
            dataTimesConstraint.append(REF_TIME).append(" in (");
            for (int i = 0; i < dataTimes.length; i++) {
                dataTimesConstraint.append("'");
                dataTimesConstraint.append(TimeUtil
                        .formatToSqlTimestamp(dataTimes[i].getRefTime()));
                dataTimesConstraint.append("'");
                if (i < (dataTimes.length - 1)) {
                    dataTimesConstraint.append(",");
                }
            }
            dataTimesConstraint.append(")");
        }
        return dataTimesConstraint.toString();
    }

    private static String buildTimeRangeConstraint(TimeRange timeRange) {
        StringBuilder timeRangeConstraint = new StringBuilder();
        if (timeRange != null) {
            timeRangeConstraint
                    .append(REF_TIME)
                    .append(" between '")
                    .append(TimeUtil.formatToSqlTimestamp(timeRange.getStart()))
                    .append("' and '")
                    .append(TimeUtil.formatToSqlTimestamp(timeRange.getEnd()))
                    .append("'");
        }
        return timeRangeConstraint.toString();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.dataaccess.impl.AbstractGeometryDatabaseFactory
     * #assembleGetAvailableLocationNames
     * (com.raytheon.uf.common.dataaccess.IDataRequest)
     */
    @Override
    protected String assembleGetAvailableLocationNames(IDataRequest request) {
        StringBuilder sqlQuery = new StringBuilder("select distinct ")
                .append(OFFICE_ID).append(" from ")
                .append(request.getDatatype());

        String keyWord = " where ";
        for (Map.Entry<String, Object> entry : request.getIdentifiers()
                .entrySet()) {
            String key = entry.getKey();
            String value = (String) entry.getValue();
            sqlQuery.append(keyWord).append(key).append(" = '").append(value)
                    .append("'");
            keyWord = " and ";
        }
        sqlQuery.append(";");

        return sqlQuery.toString();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.dataaccess.impl.AbstractGeometryDatabaseFactory
     * #makeGeometry(java.lang.Object[], java.lang.String[], java.util.Map)
     */
    @Override
    protected IGeometryData makeGeometry(Object[] data, String[] paramNames,
            Map<String, Object> attrs) {
        DataTime dataTime = buildDataTimeFromQueryResults(data);

        Geometry geometry = null;
        byte[] geomBytes = (byte[]) data[5];
        if ((geomBytes != null) && (geomBytes.length > 0)) {
            WKBReader wkbreader = new WKBReader();
            try {
                geometry = wkbreader.read(geomBytes);
            } catch (ParseException e) {
                throw new DataRetrievalException(
                        "Failed to parse the geometry.", e);
            }
        }

        if (geometry == null) {
            return null;
        }

        String loc = (String) data[6];

        return super.buildGeometryData(dataTime, null, geometry, loc, attrs, 7,
                data, paramNames);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.dataaccess.impl.AbstractGeometryDatabaseFactory
     * #buildDataTimeFromQueryResults(java.lang.Object[])
     */
    @Override
    protected DataTime buildDataTimeFromQueryResults(Object[] results) {
        DataTime dataTime = super.buildDataTimeFromQueryResults(results);
        if (results.length > 1) {
            dataTime.setFcstTime((int) results[1]);
            TimeRange timeRange = new TimeRange(((Date) results[2]).getTime(),
                    ((Date) results[3]).getTime());
            dataTime.setValidPeriod(timeRange);

            EnumSet<FLAG> utilityFlags = EnumSet.noneOf(FLAG.class);
            String flagsString = (String) results[4];
            flagsString = flagsString.substring(1, flagsString.length() - 1);
            if (!flagsString.trim().isEmpty()) {
                String[] flags = flagsString.split(", ");
                for (String flag : flags) {
                    utilityFlags.add(FLAG.valueOf(flag));
                }
            }
            dataTime.setUtilityFlags(utilityFlags);
        }
        return dataTime;
    }
}
