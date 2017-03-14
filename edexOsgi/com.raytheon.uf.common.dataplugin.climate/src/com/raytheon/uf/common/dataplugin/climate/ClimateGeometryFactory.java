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
package com.raytheon.uf.common.dataplugin.climate;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;

import com.raytheon.uf.common.dataaccess.IDataRequest;
import com.raytheon.uf.common.dataaccess.exception.IncompatibleRequestException;
import com.raytheon.uf.common.dataaccess.geom.IGeometryData;
import com.raytheon.uf.common.dataaccess.impl.AbstractGeometryDatabaseFactory;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;

/**
 * A data factory for retrieving climate data from the hmdb database.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * May 14, 2015  4409     mapeters  Initial creation.
 * May 21, 2015  4409     mapeters  Extract query results correctly in
 *                                  getTimeColumnNames()
 * Aug 05, 2015  4486     rjpeter   Changed Timestamp to Date.
 * Aug 21, 2015  4409     mapeters  Alias columns that are automatically
 *                                  retrieved when getting geometry data
 * Apr 07, 2016  5397     mapeters  Include Short in instanceof checks for
 *                                  integer value
 * Jun 09, 2016  5574     mapeters  Add advanced query support, don't require
 *                                  schema in table name
 * Oct 06, 2016  5926     dgilling  Use all request identifiers for 
 *                                  getAvailableLocationNames and 
 *                                  getAvailableTimes requests, fix month off 
 *                                  by one errors in buildDataTimesConstraint.
 * 
 * </pre>
 * 
 * @author mapeters
 */

public class ClimateGeometryFactory extends AbstractGeometryDatabaseFactory {

    private static final String HMDB_DATABASE = "hmdb";

    private static final String TABLE = "table";

    private static final String STATION_CODE = "station_code";

    private static final String STATION_ID = "station_id";

    private static final String INFORM_ID = "inform_id";

    private static final String ICAO_LOC_ID = "icao_loc_id";

    private static final String YEAR = "year";

    private static final String DAY_OF_YEAR = "day_of_year";

    private static final String MONTH = "month";

    private static final String PERIOD_START = "period_start";

    private static final String PERIOD_END = "period_end";

    private static final String DATE = "date";

    private static final String VALID_DTIME = "valid_dtime";

    private static final String MONTH_OF_YEAR = "month_of_year";

    private static final String CLI_STA_SETUP = "cli_sta_setup";

    private static final String RPT = "rpt";

    private static final String PUBLIC_RPT = "public.rpt";

    private static final String CLI_STA_SETUP_LON = "longitude_e";

    private static final String LOCATION_COLUMN_ALIAS_PREFIX = "location_column_";

    private static final String TIME_COLUMN_ALIAS_PREFIX = "time_column_";

    private static final String GEOM_COLUMN_ALIAS_PREFIX = "geom_column_";

    private static final Collection<String> FUZZY_DATE_FIELDS = new HashSet<>(
            Arrays.asList(YEAR, MONTH, MONTH_OF_YEAR, DAY_OF_YEAR));

    public ClimateGeometryFactory() {
        super(HMDB_DATABASE, new String[] { TABLE },
                new String[] { COL_NAME_OPTION });
    }

    /**
     * Get the name(s) of the column(s) that contain the table's concept of
     * time. Throw IncompatibleRequestException if the table has no supported
     * time columns.
     * 
     * @param request
     *            the database request being performed
     * 
     * @return the name(s) of the time column(s)
     */
    private List<String> getTimeColumnNames(IDataRequest request) {
        String tableName = extractTableName(request);
        String[] timeColumns = new String[] { YEAR, DAY_OF_YEAR, MONTH,
                PERIOD_START, PERIOD_END, DATE, VALID_DTIME, MONTH_OF_YEAR };
        Collection<String> results = this.executeGetColumnNames(timeColumns,
                request);
        if (!results.isEmpty()) {
            return new ArrayList<>(results);
        }
        throw new IncompatibleRequestException(
                "Geometry data cannot be retrieved from table " + tableName
                        + " as it lacks a supported concept of time");
    }

    /**
     * Get the name of the column that contains the table's concept of location.
     * Throw IncompatibleRequestException if the table doesn't have a supported
     * location column.
     * 
     * @param request
     *            the database request being performed
     * @param tableName
     *            the name of the table
     * @return the name of the location column
     */
    private String getLocationColumnName(IDataRequest request,
            String tableName) {
        String[] locationColumns = new String[] { STATION_CODE, STATION_ID,
                INFORM_ID, ICAO_LOC_ID };
        Collection<String> results = this.executeGetColumnNames(locationColumns,
                request);
        if (!results.isEmpty()) {
            return results.iterator().next();
        }
        throw new IncompatibleRequestException(
                "Geometry data cannot be retrieved from table " + tableName
                        + " as it lacks a supported concept of location");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String assembleGetAvailableLocationNames(IDataRequest request) {
        String tableName = extractTableName(request);
        String locationColumnName = getLocationColumnName(request, tableName);

        /*
         * to ensure we get the most accurate and useful list of location names
         * back to the user, we join with the RPT or CLI_STA_SETUP table because
         * that is our source of geo-location data for getGeometryData requests.
         */
        StringBuilder sqlQuery = new StringBuilder("select distinct ");
        if (locationColumnName.equals(STATION_CODE)
                || locationColumnName.equals(ICAO_LOC_ID)) {
            sqlQuery.append(locationColumnName);
        } else {
            sqlQuery.append(CLI_STA_SETUP).append(".").append(STATION_CODE);
        }
        sqlQuery.append(buildFrom(tableName, locationColumnName, true));

        List<String> constraints = new ArrayList<>();
        String joinConstraint = buildJoinConstraint(tableName,
                locationColumnName);
        if (!joinConstraint.trim().isEmpty()) {
            constraints.add(joinConstraint);
        }
        constraints
                .addAll(buildIdentifierConstraints(request.getIdentifiers()));

        String keyWord = " where ";
        for (String constraint : constraints) {
            sqlQuery.append(keyWord).append(constraint);
            keyWord = " and ";
        }

        sqlQuery.append(';');

        return sqlQuery.toString();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String assembleGetTimes(IDataRequest request,
            boolean refTimeOnly) {
        List<String> timeColumnNames = getTimeColumnNames(request);

        StringBuilder sqlQuery = new StringBuilder("select distinct ");
        sqlQuery.append(StringUtils.join(timeColumnNames, ", "));

        String tableName = extractTableName(request);
        String locationColumnName = getLocationColumnName(request, tableName);
        boolean needJoin = !ArrayUtils.isEmpty(request.getLocationNames());
        sqlQuery.append(buildFrom(tableName, locationColumnName, needJoin));

        Collection<String> constraints = new ArrayList<>();

        if (needJoin) {
            String joinConstraint = buildJoinConstraint(tableName,
                    locationColumnName);
            if (!joinConstraint.trim().isEmpty()) {
                constraints.add(joinConstraint);
            }

            String locationNameConstraint = buildLocationNameConstraint(
                    request.getLocationNames(), locationColumnName);
            if (!locationNameConstraint.trim().isEmpty()) {
                constraints.add(locationNameConstraint);
            }
        }

        List<String> identifierConstraints = buildIdentifierConstraints(
                request.getIdentifiers());
        if (!identifierConstraints.isEmpty()) {
            constraints.addAll(identifierConstraints);
        }

        String keyWord = " where ";
        for (String constraint : constraints) {
            sqlQuery.append(keyWord).append(constraint);
            keyWord = " and ";
        }

        sqlQuery.append(';');

        return sqlQuery.toString();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected DataTime buildDataTimeFromQueryResults(Object[] results) {
        return buildDataTimeFromQueryResults(results, 0).dataTime;
    }

    /**
     * Builds a DataTime from the given query results. Also determines the start
     * index of additional data in the query results based on the number of
     * elements used to create the DataTime. The startIndex and the returned
     * dataIndex are ignored when this method is called as part of
     * getAvailableTimes(), but used as part of getGeometryData().
     * 
     * @param results
     *            the query results to build from
     * @param startIndex
     *            the index of the first element involved in building the
     *            DataTime
     * 
     * @return a DataTimeWithDataIndex containing the DataTime and the start
     *         index of additional data. The DataTime may be null if the data is
     *         faulty and doesn't match expected data types.
     */
    private static DataTimeWithDataIndex buildDataTimeFromQueryResults(
            Object[] results, int startIndex) {
        int dataIndex = startIndex + 1;
        DataTime dataTime = null;
        Object dataTimeElement = results[startIndex];
        /*
         * This second element may form a part of the DataTime or be the start
         * of the parameters' values, depending on the table queried. If it is
         * used, the dataIndex is incremented by 1 as the additional data starts
         * one index later.
         */
        Object potentialDataTimeElement = null;
        if (results.length > (startIndex + 1)) {
            potentialDataTimeElement = results[startIndex + 1];
        }

        if (dataTimeElement instanceof Date) {
            Date date = (Date) dataTimeElement;
            if (potentialDataTimeElement instanceof Date) {
                // For tables with start Date and end Date
                TimeRange timeRange = new TimeRange(date,
                        (Date) potentialDataTimeElement);
                dataTime = new DataTime(date.getTime(), timeRange);
                dataIndex++;
            } else {
                // For tables with one Date
                dataTime = new DataTime(new Date(date.getTime()));
            }
        } else if (dataTimeElement instanceof Short
                || dataTimeElement instanceof Integer) {
            int firstValue = ((Number) dataTimeElement).intValue();
            /*
             * Build DataTime from case where firstValue is an int (the year or
             * month)
             */
            return buildDataTimeFromIntElement(firstValue,
                    potentialDataTimeElement, dataIndex);
        } else if (dataTimeElement instanceof String) {
            /*
             * For tables with data for a day (dataTimeElement is a "mm-dd"
             * String) in all years
             */
            dataTime = buildDataTimeFromDayOfYear(4, (String) dataTimeElement);
        } else {
            throw new IllegalStateException(
                    "Received incompatible dataTimeElement of type "
                            + dataTimeElement.getClass());
        }

        return new DataTimeWithDataIndex(dataTime, dataIndex);
    }

    /**
     * Build a DataTime from an integer element and potentially a second
     * DataTime element. Also return the array index of additional data
     * following the DataTime element(s).
     * 
     * @param intElement
     *            the integer DataTime element
     * @param potentialDataTimeElement
     *            the potential second DataTime element
     * @param dataIndex
     *            the index used to determine the starting index of additional
     *            data
     * @return a DataTimeWithDataIndex containing the DataTime and the start
     *         index of additional data. The DataTime may be null if the data is
     *         faulty and doesn't match expected data types.
     */
    private static DataTimeWithDataIndex buildDataTimeFromIntElement(
            int intElement, Object potentialDataTimeElement, int dataIndex) {
        DataTime dataTime = null;
        int index = dataIndex;

        // Check if the int element represents a month
        if (intElement <= 12) {
            // For tables with data for a month in all years
            dataTime = buildDataTimeForEntireMonth(4, intElement - 1);
        } else if (potentialDataTimeElement instanceof Short
                || potentialDataTimeElement instanceof Integer) {
            int secondValue = ((Number) potentialDataTimeElement).intValue();
            /*
             * Check if the second element represents a month (making the first
             * represent the year)
             */
            if (secondValue <= 12) {
                /*
                 * For tables with year and month int values, build data time
                 * covering entire month
                 */
                dataTime = buildDataTimeForEntireMonth(intElement,
                        secondValue - 1);
                index++;
            }
        } else if (potentialDataTimeElement instanceof String) {
            /*
             * For tables with a year int (intElement) and a "mm-dd" String
             * (potentialDataTimeElement)
             */
            dataTime = buildDataTimeFromDayOfYear(intElement,
                    (String) potentialDataTimeElement);
            index++;
        } else {
            throw new IllegalStateException(
                    "Received incompatible potentialDataTimeElement of type "
                            + potentialDataTimeElement.getClass());
        }
        return new DataTimeWithDataIndex(dataTime, index);
    }

    /**
     * Build and return a DataTime with a TimeRange covering the entire given
     * month.
     * 
     * @param year
     * @param month
     * @return the DataTime
     */
    private static DataTime buildDataTimeForEntireMonth(int year, int month) {
        Calendar firstDay = new GregorianCalendar(year, month, 1);
        Calendar lastDay = new GregorianCalendar(year, month,
                firstDay.getActualMaximum(Calendar.DAY_OF_MONTH));
        for (int field : new int[] { Calendar.HOUR_OF_DAY, Calendar.MINUTE,
                Calendar.SECOND, Calendar.MILLISECOND }) {
            lastDay.set(field, lastDay.getActualMaximum(field));
        }
        return new DataTime(firstDay, new TimeRange(firstDay, lastDay));
    }

    /**
     * Build and return a DataTime from a year integer and a "mm-dd" String.
     * 
     * @param year
     *            the integer representing the year
     * @param dayOfYear
     *            the "mm-dd" String representing the month and day
     * @return the DataTime
     */
    private static DataTime buildDataTimeFromDayOfYear(int year,
            String dayOfYear) {
        String[] splitMonthAndDay = dayOfYear.split("-");
        int month = Integer.valueOf(splitMonthAndDay[0]) - 1;
        int day = Integer.valueOf(splitMonthAndDay[1]);
        return new DataTime(new GregorianCalendar(year, month, day));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String assembleGetData(IDataRequest request, DataTime... times) {
        List<String> timeColumnNames = getTimeColumnNames(request);
        return assembleGetData(request,
                buildDataTimesConstraint(timeColumnNames, times),
                timeColumnNames);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String assembleGetData(IDataRequest request,
            TimeRange timeRange) {
        List<String> timeColumnNames = getTimeColumnNames(request);
        return assembleGetData(request,
                buildTimeRangeConstraint(timeColumnNames, timeRange),
                timeColumnNames);
    }

    private String assembleGetData(IDataRequest request, String timeConstraint,
            List<String> timeColumnNames) {
        String tableName = extractTableName(request);
        boolean isRpt = tableName.equals(RPT);
        String locationColumnName = getLocationColumnName(request, tableName);
        boolean hasStationCode = locationColumnName.equals(STATION_CODE);
        // Build SELECT statement
        StringBuilder sqlQuery = new StringBuilder("select ");

        /*
         * We must alias all columns making up the geometry. If a
         * location/lat/lon/time column is also requested as a parameter, this
         * makes them differentiable.
         */
        String locColumnAsAlias;
        String latColumnAsAlias;
        String lonColumnAsAlias;
        /*
         * rpt table is used to get location names and coordinates for tables
         * with station_code location column (or rpt table itself). Other tables
         * with station_id or inform_id location column are joined with
         * cli_sta_setup table to get location name and coordinates.
         */
        if (hasStationCode || isRpt) {
            locColumnAsAlias = buildAlias(RPT, ICAO_LOC_ID,
                    LOCATION_COLUMN_ALIAS_PREFIX);
            latColumnAsAlias = buildAlias(RPT, "lat", GEOM_COLUMN_ALIAS_PREFIX);
            lonColumnAsAlias = buildAlias(RPT, "lon", GEOM_COLUMN_ALIAS_PREFIX);
        } else {
            locColumnAsAlias = buildAlias(CLI_STA_SETUP, STATION_CODE,
                    LOCATION_COLUMN_ALIAS_PREFIX);
            latColumnAsAlias = buildAlias(CLI_STA_SETUP, "latitude_n",
                    GEOM_COLUMN_ALIAS_PREFIX);
            /*
             * TODO: The longitude values in cli_sta_setup are all positive when
             * they should all be negative. This problem is inherited from A1,
             * in which the negative sign was assumed since only the U.S. was
             * covered. Changing the longitude sign works for now, but potential
             * future stations in different hemispheres may cause problems here.
             */
            lonColumnAsAlias = new StringBuilder(CLI_STA_SETUP).append(".")
                    .append(CLI_STA_SETUP_LON).append(" * -1 as ")
                    .append(GEOM_COLUMN_ALIAS_PREFIX).append(CLI_STA_SETUP_LON)
                    .toString();
        }
        sqlQuery.append(locColumnAsAlias).append(", ").append(latColumnAsAlias)
                .append(", ").append(lonColumnAsAlias);

        for (String timeColumnName : timeColumnNames) {
            sqlQuery.append(", ").append(buildAlias(tableName, timeColumnName,
                    TIME_COLUMN_ALIAS_PREFIX));
        }

        String[] params = request.getParameters();
        for (String param : params) {
            sqlQuery.append(", ").append(tableName).append(".").append(param);
        }

        // Build FROM statement
        sqlQuery.append(buildFrom(tableName, locationColumnName, true));

        // Build constraints
        List<String> constraints = new ArrayList<>();

        String joinConstraint = buildJoinConstraint(tableName,
                locationColumnName);
        if (!joinConstraint.trim().isEmpty()) {
            constraints.add(joinConstraint);
        }

        String locationNameConstraint = buildLocationNameConstraint(
                request.getLocationNames(), locationColumnName);
        if (!locationNameConstraint.trim().isEmpty()) {
            constraints.add(locationNameConstraint);
        }

        List<String> identifierConstraints = buildIdentifierConstraints(
                request.getIdentifiers());
        if (!identifierConstraints.isEmpty()) {
            constraints.addAll(identifierConstraints);
        }

        if (!timeConstraint.trim().isEmpty()) {
            constraints.add(timeConstraint);
        }

        String keyWord = " where ";
        for (String constraint : constraints) {
            sqlQuery.append(keyWord).append(constraint);
            keyWord = " and ";
        }

        sqlQuery.append(";");

        return sqlQuery.toString();
    }

    /**
     * Build an SQL FROM statement given the specified table to query from. If
     * necessary, will add the appropriate location table to the from statement
     * if a join to that table is desired.
     * 
     * @param tableName
     * @param locationColumnName
     * @param joinWithLocationTable
     * @return
     */
    private static String buildFrom(String tableName, String locationColumnName,
            boolean joinWithLocationTable) {
        boolean isRpt = tableName.equals(RPT) || tableName.equals(PUBLIC_RPT);
        boolean hasStationCode = locationColumnName.equals(STATION_CODE);

        StringBuilder fromStatement = new StringBuilder(" from ");
        fromStatement.append(tableName);

        if (joinWithLocationTable && !isRpt) {
            fromStatement.append(", ")
                    .append(hasStationCode ? RPT : CLI_STA_SETUP);
        }

        return fromStatement.toString();
    }

    /**
     * Alias the given column by prepending the aliasPrefix to it, to be used in
     * a select statement.
     * 
     * @param table
     * @param column
     * @param aliasPrefix
     * @return the aliased column, in the format: "table.column as alias"
     */
    private static String buildAlias(String table, String column,
            String aliasPrefix) {
        return new StringBuilder(table).append(".").append(column)
                .append(" as ").append(aliasPrefix).append(column).toString();
    }

    private static String buildLocationNameConstraint(String[] locationNames,
            String locationColumnName) {
        StringBuilder locationConstraint = new StringBuilder();
        if (locationNames.length > 0) {
            if (locationColumnName.equals(STATION_CODE)
                    || locationColumnName.equals(ICAO_LOC_ID)) {
                locationConstraint.append(locationColumnName);
            } else {
                locationConstraint.append(CLI_STA_SETUP).append(".")
                        .append(STATION_CODE);
            }
            locationConstraint.append(" in (");
            boolean first = true;
            for (String locationName : locationNames) {
                if (!first) {
                    locationConstraint.append(",");
                } else {
                    first = false;
                }
                locationConstraint.append("'").append(locationName).append("'");
            }
            locationConstraint.append(")");
        }
        return locationConstraint.toString();
    }

    /**
     * Builds an SQL WHERE clause to perform a join for the given table to the
     * appropriate location table joining on the specified location column name.
     * 
     * @param tableName
     * @param locationColumnName
     * @return
     */
    private static String buildJoinConstraint(String tableName,
            String locationColumnName) {
        boolean isRpt = tableName.equals(RPT) || tableName.equals(PUBLIC_RPT);
        boolean hasStationCode = locationColumnName.equals(STATION_CODE);

        StringBuilder joinConstraint = new StringBuilder();

        if (!isRpt) {
            if (hasStationCode) {
                joinConstraint.append(RPT).append(".").append(ICAO_LOC_ID);
            } else {
                /*
                 * inform_id and station_id location columns both match
                 * cli_sta_setup's station_id column
                 */
                joinConstraint.append(CLI_STA_SETUP).append(".")
                        .append(STATION_ID);
            }
            joinConstraint.append(" = ").append(tableName).append(".")
                    .append(locationColumnName);
        }

        return joinConstraint.toString();
    }

    private static List<String> buildIdentifierConstraints(
            Map<String, Object> identifiers) {
        List<String> identifierConstraints = new ArrayList<>();
        for (Map.Entry<String, Object> entry : identifiers.entrySet()) {
            String key = entry.getKey();
            if (key.equals(TABLE)) {
                continue;
            }

            Object value = entry.getValue();
            RequestConstraint requestConstraint;
            if (value instanceof RequestConstraint) {
                requestConstraint = (RequestConstraint) value;
            } else {
                requestConstraint = new RequestConstraint(value.toString());
            }

            StringBuilder constraint = new StringBuilder(key)
                    .append(requestConstraint.toSqlString());
            identifierConstraints.add(constraint.toString());
        }
        return identifierConstraints;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected IGeometryData makeGeometry(Object[] data, String[] paramNames,
            Map<String, Object> attrs) {
        String locName = (String) data[0];
        Coordinate coordinate = new Coordinate((double) data[1],
                (double) data[2]);

        DataTimeWithDataIndex dataTimeWithDataIndex = buildDataTimeFromQueryResults(
                data, 3);
        // Ignore data with invalid/faulty time column values.
        if (dataTimeWithDataIndex.dataTime == null) {
            return null;
        }

        GeometryFactory geomFactory = new GeometryFactory();
        return super.buildGeometryData(dataTimeWithDataIndex.dataTime, null,
                geomFactory.createPoint(coordinate), locName, attrs,
                dataTimeWithDataIndex.dataIndex, data, paramNames);
    }

    /**
     * Creates am SQL WHERE clause for the given columns given the specified
     * {@link DataTime} instances.
     * 
     * @param timeColumnNames
     *            list of database columns names to use in the query constraint.
     * @param dataTimes
     *            {@code DataTime} instances to use to build the constaints
     * @return an SQL WHERE clause that uses the given times as constraints on
     *         the given columns.
     */
    private static String buildDataTimesConstraint(List<String> timeColumnNames,
            DataTime... dataTimes) {
        StringBuilder dataTimesConstraint = new StringBuilder();
        if ((dataTimes != null) && (dataTimes.length > 0)) {
            if (CollectionUtils.containsAny(timeColumnNames,
                    FUZZY_DATE_FIELDS)) {
                Map<String, List<String>> dateColumnsValues = extractCalendarValues(
                        timeColumnNames, dataTimes);
                dataTimesConstraint
                        .append(buildDateConstraints(dateColumnsValues));
            } else if (timeColumnNames.contains(PERIOD_START)
                    && timeColumnNames.contains(PERIOD_END)) {
                // For tables with start and end Date
                for (int i = 0; i < dataTimes.length; i++) {
                    dataTimesConstraint.append("'")
                            .append(dataTimes[i].getRefTime()).append("'")
                            .append(" between ").append(PERIOD_START)
                            .append(" and ").append(PERIOD_END);
                    if (i < (dataTimes.length - 1)) {
                        dataTimesConstraint.append(" or ");
                    }
                }
            } else if (timeColumnNames.contains(DATE)
                    || timeColumnNames.contains(VALID_DTIME)) {
                // For tables with Date/Timestamp
                String columnName = timeColumnNames.contains(DATE) ? DATE
                        : VALID_DTIME;
                dataTimesConstraint.append(columnName).append(" in (");
                for (int i = 0; i < dataTimes.length; i++) {
                    dataTimesConstraint.append("'")
                            .append(dataTimes[i].getRefTime()).append("'");
                    if (i < (dataTimes.length - 1)) {
                        dataTimesConstraint.append(",");
                    }
                }
                dataTimesConstraint.append(")");
            }
        }
        return dataTimesConstraint.toString();
    }

    /**
     * Helper function to build time-based constraints for tables that use YEAR,
     * MONTH, MONTH_OF_YEAR, and/or DAY_OF_YEAR fields. Takes all
     * {@link DataTime} instances and extracts the appropriate values based on
     * the time columns in the table. Those values are then collected in a map
     * that links column name with the list of values.
     * 
     * @param columnNames
     * @param dataTimes
     * @return
     */
    private static Map<String, List<String>> extractCalendarValues(
            List<String> columnNames, DataTime... dataTimes) {
        Map<String, List<String>> retValues = new HashMap<>();

        for (DataTime time : dataTimes) {
            Calendar timeAsCalendar = time.getRefTimeAsCalendar();
            for (String columnName : columnNames) {
                String value;
                if (YEAR.equals(columnName)) {
                    value = Integer.toString(timeAsCalendar.get(Calendar.YEAR));
                } else if (MONTH.equals(columnName)
                        || MONTH_OF_YEAR.equals(columnName)) {
                    value = Integer
                            .toString(timeAsCalendar.get(Calendar.MONTH) + 1);
                } else if (DAY_OF_YEAR.equals(columnName)) {
                    value = extractDayOfYearString(timeAsCalendar);
                } else {
                    continue;
                }

                List<String> valuesForColumn = retValues.get(columnName);
                if (valuesForColumn == null) {
                    valuesForColumn = new ArrayList<>();
                    retValues.put(columnName, valuesForColumn);
                }
                valuesForColumn.add(value);
            }
        }

        return retValues;
    }

    /**
     * Builds a string that matches the format of the day_of_year column used in
     * some of the climate database tables. The day_of_year column is a string
     * the combines the month and day of month fields from a calendar. For
     * example, given a calendar with the datetime 2010-08-15 12:00, this
     * function will return the String "08-15".
     * 
     * @param timeAsCalendar
     * @return
     */
    private static String extractDayOfYearString(Calendar timeAsCalendar) {
        int month = timeAsCalendar.get(Calendar.MONTH) + 1;
        int day = timeAsCalendar.get(Calendar.DAY_OF_MONTH);
        return String.format("%02d-%02d", month, day);
    }

    /**
     * Build the SQL WHERE clause based on the time columns in the table and the
     * values for those columns.
     * 
     * @param dateColumnsValues
     * @return
     */
    private static String buildDateConstraints(
            Map<String, List<String>> dateColumnsValues) {
        if (dateColumnsValues.keySet().size() > 1) {
            return buildCompoundDateConstraint(dateColumnsValues);
        } else if (dateColumnsValues.keySet().size() == 1) {
            String keyName = dateColumnsValues.keySet().iterator().next();
            return buildInConstraint(keyName, dateColumnsValues.get(keyName));
        } else {
            return StringUtils.EMPTY;
        }
    }

    /**
     * Helper function that builds the SQL WHERE clause for time-based
     * constraints where the values of multiple columns must be combined
     * together for the constraint.
     * <p>
     * For example if the column uses YEAR and MONTH an example clause generated
     * by this function might look like:
     * 
     * <pre>
     * ((YEAR='20xx' AND MONTH='ii') OR (YEAR='20yy' AND MONTH='jj') OR (YEAR='20zz' AND MONTH='kk'))
     * </pre>
     * 
     * @param dateColumnsValues
     * @return
     */
    private static String buildCompoundDateConstraint(
            Map<String, List<String>> dateColumnsValues) {
        StringBuilder constraint = new StringBuilder();
        constraint.append('(');

        /*
         * safe guard for if lists are not even length. in that case we loop to
         * the size of the shortest list of all.
         */
        int listSize = Integer.MAX_VALUE;
        for (List<String> values : dateColumnsValues.values()) {
            listSize = Math.min(listSize, values.size());
        }

        for (int i = 0; i < listSize; i++) {
            List<String> clauses = new ArrayList<>(dateColumnsValues.size());
            for (String columnName : dateColumnsValues.keySet()) {
                String value = dateColumnsValues.get(columnName).get(i);
                clauses.add(columnName + "= '" + value + "'");
            }
            constraint.append('(');
            constraint.append(StringUtils.join(clauses, " and "));
            constraint.append(')');

            if (i < (listSize - 1)) {
                constraint.append(" or ");
            }
        }

        constraint.append(')');

        return constraint.toString();
    }

    /**
     * Helper function that builds the SQL WHERE clause for time-based
     * constraints where we simply need to use the values for a single column to
     * build our constraint.
     * <p>
     * For example if the table uses the DAY_OF_YEAR an example clause generated
     * by this function might look like:
     * 
     * <pre>
     * DAY_OF_YEAR IN ('ii-xx', 'jj-yy', 'kk-zz')
     * </pre>
     * 
     * @param columnName
     * @param values
     * @return
     */
    private static String buildInConstraint(String columnName,
            List<String> values) {
        StringBuilder inConstraint = new StringBuilder();
        inConstraint.append(columnName).append(" in (");
        inConstraint.append(StringUtils.join(new HashSet<>(values), ','));
        inConstraint.append(')');
        return inConstraint.toString();
    }

    private static String buildTimeRangeConstraint(List<String> timeColumnNames,
            TimeRange timeRange) {
        StringBuilder timeRangeConstraint = new StringBuilder();
        if (timeRange != null) {
            Date startTime = timeRange.getStart();
            Calendar startTimeAsCalendar = TimeUtil.newGmtCalendar(startTime);
            Date endTime = timeRange.getEnd();
            Calendar endTimeAsCalendar = TimeUtil.newGmtCalendar(endTime);
            if (timeColumnNames.contains(YEAR)) {
                if (timeColumnNames.contains(DAY_OF_YEAR)) {
                    // For tables with year int and "mm-dd" String
                    timeRangeConstraint
                            .append(buildYearAndDayOfYearBetweenConstraint(
                                    startTimeAsCalendar, endTimeAsCalendar));
                } else if (timeColumnNames.contains(MONTH)) {
                    // For tables with year and month ints
                    timeRangeConstraint
                            .append(buildYearAndMonthBetweenConstraint(
                                    startTimeAsCalendar, endTimeAsCalendar));
                } else {
                    timeRangeConstraint
                            .append(buildBetweenConstraint(YEAR, Calendar.YEAR,
                                    startTimeAsCalendar, endTimeAsCalendar));
                }
            } else if (timeColumnNames.contains(PERIOD_START)
                    && timeColumnNames.contains(PERIOD_END)) {
                // For tables with start and end Date
                timeRangeConstraint.append(PERIOD_START).append(" >= '")
                        .append(startTime).append("' and ").append(PERIOD_END)
                        .append(" <= '").append(endTime).append("'");
            } else if (timeColumnNames.contains(DATE)
                    || timeColumnNames.contains(VALID_DTIME)) {
                // For tables with Date/Timestamp
                String columnName = timeColumnNames.contains(DATE) ? DATE
                        : VALID_DTIME;
                timeRangeConstraint.append(columnName).append(" between '")
                        .append(startTime).append("' and '").append(endTime)
                        .append("'");
            } else if (timeColumnNames.contains(DAY_OF_YEAR)) {
                // For tables with "mm-dd" String
                timeRangeConstraint.append(buildDayOfYearBetweenConstraint(
                        startTimeAsCalendar, endTimeAsCalendar));
            } else if (timeColumnNames.contains(MONTH_OF_YEAR)) {
                // For tables with month int
                timeRangeConstraint.append(
                        buildBetweenConstraint(MONTH_OF_YEAR, Calendar.MONTH,
                                startTimeAsCalendar, endTimeAsCalendar));
            }
        }
        return timeRangeConstraint.toString();
    }

    private static String buildBetweenConstraint(String columnName,
            int calendarField, Calendar startTime, Calendar endTime) {
        int startTimeValue = startTime.get(calendarField);
        int endTimeValue = endTime.get(calendarField);
        if (Calendar.MONTH == calendarField) {
            startTimeValue += 1;
            endTimeValue += 1;
        }

        StringBuilder betweenConstraint = new StringBuilder();
        betweenConstraint.append(columnName).append(" between ")
                .append(startTimeValue).append(" and ").append(endTimeValue);

        return betweenConstraint.toString();
    }

    /**
     * Helper function that builds the SQL WHERE clause for time range based
     * constraints where the climo database table use of year and day_of_year
     * columns to denote dates.
     * <p>
     * For example if we're trying to retrieve data for the dates between
     * 2009-03-01 and 2009-09-30, this function would return the string:
     * 
     * <pre>
     * (YEAR = '2009' AND DAY_OF_YEAR BETWEEN '03-01' AND '09-30')
     * </pre>
     * 
     * @param startCal
     * @param endCal
     * @return
     */
    private static String buildYearAndDayOfYearBetweenConstraint(
            Calendar startCal, Calendar endCal) {
        int startYear = startCal.get(Calendar.YEAR);
        String startDayOfYear = extractDayOfYearString(startCal);

        int endYear = endCal.get(Calendar.YEAR);
        String endDayOfYear = extractDayOfYearString(endCal);

        // We have 3 distinct situations that have to be handled by this query:
        // 1. Querying for dates when the start and end year are equal.
        // 2. Querying for dates when the start and end year are 1 year apart.
        // 3. Querying for dates when the start and end year are 2+ years apart.
        StringBuilder sb = new StringBuilder();
        sb.append('(');
        /*
         * Situation 1: We can build a simple query string with a static year
         * and a between constraint for the months (e.g.
         * "year = 2010 and day_of_year between '03-01' and '09-01'").
         */
        if (startYear == endYear) {
            sb.append(YEAR).append(" = ").append(startYear);
            sb.append(" and ").append(DAY_OF_YEAR).append(" between '")
                    .append(startDayOfYear).append("' and '")
                    .append(endDayOfYear).append('\'');
        } else {
            /*
             * Situation 2: Simple or statement where greater than or equal to
             * startCal and less than or equal to endCal (e.g. "(year = 2010 and
             * day_of_year >= '03-01') or (year = 2011 and day_of_year <=
             * '09-01')" ).
             */
            sb.append("(");
            sb.append(YEAR).append(" = ").append(startYear);
            sb.append(" and ").append(DAY_OF_YEAR).append(" >= '")
                    .append(startDayOfYear).append("'");
            sb.append(")");

            sb.append(" or ");

            sb.append("(");
            sb.append(YEAR).append(" = ").append(endYear);
            sb.append(" and ").append(DAY_OF_YEAR).append(" <= '")
                    .append(endDayOfYear).append("'");
            sb.append(")");

            /*
             * Situation 3: Adds a clause to situation 2's query to capture all
             * the full years between startCal and endCal (e.g. "(year = 2010
             * and day_of_year >= '03-01') or (year = 2015 and day_of_year <=
             * '09-01') or year between 2011 and 2014" ).
             */
            if (endYear - startYear > 1) {
                sb.append(" or ");
                if ((startYear + 1) == (endYear - 1)) {
                    sb.append(YEAR).append(" = ").append(startYear + 1);
                } else {
                    sb.append(YEAR).append(" between ").append(startYear + 1)
                            .append(" and ").append(endYear - 1);
                }
            }
        }
        sb.append(')');

        return sb.toString();
    }

    /**
     * Helper function that builds the SQL WHERE clause for time range based
     * constraints where the climo database table use of year and month columns
     * to denote dates.
     * <p>
     * For example if we're trying to retrieve data for the dates between
     * 2009-03-01 and 2009-09-30, this function would return the string:
     * 
     * <pre>
     * (YEAR = 2009 AND MONTH BETWEEN 3 AND 9)
     * </pre>
     * 
     * @param startCal
     * @param endCal
     * @return
     */
    private static String buildYearAndMonthBetweenConstraint(Calendar startCal,
            Calendar endCal) {
        int startYear = startCal.get(Calendar.YEAR);
        int endYear = endCal.get(Calendar.YEAR);
        int startMonth = startCal.get(Calendar.MONTH) + 1;
        int endMonth = endCal.get(Calendar.MONTH) + 1;

        // We have 3 distinct situations that have to be handled by this query:
        // 1. Querying for dates when the start and end year are equal.
        // 2. Querying for dates when the start and end year are 1 year apart.
        // 3. Querying for dates when the start and end year are 2+ years apart.
        StringBuilder sb = new StringBuilder();
        sb.append('(');
        /*
         * Situation 1: We can build a simple query string with a static year
         * and a between constraint for the months (e.g.
         * "year = 2010 and month between 3 and 9").
         */
        if (startYear == endYear) {
            sb.append(YEAR).append(" = ").append(startYear);
            sb.append(" and ").append(MONTH).append(" between ");
            sb.append(startMonth).append(" and ").append(endMonth);
        } else {
            /*
             * Situation 2: Simple or statement where greater than or equal to
             * startCal and less than or equal to endCal (e.g.
             * "(year = 2010 and month >= 3) or (year = 2011 and month <= 9)").
             */
            sb.append("(");
            sb.append(YEAR).append(" = ").append(startYear);
            sb.append(" and ").append(MONTH).append(" >= ").append(startMonth);
            sb.append(")");

            sb.append(" or ");

            sb.append("(");
            sb.append(YEAR).append(" = ").append(endYear);
            sb.append(" and ").append(MONTH).append(" <= ").append(endMonth);
            sb.append(")");

            /*
             * Situation 3: Adds a clause to situation 2's query to capture all
             * the full years between startCal and endCal (e.g. "(year = 2010
             * and month >= 3) or (year = 2015 and month <= 9) or year between
             * 2011 and 2014" ).
             */
            if (endYear - startYear > 1) {
                sb.append(" or ");
                if ((startYear + 1) == (endYear - 1)) {
                    sb.append(YEAR).append(" = ").append(startYear + 1);
                } else {
                    sb.append(YEAR).append(" between ").append(startYear + 1)
                            .append(" and ").append(endYear - 1);
                }
            }
        }
        sb.append(')');

        return sb.toString();
    }

    private static String buildDayOfYearBetweenConstraint(Calendar startTime,
            Calendar endTime) {
        StringBuilder dayOfYearBetweenConstraint = new StringBuilder();
        dayOfYearBetweenConstraint.append(DAY_OF_YEAR).append(" between '");
        dayOfYearBetweenConstraint.append(extractDayOfYearString(startTime))
                .append("' and '").append(extractDayOfYearString(endTime))
                .append('\'');
        return dayOfYearBetweenConstraint.toString();
    }

    private static class DataTimeWithDataIndex {

        public final DataTime dataTime;

        public final int dataIndex;

        public DataTimeWithDataIndex(DataTime dataTime, int dataIndex) {
            this.dataTime = dataTime;
            this.dataIndex = dataIndex;
        }
    }
}
