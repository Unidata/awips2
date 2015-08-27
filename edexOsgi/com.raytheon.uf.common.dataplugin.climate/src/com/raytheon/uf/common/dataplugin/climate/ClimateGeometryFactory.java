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
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataaccess.IDataRequest;
import com.raytheon.uf.common.dataaccess.exception.IncompatibleRequestException;
import com.raytheon.uf.common.dataaccess.geom.IGeometryData;
import com.raytheon.uf.common.dataaccess.impl.AbstractGeometryDatabaseFactory;
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
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * May 14, 2015  4409     mapeters    Initial creation.
 * May 21, 2015  4409     mapeters    Extract query results correctly in getTimeColumnNames()
 * Aug 05, 2015  4486     rjpeter     Changed Timestamp to Date.
 * Aug 21, 2015  4409     mapeters    Alias columns that are automatically retrieved when
 *                                    getting geometry data
 * </pre>
 * 
 * @author mapeters
 * @version 1.0
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

    private static final String CLI_STA_SETUP_LON = "longitude_e";

    private static final String LOCATION_COLUMN_ALIAS_PREFIX = "location_column_";

    private static final String TIME_COLUMN_ALIAS_PREFIX = "time_column_";

    private static final String GEOM_COLUMN_ALIAS_PREFIX = "geom_column_";

    public ClimateGeometryFactory() {
        super(HMDB_DATABASE, new String[] { TABLE },
                new String[] { COL_NAME_OPTION });
    }

    /**
     * Queries the table with the given tableName to determine which of the
     * given columnsToCheck it contains.
     * 
     * @param columnsToCheck
     *            the columns to check the existence of
     * @param request
     *            the IDataRequest being performed
     * @param tableName
     *            the name of the table to query
     * @return the names of the columns in columnsToCheck that the table has
     */
    private List<Object[]> executeGetColumnNames(String[] columnsToCheck,
            IDataRequest request, String tableName) {
        StringBuilder existenceQuery = new StringBuilder(
                "select column_name from information_schema.columns where column_name in (");
        boolean first = true;
        for (String columnName : columnsToCheck) {
            if (!first) {
                existenceQuery.append(", ");
            } else {
                first = false;
            }
            existenceQuery.append("'").append(columnName).append("'");
        }
        existenceQuery.append(") and table_name = '").append(tableName)
                .append("';");
        return this.executeQuery(existenceQuery.toString(), request);
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
        String tableName = ((String) (request.getIdentifiers().get(TABLE)))
                .split("\\.")[1];
        String[] timeColumns = new String[] { YEAR, DAY_OF_YEAR, MONTH,
                PERIOD_START, PERIOD_END, DATE, VALID_DTIME, MONTH_OF_YEAR };
        List<Object[]> results = this.executeGetColumnNames(timeColumns,
                request, tableName);
        if (!results.isEmpty()) {
            List<String> timeColumnNames = new ArrayList<>();
            for (Object[] timeColumn : results) {
                timeColumnNames.add((String) timeColumn[0]);
            }
            return timeColumnNames;
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
    private String getLocationColumnName(IDataRequest request, String tableName) {
        String[] locationColumns = new String[] { STATION_CODE, STATION_ID,
                INFORM_ID, ICAO_LOC_ID };
        List<Object[]> results = this.executeGetColumnNames(locationColumns,
                request, tableName);
        if (!results.isEmpty()) {
            return (String) results.get(0)[0];
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
        String tableName = ((String) (request.getIdentifiers().get(TABLE)))
                .split("\\.")[1];
        String locationColumnName = getLocationColumnName(request, tableName);
        StringBuilder sqlQuery = new StringBuilder("select distinct ");
        if (locationColumnName.equals(STATION_CODE)
                || locationColumnName.equals(ICAO_LOC_ID)) {
            sqlQuery.append(locationColumnName).append(" from ")
                    .append(tableName);
            if (locationColumnName.equals(STATION_CODE)) {
                sqlQuery.append(", ").append(RPT).append(" where ").append(RPT)
                        .append(".").append(ICAO_LOC_ID).append(" = ")
                        .append(tableName).append(".")
                        .append(locationColumnName);
            }
        } else {
            sqlQuery.append(CLI_STA_SETUP).append(".").append(STATION_CODE)
                    .append(" from ").append(CLI_STA_SETUP).append(", ")
                    .append(tableName).append(" where ").append(CLI_STA_SETUP)
                    .append(".").append(STATION_ID).append(" = ")
                    .append(tableName).append(".").append(locationColumnName);
        }
        sqlQuery.append(";");
        return sqlQuery.toString();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String assembleGetTimes(IDataRequest request, boolean refTimeOnly) {
        List<String> timeColumnNames = getTimeColumnNames(request);

        StringBuilder sqlQuery = new StringBuilder("select distinct ");
        boolean first = true;
        for (String timeColumnName : timeColumnNames) {
            if (!first) {
                sqlQuery.append(", ");
            } else {
                first = false;
            }
            sqlQuery.append(timeColumnName);
        }
        String tableName = ((String) (request.getIdentifiers().get(TABLE)))
                .split("\\.")[1];
        sqlQuery.append(" from ").append(tableName);

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
        } else if (dataTimeElement instanceof Integer) {
            int firstValue = (int) dataTimeElement;
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
        } else if (potentialDataTimeElement instanceof Integer) {
            int secondValue = (int) potentialDataTimeElement;
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
            // For tables with a year int (intElement) and a "mm-dd" String
            // (potentialDataTimeElement)
            dataTime = buildDataTimeFromDayOfYear(intElement,
                    (String) potentialDataTimeElement);
            index++;
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
    protected String assembleGetData(IDataRequest request, TimeRange timeRange) {
        List<String> timeColumnNames = getTimeColumnNames(request);
        return assembleGetData(request,
                buildTimeRangeConstraint(timeColumnNames, timeRange),
                timeColumnNames);
    }

    private String assembleGetData(IDataRequest request, String timeConstraint,
            List<String> timeColumnNames) {
        String tableName = ((String) (request.getIdentifiers().get(TABLE)))
                .split("\\.")[1];
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
            sqlQuery.append(", ").append(
                    buildAlias(tableName, timeColumnName,
                            TIME_COLUMN_ALIAS_PREFIX));
        }

        String[] params = request.getParameters();
        for (String param : params) {
            sqlQuery.append(", ").append(tableName).append(".").append(param);
        }

        // Build FROM statement
        sqlQuery.append(" from ").append(tableName);
        if (!isRpt) {
            sqlQuery.append(", ").append(hasStationCode ? RPT : CLI_STA_SETUP);
        }

        // Build constraints
        List<String> constraints = new ArrayList<>();
        if (!isRpt) {
            StringBuilder joinConstraint = new StringBuilder();
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
            constraints.add(joinConstraint.toString());
        }

        String locationNameConstraint = buildLocationNameConstraint(
                request.getLocationNames(), locationColumnName);
        if (!locationNameConstraint.trim().isEmpty()) {
            constraints.add(locationNameConstraint);
        }

        List<String> identifierConstraints = buildIdentifierConstraints(request
                .getIdentifiers());
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

    private static List<String> buildIdentifierConstraints(
            Map<String, Object> identifiers) {
        List<String> identifierConstraints = new ArrayList<String>();
        for (Map.Entry<String, Object> entry : identifiers.entrySet()) {
            String key = entry.getKey();
            if (key.equals(TABLE)) {
                continue;
            }
            String value = entry.getValue().toString();
            StringBuilder constraint = new StringBuilder(key).append(" = '")
                    .append(value).append("'");
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

    private static String buildDataTimesConstraint(
            List<String> timeColumnNames, DataTime... dataTimes) {
        StringBuilder dataTimesConstraint = new StringBuilder();
        if ((dataTimes != null) && (dataTimes.length > 0)) {
            if (timeColumnNames.contains(YEAR)) {
                dataTimesConstraint.append(buildInConstraint(YEAR,
                        Calendar.YEAR, dataTimes));
                if (timeColumnNames.contains(DAY_OF_YEAR)) {
                    // For tables with year int and "mm-dd" String
                    dataTimesConstraint.append(" and ").append(
                            buildDayOfYearInConstraint(dataTimes));
                } else if (timeColumnNames.contains(MONTH)) {
                    // For tables with year and month ints
                    dataTimesConstraint.append(" and ")
                            .append(buildInConstraint(MONTH, Calendar.MONTH,
                                    dataTimes));
                }
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
            } else if (timeColumnNames.contains(DAY_OF_YEAR)) {
                // For tables with "mm-dd" String
                dataTimesConstraint
                        .append(buildDayOfYearInConstraint(dataTimes));
            } else if (timeColumnNames.contains(MONTH_OF_YEAR)) {
                // For tables with month int
                dataTimesConstraint.append(buildInConstraint(MONTH_OF_YEAR,
                        Calendar.MONTH, dataTimes));
            }
        }
        return dataTimesConstraint.toString();
    }

    private static String buildInConstraint(String columnName,
            int calendarField, DataTime... dataTimes) {
        StringBuilder inConstraint = new StringBuilder();
        inConstraint.append(columnName).append(" in (");
        for (int i = 0; i < dataTimes.length; i++) {
            inConstraint.append(dataTimes[i].getRefTimeAsCalendar().get(
                    calendarField));
            if (i < (dataTimes.length - 1)) {
                inConstraint.append(",");
            }
        }
        inConstraint.append(")");

        return inConstraint.toString();
    }

    private static String buildDayOfYearInConstraint(DataTime... dataTimes) {
        StringBuilder dayOfYearInConstraint = new StringBuilder(DAY_OF_YEAR)
                .append(" in (");
        for (int i = 0; i < dataTimes.length; i++) {
            dayOfYearInConstraint.append("'");
            int month = dataTimes[i].getRefTimeAsCalendar().get(Calendar.MONTH) + 1;
            int day = dataTimes[i].getRefTimeAsCalendar().get(
                    Calendar.DAY_OF_MONTH);
            dayOfYearInConstraint
                    .append(String.format("%02d-%02d", month, day));
            dayOfYearInConstraint.append("'");
            if (i < (dataTimes.length - 1)) {
                dayOfYearInConstraint.append(",");
            }
        }
        dayOfYearInConstraint.append(")");
        return dayOfYearInConstraint.toString();
    }

    private static String buildTimeRangeConstraint(
            List<String> timeColumnNames, TimeRange timeRange) {
        StringBuilder timeRangeConstraint = new StringBuilder();
        if (timeRange != null) {
            Date startTime = timeRange.getStart();
            Calendar startTimeAsCalendar = TimeUtil.newGmtCalendar(startTime);
            Date endTime = timeRange.getEnd();
            Calendar endTimeAsCalendar = TimeUtil.newGmtCalendar(endTime);
            if (timeColumnNames.contains(YEAR)) {
                timeRangeConstraint.append(buildBetweenConstraint(YEAR,
                        Calendar.YEAR, startTimeAsCalendar, endTimeAsCalendar));
                if (timeColumnNames.contains(DAY_OF_YEAR)) {
                    // For tables with year int and "mm-dd" String
                    timeRangeConstraint.append(" and ").append(
                            buildDayOfYearBetweenConstraint(
                                    startTimeAsCalendar, endTimeAsCalendar));
                } else if (timeColumnNames.contains(MONTH)) {
                    // For tables with year and month ints
                    timeRangeConstraint.append(" and ").append(
                            buildBetweenConstraint(MONTH, Calendar.MONTH,
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
                timeRangeConstraint.append(buildBetweenConstraint(
                        MONTH_OF_YEAR, Calendar.MONTH, startTimeAsCalendar,
                        endTimeAsCalendar));
            }
        }
        return timeRangeConstraint.toString();
    }

    private static String buildBetweenConstraint(String columnName,
            int calendarField, Calendar startTime, Calendar endTime) {
        StringBuilder betweenConstraint = new StringBuilder();
        betweenConstraint.append(columnName).append(" between ")
                .append(startTime.get(calendarField)).append(" and ")
                .append(endTime.get(calendarField));

        return betweenConstraint.toString();
    }

    private static String buildDayOfYearBetweenConstraint(Calendar startTime,
            Calendar endTime) {
        StringBuilder dayOfYearBetweenConstraint = new StringBuilder();
        dayOfYearBetweenConstraint.append(DAY_OF_YEAR).append(" between '");
        int startMonth = startTime.get(Calendar.MONTH) + 1;
        int startDay = startTime.get(Calendar.DAY_OF_MONTH);
        dayOfYearBetweenConstraint.append(
                String.format("%02d-%02d", startMonth, startDay)).append(
                "' and '");
        int endMonth = endTime.get(Calendar.MONTH) + 1;
        int endDay = endTime.get(Calendar.DAY_OF_MONTH);
        dayOfYearBetweenConstraint.append(
                String.format("%02d-%02d", endMonth, endDay)).append("'");

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
