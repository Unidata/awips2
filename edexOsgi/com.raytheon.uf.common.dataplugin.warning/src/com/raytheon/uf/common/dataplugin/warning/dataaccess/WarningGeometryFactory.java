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
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Pattern;

import org.apache.commons.lang3.StringUtils;

import com.raytheon.uf.common.dataaccess.IDataRequest;
import com.raytheon.uf.common.dataaccess.exception.DataRetrievalException;
import com.raytheon.uf.common.dataaccess.exception.IncompatibleRequestException;
import com.raytheon.uf.common.dataaccess.geom.IGeometryData;
import com.raytheon.uf.common.dataaccess.impl.AbstractGeometryDatabaseFactory;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
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
 * Apr 11, 2016  5549     tgurney     Allow filter on office ID
 * Apr 26, 2016  5587     tgurney     Support getIdentifierValues()
 * May 24, 2016  5657     tgurney     Clean up identifier and parameter lists
 * Jun 01, 2016  5587     tgurney     Make getRequiredIdentifiers() return
 *                                    empty (not null)
 * Jun 08, 2016  5574     tgurney     Add advanced query support
 * Nov 08, 2016  5985     tgurney     Add constraints to available times query
 * </pre>
 *
 * @author mapeters
 */

public class WarningGeometryFactory extends AbstractGeometryDatabaseFactory {

    private static final String REF_TIME = "reftime";

    private static final String FCST_TIME = "forecasttime";

    private static final String UTIL_FLAGS = "utilityflags";

    private static final String RANGE_END = "rangeend";

    private static final String RANGE_START = "rangestart";

    private static final String GEOM = "geometry";

    private static final String OFFICE_ID = "officeid";

    private static final Pattern OFFICE_ID_PATTERN = Pattern
            .compile("[A-Za-z]{3,4}");

    private static final String[] NOT_PARAMETERS = new String[] { FCST_TIME,
            REF_TIME, UTIL_FLAGS, RANGE_END, RANGE_START, GEOM, OFFICE_ID,
            "datauri" };

    private static final String[] OPTIONAL_IDS = new String[] { "act", "etn",
            "phen", "locationid", OFFICE_ID, "phen", "phensig", "pil", "sig",
            "ugczones", "xxxid" };

    public WarningGeometryFactory() {
        super("metadata", new String[0], OPTIONAL_IDS);
    }

    @Override
    public String[] getAvailableParameters(IDataRequest request) {
        String[] columnsArray = super.getAvailableParameters(request);
        ArrayList<String> columnsList = new ArrayList<>(columnsArray.length);
        columnsList.addAll(Arrays.asList(columnsArray));
        for (String column : NOT_PARAMETERS) {
            columnsList.remove(column);
        }
        return columnsList.toArray(new String[0]);
    }

    @Override
    protected String assembleGetAvailableParameters(IDataRequest request) {
        return "select column_name from information_schema.columns where table_name = '"
                + request.getDatatype() + "';";
    }

    @Override
    protected String assembleGetTimes(IDataRequest request,
            boolean refTimeOnly) {
        StringBuilder sb = new StringBuilder();
        sb.append("select distinct ").append(REF_TIME);
        if (!refTimeOnly) {
            for (String column : new String[] { FCST_TIME, RANGE_START,
                    RANGE_END, UTIL_FLAGS }) {
                sb.append(", ").append(column);
            }
        }
        sb.append(" from ").append(request.getDatatype());

        Map<String, Object> identifiers = request.getIdentifiers();
        List<String> whereClauseParts = new ArrayList<>();
        whereClauseParts.add("geometry is not null");
        if (identifiers != null) {
            for (Entry<String, Object> entry : identifiers.entrySet()) {
                Object value = entry.getValue();
                String sqlConstraint = entry.getKey() + " ";
                if (value instanceof RequestConstraint) {
                    sqlConstraint += ((RequestConstraint) value).toSqlString();
                } else {
                    sqlConstraint += new RequestConstraint(value.toString())
                            .toSqlString();
                }
                whereClauseParts.add(sqlConstraint);
            }
            if (!whereClauseParts.isEmpty()) {
                sb.append(" where ")
                        .append(StringUtils.join(whereClauseParts, " and "));
            }
        }
        return sb.append(";").toString();
    }

    @Override
    protected String assembleGetData(IDataRequest request,
            DataTime... dataTimes) {
        return assembleGetData(request, buildDataTimesConstraint(dataTimes));
    }

    @Override
    protected String assembleGetData(IDataRequest request,
            TimeRange timeRange) {
        return assembleGetData(request, buildTimeRangeConstraint(timeRange));
    }

    private String assembleGetData(IDataRequest request,
            String timeConstraint) {
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

            Object value = entry.getValue();
            if (value instanceof RequestConstraint) {
                sqlQuery.append(keyWord).append(key).append(" ")
                        .append(((RequestConstraint) value).toSqlString());
            } else {
                sqlQuery.append(keyWord).append(key).append(" = '")
                        .append(value.toString()).append("'");
            }
            keyWord = " and ";
        }
        if (!timeConstraint.trim().isEmpty()) {
            sqlQuery.append(keyWord).append(timeConstraint);
        }
        String[] locationNames = request.getLocationNames();
        if (locationNames != null && locationNames.length > 0) {
            sqlQuery.append(keyWord).append(WarningGeometryFactory
                    .buildLocationConstraint(locationNames));
        }
        sqlQuery.append(";");

        return sqlQuery.toString();
    }

    private static String buildLocationConstraint(String[] locationNames) {
        StringBuilder locationConstraint = new StringBuilder();
        if (locationNames != null && locationNames.length > 0) {
            locationConstraint.append(OFFICE_ID).append(" in (");
            boolean first = true;
            for (String locationName : locationNames) {
                if (!OFFICE_ID_PATTERN.matcher(locationName).matches()) {
                    throw new IncompatibleRequestException(
                            "Office ID \"" + locationName + "\" is invalid");
                }
                String locNameQuoted = "'" + locationName + "'";
                if (first) {
                    locationConstraint.append(locNameQuoted);
                    first = false;
                } else {
                    locationConstraint.append(",").append(locNameQuoted);
                }
            }
            locationConstraint.append(")");
        }
        return locationConstraint.toString();
    }

    private static String buildDataTimesConstraint(DataTime... dataTimes) {
        StringBuilder dataTimesConstraint = new StringBuilder();
        if (dataTimes != null && dataTimes.length > 0) {
            dataTimesConstraint.append(REF_TIME).append(" in (");
            for (int i = 0; i < dataTimes.length; i++) {
                dataTimesConstraint.append("'");
                dataTimesConstraint.append(TimeUtil
                        .formatToSqlTimestamp(dataTimes[i].getRefTime()));
                dataTimesConstraint.append("'");
                if (i < dataTimes.length - 1) {
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
            timeRangeConstraint.append(REF_TIME).append(" between '")
                    .append(TimeUtil.formatToSqlTimestamp(timeRange.getStart()))
                    .append("' and '")
                    .append(TimeUtil.formatToSqlTimestamp(timeRange.getEnd()))
                    .append("'");
        }
        return timeRangeConstraint.toString();
    }

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

    @Override
    protected IGeometryData makeGeometry(Object[] data, String[] paramNames,
            Map<String, Object> attrs) {
        DataTime dataTime = buildDataTimeFromQueryResults(data);

        Geometry geometry = null;
        byte[] geomBytes = (byte[]) data[5];
        if (geomBytes != null && geomBytes.length > 0) {
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

    @Override
    protected String assembleGetIdentifierValues(IDataRequest request,
            String identifierKey) {
        return assembleGetColumnValues(request.getDatatype(), identifierKey);
    }
}
