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

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataaccess.exception.IncompatibleRequestException;
import com.raytheon.uf.common.dataaccess.geom.IGeometryRequest;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Utilities for assembling a SQL query based on an IGeometryRequest
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 15, 2012            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class HydroQueryAssembler {

    protected static final String TABLE = "table";

    private static final String TIME_COL = "producttime";

    private static final String LID_COL = "lid";

    /**
     * Don't allow instantiation
     */
    private HydroQueryAssembler() {

    }

    /**
     * Assembles a SQL query for data
     * 
     * @param request
     *            the request for data
     * @param times
     *            the times of data to request
     * @return the SQL query
     */
    public static String assembleGetData(IGeometryRequest request,
            DataTime[] times) {
        return assembleGetData(request, buildTimeConstraint(times)).toString();
    }

    /**
     * Assembles a SQL query for data
     * 
     * @param request
     *            the request for data
     * @param timeRange
     *            the time range of data to request
     * @return the SQL query
     */
    public static String assembleGetData(IGeometryRequest request,
            TimeRange timeRange) {
        return assembleGetData(request, buildTimeConstraint(timeRange))
                .toString();
    }

    /**
     * Assembles a SQL string to query corresponding to the request.
     * 
     * @param request
     *            the request to transform to SQL
     * @param timeConstraint
     *            a time constraint to apply to the where clause, if applicable.
     *            May be null.
     * @return a SQL string that corresponds to the request
     */
    private static CharSequence assembleGetData(IGeometryRequest request,
            CharSequence timeConstraint) {
        StringBuilder sb = new StringBuilder();
        // this method assembles a sql string such as:
        // select d.lid, d.productttime, d.value, l.lat, l.lon from height d,
        // location l where d.lid = 'ABRN1' and d.lid = l.lid;
        // It is requesting the lat/lons of the location every time which
        // could potentially be improved in efficiency.

        // select
        sb.append(buildSelectParams(request));

        // from table name
        sb.append(buildFromWithLocation(request));

        // where
        CharSequence where = buildWhere(request, timeConstraint);
        if (where.length() > 0) {
            sb.append(where);
            sb.append(" and d.lid = l.lid");
        } else {
            sb.append("where d.lid = l.lid");
        }
        sb.append(buildOrderByTime());
        sb.append(";");

        // TODO do i need a safety check to limit it, like at 5000 rows or
        // something like that?
        return sb;
    }

    /**
     * Assembles a SQL query for available times that match the request
     * 
     * @param request
     *            the request to find available times for
     * @return the SQL query
     */
    public static String assembleGetTimes(IGeometryRequest request) {
        StringBuilder sb = new StringBuilder();

        // select
        sb.append("select distinct ");
        sb.append(TIME_COL);

        // from
        sb.append(buildFrom(request));

        // where
        sb.append(buildWhere(request, null));
        sb.append(buildOrderByTime());
        sb.append(";");

        // TODO do i need a safety check to limit it, like at 5000 rows or
        // something like that?

        return sb.toString();
    }

    /**
     * Assembles a select statement of a query, such as
     * "select d.lid, d.producttime, l.lat, l.lon, d.value"
     * 
     * @param request
     *            the request to form a select statement on
     * @return the select statement
     */
    private static CharSequence buildSelectParams(IGeometryRequest request) {
        StringBuilder sb = new StringBuilder();
        // always want the location name and time even if they didn't request it
        // so that returned objects will have that information
        sb.append("select d.");
        sb.append(LID_COL);
        sb.append(", d.");
        sb.append(TIME_COL);
        // request lat and lon for the returned geometry objects
        sb.append(", l.lat, l.lon");

        // request other columns
        for (String param : request.getParameters()) {
            sb.append(", d.");
            sb.append(param);
        }

        return sb;
    }

    /**
     * Assembles a from statement, such as "from height"
     * 
     * @param request
     *            the request to determine the tablename from
     * @return the from statement
     */
    private static CharSequence buildFrom(IGeometryRequest request) {
        return " from " + request.getIdentifiers().get(TABLE);
    }

    /**
     * Assembles a from statement with a location added, such as "from height,
     * location l"
     * 
     * @param request
     *            the request to determine the tablename from
     * @return the from statement
     */
    private static CharSequence buildFromWithLocation(IGeometryRequest request) {
        StringBuilder sb = new StringBuilder();
        sb.append(buildFrom(request));
        sb.append(" d, location l");
        return sb;
    }

    /**
     * Assembles a SQL where clause based on the request
     * 
     * @param request
     *            the request
     * @param timeConstraint
     *            the time constraint String as produced by
     *            buildTimeConstraint(), or null
     * @return the where clause
     */
    private static CharSequence buildWhere(IGeometryRequest request,
            CharSequence timeConstraint) {
        StringBuilder sb = new StringBuilder();
        CharSequence locationConstraint = buildLocationConstraint(request
                .getLocationNames());
        CharSequence extraConstraints = buildIdentifierConstraint(request
                .getIdentifiers());
        if (locationConstraint != null || extraConstraints != null
                || timeConstraint != null) {
            sb.append(" where ");
            if (locationConstraint != null) {
                sb.append(locationConstraint);
                if (extraConstraints != null || timeConstraint != null) {
                    sb.append(" and ");
                }
            }
            if (extraConstraints != null) {
                sb.append(extraConstraints);
                if (timeConstraint != null) {
                    sb.append(" and ");
                }
            }
            if (timeConstraint != null) {
                sb.append(timeConstraint);
            }
        }

        return sb;
    }

    /**
     * Assembles a SQL string that can be used as part of a where clause to
     * limit the locations returned.
     * 
     * @param locationNames
     *            the names of hydro gages to limit on
     * @return a SQL string based on lid, such as "lid = 'ABRN1'" or "lid in
     *         ('ARBN1','ARBN2')"
     */
    private static CharSequence buildLocationConstraint(String[] locationNames) {
        StringBuilder sb = null;
        if (locationNames != null && locationNames.length > 0) {
            sb = new StringBuilder();
            sb.append(LID_COL);
            if (locationNames.length == 1) {
                sb.append(" = '");
                sb.append(locationNames[0]);
                sb.append("'");
            } else {
                sb.append(" in (");
                for (int i = 0; i < locationNames.length; i++) {
                    sb.append("'");
                    sb.append(locationNames[i]);
                    sb.append("'");
                    if (i < locationNames.length - 1) {
                        sb.append(",");
                    }
                }
                sb.append(")");
            }
        }
        return sb;
    }

    /**
     * Assembles a SQL string that can be used as part of a where clause to
     * limit the data returned.
     * 
     * @param identifiers
     *            the constraints to use
     * @return a SQL string based on the map, such as "ts = 'RG'"
     */
    private static CharSequence buildIdentifierConstraint(
            Map<String, Object> identifiers) {
        StringBuilder sb = new StringBuilder();
        Map<String, Object> copy = new HashMap<String, Object>(identifiers);
        copy.remove(TABLE);
        Iterator<Map.Entry<String, Object>> itr = copy.entrySet().iterator();
        while (itr.hasNext()) {
            Map.Entry<String, Object> entry = itr.next();
            String key = entry.getKey();
            sb.append(entry.getKey());
            Object value = entry.getValue();
            if (value instanceof Number) {
                sb.append(" = ");
                sb.append(value);
            } else if (value instanceof String) {
                sb.append(" = ");
                sb.append("'");
                sb.append(value);
                sb.append("'");
            } else if (value instanceof List) {
                sb.append(" in (");
                List<?> list = (List<?>) value;
                Iterator<?> itrList = list.iterator();
                while (itrList.hasNext()) {
                    sb.append("'");
                    sb.append(itrList.next().toString());
                    sb.append("'");
                    if (itrList.hasNext()) {
                        sb.append(",");
                    }
                }
                sb.append(")");
            } else {
                throw new IncompatibleRequestException(
                        "Unable to handle identifier " + key + " of type "
                                + value);
            }

            if (itr.hasNext()) {
                sb.append(" and ");
            }
        }

        return ((sb.length() > 0) ? sb : null);
    }

    /**
     * Assembles a SQL string that can be used as part of a where clause to
     * limit the data to between the start and end of a time range.
     * 
     * @param timeRange
     *            the time range to use to make the string
     * @return a SQL between statement corresponding to the time range, such as
     *         "between '2012-09-29 09:00:00' and '2012-09-29 12:00:00'"
     * 
     */
    private static CharSequence buildTimeConstraint(TimeRange timeRange) {
        String result = null;
        if (timeRange != null) {
            result = TIME_COL + " between '"
                    + TimeUtil.formatToSqlTimestamp(timeRange.getStart())
                    + "' and '"
                    + TimeUtil.formatToSqlTimestamp(timeRange.getEnd()) + "'";
        }
        return result;
    }

    /**
     * Assembles a SQL string that can be used as part of a where clause to
     * limit the data to a set of specific times.
     * 
     * @param dataTimes
     *            the time to limit it to.
     * @return a SQL in statement corresponding to the times as strings
     */
    private static CharSequence buildTimeConstraint(DataTime[] dataTimes) {
        StringBuilder sb = new StringBuilder();
        if (dataTimes != null && dataTimes.length > 0) {
            sb.append(TIME_COL);
            sb.append(" in (");
            for (int i = 0; i < dataTimes.length; i++) {
                sb.append("'");
                sb.append(TimeUtil.formatToSqlTimestamp(dataTimes[i]
                        .getRefTime()));
                sb.append("'");
                if (i < dataTimes.length - 1) {
                    sb.append(",");
                }
            }
            sb.append(")");
        }
        return ((sb.length() > 0) ? sb : null);
    }

    /**
     * Assembles a SQL string to order results by time, such as
     * "order by producttime"
     * 
     * @return a SQL statement that orders results by time
     */
    private static CharSequence buildOrderByTime() {
        StringBuilder sb = new StringBuilder();
        sb.append(" order by ");
        sb.append(TIME_COL);
        sb.append(" asc");
        return sb;
    }

}
