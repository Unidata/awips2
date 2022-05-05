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
package com.raytheon.viz.hydro.pointprecipitation;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.OptionalInt;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.apache.commons.lang3.StringUtils;

import com.raytheon.uf.common.dataplugin.shef.tables.Rawpc;
import com.raytheon.uf.common.dataplugin.shef.tables.RawpcId;
import com.raytheon.uf.common.dataplugin.shef.tables.Rawpp;
import com.raytheon.uf.common.dataplugin.shef.tables.RawppId;
import com.raytheon.uf.common.dataplugin.shef.tables.Shefdur;
import com.raytheon.uf.common.dataplugin.shef.util.ShefConstants;
import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydro.pointprecipitation.PointPrecipConstants.RawPrecipTable;
import com.raytheon.viz.hydrocommon.datamanager.HydroDBDataManager;
import com.raytheon.viz.hydrocommon.datamanager.HydroDataManager;
import com.raytheon.viz.hydrocommon.util.QualityCodeUtil;

/**
 * Class for managing database query calls. PointPrecipDataManager.java
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03Sept2008   #1509      dhladky     Initial Creation.
 * 31Aug2009    #2257      mpduff      Adding data access methods.
 * sep292010    #4384      lbousaidi   Fixed quality_code and change name of table
 *                                     that retrieves PC data from rawpp to rawpc
 * Jul 21, 2015 4500       rjpeter     Use Number in blind cast.
 * Sep 21, 2018 7379       mduff       Support PDC Refactor.
 * Jan 31, 2019 6951       dgilling    Rewrite as module of static functions.
 * </pre>
 *
 * @author dhladky
 */

public class PointPrecipDataManager extends HydroDataManager {

    private static IUFStatusHandler statusHandler = UFStatus
            .getHandler(PointPrecipDataManager.class);

    private static final DateTimeFormatter DB_DATE_TIME_FORMATTER = DateTimeFormatter
            .ofPattern(ShefConstants.POSTGRES_DATE_STRING);

    private static OptionalInt adjustedStarttimeHours = OptionalInt.empty();

    private static String where;

    /**
     * Query 'locclass' view in ihfs database for selected 'hsa' field using
     * SQL.
     *
     * @throws VizException
     */
    public static String[] queryHsa() throws VizException {
        String query = "select distinct hsa from locclass";
        QueryResult result = HydroDBDataManager.getInstance()
                .runMappedQuery(query);

        String[] items = Arrays.stream(result.getRows())
                .map((r) -> r.getColumn(0).toString()).toArray(String[]::new);
        return items;
    }

    /**
     * Query 'ingestfilter' table in ihfs database for selected fields using
     * SQL.
     *
     * @throws VizException
     */
    public static String[] queryIngestfilterPc() throws VizException {
        String query = "select distinct ing.ts from ingestfilter ing where pe "
                + "= 'PC' and ing.ts like 'R%' order by 1";
        QueryResult result = HydroDBDataManager.getInstance()
                .runMappedQuery(query);

        String[] items = Arrays.stream(result.getRows())
                .map((r) -> r.getColumn(0).toString()).toArray(String[]::new);
        return items;
    }

    /**
     * Query 'ingestfilter' table in ihfs database for selected fields using
     * SQL.
     *
     * @return
     * @throws VizException
     */
    public static String[] queryIngestfilterPp() throws VizException {
        String query = "select distinct ing.ts from ingestfilter ing where pe = 'PP' and ing.ts like 'R%' order by 1";
        QueryResult result = HydroDBDataManager.getInstance()
                .runMappedQuery(query);

        String[] items = Arrays.stream(result.getRows())
                .map((r) -> r.getColumn(0).toString()).toArray(String[]::new);
        return items;
    }

    public static Collection<Rawpc> loadPCRaw(LocalDateTime queryBeginTime,
            LocalDateTime queryEndTime, String lid, List<String> ts,
            RawPrecipTable table) {
        /*
         * In order to consider cases that there is no PC data during dry
         * periods, retrieve more wider data for gages, use token
         * adjust_PC_startingtime to specify the PC data retrieval starting
         * point
         */
        if (!adjustedStarttimeHours.isPresent()) {
            int newValue = AppsDefaults.getInstance().getInt(
                    "adjust_PC_startingtime",
                    PointPrecipConstants.DEFAULT_ADJUSTED_STARTTIME_HRS);
            if (newValue <= 0) {
                newValue = PointPrecipConstants.DEFAULT_ADJUSTED_STARTTIME_HRS;
            }

            adjustedStarttimeHours = OptionalInt.of(newValue);
        }

        queryBeginTime = queryBeginTime
                .minusHours(adjustedStarttimeHours.getAsInt());

        /*
         * Convert the query begin and end times to INFORMIX ANSI year to
         * seconds strings (YYYY-MM-DD hh:mm:ss).
         */
        String beginStr = DB_DATE_TIME_FORMATTER.format(queryBeginTime);
        String endStr = DB_DATE_TIME_FORMATTER.format(queryEndTime);

        String tsClause = buildTsClause(ts);

        StringBuilder whereClause = new StringBuilder();
        if ((lid != null) && (!tsClause.isEmpty())) {
            whereClause.append(
                    String.format(" where lid = '%s' and %s ", lid,
                            tsClause));
            whereClause.append(" AND value != '-9999.0' AND ");
            whereClause
                    .append(String.format(" obstime >= '%s' AND ", beginStr));
            whereClause.append(String.format(" obstime <= '%s' ", endStr));
            whereClause.append("AND value IS NOT NULL AND ");
            whereClause.append(
                    " shef_qual_code NOT LIKE 'B%%' AND shef_qual_code ");
            whereClause.append(" NOT LIKE 'R%%' ORDER BY obstime DESC ");
        } else if (!tsClause.isEmpty()) {
            whereClause.append(String.format(" WHERE %s ", tsClause));
            whereClause.append(" AND value != '-9999.0' AND ");
            whereClause.append(String.format("obstime >= '%s' AND ", beginStr));
            whereClause.append(String.format(" obstime <= '%s' ", endStr));
            whereClause.append("AND value IS NOT NULL AND ");
            whereClause.append(
                    " shef_qual_code NOT LIKE 'B%%' AND shef_qual_code ");
            whereClause
                    .append(" NOT LIKE 'R%%' ORDER BY lid ASC, obstime DESC");
        } else if (lid != null) {
            whereClause.append(String.format(" WHERE lid = '%s' ", lid));
            whereClause.append(" AND value != '-9999.0' AND ");
            whereClause.append(String.format("obstime >= '%s' AND ", beginStr));
            whereClause.append(String.format(" obstime <= '%s' ", endStr));
            whereClause.append("AND value IS NOT NULL AND ");
            whereClause.append(
                    " shef_qual_code NOT LIKE 'B%%' AND shef_qual_code ");
            whereClause.append(" NOT LIKE 'R%%' ORDER BY ts ASC, obstime DESC");
        } else {
            whereClause.append(" WHERE value != '-9999.0' AND ");
            whereClause.append(String.format("obstime >= '%s' AND ", beginStr));
            whereClause.append(String.format(" obstime <= '%s' ", endStr));
            whereClause.append("AND value IS NOT NULL AND ");
            whereClause.append(
                    " shef_qual_code NOT LIKE 'B%%' AND shef_qual_code ");
            whereClause.append(
                    " NOT LIKE 'R%%' ORDER BY lid ASC, ts ASC, obstime DESC");
        }

        where = whereClause.toString();

        StringBuilder query = new StringBuilder(
                "SELECT lid, pe, dur, ts, extremum, obstime, value, shef_qual_code, quality_code, revision, product_id, producttime, postingtime FROM ");
        /* get the data */
        if (table == RawPrecipTable.CurRawPrecip) {
            query.append("CurPC");
        } else {
            query.append("RawPC");
        }
        query.append(' ').append(whereClause.toString());

        try {
            QueryResult pcData = HydroDBDataManager.getInstance()
                    .runMappedQuery(query.toString());

            if ((pcData != null) && (pcData.getResultCount() > 0)) {
                List<Rawpc> retVal = IntStream.range(0, pcData.getResultCount())
                        .mapToObj((i) -> {
                            RawpcId id = new RawpcId(
                                    pcData.getRowColumnValue(i, "lid")
                                            .toString(),
                                    pcData.getRowColumnValue(i, "ts")
                                            .toString(),
                                    pcData.getRowColumnValue(i, "extremum")
                                            .toString(),
                                    (Date) pcData.getRowColumnValue(i,
                                            "obstime"));
                            Rawpc rawpc = new Rawpc(id,
                                    pcData.getRowColumnValue(i, "pe")
                                            .toString(),
                                    ((Number) pcData.getRowColumnValue(i,
                                            "dur")).shortValue(),
                                    ((Number) pcData.getRowColumnValue(i,
                                            "value")).doubleValue(),
                                    pcData.getRowColumnValue(i,
                                            "shef_qual_code").toString(),
                                    ((Number) pcData.getRowColumnValue(i,
                                            "quality_code")).intValue(),
                                    ((Number) pcData.getRowColumnValue(i,
                                            "revision")).shortValue(),
                                    pcData.getRowColumnValue(i, "product_id")
                                            .toString(),
                                    (Date) pcData.getRowColumnValue(i,
                                            "producttime"),
                                    (Date) pcData.getRowColumnValue(i,
                                            "postingtime"));
                            return rawpc;
                        }).collect(Collectors.toList());

                return retVal;
            }

        } catch (VizException e) {
            statusHandler.error(
                    "Failed to retrieve RawPC data. Query: " + query.toString(),
                    e);
        }

        return Collections.emptyList();
    }

    public static Collection<Rawpp> loadPPRaw(LocalDateTime queryBeginTime,
            LocalDateTime queryEndTime, String lid, List<String> ts,
            RawPrecipTable table) {
        String beginStr = DB_DATE_TIME_FORMATTER.format(queryBeginTime);
        String endStr = DB_DATE_TIME_FORMATTER.format(queryEndTime);

        /* Only retrieve valid PP data. */
        String qcWhere = QualityCodeUtil
                .buildQcWhere(PointPrecipConstants.QC_NOT_FAILED);

        String tsClause = buildTsClause(ts);

        StringBuilder whereClause = new StringBuilder();
        if ((lid != null) && (!tsClause.isEmpty())) {
            whereClause.append(String.format(" where lid = '%s' and %s ",
                    lid, tsClause));
            whereClause.append(" AND value != '-9999.0' AND ");
            whereClause
                    .append(String.format(" obstime >= '%s' AND ", beginStr));
            whereClause.append(String.format(" obstime <= '%s' AND ", endStr));
            whereClause.append(qcWhere);
            whereClause.append(" order by dur desc, obstime desc");
        } else if (!tsClause.isEmpty()) {
            whereClause.append(String.format(" WHERE %s ", tsClause));
            whereClause.append(" AND value != '-9999.0' AND ");
            whereClause.append(String.format("obstime >= '%s' AND ", beginStr));
            whereClause.append(String.format(" obstime <= '%s' AND ", endStr));
            whereClause.append(qcWhere);
            whereClause.append(" order by lid asc,");
            whereClause.append(" dur DESC, obstime DESC");
        } else if (lid != null) {
            whereClause.append(String.format(" WHERE lid = '%s' ", lid));
            whereClause.append(" AND value != '-9999.0' AND ");
            whereClause.append(String.format("obstime >= '%s' AND ", beginStr));
            whereClause.append(
                    String.format(" obstime <= '%s' AND %s ", endStr, qcWhere));
            whereClause.append(" ORDER BY ts ASC, DUR DESC, obstime DESC");
        } else {
            whereClause.append(" WHERE value != '-9999.0' AND ");
            whereClause.append(String.format("obstime >= '%s' AND ", beginStr));
            whereClause.append(
                    String.format("obstime <= '%s' AND %s ", endStr, qcWhere));
            whereClause.append(
                    " ORDER BY lid ASC, ts ASC, dur DESC, obstime DESC");
        }

        where = whereClause.toString();

        StringBuilder query = new StringBuilder(
                "SELECT lid, pe, dur, ts, extremum, obstime, value, shef_qual_code, quality_code, revision, product_id, producttime, postingtime FROM ");
        /* get the data */
        if (table == RawPrecipTable.CurRawPrecip) {
            query.append("CurPP");
        } else {
            query.append("RawPP");
        }
        query.append(' ').append(whereClause.toString());

        try {
            QueryResult ppData = HydroDBDataManager.getInstance()
                    .runMappedQuery(query.toString());

            if ((ppData != null) && (ppData.getResultCount() > 0)) {
                List<Rawpp> retVal = IntStream.range(0, ppData.getResultCount())
                        .mapToObj((i) -> {
                            RawppId id = new RawppId(
                                    ppData.getRowColumnValue(i, "lid")
                                            .toString(),
                                    ((Number) ppData.getRowColumnValue(i,
                                            "dur")).shortValue(),
                                    ppData.getRowColumnValue(i, "ts")
                                            .toString(),
                                    ppData.getRowColumnValue(i, "extremum")
                                            .toString(),
                                    (Date) ppData.getRowColumnValue(i,
                                            "obstime"));
                            Rawpp rawpp = new Rawpp(id,
                                    ppData.getRowColumnValue(i, "pe")
                                            .toString(),
                                    ((Number) ppData.getRowColumnValue(i,
                                            "value")).doubleValue(),
                                    ppData.getRowColumnValue(i,
                                            "shef_qual_code").toString(),
                                    ((Number) ppData.getRowColumnValue(i,
                                            "quality_code")).intValue(),
                                    ((Number) ppData.getRowColumnValue(i,
                                            "revision")).shortValue(),
                                    ppData.getRowColumnValue(i, "product_id")
                                            .toString(),
                                    (Date) ppData.getRowColumnValue(i,
                                            "producttime"),
                                    (Date) ppData.getRowColumnValue(i,
                                            "postingtime"));
                            return rawpp;
                        }).collect(Collectors.toList());

                return retVal;
            }

        } catch (VizException e) {
            statusHandler.error(
                    "Failed to retrieve RawPP data. Query: " + query.toString(),
                    e);
        }

        return Collections.emptyList();
    }

    public static String buildTsClause(List<String> ts) {
        if ((ts == null) || ts.isEmpty()) {
            return StringUtils.EMPTY;
        }
        StringBuilder tsClause = new StringBuilder("ts ");

        if (ts.get(0).startsWith("!")) {
            tsClause.append("not in ('");
            tsClause.append(ts.get(0).substring(1));
        } else {
            tsClause.append("in ('");
            tsClause.append(ts.get(0));
        }

        for (int i = 1; i < ts.size(); i++) {
            tsClause.append("', '");
            tsClause.append(ts.get(i));
        }
        tsClause.append("')");

        return tsClause.toString();
    }

    public static List<Shefdur> getShefDur(final String where)
            throws VizException {
        String selectStatement = "SELECT dur, durcode, name FROM ShefDur ";
        StringBuilder query = new StringBuilder(selectStatement);
        if (StringUtils.isNotEmpty(where)) {
            query.append(where);
        }

        QueryResult data = HydroDBDataManager.getInstance()
                .runMappedQuery(query.toString());

        if (data != null) {
            List<Shefdur> rval = new ArrayList<>();
            for (QueryResultRow entry : data.getRows()) {
                short dur = ((Number) entry
                        .getColumn(data.getColumnNames().get("dur")))
                                .shortValue();
                Object code = entry
                        .getColumn(data.getColumnNames().get("durcode"));
                String durCode = (code != null) ? code.toString()
                        : StringUtils.EMPTY;
                Object name = entry
                        .getColumn(data.getColumnNames().get("name"));
                String nameString = (name != null) ? name.toString()
                        : StringUtils.EMPTY;
                rval.add(new Shefdur(dur, durCode, nameString,
                        Collections.emptySet(), Collections.emptySet(),
                        Collections.emptySet(), Collections.emptySet()));
            }

            return rval;
        }

        return Collections.emptyList();
    }

    public static String getPCPPQuery() {
        return where;
    }
}
