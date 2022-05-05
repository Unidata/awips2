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
package com.raytheon.viz.hydro.flashfloodguidance;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.catalog.DbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * Class for managing database query calls. FlashFloodGuidanceDataManager.java
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03Sept2008   #1507      dhladky     Initial Creation.
 * 12Oct2009    2256       mpduff      Added additional data query capability.
 * Apr 21, 2014 2060       njensen     Remove dependency on grid dataURI column
 * Nov 18, 2017 17911      wkwock      Move RFC_SITEMAP and RFCMAP to com.raytheon.uf.common.mpe.util.
 * Jun 13, 2018 #6643      dgilling    Rewrite getContingencyValue query to
 *                                     match A1.
 *
 * </pre>
 *
 * @author dhladky
 * @version 1.0
 */

public class FlashFloodGuidanceDataManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(FlashFloodGuidanceDataManager.class);

    /** Instance of this class */
    private static FlashFloodGuidanceDataManager instance = null;

    /**
     * Private constructor for Singleton instance.
     */
    private FlashFloodGuidanceDataManager() {

    }

    /**
     * Returns an instance of this class.
     *
     * @return An instance of this class
     */
    public static synchronized FlashFloodGuidanceDataManager getInstance() {
        if (instance == null) {
            instance = new FlashFloodGuidanceDataManager();
        }

        return instance;
    }

    /**
     * Queries IHFS for available gridded FFG data.
     *
     * @return ArrayList<Object[]> of data
     */
    public List<Object[]> getGriddedDataList() {
        List<Object[]> rs = null;

        /** Query to find available gridded data */
        DbQuery query = new DbQuery(GridRecord.class, "metadata");
        query.addColumn(GridConstants.DATASET_ID);
        query.addColumn(GridConstants.PARAMETER_ABBREVIATION);
        query.addColumn("dataTime.refTime");
        query.addConstraint(GridConstants.DATASET_ID, new RequestConstraint(
                "FFG%", ConstraintType.LIKE));
        query.addConstraint(GridConstants.SECONDARY_ID, "Version0");
        query.addOrderBy(GridConstants.DATASET_ID);
        query.addOrderBy(GridConstants.PARAMETER_ABBREVIATION);
        query.addOrderBy("dataTime.refTime");
        try {
            rs = query.performQuery();
        } catch (VizException e) {
            statusHandler.error(
                    "Data Query:" + " Error querying Metadata for FFG data.",
                    e);
        }

        return rs;
    }

    public GridRecord getGridRecord(String uri) {
        StringBuilder query = new StringBuilder();
        query.append("from "
                + com.raytheon.uf.common.dataplugin.grid.GridRecord.class
                        .getName());
        GridRecord gr = null;
        try {
            List<Object[]> results = DirectDbQuery.executeQuery(
                    query.toString(), "metadata", QueryLanguage.HQL);
            gr = (GridRecord) results.get(0)[0];
        } catch (VizException e) {
            statusHandler.error("FFG Query" + " Error querying for areal FFG",
                    e);
        }

        return gr;
    }

    public List<Object[]> getGeoArea(String where) {
        List<Object[]> rs = null;

        try {
            rs = DirectDbQuery.executeQuery(
                    "select area_id, interior_lat, interior_lon from geoArea "
                            + where, HydroConstants.IHFS, QueryLanguage.SQL);
        } catch (VizException e) {
            statusHandler.error("FFG Query" + " Error querying GeoArea table",
                    e);
        }

        return rs;
    }

    public List<Object[]> getContingencyValue(String areaId, int duration,
            Date refTime) {
        List<Object[]> rs = null;
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));

        String date = sdf.format(refTime);
        duration = duration / (1000 * 60 * 60);
        int dur = 1001;
        if (duration == 1) {
            dur = 1001;
        } else if (duration == 3) {
            dur = 1003;
        } else if (duration == 6) {
            dur = 1006;
        } else if (duration == 12) {
            dur = 1012;
        } else if (duration == 24) {
            dur = 2001;
        }

        String where = " where pe = 'PP' and ts = 'CF' and " + "validtime >= '"
                + date + "' and lid = '" + areaId + "' " + "and dur = " + dur
                + " order by validtime desc;";

        try {
            rs = DirectDbQuery.executeQuery(
                    "select lid, validtime, value from  " + "contingencyvalue "
                            + where, HydroConstants.IHFS, QueryLanguage.SQL);
        } catch (VizException e) {
            statusHandler.error(
                    "FFG Query" + " Error querying ContingencyValue table", e);
        }

        return rs;
    }

    public List<Object[]> getContingencyValue(String boundaryType) {
        List<Object[]> rs = null;

        StringBuilder sql = new StringBuilder(
                "SELECT DISTINCT validtime, dur FROM contingencyvalue")
                        .append(" WHERE pe='PP' AND ts='CF'")
                        .append(" AND lid IN ")
                        .append("(SELECT area_id FROM GeoArea where boundary_type='")
                        .append(boundaryType.toUpperCase()).append("')")
                        .append(" ORDER BY 1 DESC");

        try {
            rs = DirectDbQuery.executeQuery(sql.toString(), HydroConstants.IHFS,
                    QueryLanguage.SQL);
        } catch (VizException e) {
            statusHandler.error(
                    "FFG Query" + " Error querying ContingencyValue table", e);
        }

        return rs;
    }
}
