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
package com.raytheon.uf.edex.plugin.hpe.util;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;

import com.raytheon.edex.site.SiteUtil;
import com.raytheon.uf.common.plugin.hpe.data.BiasDynRecord;
import com.raytheon.uf.common.plugin.hpe.data.HpeRadarResult;
import com.raytheon.uf.common.plugin.hpe.data.HpeEnums.HpeDataSource;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.plugin.mpe.dao.impl.HpeRadarResultDao;

/**
 * HPE database access utility class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 26, 2014    3026    mpduff      Initial creation
 * Nov 12, 2014    3026    mpduff      Fix handling of query results and query by current hour
 * Oct 12, 2016    5631    bkowal      Use {@link HpeRadarResultDao}.
 * 
 * </pre>
 * 
 * @author mpduff
 */

public class HpeDataAccessor {
    private static final String IHFS = "ihfs";

    private final HpeRadarResultDao hpeRadarResultDao = new HpeRadarResultDao();

    /** The data access object */
    private final CoreDao dao;

    /**
     * Constructor.
     */
    public HpeDataAccessor() {
        dao = new CoreDao(DaoConfig.forDatabase(IHFS));
    }

    /**
     * Get a map of radar id->BiasDynRecords
     * 
     * @param recdate
     * @param productName
     * @return BiasDynRecords
     * @throws Exception
     */
    public SortedMap<String, List<BiasDynRecord>> getBiasDynRecords(
            Date recdate, String productName) throws Exception {
        SortedMap<String, List<BiasDynRecord>> dataMap = new TreeMap<>();

        /*
         * Bias data are by the hour. Get the current hour to query on
         */
        long ms = recdate.getTime();
        Calendar currentHour = TimeUtil.newGmtCalendar();
        currentHour.setTimeInMillis(ms);
        currentHour.set(Calendar.MINUTE, 0);
        currentHour.set(Calendar.SECOND, 0);
        currentHour.set(Calendar.MILLISECOND, 0);

        HpeRadarResult hpeResult = getHpeRadarResult(recdate, productName);
        HpeDataSource source = hpeResult.getRadarDataSource();

        String table = null;
        if (source == HpeDataSource.S) {
            table = "RWBiasDyn";
        } else if (source == HpeDataSource.D) {
            table = "DAABiasDyn";
        } else {
            throw new Exception(
                    "Invalid bias source defined in HPERadarResult table: "
                            + source);
        }

        String office = SiteUtil.getSite();
        StringBuilder query = new StringBuilder("select radid, office_id, ");
        query.append("obstime, memspan_ind, numpairs, sumgag, sumrad, bias");
        query.append(" from ").append(table);
        query.append(" where office_id = :officeId");
        query.append(
                " and obstime = :obsTime order by radid asc, memspan_ind asc");

        final Map<String, Object> paramMap = new HashMap<>(2, 1.0f);
        paramMap.put("officeId", office);
        paramMap.put("obsTime", currentHour.getTime());

        Object[] results = dao.executeSQLQuery(query.toString(), paramMap);
        for (Object result : results) {
            if (result instanceof Object[]) {
                Object[] oa = (Object[]) result;
                BiasDynRecord rec = new BiasDynRecord();
                rec.setRadarId((String) oa[0]);
                rec.setOfficeId((String) oa[1]);
                rec.setObsTime((Date) oa[2]);
                rec.setMemspanIndex((Short) oa[3]);
                rec.setNumPairs((Double) oa[4]);
                rec.setSumGages((Float) oa[5]);
                rec.setSumRadars((Float) oa[6]);
                rec.setBias((Float) oa[7]);

                if (!dataMap.containsKey(rec.getRadarId())) {
                    dataMap.put(rec.getRadarId(),
                            new ArrayList<BiasDynRecord>());
                }

                dataMap.get(rec.getRadarId()).add(rec);
            } else {
                throw new Exception(
                        "Unexpected return type from bias query, expected Object[], got "
                                + result.getClass().getName());
            }
        }

        return dataMap;
    }

    /**
     * Get the hpeRadarResult entry for this product.
     * 
     * @param date
     *            date of the product
     * @param productName
     *            name of the product
     * @return record object
     * @throws Exception
     */
    public HpeRadarResult getHpeRadarResult(Date date, String productName)
            throws Exception {
        try {
            return hpeRadarResultDao
                    .retrieveByProductNameAndProductTime(productName, date);
        } catch (Exception e) {
            throw new Exception("Error querying the IHFS hperadarresult table",
                    e);
        }
    }

    /**
     * Get the nPairBiasSelect value.
     * 
     * @return the nPairBiasSelect value
     * @throws Exception
     */
    public int getNPairBiasSelect() throws Exception {
        try {
            Object[] results = dao.executeSQLQuery(
                    "select npair_bias_select from RWBiasStat");
            if (results != null && results.length == 1) {
                return (Integer) results[0];
            }
        } catch (Exception e) {
            throw new Exception("Error querying the IHFS hperadarresult table",
                    e);
        }

        return 0;
    }
}