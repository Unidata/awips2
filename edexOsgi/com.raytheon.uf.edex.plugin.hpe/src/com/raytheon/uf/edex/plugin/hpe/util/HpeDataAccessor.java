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

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.SortedMap;
import java.util.TimeZone;
import java.util.TreeMap;

import com.raytheon.edex.site.SiteUtil;
import com.raytheon.uf.common.plugin.hpe.data.BiasDynRecord;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.plugin.hpe.util.HpeEnums.HpeDataSource;

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
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class HpeDataAccessor {
    private static final String IHFS = "ihfs";

    private static final String POSTGRES_DATE_STRING = "yyyy-MM-dd HH:mm:ss";

    private static final String HPE_RADAR_QUERY = "select distinct(radid) from ";

    /**
     * Full query string for HpeRadarResult table
     */
    public static final String FULL_HPE_RADAR_RESULT_QUERY = "select hpe_productname, producttime, "
            + "num_radar_avail, bias_source, radar_data_source from hperadarresult";

    /**
     * Database date string format
     */
    private final ThreadLocal<SimpleDateFormat> sdf = TimeUtil
            .buildThreadLocalSimpleDateFormat(POSTGRES_DATE_STRING,
                    TimeZone.getTimeZone("GMT"));

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
        SortedMap<String, List<BiasDynRecord>> dataMap = new TreeMap<String, List<BiasDynRecord>>();

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
        query.append(" where office_id ").append(" = '").append(office)
                .append("'");
        query.append(" and obstime = '").append(sdf.get().format(recdate));
        query.append("'").append(" order by radid asc, memspan_ind asc");

        Object[] results = dao.executeSQLQuery(query.toString());
        for (Object result : results) {
            if (result instanceof Object[]) {
                Object[] oa = (Object[]) result;
                BiasDynRecord rec = new BiasDynRecord();
                rec.setRadarId((String) oa[0]);
                rec.setOfficeId((String) oa[1]);
                rec.setObsTime((Date) oa[2]);
                rec.setMemspanIndex((Integer) oa[3]);
                rec.setNumPairs((Float) oa[4]);
                rec.setSumGages((Float) oa[5]);
                rec.setSumRadars((Float) oa[6]);
                rec.setBias((Float) oa[7]);

                if (!dataMap.containsKey(rec.getOfficeId())) {
                    dataMap.put(rec.getOfficeId(),
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
        HpeRadarResult hpeResult = new HpeRadarResult();
        try {
            String where = " where producttime = '" + sdf.get().format(date)
                    + "' and hpe_productname = '" + productName + "'";

            Object[] results = dao.executeSQLQuery(FULL_HPE_RADAR_RESULT_QUERY
                    + where);
            if (results != null && results.length == 5) {
                hpeResult.setHpeProductName((String) results[0]);
                hpeResult.setProductTime((Date) results[1]);

                if (results[2] != null) {
                    hpeResult.setNumRadarAvailable((Integer) results[2]);
                }

                if (results[3] != null) {
                    hpeResult.setBiasSource((String) results[3]);
                }

                if (results[4] != null) {
                    hpeResult.setRadarDataSource((String) results[4]);
                }
            }
        } catch (Exception e) {
            throw new Exception("Error querying the IHFS hperadarresult table",
                    e);
        }

        return hpeResult;
    }

    /**
     * Get the nPairBiasSelect value.
     * 
     * @return the nPairBiasSelect value
     * @throws Exception
     */
    public int getNPairBiasSelect() throws Exception {
        try {
            Object[] results = dao
                    .executeSQLQuery("select npair_bias_select from RWBiasStat");
            if (results != null && results.length == 1) {
                return (Integer) results[0];
            }
        } catch (Exception e) {
            throw new Exception("Error querying the IHFS hperadarresult table",
                    e);
        }

        return 0;
    }

    /**
     * Get a list of HPE radars for the current time.
     * 
     * @param date
     *            The obstime
     * @param table
     *            The table to query
     * @return List of radars
     * @throws Exception
     */
    public List<String> getHpeRadars(Date date, String table) throws Exception {
        StringBuilder query = new StringBuilder(HPE_RADAR_QUERY);
        query.append(table).append(" where obstime = '");
        query.append(sdf.get().format(date)).append("' order by radid");

        List<String> radarList;

        try {
            Object[] results = dao.executeSQLQuery(query.toString());
            radarList = new ArrayList<String>(results.length);
            for (Object o : results) {
                radarList.add((String) o);
            }

            return radarList;
        } catch (Exception e) {
            throw new Exception("Error querying the IHFS hperadarresult table",
                    e);
        }
    }
}