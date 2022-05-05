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
package com.raytheon.uf.edex.plugin.text.dao;

import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.edex.db.dao.DefaultPluginDao;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataquery.db.QueryParam.QueryOperand;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.processor.IDatabaseProcessor;
import com.raytheon.uf.edex.database.purge.PurgeLogger;
import com.raytheon.uf.edex.database.query.DatabaseQuery;
import com.raytheon.uf.edex.plugin.text.db.TextDB;

/**
 * DAO for text products
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jul 10, 2009  2191     rjpeter   Update retention time handling.
 * Aug 18, 2009  2191     rjpeter   Changed to version purging.
 * Dec 13, 2013  2555     rjpeter   Renamed getRecordsToArchive to
 *                                  processArchiveRecords.
 * Jul 30, 2015  1574     nabowle   Override purgeOrphanedData to noop.
 * Apr 25, 2018  6966     randerso  Fixed full purge to actually run every n
 *                                  hours and correctly log the number of
 *                                  records deleted.
 *
 * </pre>
 *
 * @author
 */
public class TextDao extends DefaultPluginDao {

    private static final int defaultFullPurgeInterval = 3;

    private static final String FullPurgeIntervalProperty = "text.fullVersionPurge.intervalhours";

    private static final long fullPurgeInterval;

    private long lastFullPurgeTime = System.currentTimeMillis();

    static {

        int val = defaultFullPurgeInterval;
        String fullPurgeProperty = System
                .getProperty(FullPurgeIntervalProperty);
        if (fullPurgeProperty != null) {
            // local logger for use in static block only
            Logger logger = LoggerFactory.getLogger(TextDao.class);

            try {
                val = Integer.parseInt(fullPurgeProperty);
                if (val < 0) {
                    val = defaultFullPurgeInterval;
                    logger.warn(String.format(
                            "The property \"%s\" must be greater than 0. Using default value: %d",
                            FullPurgeIntervalProperty,
                            defaultFullPurgeInterval));
                }
            } catch (Exception e) {
                val = defaultFullPurgeInterval;
                logger.warn(String.format(
                        "The property \"%s\" is set to an invalid value: %s. Using default value: %d",
                        FullPurgeIntervalProperty, fullPurgeProperty,
                        defaultFullPurgeInterval), e);
            }
        }
        fullPurgeInterval = val * TimeUtil.MILLIS_PER_HOUR;
    }

    /**
     * Constructor
     *
     * @param pluginName
     * @throws PluginException
     */
    public TextDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    @Override
    public void purgeAllData() {
        logger.warn(
                "purgeAllPluginData not implemented for text. No data will be purged.");
    }

    protected void loadScripts() throws PluginException {
        // no op
    }

    @Override
    public void purgeExpiredData() throws PluginException {
        /*
         * only do full purge every few hours since incremental purge runs every
         * minute
         */
        long t0 = System.currentTimeMillis();
        if ((t0 - lastFullPurgeTime) > fullPurgeInterval) {
            lastFullPurgeTime = t0;

            // purge the operational table
            int deletedRecords = TextDB.purgeStdTextProducts(true);
            long t1 = System.currentTimeMillis();
            PurgeLogger.logInfo("Full purge: Purged " + deletedRecords
                    + " rows from stdTextProducts in " + (t1 - t0) + " ms",
                    "TEXT");

            // purge the practice table
            deletedRecords = TextDB.purgeStdTextProducts(false);
            long t2 = System.currentTimeMillis();
            PurgeLogger.logInfo("Full purge: Purged " + deletedRecords
                    + " rows from practiceStdTextProducts in " + (t2 - t1)
                    + " ms", "TEXT");
        }
    }

    @Override
    public void purgeOrphanedData() {
        // Noop
    }

    @Override
    public int processArchiveRecords(Calendar insertStartTime,
            Calendar insertEndTime, IDatabaseProcessor processor)
            throws DataAccessLayerException {
        StdTextProductDao dao = new StdTextProductDao(true);
        DatabaseQuery dbQuery = new DatabaseQuery(dao.getDaoClass());
        dbQuery.addQueryParam("insertTime", insertStartTime,
                QueryOperand.GREATERTHANEQUALS);
        dbQuery.addQueryParam("insertTime", insertEndTime,
                QueryOperand.LESSTHAN);
        dbQuery.addOrder("insertTime", true);
        dbQuery.addOrder("refTime", true);

        return this.processByCriteria(dbQuery, processor);
    }

    @Override
    public Date getMinInsertTime(Map<String, String> productKeys)
            throws DataAccessLayerException {
        StdTextProductDao dao = new StdTextProductDao(true);
        DatabaseQuery query = new DatabaseQuery(dao.getDaoClass());

        if ((productKeys != null) && (productKeys.size() > 0)) {
            for (Map.Entry<String, String> pair : productKeys.entrySet()) {
                query.addQueryParam(pair.getKey(), pair.getValue());
            }
        }

        query.addReturnedField("insertTime");
        query.addOrder("insertTime", true);
        query.setMaxResults(1);
        @SuppressWarnings("unchecked")
        List<Calendar> result = (List<Calendar>) dao.queryByCriteria(query);
        if (result.isEmpty()) {
            return null;
        } else {
            return result.get(0).getTime();
        }
    }
}
