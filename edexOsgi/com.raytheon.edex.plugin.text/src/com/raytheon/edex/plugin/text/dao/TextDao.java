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
package com.raytheon.edex.plugin.text.dao;

import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Map;

import com.raytheon.edex.db.dao.DefaultPluginDao;
import com.raytheon.edex.textdb.dao.StdTextProductDao;
import com.raytheon.edex.textdb.dbapi.impl.TextDB;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataquery.db.QueryParam.QueryOperand;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.processor.IDatabaseProcessor;
import com.raytheon.uf.edex.database.purge.PurgeLogger;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * DAO for text products
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jul 10, 2009 2191        rjpeter     Update retention time handling.
 * Aug 18, 2009 2191        rjpeter     Changed to version purging.
 * Dec 13, 2013 2555        rjpeter     Renamed getRecordsToArchive to processArchiveRecords.
 * </pre>
 * 
 * @author
 * @version 1
 */
public class TextDao extends DefaultPluginDao {
    private static final int fullPurgeInterval;

    static {
        String fullPurgeProperty = System.getProperty(
                "text.fullVersionPurge.intervalhours", "3");
        Integer val = null;
        try {
            val = Integer.parseInt(fullPurgeProperty);
            if ((val < 0) || (val > 23)) {

            }
        } catch (Exception e) {
            val = new Integer(3);
        }
        fullPurgeInterval = val.intValue();
    }

    public TextDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    @Override
    public void purgeAllData() {
        logger.warn("purgeAllPluginData not implemented for text. No data will be purged.");
    }

    protected void loadScripts() throws PluginException {
        // no op
    }

    @Override
    public void purgeExpiredData() throws PluginException {
        int deletedRecords = 0;

        // only do full purge every few hours since incremental purge runs every
        // minute
        if ((TimeUtil.newGmtCalendar().get(Calendar.HOUR_OF_DAY) % fullPurgeInterval) == 0) {
            TextDB.purgeStdTextProducts();
        }

        PurgeLogger.logInfo("Purged " + deletedRecords + " items total.",
                "text");
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
