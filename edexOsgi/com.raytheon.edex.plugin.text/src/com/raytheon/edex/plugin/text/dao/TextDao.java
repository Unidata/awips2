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
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.dataquery.db.QueryParam.QueryOperand;
import com.raytheon.uf.edex.database.DataAccessLayerException;
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
 * </pre>
 * 
 * @author
 * @version 1
 */
public class TextDao extends DefaultPluginDao {

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
        if (Calendar.getInstance().get(Calendar.HOUR_OF_DAY) % 3 == 0) {
            TextDB.purgeStdTextProducts();
        }

        PurgeLogger.logInfo("Purged " + deletedRecords + " items total.",
                "text");
    }

    @SuppressWarnings("unchecked")
    @Override
    public List<PersistableDataObject> getRecordsToArchive(
            Calendar insertStartTime, Calendar insertEndTime)
            throws DataAccessLayerException {
        StdTextProductDao dao = new StdTextProductDao(true);
        DatabaseQuery dbQuery = new DatabaseQuery(dao.getDaoClass());
        dbQuery.addQueryParam("insertTime", insertStartTime,
                QueryOperand.GREATERTHANEQUALS);
        dbQuery.addQueryParam("insertTime", insertEndTime,
                QueryOperand.LESSTHAN);
        dbQuery.addOrder("insertTime", true);

        return (List<PersistableDataObject>) dao.queryByCriteria(dbQuery);
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
