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

package com.raytheon.edex.services;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.status.IPerformanceStatusHandler;
import com.raytheon.uf.common.status.PerformanceStatus;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;

/**
 * Receives events from the file endpoint.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                         fgriffit    Initial Creation.
 * 20080408     1039       jkorman     Added traceId for tracing data.
 * Nov 11, 2008            chammack    Refactored for Camel
 * 02/06/09     1990       bphillip    Refactored to use plugin daos
 * Mar 19, 2013 1785       bgonzale    Added performance status to indexOne and index.
 * </pre>
 * 
 * @author Frank Griffith
 * @version 1.0
 */
public class IndexSrv {

    private String sessionFactory;

    private String txFactory;

    private Log logger = LogFactory.getLog(getClass());

    private final IPerformanceStatusHandler perfLog = PerformanceStatus
            .getHandler("DataBase:");

    /** The default constructor */
    public IndexSrv() {
    }

    /** Addtional constructor */
    public IndexSrv(String dbname) {
    }

    /**
     * Index a single record
     * 
     * Return null if the indexing was not successful, else return the record.
     * 
     * @param record
     *            the record
     * @return the record, else null if indexing failed
     * @throws PluginException
     */
    public PluginDataObject indexOne(PluginDataObject record)
            throws PluginException {
        String pluginName = record.getPluginName();
        PluginDao dao = PluginFactory.getInstance().getPluginDao(pluginName);
        ITimer timer = TimeUtil.getTimer();
        timer.start();
        dao.persistToDatabase(record);
        timer.stop();
        perfLog.logDuration(pluginName + ": Saved a record: Time to Save",
                timer.getElapsedTime());
        if (logger.isDebugEnabled()) {
            logger.debug("Persisted: " + record + " to database");
        }
        return record;
    }

    /**
     * Index all records in an array
     * 
     * Return the list of records that were successfully persisted
     * 
     * @param record
     *            a record array
     * @return the list of objects that were successfully persisted
     * @throws PluginException
     */
    public PluginDataObject[] index(PluginDataObject[] record)
            throws PluginException {

        if (record == null || record.length == 0) {
            return new PluginDataObject[0];
        }

        try {
            String pluginName = record[0].getPluginName();
            PluginDao dao = PluginFactory.getInstance().getPluginDao(pluginName);
            EDEXUtil.checkPersistenceTimes(record);
            ITimer timer = TimeUtil.getTimer();
            timer.start();
            PluginDataObject[] persisted = dao.persistToDatabase(record);
            timer.stop();
            perfLog.logDuration(pluginName + ": Saved " + persisted.length
                    + " record(s): Time to Save",
                    timer.getElapsedTime());
            if (logger.isDebugEnabled()) {
                for (PluginDataObject rec : record) {
                    logger.debug("Persisted: " + rec + " to database");
                }
            }

            return persisted;
        } catch (Throwable e) {
            logger.error("Error occurred during persist", e);
            return new PluginDataObject[0];
        }
    }

    public void dispose() {
    }

    public String getSessionFactory() {
        return sessionFactory;
    }

    public void setSessionFactory(String sessionFactory) {
        this.sessionFactory = sessionFactory;
    }

    public String getTxFactory() {
        return txFactory;
    }

    public void setTxFactory(String txFactory) {
        this.txFactory = txFactory;
    }

}
