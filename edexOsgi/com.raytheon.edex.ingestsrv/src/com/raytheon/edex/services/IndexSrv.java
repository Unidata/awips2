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
 *                            fgriffit Initial Creation.
 * 20080408           1039     jkorman Added traceId for tracing data.
 * Nov 11, 2008               chammack Refactored for Camel
 * 02/06/09     1990       bphillip     Refactored to use plugin daos
 * </pre>
 * 
 * @author Frank Griffith
 * @version 1.0
 */
public class IndexSrv {

    private String sessionFactory;

    private String txFactory;

    private Log logger = LogFactory.getLog(getClass());

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
        PluginDao dao = PluginFactory.getInstance().getPluginDao(
                record.getPluginName());
        dao.persistToDatabase(record);
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
            PluginDao dao = PluginFactory.getInstance().getPluginDao(
                    record[0].getPluginName());
            EDEXUtil.checkPersistenceTimes(record);
            PluginDataObject[] persisted = dao.persistToDatabase(record);
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
