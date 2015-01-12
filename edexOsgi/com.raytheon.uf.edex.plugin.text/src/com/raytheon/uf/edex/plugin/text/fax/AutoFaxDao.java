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
package com.raytheon.uf.edex.plugin.text.fax;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.Transaction;

import com.raytheon.edex.db.dao.DefaultPluginDao;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.text.AutoFaxContainer;
import com.raytheon.uf.common.dataplugin.text.db.AutoFaxRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.core.EDEXUtil;

/**
 * Data access object for storing and retrieving AutoFaxRecords
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 2, 2010            bfarmer     Initial creation
 * May 20, 2014 2536      bclement    moved from edex.textdb to edex.plugin.text
 * 
 * </pre>
 * 
 * @author bfarmer
 * @version 1.0
 */

public class AutoFaxDao extends DefaultPluginDao {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(AutoFaxDao.class);
    /**
     * Route to put a subscription notify message.
     */
    private static String autoFaxNotifyURI;

    /**
     * List of all cached subscription records
     */
    private static List<AutoFaxRecord> cachedRecords = new ArrayList<AutoFaxRecord>();

    /**
     * Maps properties to subscription records
     */
    private static Map<String, List<AutoFaxRecord>> recordsMap = new HashMap<String, List<AutoFaxRecord>>();

    private static boolean dirtyRecords = true;

    private static boolean dirtyMap = true;

    public AutoFaxDao() throws PluginException {
        this("text");
    }

    public AutoFaxDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    @Override
    public void purgeExpiredData() throws PluginException {
        // no op
    }

    public synchronized boolean addAutoFaxRecord(AutoFaxRecord record) {
        boolean success = true;
        try {
            persist(record);
        } catch (Exception e) {
            logger.error("Error occurred creating AutoFax record.", e);
            success = false;
        }
        if (success) {
            sendAutoFaxNotifyMessage(String.valueOf(record.getId().getAfosPil()
                    + record.getId().getFaxNumber()));
        }
        return success;
    }

    public synchronized boolean removeAutoFaxRecord(AutoFaxRecord record) {
        boolean success = true;
        Session sess = null;
        Transaction trans = null;
        try {
            sess = getSessionFactory().openSession();
            trans = sess.beginTransaction();
            sess.delete(record);
            trans.commit();
        } catch (Exception e) {
            logger.error("Error occurred deleting AutoFax record.", e);
            success = false;
            if (trans != null) {
                try {
                    trans.rollback();
                } catch (Exception e1) {
                    logger.error("Error occurred rolling back transaction", e);
                }
            }
        } finally {
            if (sess != null) {
                try {
                    sess.close();
                } catch (Exception e) {
                    logger.error("Error occurred closing session", e);
                }
            }
        }
        if (success) {
            sendAutoFaxNotifyMessage(String.valueOf(record.getId().getAfosPil()
                    + record.getId().getFaxNumber()));
        }
        return success;
    }

    public synchronized AutoFaxContainer getAllAutoFaxRecords() {
        AutoFaxContainer rval = new AutoFaxContainer();

        if (cachedRecords == null || dirtyRecords) {
            cachedRecords = new ArrayList<AutoFaxRecord>();
            Session sess = null;
            Transaction trans = null;
            try {
                sess = getSessionFactory().openSession();
                trans = sess.beginTransaction();

                Criteria crit = sess.createCriteria(AutoFaxRecord.class);
                List<?> vals = crit.list();

                for (Object val : vals) {
                    cachedRecords.add((AutoFaxRecord) val);
                }
                trans.commit();
                dirtyRecords = false;
            } catch (Exception e) {
                logger.error("Error occurred looking up all Autofax records.",
                        e);

                if (trans != null) {
                    try {
                        trans.rollback();
                    } catch (Exception e1) {
                        logger.error("Error occurred rolling back transaction",
                                e);
                    }
                }
            } finally {
                if (sess != null) {
                    try {
                        sess.close();
                    } catch (Exception e) {
                        logger.error("Error occurred closing session", e);
                    }
                }
            }
        } else {
            rval.setAutoFaxList(cachedRecords);
        }
        return rval;
    }

    public synchronized AutoFaxContainer getAllRecordsForPil(String afosPil) {
        AutoFaxContainer rval = new AutoFaxContainer();

        if (recordsMap == null || dirtyMap) {
            if (cachedRecords == null || dirtyRecords) {
                cachedRecords = new ArrayList<AutoFaxRecord>();
                Session sess = null;
                Transaction trans = null;
                try {
                    sess = getSessionFactory().openSession();
                    trans = sess.beginTransaction();

                    Criteria crit = sess.createCriteria(AutoFaxRecord.class);
                    List<?> vals = crit.list();

                    for (Object val : vals) {
                        cachedRecords.add((AutoFaxRecord) val);
                    }
                    trans.commit();
                    dirtyRecords = false;
                } catch (Exception e) {
                    logger.error(
                            "Error occurred looking up all Autofax records.", e);

                    if (trans != null) {
                        try {
                            trans.rollback();
                        } catch (Exception e1) {
                            logger.error(
                                    "Error occurred rolling back transaction",
                                    e);
                        }
                    }
                } finally {
                    if (sess != null) {
                        try {
                            sess.close();
                        } catch (Exception e) {
                            logger.error("Error occurred closing session", e);
                        }
                    }
                }
            }
            if (!dirtyRecords) {
                for (AutoFaxRecord faxRecord : cachedRecords) {
                    if (recordsMap.get(faxRecord.getId().getAfosPil()) == null) {
                        recordsMap.put(faxRecord.getId().getAfosPil(),
                                new ArrayList<AutoFaxRecord>());
                    }
                    recordsMap.get(faxRecord.getId().getAfosPil()).add(
                            faxRecord);
                }
                dirtyMap = false;
            }
        }
        rval.setAutoFaxList(recordsMap.get(afosPil));
        return rval;
    }

    private synchronized void sendAutoFaxNotifyMessage(String id) {
        dirtyMap = true;
        dirtyRecords = true;
        if (autoFaxNotifyURI != null) {
            try {
                EDEXUtil.getMessageProducer()
                        .sendAsyncUri(autoFaxNotifyURI, id);
            } catch (Exception e) {
                statusHandler.handle(Priority.SIGNIFICANT,
                        "Failed to send AutoFax notify message", e);
            }
        }
    }

    public synchronized void setAutoFaxNotifyURI(String uri) {
        autoFaxNotifyURI = uri;
    }

    public synchronized void updateCache(Object obj) {
        dirtyRecords = true;
        dirtyMap = true;
    }
}
