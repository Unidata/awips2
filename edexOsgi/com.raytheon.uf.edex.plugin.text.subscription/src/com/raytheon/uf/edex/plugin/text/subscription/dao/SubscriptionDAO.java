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
package com.raytheon.uf.edex.plugin.text.subscription.dao;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.hibernate.Criteria;
import org.hibernate.Query;
import org.hibernate.Session;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.TransactionCallback;

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.dataplugin.text.subscription.db.SubscriptionRecord;
import com.raytheon.uf.common.dataquery.db.QueryParam;
import com.raytheon.uf.common.message.Property;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * DAO for interaction with the subscription database.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 14Nov2008    1709       MW Fegan    Initial creation.
 * 14Apr2011	5163	   cjeanbap	   NWRWAVES Setting AFOS text triggers in AWIPS II
 * 04/24/13     1949       rjpeter     Removed @Override on delete.
 * Nov 08, 2013 2361       njensen     Chaged method signature of saveOrUpdate(Object)
 * May 22, 2014 2536       bclement    moved from autobldsrv to edex.plugin.text
 * 10/16/2014   3454       bphillip    Upgrading to Hibernate 4
 * 10/28/2014   3454       bphillip    Fix usage of getSession()
 * Jan 18, 2016 4562       tjensen     Moved from edex.plugin.text to 
 *                                     edex.plugin.text.subscription
 * </pre>
 * 
 * @author mfegan
 * @version 1.0
 */

public class SubscriptionDAO extends CoreDao {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SubscriptionDAO.class);

    /**
     * Route to put a subscription notify message.
     */
    private static String subscriptionNotifyURI;

    /**
     * List of all cached subscription records
     */
    private static List<SubscriptionRecord> cachedRecords = null;

    /**
     * Maps properties to subscription records
     */
    private static Map<String, List<SubscriptionRecord>> recordsMap = new HashMap<String, List<SubscriptionRecord>>();

    private static boolean dirtyRecords = true;

    private static boolean dirtyMap = true;

    public SubscriptionDAO() {
        this(DaoConfig.forClass("fxa", SubscriptionRecord.class));
    }

    /**
     * @param config
     */
    public SubscriptionDAO(DaoConfig config) {
        super(config);
    }

    public void delete(PersistableDataObject<?> obj) {
        super.delete(obj);
        sendSubscriptionNotifyMessage(String.valueOf(obj.getIdentifier()));
    }

    /**
     * Write record but do not attempt to update if already exists
     * 
     * @param record
     * @return
     */
    public boolean write(SubscriptionRecord record) {
        // query
        Session session = this.getSession();
        try {
            Query query = session
                    .createQuery("from SubscriptionRecord where type = :type and trigger = :trigger and runner = :runner and script = :script and filepath = :filepath and arguments = :arguments");
            query.setParameter("type", record.getType());
            query.setParameter("trigger", record.getTrigger());
            query.setParameter("runner", record.getRunner());
            query.setParameter("script", record.getScript());
            query.setParameter("filepath", record.getFilepath());
            query.setParameter("arguments", record.getArguments());
            List<?> results = query.list();

            if (results.size() > 0) {
                return false;
            } else {
                create(record);
                sendSubscriptionNotifyMessage(String.valueOf(record
                        .getIdentifier()));
                return true;
            }
        } finally {
            if (session != null) {
                session.close();
            }
        }
    }

    @Override
    public void saveOrUpdate(Object obj) {
        super.saveOrUpdate(obj);
        sendSubscriptionNotifyMessage(String
                .valueOf(((PersistableDataObject) obj).getIdentifier()));
    }

    @Override
    public void update(PersistableDataObject obj) {
        super.update(obj);
        sendSubscriptionNotifyMessage(String.valueOf(obj.getIdentifier()));
    }

    /**
     * Returns a list containing all subscriptions currently in the database.
     * 
     * @return the list of subscriptions
     */
    public List<SubscriptionRecord> getSubscriptions() {
        if ((cachedRecords == null) || dirtyRecords) {
            List<SubscriptionRecord> retVal = loadAll();
            if (retVal == null) {
                logger.info("Unable to perform query, 'null' result returned");
                cachedRecords = new ArrayList<SubscriptionRecord>();
            } else {
                cachedRecords = (List<SubscriptionRecord>) retVal;
                dirtyRecords = false;
            }
        }

        return cachedRecords;
    }

    @SuppressWarnings("unchecked")
    private List<SubscriptionRecord> loadAll() {
        return (List<SubscriptionRecord>) txTemplate
                .execute(new TransactionCallback<List<?>>() {
                    @Override
                    public List<SubscriptionRecord> doInTransaction(
                            TransactionStatus status) {
                        Criteria criteria = getCurrentSession().createCriteria(
                                daoClass);
                        return criteria.list();
                    }
                });
    }

    /**
     * 
     * @param props
     * @return
     */
    @SuppressWarnings("unchecked")
    public List<SubscriptionRecord> getSubscriptions(Collection<Property> props) {
        StringBuilder sb = new StringBuilder();
        for (Property p : props) {
            sb.append(p.getName()).append('=').append(p.getValue()).append(' ');
        }
        String key = sb.toString().trim();
        if (dirtyMap) {
            synchronized (recordsMap) {
                recordsMap.clear();
            }
            dirtyMap = false;
        }

        List<SubscriptionRecord> rval = null;
        synchronized (recordsMap) {
            rval = recordsMap.get(key);
        }
        if ((rval == null) || rval.isEmpty()) {
            List<?> retVal = null;
            List<QueryParam> params = new ArrayList<QueryParam>();
            for (Property prop : props) {
                QueryParam param = new QueryParam(prop.getName(),
                        prop.getValue());
                param.setClassName(this.daoClass.getName());
                params.add(param);
            }
            DatabaseQuery query = new DatabaseQuery(this.daoClass, params);
            logger.debug("Executing query" + query.createHQLQuery());
            try {
                retVal = queryByCriteria(query);
                logger.debug("Query result: " + retVal);
                if (retVal == null) {
                    logger.info("Unable to perform query, 'null' result returned");
                    return new ArrayList<SubscriptionRecord>();
                }
            } catch (DataAccessLayerException e) {
                logger.error("Unable to perform query, ", e);
                return new ArrayList<SubscriptionRecord>();
            }
            rval = (List<SubscriptionRecord>) retVal;
            synchronized (recordsMap) {
                recordsMap.put(key, rval);
            }
        }
        return rval;
    }

    public void updateCache(Object obj) {
        dirtyRecords = true;
        dirtyMap = true;
    }

    public void setSubscriptionNotifyURI(String uri) {
        subscriptionNotifyURI = uri;
    }

    private void sendSubscriptionNotifyMessage(String id) {
        dirtyMap = true;
        dirtyRecords = true;
        if (subscriptionNotifyURI != null) {
            try {
                EDEXUtil.getMessageProducer().sendAsyncUri(
                        subscriptionNotifyURI, id);
            } catch (Exception e) {
                statusHandler.handle(Priority.SIGNIFICANT,
                        "Failed to send subscription notify message", e);
            }
        }
    }
}
