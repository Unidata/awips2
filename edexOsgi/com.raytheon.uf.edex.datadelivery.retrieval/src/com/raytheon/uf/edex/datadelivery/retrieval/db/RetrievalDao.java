package com.raytheon.uf.edex.datadelivery.retrieval.db;

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.LockOptions;
import org.hibernate.Query;
import org.hibernate.Session;
import org.hibernate.Transaction;
import org.hibernate.criterion.Restrictions;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecord.State;

public class RetrievalDao extends CoreDao {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RetrievalDao.class);

    public RetrievalDao() {
        super(DaoConfig.forClass(RetrievalRequestRecord.class));
    }

    /**
     * Returns the next PENDING retrieval request, puts it into a RUNNING state,
     * based on current time.
     * 
     * @return
     */
    public RetrievalRequestRecord activateNextRetrievalRequest()
            throws DataAccessLayerException {
        Session sess = null;
        Transaction tx = null;
        RetrievalRequestRecord rval = null;

        try {
            sess = getHibernateTemplate().getSessionFactory().openSession();
            tx = sess.beginTransaction();

            final String minPriHql = "select min(rec.priority) from RetrievalRequestRecord rec "
                    + "where rec.state = :statePending";
            final String minInsertHql = "select min(rec.insertTime) from RetrievalRequestRecord rec "
                    + "where rec.state = :statePending and rec.priority = :minPri";
            // descending record order to retrieve all for a given subscription
            // before moving to the next one if two have the same
            // priority/insertTime
            final String pkHql = "select rec.id.subscriptionName, min(rec.id.index) from RetrievalRequestRecord rec "
                    + "where rec.state = :statePending and rec.priority = :minPri and rec.insertTime = :minInsert "
                    + "group by rec.id.subscriptionName order by min(rec.id.index) desc";

            Query minPriQuery = sess.createQuery(minPriHql);
            minPriQuery.setParameter("statePending", State.PENDING);
            Query minInsertQuery = sess.createQuery(minInsertHql);
            minInsertQuery.setParameter("statePending", State.PENDING);
            Query pkQuery = sess.createQuery(pkHql);
            pkQuery.setParameter("statePending", State.PENDING);
            boolean done = false;

            while (!done) {
                Object result = minPriQuery.uniqueResult();
                if (result != null) {
                    int minPri = ((Number) result).intValue();
                    minInsertQuery.setInteger("minPri", minPri);
                    result = minInsertQuery.uniqueResult();
                    if (result != null) {
                        Date minInsert = (Date) result;
                        pkQuery.setInteger("minPri", minPri);
                        pkQuery.setTimestamp("minInsert", minInsert);
                        pkQuery.setMaxResults(1);
                        while (rval == null) {
                            // TODO: Verify recalling uniqueResult causes query
                            // to happen again
                            result = pkQuery.uniqueResult();
                            if (result != null) {
                                Object[] results = (Object[]) result;
                                RetrievalRequestRecordPK pk = new RetrievalRequestRecordPK();
                                pk.setSubscriptionName((String) results[0]);
                                pk.setIndex(((Number) results[1]).intValue());

                                // lookup/lock row
                                rval = (RetrievalRequestRecord) sess.get(
                                        RetrievalRequestRecord.class, pk,
                                        LockOptions.UPGRADE);
                                if (rval == null
                                        || !State.PENDING.equals(rval
                                                .getState())) {
                                    // another thread grabbed request while
                                    // waiting for upgrade lock, redo sub query,
                                    // null out rval in case it was due to state
                                    // change
                                    rval = null;
                                    continue;
                                }

                                done = true;
                            } else {
                                // another thread grabbed last entry for this
                                // priority/insertTime
                                break;
                            }
                        }
                    }
                    // else another thread grabbed last entry for this priority,
                    // repeat loop
                } else {
                    // no Pending entries
                    done = true;
                }
            }

            if (rval != null) {
                rval.setState(State.RUNNING);
                sess.update(rval);
            }
            tx.commit();
        } catch (Exception e) {
            throw new DataAccessLayerException(
                    "Failed looking up next retrieval", e);
        } finally {
            if (tx != null && !tx.wasCommitted()) {
                try {
                    tx.rollback();
                } catch (Exception e) {
                    // ignore
                }
            }
            if (sess != null) {
                try {
                    sess.close();
                } catch (Exception e) {
                    // ignore
                }
            }
        }

        return rval;
    }

    public void completeRetrievalRequest(RetrievalRequestRecord rec)
            throws DataAccessLayerException {
        Session sess = null;
        Transaction tx = null;

        try {
            sess = getHibernateTemplate().getSessionFactory().openSession();
            tx = sess.beginTransaction();
            sess.update(rec);

            tx.commit();
        } catch (HibernateException e) {
            throw new DataAccessLayerException(
                    "Failed to update the database while changing the status on ["
                            + rec.getId() + "]" + " to [" + rec.getState()
                            + "]", e);
        } finally {
            if (tx != null && !tx.wasCommitted()) {
                try {
                    tx.rollback();
                } catch (Exception e) {
                    // ignore
                }
            }
            if (sess != null) {
                try {
                    sess.close();
                } catch (Exception e) {
                    // ignore
                }
            }
        }
    }

    /**
     * TODO: This will fail in a cluster, need to limit by machine in a cluster
     * 
     * @return
     */
    public boolean resetRunningRetrievalsToPending() {
        Session sess = null;
        Transaction tx = null;
        boolean rval = true;

        try {
            sess = getHibernateTemplate().getSessionFactory().openSession();
            tx = sess.beginTransaction();
            String hql = "update RetrievalRequestRecord rec set rec.state = :pendState where rec.state = :runState";
            Query query = sess.createQuery(hql);
            query.setParameter("pendState", State.PENDING);
            query.setParameter("runState", State.RUNNING);
            query.executeUpdate();
            tx.commit();
        } catch (Exception e) {
            rval = false;
            statusHandler.error(
                    "Unable to reset old RUNNING retrievals to PENDING", e);
        } finally {
            if (tx != null && !tx.wasCommitted()) {
                try {
                    tx.rollback();
                } catch (Exception e) {
                    // ignore
                }
            }
            if (sess != null) {
                try {
                    sess.close();
                } catch (Exception e) {
                    // ignore
                }
            }
        }

        return rval;
    }

    /**
     * Returns the state counts for the passed subscription.
     * 
     * @param sess
     * @param subName
     * @return
     */
    public Map<State, Integer> getSubscriptionStateCounts(String subName)
            throws DataAccessLayerException {
        Session sess = null;
        Transaction tx = null;
        Map<State, Integer> rval = new HashMap<State, Integer>(8);

        try {
            sess = getHibernateTemplate().getSessionFactory().openSession();
            tx = sess.beginTransaction();
            String hql = "select rec.state, count(rec.id.subscriptionName) from RetrievalRequestRecord rec "
                    + "where rec.id.subscriptionName = :subName group by rec.state";
            Query query = sess.createQuery(hql);
            query.setString("subName", subName);
            @SuppressWarnings("unchecked")
            List<Object> result = query.list();

            if (result != null && result.size() > 0) {
                for (Object row : result) {
                    if (row instanceof Object[]) {
                        Object[] cols = (Object[]) row;
                        rval.put((State) cols[0], ((Number) cols[1]).intValue());
                    } else {
                        throw new DataAccessLayerException(
                                "Unhandled result from database.  Expected ["
                                        + Object[].class + "], received ["
                                        + row.getClass() + "]");
                    }
                }
            }
            tx.commit();
        } catch (Exception e) {
            throw new DataAccessLayerException(
                    "Failed check pending/running retrieval count for subscription ["
                            + subName + "]", e);
        } finally {
            if (tx != null && !tx.wasCommitted()) {
                try {
                    tx.rollback();
                } catch (Exception e) {
                    // ignore
                }
            }
            if (sess != null) {
                try {
                    sess.close();
                } catch (Exception e) {
                    // ignore
                }
            }
        }

        return rval;
    }

    @SuppressWarnings("unchecked")
    public List<RetrievalRequestRecord> getFailedRequests(String subName)
            throws DataAccessLayerException {
        Session sess = null;
        Transaction tx = null;

        try {
            sess = getHibernateTemplate().getSessionFactory().openSession();
            tx = sess.beginTransaction();
            Criteria query = sess.createCriteria(RetrievalRequestRecord.class);
            query.add(Restrictions.eq("state", State.FAILED));
            query.add(Restrictions.eq("id.subscriptionName", subName));
            List<RetrievalRequestRecord> rval = query.list();
            tx.commit();
            return rval;
        } catch (Exception e) {
            throw new DataAccessLayerException(
                    "Failed check pending/running retrieval count for subscription ["
                            + subName + "]", e);
        } finally {
            if (tx != null && !tx.wasCommitted()) {
                try {
                    tx.rollback();
                } catch (Exception e) {
                    // ignore
                }
            }
            if (sess != null) {
                try {
                    sess.close();
                } catch (Exception e) {
                    // ignore
                }
            }
        }
    }

    public boolean removeSubscription(String subName)
            throws DataAccessLayerException {
        Session sess = null;
        Transaction tx = null;
        boolean rval = false;

        try {
            sess = getHibernateTemplate().getSessionFactory().openSession();
            tx = sess.beginTransaction();
            String hql = "delete from RetrievalRequestRecord rec "
                    + "where rec.id.subscriptionName = :subName";
            Query query = sess.createQuery(hql);
            query.setString("subName", subName);
            query.executeUpdate();
            tx.commit();
            rval = true;
        } catch (Exception e) {
            throw new DataAccessLayerException(
                    "Failed removing retrievals for subscription [" + subName
                            + "]", e);
        } finally {
            if (tx != null && !tx.wasCommitted()) {
                try {
                    tx.rollback();
                } catch (Exception e) {
                    // ignore
                }
            }
            if (sess != null) {
                try {
                    sess.close();
                } catch (Exception e) {
                    // ignore
                }
            }
        }
        return rval;
    }

    /**
     * Get all requests for the subscription name.
     * 
     * @param subName
     * @return
     */
    // TODO: Change to use SessionManager code
    @SuppressWarnings("unchecked")
    public List<RetrievalRequestRecord> getRequests(String subName)
            throws DataAccessLayerException {
        Session sess = null;
        Transaction tx = null;

        try {
            sess = getHibernateTemplate().getSessionFactory().openSession();
            tx = sess.beginTransaction();
            Criteria query = sess.createCriteria(RetrievalRequestRecord.class);
            query.add(Restrictions.eq("id.subscriptionName", subName));
            List<RetrievalRequestRecord> rval = query.list();
            tx.commit();
            return rval;
        } catch (Exception e) {
            throw new DataAccessLayerException(
                    "Failed to return retrieval records for subscription ["
                            + subName + "]", e);
        } finally {
            if (tx != null && !tx.wasCommitted()) {
                try {
                    tx.rollback();
                } catch (Exception e) {
                    // ignore
                }
            }
            if (sess != null) {
                try {
                    sess.close();
                } catch (Exception e) {
                    // ignore
                }
            }
        }
    }
}
