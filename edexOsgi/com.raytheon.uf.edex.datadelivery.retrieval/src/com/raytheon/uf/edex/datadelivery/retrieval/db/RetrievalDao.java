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
import org.hibernate.criterion.Restrictions;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.SessionManagedDao;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecord.State;

/**
 * 
 * DAO for {@link RetrievalRequestRecord} entities.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 30, 2013 1543       djohnson     Add SW history.
 * Feb 07, 2013 1543       djohnson     Use session management code.
 * Feb 13, 2013 1543       djohnson     Exported interface which is now implemented.
 * Feb 22, 2013 1543       djohnson     Made public as YAJSW doesn't like Spring exceptions.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@Repository
@Transactional
// TODO: Split service functionality from DAO functionality
public class RetrievalDao extends
        SessionManagedDao<RetrievalRequestRecordPK, RetrievalRequestRecord> implements IRetrievalDao {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RetrievalDao.class);

    /**
     * Constructor.
     */
    public RetrievalDao() {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public RetrievalRequestRecord activateNextRetrievalRequest(Network network)
            throws DataAccessLayerException {
        Session sess = null;
        RetrievalRequestRecord rval = null;

        try {
            sess = template.getSessionFactory().getCurrentSession();

            final String minPriHql = "select min(rec.priority) from RetrievalRequestRecord rec "
                    + "where rec.state = :statePending and rec.network = :network";
            final String minInsertHql = "select min(rec.insertTime) from RetrievalRequestRecord rec "
                    + "where rec.state = :statePending and rec.priority = :minPri and rec.network = :network";
            // descending record order to retrieve all for a given subscription
            // before moving to the next one if two have the same
            // priority/insertTime
            final String pkHql = "select rec.id.subscriptionName, min(rec.id.index) from RetrievalRequestRecord rec "
                    + "where rec.state = :statePending and rec.priority = :minPri and rec.insertTime = :minInsert "
                    + "and rec.network = :network "
                    + "group by rec.id.subscriptionName order by min(rec.id.index) desc";

            Query minPriQuery = sess.createQuery(minPriHql);
            setQueryState(minPriQuery, State.PENDING);
            setQueryNetwork(minPriQuery, network);

            Query minInsertQuery = sess.createQuery(minInsertHql);
            setQueryState(minInsertQuery, State.PENDING);
            setQueryNetwork(minInsertQuery, network);

            Query pkQuery = sess.createQuery(pkHql);
            setQueryState(pkQuery, State.PENDING);
            setQueryNetwork(pkQuery, network);

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
        } catch (Exception e) {
            throw new DataAccessLayerException(
                    "Failed looking up next retrieval", e);
        }

        return rval;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void completeRetrievalRequest(RetrievalRequestRecord rec)
            throws DataAccessLayerException {
        try {
            update(rec);
        } catch (HibernateException e) {
            throw new DataAccessLayerException(
                    "Failed to update the database while changing the status on ["
                            + rec.getId() + "]" + " to [" + rec.getState()
                            + "]", e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean resetRunningRetrievalsToPending() {
        boolean rval = false;

        try {
            String hql = "update RetrievalRequestRecord rec set rec.state = :pendState where rec.state = :runState";

            Query query = template.getSessionFactory().getCurrentSession()
                    .createQuery(hql);
            query.setParameter("pendState", State.PENDING);
            query.setParameter("runState", State.RUNNING);
            query.executeUpdate();
            rval = true;
        } catch (Exception e) {
            statusHandler.error(
                    "Unable to reset old RUNNING retrievals to PENDING", e);
        }

        return rval;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<State, Integer> getSubscriptionStateCounts(String subName)
            throws DataAccessLayerException {
        Map<State, Integer> rval = new HashMap<State, Integer>(8);

        try {
            String hql = "select rec.state, count(rec.id.subscriptionName) from RetrievalRequestRecord rec "
                    + "where rec.id.subscriptionName = :subName group by rec.state";
            Query query = template.getSessionFactory().getCurrentSession()
                    .createQuery(hql);
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
        } catch (Exception e) {
            throw new DataAccessLayerException(
                    "Failed check pending/running retrieval count for subscription ["
                            + subName + "]", e);
        }

        return rval;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    @SuppressWarnings("unchecked")
    public List<RetrievalRequestRecord> getFailedRequests(String subName)
            throws DataAccessLayerException {
        try {
            Criteria query = template.getSessionFactory().getCurrentSession()
                    .createCriteria(RetrievalRequestRecord.class);
            query.add(Restrictions.eq("state", State.FAILED));
            query.add(Restrictions.eq("id.subscriptionName", subName));
            List<RetrievalRequestRecord> rval = query.list();
            return rval;
        } catch (Exception e) {
            throw new DataAccessLayerException(
                    "Failed check pending/running retrieval count for subscription ["
                            + subName + "]", e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean removeSubscription(String subName)
            throws DataAccessLayerException {
        boolean rval = false;

        try {
            String hql = "delete from RetrievalRequestRecord rec "
                    + "where rec.id.subscriptionName = :subName";
            Query query = template.getSessionFactory().getCurrentSession()
                    .createQuery(hql);
            query.setString("subName", subName);
            query.executeUpdate();
            rval = true;
        } catch (Exception e) {
            throw new DataAccessLayerException(
                    "Failed removing retrievals for subscription [" + subName
                            + "]", e);
        }
        return rval;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    @SuppressWarnings("unchecked")
    public List<RetrievalRequestRecord> getRequests(String subName)
            throws DataAccessLayerException {
        try {
            Criteria query = template.getSessionFactory().getCurrentSession()
                    .createCriteria(RetrievalRequestRecord.class);
            query.add(Restrictions.eq("id.subscriptionName", subName));
            List<RetrievalRequestRecord> rval = query.list();
            return rval;
        } catch (Exception e) {
            throw new DataAccessLayerException(
                    "Failed to return retrieval records for subscription ["
                            + subName + "]", e);
        }
    }

    /**
     * @param query
     * @param state
     */
    private void setQueryState(Query query, State state) {
        query.setParameter("statePending", state);
    }

    /**
     * @param query
     * @param network
     */
    private void setQueryNetwork(Query query, Network network) {
        query.setParameter("network", network);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Class<RetrievalRequestRecord> getEntityClass() {
        return RetrievalRequestRecord.class;
    }

}
