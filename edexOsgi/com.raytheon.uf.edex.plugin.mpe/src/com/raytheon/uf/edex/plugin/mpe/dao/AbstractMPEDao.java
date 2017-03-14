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
package com.raytheon.uf.edex.plugin.mpe.dao;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import org.hibernate.Query;
import org.hibernate.Session;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.TransactionCallback;
import org.springframework.util.CollectionUtils;

import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * Abstract MPE Dao implementation. Provides a common interface to the Core DAO
 * and a common set of procedures (currently not available in the CoreDao or any
 * other abstraction thereof) for executing named queries.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 28, 2016 5614       bkowal      Initial creation
 * May 13, 2016 5576       bkowal      Implemented common retrieval methods based on
 *                                     named queries.
 * May 19, 2016 5576       bkowal      JavaDoc generic type params
 * May 20, 2016 5576       bkowal      Added {@link #findByNamedQueryAndNamedParams(String, String[], Object[])}.
 * Nov 10, 2016 5999       bkowal      Added {@link #findPartialRecordByNamedQuery(String)}.
 * 
 * </pre>
 * 
 * @author bkowal
 * @param <T>
 *            the Entity (annotated with the @Entity) tag that the DAO has been
 *            created for.
 * @param <I>
 *            the identifier of the Entity that the DAO has been created for.
 *            Must extend Serializable.
 */

public abstract class AbstractMPEDao<T, I extends Serializable>
        extends CoreDao {

    private final Class<T> entityClass;

    protected AbstractMPEDao(final String dbGenericName, Class<T> entityClass) {
        super(DaoConfig.forClass(dbGenericName, entityClass));
        this.entityClass = entityClass;
    }

    /*
     * The following functions are more or less from AbstractBMHDao (with
     * improvements). In the future, it would be better to provide a further
     * abstraction of CoreDao that supports named queries (unless we switch to
     * the Spring Data JPA in which case the named queries are defined above an
     * interface method using a Query annotation in which case custom common
     * methods would not be needed to execute the queries).
     */

    public List<T> findByNamedQuery(final String queryName) {
        return txTemplate.execute(new TransactionCallback<List<T>>() {
            @Override
            public List<T> doInTransaction(TransactionStatus status) {
                return castListResults(
                        getCurrentSession().getNamedQuery(queryName).list());
            }
        });
    }

    public List<T> findByNamedQueryAndNamedParam(final String queryName,
            final String name, final Object parameter) {
        return txTemplate.execute(new TransactionCallback<List<T>>() {
            @Override
            public List<T> doInTransaction(TransactionStatus status) {
                Session session = getCurrentSession();
                Query query = session.getNamedQuery(queryName);
                return castListResults(
                        query.setParameter(name, parameter).list());
            }
        });
    }

    public List<T> findByNamedQueryAndNamedParams(final String queryName,
            final String[] names, final Object[] parameters) {
        if ((names == null) || (parameters == null)
                || (names.length != parameters.length)) {
            throw new IllegalArgumentException(
                    "Length of parameter names and parameter value arrays must match!");

        }

        return txTemplate.execute(new TransactionCallback<List<T>>() {
            @Override
            public List<T> doInTransaction(TransactionStatus status) {
                Session session = getCurrentSession();
                Query query = session.getNamedQuery(queryName);
                for (int i = 0; i < names.length; i++) {
                    if (parameters[i] instanceof Collection) {
                        query.setParameterList(names[i],
                                (Collection<?>) parameters[i]);
                    } else {
                        query.setParameter(names[i], parameters[i]);
                    }
                }
                return castListResults(query.list());
            }
        });
    }

    public List<?> findPartialRecordByNamedQueryAndNamedCollection(
            final String queryName, final String inParameterName,
            final Set<? extends Serializable> inList) {
        if (CollectionUtils.isEmpty(inList)) {
            return Collections.emptyList();
        }

        return txTemplate.execute(new TransactionCallback<List<?>>() {
            @Override
            public List<?> doInTransaction(TransactionStatus status) {
                Session session = getCurrentSession();
                Query query = session.getNamedQuery(queryName);
                final List<?> objects = query
                        .setParameterList(inParameterName, inList).list();
                return (objects == null) ? Collections.emptyList() : objects;
            }
        });
    }

    public List<?> findPartialRecordByNamedQuery(final String queryName) {
        return txTemplate.execute(new TransactionCallback<List<?>>() {
            @Override
            public List<?> doInTransaction(TransactionStatus status) {
                final List<?> objects = getCurrentSession()
                        .getNamedQuery(queryName).list();
                return (objects == null) ? Collections.emptyList() : objects;
            }
        });
    }

    public T retrieveById(final I id) {
        return txTemplate.execute(new TransactionCallback<T>() {
            @Override
            public T doInTransaction(TransactionStatus status) {
                Object object = getCurrentSession().byId(entityClass).load(id);
                return (object == null) ? null : entityClass.cast(object);
            }
        });
    }

    protected List<T> castListResults(final List<?> objects) {
        if (objects == null || objects.isEmpty()) {
            return Collections.emptyList();
        }
        List<T> records = new ArrayList<>(objects.size());
        for (Object object : objects) {
            records.add(entityClass.cast(object));
        }
        return records;
    }
}