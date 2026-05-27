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
package com.raytheon.uf.edex.parameter;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.hibernate.Criteria;
import org.hibernate.JDBCException;
import org.hibernate.Session;
import org.hibernate.Transaction;

import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.parameter.localization.ParameterLocalizationLookup;
import com.raytheon.uf.common.parameter.localization.ParameterLocalizationLookup.ParameterLocalizationListener;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * 
 * Task which synchronizes all the Parameter Names defined in localization with
 * the parameters defined in the database. An initial synchronization is
 * performed during construction and it continues to listen for changes to
 * localization files until {@link #close()} is called.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Oct 04, 2016  5890     bsteffen  Initial creation
 * 
 * </pre>
 *
 * @author bsteffen
 */
public class ParameterUpdater implements AutoCloseable {

    /**
     * The load limit is above the current reasonable limit for the number of
     * parameters in the system so this should never occur, however in the event
     * that the database explodes it is better to disable the updating rather
     * than run the JVM out of memory trying to load them all. If there is ever
     * a useful use case for having this many parameters then this class will
     * need to be rewritten.
     */
    private static final int LOAD_LIMIT = 20000;

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ParameterUpdater.class);

    private final ParameterLocalizationLookup fileLookup;

    public ParameterUpdater() {
        fileLookup = new ParameterLocalizationLookup();
        fileLookup.addListener(new ParameterLocalizationListener() {

            @Override 
            public void parametersChanged() {
                updateDatabase();
            }
        });
        updateDatabase();
    }

    protected synchronized void updateDatabase() {
        CoreDao dao = new CoreDao(DaoConfig.forClass(Parameter.class));
        Session session = null;
        try {
            session = dao.getSession();
            /*
             * In a clustered environment there may be multiple nodes attempting
             * the same update, a retry will usually succeed.
             */
            for (int retries = 2; retries >= 0; retries -= 1) {
                try {
                    updateDatabase(session);
                    break;
                } catch (JDBCException e) {
                    if (retries == 0) {
                        throw e;
                    }
                }
            }
        } catch (JDBCException e) {
            statusHandler.error(
                    "Failed to update parameter table from definition files.",
                    e);
        } finally {
            if (session != null) {
                session.close();
            }
        }
    }

    /**
     * Internal logic to update the database in a single transaction.
     */
    private void updateDatabase(Session session) {
        Transaction transaction = null;
        try {
            transaction = session.beginTransaction();
            Map<String, Parameter> dbMap = loadAllParameters(session);
            for (Parameter parameter : fileLookup.getAllParameters()) {
                Parameter dbParam = dbMap.get(parameter.getAbbreviation());
                if (dbParam == null) {
                    session.save(parameter);
                } else {
                    if (!dbParam.getName().equals(parameter.getName())) {
                        dbParam.setName(parameter.getName());
                        session.update(dbParam);
                    }
                }
            }
            transaction.commit();
            transaction = null;
        } finally {
            if (transaction != null) {
                transaction.rollback();
            }
        }
    }

    private static Map<String, Parameter> loadAllParameters(Session session) {
        Criteria criteria = session.createCriteria(Parameter.class);
        criteria.setMaxResults(LOAD_LIMIT);
        List<?> parameters = criteria.list();
        if (parameters.size() == LOAD_LIMIT) {
            throw new IllegalStateException(
                    "Database contains too many parameters, updates from localization files will be disabled.");
        }
        Map<String, Parameter> mappedResults = new HashMap<>();
        for (Object parameterObject : parameters) {
            Parameter parameter = (Parameter) parameterObject;
            mappedResults.put(parameter.getAbbreviation(), parameter);
        }
        return mappedResults;
    }

    @Override
    public void close() {
        fileLookup.close();
    }

}
