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

import java.util.List;

import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.Transaction;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.Restrictions;

import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.parameter.request.GetParameterRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * 
 * Cache the parameters defined in the database to avoid repeated lookups.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 7, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GetParameterHandler implements
        IRequestHandler<GetParameterRequest> {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GetParameterHandler.class);

    private final CoreDao dao;

    public GetParameterHandler() {
        dao = new CoreDao(DaoConfig.forClass(Parameter.class));
    }

    private Parameter checkDatabase(Parameter parameter, boolean create) {
        Parameter rval = null;

        Session sess = null;
        Transaction trans = null;
        for (int tries = 0; tries < 2 && rval == null; tries++) {
            // If this fails the most likely cause is that another node in the
            // cluster is trying to insert at the same time. Trying again will
            // work without errors, if it turns out this was a real problem then
            // the second try will catch it and log it.
            try {
                sess = dao.getSessionFactory().openSession();
                trans = sess.beginTransaction();

                Criteria crit = sess.createCriteria(Parameter.class);

                Criterion nameCrit = Restrictions.eq("abbreviation",
                        parameter.getAbbreviation());
                crit.add(nameCrit);
                List<?> vals = crit.list();

                if (vals.size() > 0) {
                    rval = (Parameter) vals.get(0);
                } else if (create) {
                    sess.saveOrUpdate(parameter);
                    rval = parameter;
                }

                trans.commit();
            } catch (Exception e) {
                if (tries > 0) {
                    // Don't log errors on the first try.
                    statusHandler.error("Error occurred looking up Parameter["
                            + parameter.getAbbreviation() + "]", e);
                }
                if (trans != null) {
                    try {
                        trans.rollback();
                    } catch (Exception e1) {
                        statusHandler.error(
                                "Error occurred rolling back transaction", e);
                    }
                }
            } finally {
                if (sess != null) {
                    try {
                        sess.close();
                    } catch (Exception e) {
                        statusHandler
                                .error("Error occurred closing session", e);
                    }
                }
            }
        }

        return rval;
    }

    @Override
    public Parameter handleRequest(GetParameterRequest request) {
        return checkDatabase(request.getParameter(), request.getCreate());
    }
}
