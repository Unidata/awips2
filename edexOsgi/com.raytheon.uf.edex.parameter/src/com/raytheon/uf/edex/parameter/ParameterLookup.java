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
import org.hibernate.Session;
import org.hibernate.Transaction;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.Restrictions;

import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.parameter.ParameterDefinitions;
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
public class ParameterLookup {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ParameterLookup.class);

    private static ParameterLookup instance;

    public static ParameterLookup getInstance() {
        if (instance == null) {
            instance = new ParameterLookup();
        }
        return instance;
    }

    private final CoreDao dao;

    private final Map<String, Parameter> parameterMap = new HashMap<String, Parameter>();

    private ParameterLookup() {
        dao = new CoreDao(DaoConfig.forClass(Parameter.class));
        for (Parameter parameter : ParameterDefinitions.getParameters()) {
            lookupParameter(parameter, true);
        }
    }

    /**
     * lookup a parameter in the database and optionally add it if it does not
     * exist.
     * 
     * @param parameter
     * @param create
     * @return
     */
    public synchronized Parameter lookupParameter(Parameter parameter,
            boolean create) {
        synchronized (parameterMap) {
            Parameter result = parameterMap.get(parameter.getAbbreviation());
            if (result == null) {
                result = checkDatabase(parameter, true);
            }
            return result;
        }
    }

    private Parameter checkDatabase(Parameter parameter, boolean create) {
        Parameter rval = null;

        Session sess = null;
        Transaction trans = null;
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
            statusHandler.error("Error occurred looking up Parameter["
                    + parameter.getAbbreviation() + "]", e);

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
                    statusHandler.error("Error occurred closing session", e);
                }
            }
        }

        return rval;
    }
}
