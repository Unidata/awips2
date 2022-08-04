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

package com.raytheon.uf.edex.registry.synchronization;

import java.util.ArrayList;
import java.util.List;

import org.hibernate.HibernateException;
import org.hibernate.Session;
import org.hibernate.Transaction;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.edex.registry.ebxml.dao.HqlQueryUtil;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ExtrinsicObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

/**
 * A Helper class used to perform local database query for a given session.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#   Engineer    Description
 * ------------- -------- --------- -----------------------
 * 12-31-2018    7238      skabasele   Initial creation
 * 07-09-2019    7889      skabasele   remove call to DeleteSlotEvent
 *
 * </pre>
 *
 * @author skabasele
 */

public class QueryLocalRegistryHelper {
    private Session session;

    protected final Logger logger = LoggerFactory.getLogger(getClass());

    public QueryLocalRegistryHelper(Session session) {
        this.session = session;

    }

    /**
     * Get the RegistryObject by Ids
     * 
     * @param Ids
     * @return
     */
    @SuppressWarnings("unchecked")
    public synchronized List<RegistryObjectType> getRegistryObjectByIds(
            List<String> Ids) {
        Transaction tx = null;
        List<RegistryObjectType> regObs = new ArrayList<>();
        try {
            tx = session.beginTransaction();

            StringBuilder str = new StringBuilder(
                    "select obj from RegistryObjectType obj where obj.id in ");
            HqlQueryUtil.assembleInClause(str, "Strings.value", Ids);

            regObs = session.createQuery(str.toString()).list();

            tx.commit();
        } catch (HibernateException e) {
            if (tx != null) {
                tx.rollback();
            }
            logger.info("Error occured while retrieving RegistryObject for ids "
                    + Ids.toString(), e);

        }

        return regObs;
    }

    /**
     * Deletes a persistent object
     * 
     * @param obj
     *            The persistent object to delete
     */
    public synchronized boolean deleteWithoutMerge(RegistryObjectType obj) {

        Transaction tx = null;
        boolean isSuccess = false;

        try {
            tx = session.beginTransaction();

            if (obj instanceof ExtrinsicObjectType) {
                ExtrinsicObjectType extrinsicObject = (ExtrinsicObjectType) obj;
                extrinsicObject.setRepositoryItem(null);
                session.update(obj);
            }

            session.delete(obj);

            tx.commit();
            isSuccess = true;
        } catch (HibernateException e) {
            if (tx != null) {
                tx.rollback();
            }
            logger.info("Error occured while deleting RegistryObject "
                    + (obj == null ? null : obj.getId()), e);

        }

        return isSuccess;
    }

}
