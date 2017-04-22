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

package com.raytheon.uf.edex.plugin.text.dao;

import java.util.List;

import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.Transaction;
import org.hibernate.criterion.Restrictions;

import com.raytheon.uf.common.dataplugin.text.db.TextProductInfo;
import com.raytheon.uf.common.dataplugin.text.db.TextProductInfoPK;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * The dao implementation associated with the TextDao classes used for all
 * database interaction.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 09/04/07     400         garmendariz Initial Check in
 * May 20, 2014 2536        bclement    moved from edex.textdb to edex.plugin.text
 * 
 * </pre>
 * 
 * @author garmendariz
 * @version 1
 */

public class TextProductInfoDao extends CoreDao {

    public TextProductInfoDao() {
        super(DaoConfig.forClass("fxa", TextProductInfo.class));
    }

    @SuppressWarnings("unchecked")
    public List<TextProductInfo> getAllTextProductInfo() {
        List<TextProductInfo> rval = null;
        Session sess = null;
        Transaction trans = null;

        try {
            sess = getSessionFactory().openSession();
            trans = sess.beginTransaction();

            Criteria crit = sess.createCriteria(TextProductInfo.class);
            rval = (List<TextProductInfo>) crit.list();

            trans.commit();
        } catch (Exception e) {
            logger.error(
                    "Error occurred looking up all TextProductInfo entries", e);

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

        return rval;
    }

    /**
     * Query for a record based on the ccid, nnnid and xxxid with the
     * 
     * @param cccid
     * @param nnnid
     * @param xxxid
     * @return
     * @throws DataAccessLayerException
     */
    @SuppressWarnings("unchecked")
    public TextProductInfo find(String cccid, String nnnid, String xxxid)
            throws DataAccessLayerException {

        TextProductInfo match = null;

        Session sess = null;
        Transaction trans = null;
        try {
            sess = getSessionFactory().openSession();
            trans = sess.beginTransaction();

            Criteria crit = sess.createCriteria(TextProductInfo.class);
            crit.add(Restrictions.eq("prodId", new TextProductInfoPK(cccid,
                    nnnid, xxxid)));
            crit.setMaxResults(1);
            List<TextProductInfo> queryResult = (List<TextProductInfo>) crit
                    .list();
            if (queryResult.size() > 0) {
                match = (TextProductInfo) queryResult.get(0);
            }
            trans.commit();
        } catch (Exception e) {
            logger.error(
                    "Error occurred looking up all TextProductInfo entries", e);

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

        return match;
    }

    /**
     * Write the TextProductInfo to the datastore.
     * 
     * @param textProduct
     * @return
     */
    public boolean write(TextProductInfo textProductInfo) {
        boolean success = false;
        try {
            saveOrUpdate(textProductInfo);
            success = true;
        } catch (Exception e) {
            logger.error("Error storing text product info!", e);
        }
        return success;
    }

    public void purgeTable() {
        executeSQLUpdate("truncate table "
                + TextProductInfo.class.getSimpleName());
    }
}
