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
package com.raytheon.edex.db.dao;

import java.util.List;

import com.raytheon.edex.subscription.Subscription;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * EDEX Data Access Layer (DAL) Data Access Object (DAO) for interactions with
 * the subscription tables (awips.subscription and awips.scripts).
 * <P>
 * Operations provided are: <br>
 * {@link #persistSubscription(AbstractDataRecord) persist a subscription},
 * {@link #getSubscriptions() get list of available subscriptions},
 * {@link #removeSubscription(AbstractDataRecord) delete a subscription}, and
 * {@link #updateSubscription(AbstractDataRecord) update an existing
 * subscription}.
 * <p>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 27Apr2007    208         MW Fegan    Initial creation.
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1
 */

public class SubscribeDAO extends CoreDao {

    /**
     * 
     */
    public SubscribeDAO() {
        super(DaoConfig.forClass(Subscription.class));
    }

    /**
     * Updates (replaces) the specified data record in the database.
     * 
     * @param val
     *            the data record to update
     * 
     * @throws DataAccessLayerException
     *             if the update operation fails
     */
    public void updateSubscription(PersistableDataObject val)
            throws DataAccessLayerException {
        getHibernateTemplate().update(val);
    }

    /**
     * Gets the list of currently available subscriptions from the database.
     * 
     * @return the list of currently available subscriptions
     * 
     * @throws DataAccessLayerException
     *             if the query fails.
     */
    @SuppressWarnings("unchecked")
    public List<Subscription> getSubscriptions()
            throws DataAccessLayerException {
        // executeHQLQuery("select datauri from awips.subscription");
        return (List<Subscription>) getHibernateTemplate().loadAll(daoClass);
    }

    /**
     * Retrieves the Subscription object matching the specified data URI.
     * 
     * @param dataURI
     *            the data URI to match
     * 
     * @return the Subscription object
     * 
     * @throws DataAccessLayerException
     *             in the event of an error retrieving the Subscription object
     */
    public Object getSubscription(String dataURI)
            throws DataAccessLayerException {

        return this.queryBySingleCriteria("identifier", dataURI);
        /*
         * Object retVal = null; List result; try { result =
         * findSubscription(dataURI); } catch (Exception e) { throw new
         * DataAccessLayerException
         * ("Unable to get subscription information for " +
         * Util.printString(dataURI),e); } if (result.size() > 0) { retVal =
         * result.get(0); } else { throw new DataAccessLayerException("No
         * subscriptions exist for " + Util.printString(dataURI)); } return
         * retVal;
         */
    }

    /**
     * Returns the Subscription objects the matches the specified data URI.
     * 
     * @param dataURI
     *            The data URI of the Subscription
     * 
     * @return A list of Subscription objects
     * 
     * @throws DataAccessLayerException
     *             if the DB query failed
     */
    // private List<?> findSubscription(String dataURI)
    // throws DataAccessLayerException {
    // List<?> list = null;
    // StringBuffer buffer = new StringBuffer();
    // try {
    // // get the subscription class and key field
    // Configuration properties = PropertiesFactory.getInstance()
    // .getConfiguration(ISubscriptionManager.CONFIGURATION_NAME);
    // String className = properties
    // .getString(ISubscriptionManager.HIB_CLASS);
    // String keyField = properties
    // .getString(ISubscriptionManager.KEY_FIELD);
    // // setup the query
    // buffer.append(" from ");
    // buffer.append(className);
    // buffer.append(" where ");
    // buffer.append(keyField).append("=?");
    // HibernateTemplate template = getHibernateTemplate();
    // // retrieve the data
    // list = template.find(buffer.toString(), new Object[] { dataURI });
    // } catch (Exception e) {
    // throw new DataAccessLayerException(
    // "Unable to find metadata for the given "
    // + Util.printString(dataURI), e);
    // }
    //
    // return list;
    // }
}
