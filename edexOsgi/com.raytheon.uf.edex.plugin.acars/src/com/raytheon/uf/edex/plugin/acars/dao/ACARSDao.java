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
package com.raytheon.uf.edex.plugin.acars.dao;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

import org.hibernate.Query;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.TransactionCallback;

import com.raytheon.edex.db.dao.DefaultPluginDao;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.acars.ACARSRecord;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.edex.database.DataAccessLayerException;

/**
 * ACARDS dao.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 21, 2009       1939 jkorman     Initial creation
 * Oct 10, 2012 1261       djohnson    Add some generics wildcarding.
 * Nov 02, 2012 1302       djohnson    Add Javadoc.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class ACARSDao extends DefaultPluginDao {

    /**
     * Creates a new ReccoDao
     * 
     * @throws PluginException
     */
    public ACARSDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    /**
     * 
     * @throws PluginException
     */
    public ACARSDao() throws PluginException {
        this("acars");
    }

    /**
     * Retrieves an bufrua report using the datauri .
     * 
     * @param dataURI
     *            The dataURI to match against.
     * @return The report record if it exists.
     */
    public ACARSRecord queryByDataURI(String dataURI) {
        ACARSRecord report = null;
        List<?> obs = null;
        try {
            obs = queryBySingleCriteria("dataURI", dataURI);
        } catch (DataAccessLayerException e) {
            e.printStackTrace();
        }
        if ((obs != null) && (obs.size() > 0)) {
            report = (ACARSRecord) obs.get(0);
        }
        return report;
    }

    /**
     * 
     * @param example
     * @return
     */
    public List<ACARSRecord> queryByExample(ACARSRecord example) {

        List<ACARSRecord> retData = null;

        @SuppressWarnings("unchecked")
        List<PersistableDataObject<?>> results = super.queryByExample(example);
        if (results != null) {
            retData = new ArrayList<ACARSRecord>();
            for (PersistableDataObject<?> d : results) {
                retData.add((ACARSRecord) d);
            }
        }

        return retData;
    }

    /**
     * Executes an HQL query
     * 
     * @param hqlQuery
     *            The HQL query string
     * @return The list of objects returned by the query
     */
    public List<?> executeACARSQuery(final String hqlQuery) {

        List<?> result = (List<?>) txTemplate
                .execute(new TransactionCallback() {
                    @Override
                    public List<?> doInTransaction(TransactionStatus status) {
                        Query hibQuery = getSession(false)
                                .createQuery(hqlQuery);
                        // hibQuery.setCacheMode(CacheMode.NORMAL);
                        // hibQuery.setCacheRegion(QUERY_CACHE_REGION);

                        return hibQuery.list();
                    }
                });
        return result;
    }

    /**
     * Get all current reports for a given tailNumber newer than a specified
     * datetime.
     * 
     * @param tailNumber
     * @param startTime
     * @return
     */
    public List<ACARSRecord> getReports(String tailNumber, Calendar startTime) {

        String queryTemplate = "from ACARSRecord a "
                + "where a.tailNumber = '%s' and "
                + "a.timeObs > '%2$tY-%2$tm-%2$td %2$tH:%2$tM:%2$tS'";

        List<ACARSRecord> retData = null;

        String query = String.format(queryTemplate, tailNumber, startTime);

        if (logger.isDebugEnabled()) {
            logger.debug(query);
        }

        List<?> result = executeACARSQuery(query);

        if (result != null) {
            retData = new ArrayList<ACARSRecord>();
            for (Object o : result) {
                retData.add((ACARSRecord) o);
            }
        }

        return retData;
    }

    /**
     * Get all current reports for a given tailNumber newer than a specified
     * datetime.
     * 
     * @param tailNumber
     * @param startTime
     * @return
     */
    public List<ACARSRecord> getReports(String tailNumber, Calendar startTime,
            Calendar stopTime) {

        String queryTemplate = "from ACARSRecord a "
                + "where a.tailNumber = '%s' and "
                + "a.timeObs >= '%2$tY-%2$tm-%2$td %2$tH:%2$tM:%2$tS' and "
                + "a.timeObs <= '%3$tY-%3$tm-%3$td %3$tH:%3$tM:%3$tS'";

        List<ACARSRecord> retData = null;

        String query = String.format(queryTemplate, tailNumber, startTime,
                stopTime);

        if (logger.isDebugEnabled()) {
            logger.debug(query);
        }

        List<?> result = executeACARSQuery(query);

        if (result != null) {
            retData = new ArrayList<ACARSRecord>();
            for (Object o : result) {
                ACARSRecord r = (ACARSRecord) o;
                r.setPluginName(pluginName);
                retData.add(r);
            }
        }

        return retData;
    }

}
