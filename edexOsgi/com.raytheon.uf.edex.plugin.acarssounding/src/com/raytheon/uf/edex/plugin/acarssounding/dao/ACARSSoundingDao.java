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
package com.raytheon.uf.edex.plugin.acarssounding.dao;

import java.util.ArrayList;
import java.util.List;

import org.hibernate.Query;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.TransactionCallback;

import com.raytheon.edex.db.dao.DefaultPluginDao;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.acarssounding.ACARSSoundingRecord;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.plugin.acarssounding.tools.ACARSSoundingTools;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 21, 2009       1939 jkorman     Initial creation
 * Aug 18, 2014 3530       bclement    removed warning from executeSoundingQuery()
 * 10/28/2014   3454        bphillip    Fix usage of getSession()
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class ACARSSoundingDao extends DefaultPluginDao {

    private static final String QUERY_TIMELIMITS = "from ACARSSoundingRecord a where "
            + "(a.tailNumber = '%s') and"
            + " (%d >= a.oldestTime - " + ACARSSoundingTools.TIMEOFFSET + ") and"
            + " (%d <= a.newestTime + " + ACARSSoundingTools.TIMEOFFSET + ")";

    /**
     * Creates a new ACARSProfilerDao
     * 
     * @throws PluginException
     */
    public ACARSSoundingDao(String pluginName) throws PluginException {
        super(pluginName);
    }

    /**
     * 
     * @throws PluginException
     */
    public ACARSSoundingDao() throws PluginException {
        this("acarssounding");
    }

    /**
     * Retrieves an bufrua report using the datauri .
     * 
     * @param dataURI
     *            The dataURI to match against.
     * @return The report record if it exists.
     */
    public ACARSSoundingRecord queryByDataURI(String dataURI) {
        ACARSSoundingRecord report = null;
        List<?> obs = null;
        try {
            obs = queryBySingleCriteria("dataURI", dataURI);
        } catch (DataAccessLayerException e) {
            e.printStackTrace();
        }
        if ((obs != null) && (obs.size() > 0)) {
            report = (ACARSSoundingRecord) obs.get(0);
        }
        return report;
    }

    /**
     * Executes an HQL query
     * 
     * @param hqlQuery
     *            The HQL query string
     * @return The list of objects returned by the query
     */
    public List<?> executeSoundingQuery(final String hqlQuery) {

        List<?> result = (List<?>) txTemplate
                .execute(new TransactionCallback<Object>() {
                    public List<?> doInTransaction(TransactionStatus status) {
                        Query hibQuery = getCurrentSession()
                                .createQuery(hqlQuery);
                        return hibQuery.list();
                    }
                });
        return result;
    }

    /**
     * 
     * @param obsTime
     * @return
     */
    public List<ACARSSoundingRecord> queryByTimeLimits(String tailNumber,
            Long startTime, Long stopTime) {
        List<ACARSSoundingRecord> retObs = null;

        String query = String.format(QUERY_TIMELIMITS, tailNumber, startTime,
                stopTime);

        if (logger.isPriorityEnabled(Priority.DEBUG)) {
            logger.debug(query);
        }

        List<?> result = executeSoundingQuery(query);

        if (result != null) {
            retObs = new ArrayList<ACARSSoundingRecord>();
            for (Object o : result) {
                retObs.add((ACARSSoundingRecord) o);
            }
        }
        return retObs;
    }
}
