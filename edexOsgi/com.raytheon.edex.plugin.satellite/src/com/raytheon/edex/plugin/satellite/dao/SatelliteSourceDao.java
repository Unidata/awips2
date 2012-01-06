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

package com.raytheon.edex.plugin.satellite.dao;

import java.util.List;

import com.raytheon.edex.util.satellite.SatelliteSource;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * Data Access Object for interacting with satellite sources
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                         bphillip    Initial Creation
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class SatelliteSourceDao extends CoreDao {

    /**
     * Constructs a new SatelliteSourceDao
     */
    public SatelliteSourceDao() {
        super(DaoConfig.forClass(SatelliteSource.class));
    }

    /**
     * Retrieves a SatelliteSource given an id
     * 
     * @param sourceId
     *            The source id
     * @return The Satellite source with the given ID
     */
    public SatelliteSource queryById(int sourceId) {
        return (SatelliteSource) super.queryById(sourceId);
    }

    /**
     * Gets the source id given the name
     * 
     * @param sourceName
     *            The source name
     * @return The source id corresponding to the given name
     * @throws DataAccessLayerException
     *             If errors occur during query
     */
    @SuppressWarnings("unchecked")
    public Integer getSourceId(String sourceName)
            throws DataAccessLayerException {
        DatabaseQuery query = new DatabaseQuery(daoClass.getName());
        query.addReturnedField("sourceId");
        query.addQueryParam("sourceName", sourceName);
        List<Integer> sources = (List<Integer>) this.queryByCriteria(query);
        if (sources.isEmpty()) {
            return null;
        } else {
            return sources.get(0);
        }
    }

    /**
     * Gets the source name given the id
     * 
     * @param sourceId
     *            The source id
     * @return The source name
     * @throws DataAccessLayerException
     *             If errors occur during query
     */
    @SuppressWarnings("unchecked")
    public String getSourceName(int sourceId){
        return queryById(sourceId).getSourceName();
    }
}
