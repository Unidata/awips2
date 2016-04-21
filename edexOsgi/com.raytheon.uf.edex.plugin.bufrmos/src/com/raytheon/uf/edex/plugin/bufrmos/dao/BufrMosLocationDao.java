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

package com.raytheon.uf.edex.plugin.bufrmos.dao;

import com.raytheon.uf.common.dataplugin.bufrmos.common.BufrMosDataLocation;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * Data access object for retrieving GribModel objects from the database
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 4/7/09       1994        bphillip    Initial Creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class BufrMosLocationDao extends CoreDao {

    /**
     * Creates a new GribModelDao
     */
    public BufrMosLocationDao() {
        super(DaoConfig.forClass(BufrMosDataLocation.class));
    }

    /**
     * Checks the database to see if a location matching the provided location
     * exists already
     * 
     * @param model
     *            The model to check
     * @return The model object from the database.
     * @throws DataAccessLayerException
     *             If problems occur while querying
     */
    public BufrMosDataLocation checkLocation(BufrMosDataLocation location)
            throws DataAccessLayerException {
        if (location.getId() == null) {
            location.generateId();
        }
        return (BufrMosDataLocation) this.queryById(location.getId());
    }
}
