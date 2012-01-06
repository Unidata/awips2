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

import com.raytheon.edex.util.satellite.SatelliteSectorId;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * A satellite creating entity
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
public class SatelliteSectorIdDao extends CoreDao {

    /**
     * Construcst a new SectorIdDao
     */
    public SatelliteSectorIdDao() {
        super(DaoConfig.forClass(SatelliteSectorId.class));
    }

    /**
     * Retrieves a SatelliteSectorId based on the given sector id
     * 
     * @param sectorId
     *            The sector id number
     * @return The satellite sector
     */
    public SatelliteSectorId queryById(int sectorId) {
        return (SatelliteSectorId) super.queryById(sectorId);
    }
    
    public String getSectorIdName(int sectorId){
        return queryById(sectorId).getSectorName();
    }

}
