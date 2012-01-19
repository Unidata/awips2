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

import com.raytheon.edex.util.satellite.SatellitePosition;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * Data Access Object for retrieving satellie positions
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 1/10/2010    4335       bphillip    Initial Creation
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class SatellitePositionDao extends CoreDao {

    public SatellitePositionDao(){
        super(DaoConfig.forClass(SatellitePosition.class));
    }
    
    public SatellitePosition queryById(String satelliteName){
        return (SatellitePosition)super.queryById(satelliteName);
    }
}
