/**
 * This Data Access Object implements database query methods to get McIDAS 
 * satellite name.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * ------------ ---------- 	----------- --------------------------
 * T. Lee		144			T. Lee		Created
 * </pre>
 * 
 * @author tlee
 * @version 1.0
 */

package gov.noaa.nws.ncep.common.dataplugin.mcidas.dao;

import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

import gov.noaa.nws.ncep.common.dataplugin.mcidas.fixed.McidasSatelliteName;

public class McidasSatelliteNameDao extends CoreDao {

    /**
     * Constructs a new McidasSatelliteNameDao
     */
    public McidasSatelliteNameDao() {   	
        super(DaoConfig.forClass(McidasSatelliteName.class));
    }

    /**
     * Query satellite name by its ID.
     * 
     * @param satelliteId
     *            The satellite ID
     * @return The McidasSatelliteName
     */
    public McidasSatelliteName queryById(int satelliteId) {
        return (McidasSatelliteName) super.queryById(satelliteId);
    }
    
    public String getSatelliteId(int satelliteId) {
        return queryById(satelliteId).getSatelliteName();
    }
}