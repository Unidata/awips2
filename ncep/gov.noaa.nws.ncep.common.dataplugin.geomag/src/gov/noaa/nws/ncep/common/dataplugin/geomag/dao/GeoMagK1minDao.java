package gov.noaa.nws.ncep.common.dataplugin.geomag.dao;

import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagK1min;

import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * Record implementation for geomag k1minDao. 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer           Description
 * ------------ ---------- ----------------   --------------------------
 * 08/14/2013   T989       qzhou              Initial creation.
 * </pre>
 * 
 * @author qzhou
 * @version 1.0
 */

public class GeoMagK1minDao extends CoreDao {
	/**
     * Creates a new GribModelDao
     */
    public GeoMagK1minDao() {
        super(DaoConfig.forClass(GeoMagK1min.class));
    }
    
    /**
     * Retrieves a GeoMagAvgId based on the given id
     *
     * @param id
     *            The given ID number
     * @return The GeoMagAvgId
     */
    public GeoMagK1min queryById(int id) {
        return (GeoMagK1min) super.queryById(id);
    }

    public int getAreaId (int id){
        return queryById(id).getId();
    }
}

