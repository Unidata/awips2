package gov.noaa.nws.ncep.common.dataplugin.geomag.dao;

/**
 * This Data Access Object is used to interact with the database to get geomag source preference
 * ID numbers and source preference order for a geomag data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * ------------ ---------- 	----------- --------------------------
 * 04/2013		975			S. Gurung	Initial Creation
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */

import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

import gov.noaa.nws.ncep.common.dataplugin.geomag.fixed.GeoMagSourcePreference;

public class GeoMagSourcePreferenceDao extends CoreDao {

	/**
	 * Constructs a new GeoMagSourcePreferenceDao
	 */
	public GeoMagSourcePreferenceDao() {
		super(DaoConfig.forClass(GeoMagSourcePreference.class));
	}

    /**
     * Retrieves a GeoMag source preference based on the given source preference id
     *
     * @param preferenceId
     *            The source preference ID number
     * @return The GeoMagSource
     */
    public GeoMagSourcePreference queryById(int preferenceId) {
        return (GeoMagSourcePreference) super.queryById(preferenceId);
    }

    public String getSourcePreferenceDesc (int preferenceId){
        return queryById(preferenceId).getDescription();
    }

   
}