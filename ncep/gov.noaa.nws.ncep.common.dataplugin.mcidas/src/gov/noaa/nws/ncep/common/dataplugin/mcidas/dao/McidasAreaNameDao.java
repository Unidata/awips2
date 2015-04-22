package gov.noaa.nws.ncep.common.dataplugin.mcidas.dao;

/**
 * This Data Access Object is used to interact with the database to get area file 
 * ID numbers and area names for a McIDAS image data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * ------------ ---------- 	----------- --------------------------
 * 11/2009		144			T. Lee		Created
 * </pre>
 * 
 * @author tlee
 * @version 1.0
 */

import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

import gov.noaa.nws.ncep.common.dataplugin.mcidas.fixed.McidasAreaName;

public class McidasAreaNameDao extends CoreDao {

	/**
	 * Constructs a new McidasImageTypeDao
	 */
	public McidasAreaNameDao() {
		super(DaoConfig.forClass(McidasAreaName.class));
	}

    /**
     * Retrieves a McidasAreaId based on the given area file id
     *
     * @param areaId
     *            The area file ID number
     * @return The mcidas area name
     */
    public McidasAreaName queryById(int areaId) {
        return (McidasAreaName) super.queryById(areaId);
    }

    public String getAreaId (int areaId){
        return queryById(areaId).getAreaName();
    }

   
}