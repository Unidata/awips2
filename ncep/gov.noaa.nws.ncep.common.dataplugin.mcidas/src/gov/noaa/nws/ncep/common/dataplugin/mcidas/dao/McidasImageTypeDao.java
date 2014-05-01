/**
 * This Data Access Object is used to interact with the database to get satellite 
 * name for a McIDAS area file.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * ------------ ---------- 	----------- --------------------------
 * 10/2009		144			T. Lee		Created
 * </pre>
 * 
 * @author tlee
 * @version 1.0
 */

package gov.noaa.nws.ncep.common.dataplugin.mcidas.dao;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

import gov.noaa.nws.ncep.common.dataplugin.mcidas.fixed.McidasImageType;

public class McidasImageTypeDao extends CoreDao {

	/**
	 * Constructs a new McidasImageTypeDao
	 */
	public McidasImageTypeDao() {
		super(DaoConfig.forClass(McidasImageType.class));
	}

	/**
	 * Retrieves a McidasImageType by the given satelliteId and imageNumber
	 * 
	 * @param satelliteId
	 *            The satellite ID satelliteId
	 * @param imageNumber
	 * 			  The image type number
	 * @return A list of McIDAS image type based on satellite ID and image number  
	 */
	@SuppressWarnings("unchecked")
    public List<McidasImageType> queryBySatelliteIdAndImageNumber(String satelliteId, 
    		String imageNumber) {
    	List<String> fields = new ArrayList<String>();
    	List<Object> values = new ArrayList<Object>();
    	fields.add("satelliteId");
    	values.add(satelliteId);
    	fields.add("imageTypeNumber");
    	values.add(imageNumber);
  
    	try {
			return (List<McidasImageType>) queryByCriteria(fields, values);
		} catch (DataAccessLayerException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
    }

    public String getImageTypeName(String satelliteId, String imageNumber){
    	List<McidasImageType> imageType = new ArrayList<McidasImageType>();
        imageType = queryBySatelliteIdAndImageNumber(satelliteId, imageNumber); 
        return imageType.get(0).getImageType();
    }
}