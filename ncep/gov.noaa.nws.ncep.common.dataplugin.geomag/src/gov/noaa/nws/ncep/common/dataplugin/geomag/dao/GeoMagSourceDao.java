package gov.noaa.nws.ncep.common.dataplugin.geomag.dao;

/**
 * This Data Access Object is used to interact with the database to get geomag source
 * ID numbers and source names for a geomag data.
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

import java.util.List;

import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

import gov.noaa.nws.ncep.common.dataplugin.geomag.fixed.GeoMagSource;

public class GeoMagSourceDao extends CoreDao {

	/**
	 * Constructs a new GeoMagSourceDao
	 */
	public GeoMagSourceDao() {
		super(DaoConfig.forClass(GeoMagSource.class));
	}

    /**
     * Retrieves a GeoMagSource based on the given source id
     *
     * @param sourceId
     *            The source ID number
     * @return The GeoMagSource with the given ID
     */
    public GeoMagSource queryById(int sourceId) {
        return (GeoMagSource) super.queryById(sourceId);
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
    public String getSourceName (int sourceId){
        return queryById(sourceId).getSourceName();
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

}