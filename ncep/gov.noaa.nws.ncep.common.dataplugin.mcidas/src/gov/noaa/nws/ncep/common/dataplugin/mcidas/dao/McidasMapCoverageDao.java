/**
 * This Data Access Object (DAO) is used to interact with the database 
 * to get McIDAS map coverage.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 10/2009      144			Tiros Lee	Created
 * </pre>
 * 
 * @author tlee
 * @version 1
 */

package gov.noaa.nws.ncep.common.dataplugin.mcidas.dao;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;


import gov.noaa.nws.ncep.common.dataplugin.mcidas.McidasMapCoverage;


public class McidasMapCoverageDao extends CoreDao {

    /**
     * Creates a new McidasMapCoverageDao
     */
    public McidasMapCoverageDao() {
        super(DaoConfig.forClass(McidasMapCoverage.class));

    }

    /**
     * To retrieve a McidasMapCoverage from the database given a map ID
     * 
     * @param mapId
     *            The Map ID
     * @return A McidasMapCoverage object with the corresponding ID. Null if
     *         not found.
     */
    public McidasMapCoverage queryByMapId(String mapId) {
        return (McidasMapCoverage) this.queryById(mapId);
    }

    /**
     * Retrieves all McIDAS map coverage windows for a given type.
     * These types include IR, Visible, and Water Vapor
     * 
     * @param type
     *            The type of McIDAS image
     * @return A list of McIDAS map coverage areas for the given type
     */
    @SuppressWarnings("unchecked")
    public List<McidasMapCoverage> queryByType(String type) throws 
    		DataAccessLayerException {
        return (List<McidasMapCoverage>) queryBySingleCriteria("type", type);
    }

    /**
     * Retrieves all McIDAS map coverage windows with the given dimensions
     * 
     * @param nx
     *            The number of pixels
     * @param ny
     *            The number of lines
     *            
     * @return A list of McIDAS maps matching the given dimensions
     */
    @SuppressWarnings("unchecked")
    public List<McidasMapCoverage> queryByDimension(Integer nx, Integer ny) 
    		throws DataAccessLayerException{
        List<String> fields = new ArrayList<String>();
        List<Object> values = new ArrayList<Object>();
        fields.add("nx");
        values.add(String.valueOf(nx));
        fields.add("ny");
        values.add(String.valueOf(ny));
        return (List<McidasMapCoverage>) queryByCriteria(fields, values);
    }

    /**
     * Retrieves a map projection based on the given criteria
     * 
     * @param mapProjection
     *            The map projection 1=Mercator 3=Lambert Conformal 5=Polar
     *            Stereographic
     * @param nx
     *            Number of points along the x-axis
     * @param ny
     *            Number of points along the y-axis
     * @param dx
     *            The horizontal resolution of the grid
     * @param dy
     *            The vertical resolution of the grid
     * @param clon
     *            The central logitude
     * @param latin
     *            The tangent latitude
     * @param dlat1
     *            The latitude of the first grid point
     * @param dlon1
     *            The longitude of the first grid point
     * @param dlat2
     *            The latitude of the last grid point (only used with Mercator
     *            projection)
     * @param dlon2
     *            The longitude of the last grid point (only used with Mercator
     *            projection)
     * @return The SatMapCoverage object matching the given criteria
     */
    @SuppressWarnings("unchecked")
    public McidasMapCoverage getSatCoverage(Integer mapProjection, Integer nx,
            Integer ny, Float dx, Float dy, Float clon, Float stdlat1, Float stdlat2, 
            Float lllat, Float lllon, Float urlat, Float urlon) 
    		throws DataAccessLayerException{    	
        List<McidasMapCoverage> queryResults = null;
        List<String> fields = new ArrayList<String>();
        List<Object> values = new ArrayList<Object>();       
        DatabaseQuery query = new DatabaseQuery(daoClass.getName());
        query.addQueryParam("projection", mapProjection);
        query.addQueryParam("nx",nx);
        query.addQueryParam("ny",ny);
        query.addQueryParam("dx",dx);
        query.addQueryParam("dy",dy);
        query.addQueryParam("clon",clon);
        query.addQueryParam("stdlat1",stdlat1);
        query.addQueryParam("stdlat2",stdlat2);
        query.addQueryParam("lllat",lllat);
        query.addQueryParam("lllon",lllon);

        if (mapProjection == 1) {
            query.addQueryParam("urlat",urlat);
            query.addQueryParam("urlon",urlon);
        }

        queryResults = (List<McidasMapCoverage>) queryByCriteria(query);
        if (queryResults != null) {
            if (queryResults.size() > 1) {
                StringBuffer out = new StringBuffer();
                out.append("Multiple map coverages return using the following criteria: [");
                for (int i = 0; i < fields.size(); i++) {
                    out.append(fields.get(i)).append("=").append(values.get(i)).append(" ");
                }
                out.append("] -- Using first result");
                logger.debug(out.toString());
            }
            if (queryResults.size() >= 1) {
                return queryResults.get(0);
            } else {
                return null;
            }
        } else {
            return null;
        }
    }

    /**
     * Check for an existing map coverage for this native projection
     * @param iproj
     * @param nx Number of points along the x-axis
     * @param ny Number of points along the y-axis
     * @param upperLeftElement image element coordinate of area line 0, element 0
     * @param upperLeftLine image line coordinate of area line 0, element 0
     * @param xres Element resolution
     * @param yres Line resolution
     * @param encodedNav Base64 encoded NAV block
     * @return
     * @throws DataAccessLayerException
     */
	public McidasMapCoverage getSatCoverage(Integer iproj, Integer nx,
			Integer ny, int upperLeftElement, int upperLeftLine, int xres,
			int yres, String encodedNav) throws DataAccessLayerException {

		McidasMapCoverage coverage = null;
		List<McidasMapCoverage> queryResults = null;
		
        DatabaseQuery query = new DatabaseQuery(daoClass.getName());
        query.addQueryParam("projection", iproj);
        query.addQueryParam("nx",nx);
        query.addQueryParam("ny",ny);
        query.addQueryParam("upperLeftElement",upperLeftElement);
        query.addQueryParam("upperLeftLine",upperLeftLine);
        query.addQueryParam("elementRes",xres);
        query.addQueryParam("lineRes",yres);
        
        queryResults = (List<McidasMapCoverage>) queryByCriteria(query);
        //System.out.println("searching through "+queryResults.size()+" results.");
        if (queryResults != null) {
        	for ( McidasMapCoverage current : queryResults ) {
        		if ( current.getCrsWKT().contains(encodedNav) ) {
        			coverage = current;
        			//System.out.println("FOUND COVERAGE IN mcidas_spatial!!!!!");
        			break;      // use first one
        		}
        	}
        }
        //if ( coverage == null ) System.out.println("COULDNT FIND Existing COVERAGE");
        
		return coverage;
	}
}
