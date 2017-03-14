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
package com.raytheon.uf.edex.plugin.ldadmesonet.dao;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.ldadmesonet.MesonetLdadRecord;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataquery.db.QueryParam;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.pointdata.spatial.ObStation;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.query.DatabaseQuery;
import com.raytheon.uf.edex.pointdata.PointDataPluginDao;
import com.raytheon.uf.edex.pointdata.spatial.ObStationDao;

/**
 * Data access object for accessing LDAD MESONET records in the database.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 9/04/09                  vkorolev    Initial creation
 * 10/09/09		DR2814		vkorolev	Refactor to Point Data Model
 * Feb 27, 2013 1638        mschenke    Moved ObStationDao to edex pointdata plugin
 * Aug 15, 2014 3530        bclement    moved from common to edex
 * 
 * </pre>
 * 
 * @author vkorolev
 * @version 1
 */

public class LdadMesonetDao extends PointDataPluginDao<MesonetLdadRecord> {
	
	public LdadMesonetDao(String pluginName) throws PluginException {
		super(pluginName);
	}
	
	public LdadMesonetDao() throws PluginException, SQLException {
		this("ldadmesonet");
	}
	
    protected IDataStore populateDataStore(IDataStore dataStore,
            IPersistable obj) throws Exception {
        // TODO Auto-generated method stub
        return null;
    }
    
    /** The station dao */
    private ObStationDao obDao = new ObStationDao();

    public List<?> queryBySpatialBox(double upperLeftLat, double upperLeftLon,
            double lowerRightLat, double lowerRightLon)
            throws DataAccessLayerException {

        List<ObStation> stationList = obDao.queryBySpatialBox(upperLeftLat,
                upperLeftLon, lowerRightLat, lowerRightLon);

        List<String> stationNames = new ArrayList<String>();
         for(ObStation station:stationList){
         stationNames.add((String)station.getIdentifier());
         }
        stationList.clear();

        DatabaseQuery query = new DatabaseQuery(MesonetLdadRecord.class);
        query.addQueryParam("location.stationId", stationNames,
                QueryParam.QueryOperand.IN);
        return queryByCriteria(query);
    }

    public List<?> queryByState(String state, Integer count)
            throws DataAccessLayerException {

         List<ObStation> results = obDao.queryByState(state);

        ArrayList<String> icaos = new ArrayList<String>();
         for (ObStation station : results) {
         icaos.add((String)station.getIdentifier());
         }

        DatabaseQuery query = new DatabaseQuery(MesonetLdadRecord.class, count);
        query.addQueryParam("location.stationId", icaos, QueryParam.QueryOperand.IN);
        return queryByCriteria(query);
    }
	
	/**
	 * Retrieves an ldadMesonet report using the datauri .
	 * 
	 * @param dataURI
	 *            The dataURI to match against.
	 * @return The report record if it exists.
	 */
	public MesonetLdadRecord queryByDataURI(String dataURI) {
		MesonetLdadRecord report = null;
		List<?> obs = null;
		try {
			obs = queryBySingleCriteria("dataURI", dataURI);
		} catch (DataAccessLayerException e) {
			e.printStackTrace();
		}
		if ((obs != null) && (obs.size() > 0)) {
			report = (MesonetLdadRecord) obs.get(0);
		}
		return report;
	}


    /**
     * Queries for to determine if a given data uri exists on the redbook table.
     * 
     * @param dataUri
     *            The DataURI to find.
     * @return An array of objects. If not null, there should only be a single
     *         element.
     */
    public Object[] queryDataUriColumn(final String dataUri) {

        String sql = "select datauri from awips.ldadmesonet where datauri='"
                + dataUri + "';";

        Object[] results = executeSQLQuery(sql);

        return results;
    }
    
    public ObStationDao getObDao() {
        return obDao;
    }

    public void setObDao(ObStationDao obDao) {
        this.obDao = obDao;
    }

    @Override
    public String[] getKeysRequiredForFileName() {
        return new String[] { "dataTime.refTime" };
    }

    @Override
    public String getPointDataFileName(MesonetLdadRecord p) {
        return "ldadmesonet.h5";
    }

    @Override
    public MesonetLdadRecord newObject() {
        return new MesonetLdadRecord();
    }
}
