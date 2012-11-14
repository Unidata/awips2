/*
 * gov.noaa.nws.ncep.common.staticDataProvider.StaticDataProvider
 * 
 * 12 March 2012
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.staticdataprovider;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.MultiPolygon;

import gov.noaa.nws.ncep.common.staticdata.CostalWater;
import gov.noaa.nws.ncep.common.staticdata.Cwa;
import gov.noaa.nws.ncep.common.staticdata.FAArea;
import gov.noaa.nws.ncep.common.staticdata.FARegion;
import gov.noaa.nws.ncep.common.staticdata.GreatLake;
import gov.noaa.nws.ncep.common.staticdata.IStaticDataProvider;
import gov.noaa.nws.ncep.common.staticdata.Rfc;
import gov.noaa.nws.ncep.common.staticdata.SPCCounty;
import gov.noaa.nws.ncep.common.staticdata.USState;
import gov.noaa.nws.ncep.edex.common.stationTables.StationTable;
import gov.noaa.nws.ncep.viz.common.dbQuery.NcDirectDbQuery;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;

/**
 * Implementation of IStaticdataProvder interface. It contains methods that load static data. 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 02/12		?			B. Yin   	Moved from PGEN 
 * 06/12        734         J. Zeng     add getAllRfcs() and getAllCwas()
 * 08/12        #770        Q. Zhou     added loadContWatchNum()

 * </pre>
 * 
 * @author	B. Yin
 */

public class StaticDataProvider implements IStaticDataProvider {

	static private StaticDataProvider dataProvider;

	static public IStaticDataProvider getInstance(){
		if (dataProvider == null)  dataProvider = new StaticDataProvider();
		return dataProvider;
	}

	private StaticDataProvider(){

	}

	static public void start(){
	};

	@Override
	public StationTable getSfStnTbl() {
		return StationTableProvider.getSfStnTbl();
	}

	@Override
	public StationTable getAnchorTbl() {
		return StationTableProvider.getAnchorTbl();
	}

	@Override
	public StationTable getVorTbl() {
		return StationTableProvider.getVorTbl();
	}

	@Override
	public StationTable getVolcanoTbl() {
		return StationTableProvider.getVolcanoTbl();
	}

	@Override
	public HashMap<String, Set<String>> getClstTbl() {
		return StationTableProvider.getClstTbl();
	}

	@Override
	public List<SPCCounty> getSPCCounties(){
		return SPCCountyProvider.getSPCCounties();
	}

	@Override
	public SPCCounty findCounty(String fips) {
		return SPCCountyProvider.findCounty(fips);

	}

	@Override
	public List<SPCCounty> getCountiesInGeometry(Geometry geo) {
		return SPCCountyProvider.getCountiesInGeometry(geo);
	}

	@Override
	public String getPgenLocalizationRoot() {
		return NcPathConstants.PGEN_ROOT;
	}

	@Override
	public String getFileAbsolutePath(String fileLoczlizationPath) {
		return NcPathManager.getInstance().getStaticFile( fileLoczlizationPath ).getAbsolutePath();
	}

	@Override
	public File getFile(String fileLoczlizationPath) {
		return NcPathManager.getInstance().getStaticFile( fileLoczlizationPath );
	}

	@Override
	public LocalizationFile getStaticLocalizationFile(String fileName) {
		return NcPathManager.getInstance().getStaticLocalizationFile( fileName );

	}

	@Override
	public File getStaticFile(String fname) {
		return NcPathManager.getInstance().getStaticFile( fname );

	}

	@Override
	public File getGeogFile() {
		return NcPathManager.getInstance().getStaticFile(
				NcPathConstants.GEOG_TBL );
	}

	@Override
	public File getSfcStnFile() {
		return NcPathManager.getInstance().getStaticFile(
				NcPathConstants.SFSTNS_TBL );
	}

	@Override
	public LocalizationContext getLocalizationContext(LocalizationType type,
			LocalizationLevel level) {
		return NcPathManager.getInstance().getContext(type, level);
	}

	@Override
	public LocalizationFile getLocalizationFile(LocalizationContext context,
			String fileName) {
		return NcPathManager.getInstance().getLocalizationFile(context,
				fileName);
	}

	@Override
	public File getFirBoundsFile() {
		return NcPathManager.getInstance().getStaticFile(
				NcPathConstants.PGEN_FIR_BOUNDS) ;
	}

	@Override
	public List<Object[]> queryNcepDB(String field, String table) {
		List<Object[]> list = null;		
		String query = "Select AsBinary(the_geom_0_016), "+field+" FROM "+table;//mapdata.states";
		
		try {
			list = NcDirectDbQuery.executeQuery( query, "ncep", QueryLanguage.SQL);
		}
		catch (Exception e ){
			System.out.println("___ Error: SigmetCommAttrDlg: initAllStates(): "+e.getMessage());
			list = new ArrayList<Object[]>();
		}
		
		return list;
	}

	@Override
	public HashMap<String, String> getZoneMap() {
		
		//  query used to grab forecast zones from EDEX Database
		// TODO - move column/table names to config file so query is not hardwired in code
		String zoneQuery = "select state,zone,cwa from mapdata.zone;";
		
		HashMap<String, String> zoneMap = new HashMap<String,String>();
		QueryResult results = null;
		
		try {
		    results = NcDirectDbQuery.executeMappedQuery(zoneQuery, "maps", QueryLanguage.SQL);
		    QueryResultRow[] rows = results.getRows();
		    Map<String,Integer> columns = results.getColumnNames();
		    //System.out.println("column nmaes: "+columns.keySet().toString());
		    for ( QueryResultRow row : rows ) {
		    	Object state = row.getColumn( columns.get("state") );
		    	Object zone = row.getColumn( columns.get("zone") );
		    	Object cwa = row.getColumn( columns.get("cwa") );
		    	if ( state!=null && zone!=null && cwa!=null ) {
		    		zoneMap.put(state.toString()+"Z"+zone.toString(), cwa.toString());
		    	}
		    }
		}
		catch ( VizException ve ) {
			ve.printStackTrace();
		}
		
		return zoneMap;
	}

	@Override
	public List<USState> getAllstates() {
		return USStateProvider.getAllStates();
	}

	@Override
	public List<USState> loadStateTable(){
		return USStateProvider.loadStateTable();
	}
	
	@Override 
	public ArrayList<USState> statesInGeometry(Geometry geo) {
		return USStateProvider.statesInGeometry(geo);
	}
	
	
	@Override
	public List<Rfc> getAllRfcs() {
		return RfcProvider.getAllRfcs();
	}

	@Override
	public List<Rfc> loadRfcTable() {
		return RfcProvider.loadRfcTable();
	}

	@Override
	public ArrayList<Rfc> rfcsInGeometry(Geometry geo) {
		return RfcProvider.rfcsInGeometry(geo);
	}
	
	@Override
	public List<Cwa> getAllCwas() {
		return CwaProvider.getAllCwas();
	}

	@Override
	public List<Cwa> loadCwaTable() {
		return CwaProvider.loadCwaTable();
	}
	@Override
	public ArrayList<Cwa> cwasInGeometry(Geometry geo) {
		return CwaProvider.cwasInGeometry(geo);
	}
	
	@Override
	public HashMap<String, String> getStateAbrvMap() {
		HashMap<String, String> stMap = new HashMap<String,String>();
		String queryStates = "Select state,name FROM mapdata.states;";
		try {
			List<Object[]> states = NcDirectDbQuery.executeQuery(
					queryStates, "maps", QueryLanguage.SQL);

			for ( Object[] st : states ){
				if ( st[0] !=null && st[1] != null){
					stMap.put((String)st[0], (String)st[1]);
				}
			}
		}
		catch (Exception e ){
			System.out.println("db exception reading state tables!");	
			e.printStackTrace();
		}
		return stMap;
	}

	@Override
	public List<FAArea> getFAAreas() {
		return FAAreaProvider.getFAAreas();
	}

	@Override
	public List<FARegion> getFARegions() {
		return FARegionProvider.getFARegions();

	}

	@Override
	public List<FAArea> getFAAreaX() {
		return FAAreaProvider.getFAAreaX();

	}

	@Override
	public List<GreatLake> getGreatLakes() {
		return GreatLakeProvider.getGreatLakes();
	}

	@Override
	public List<CostalWater> getCostalWaters() {
		return CostalWaterProvider.getCostalWaters();

	}

	@Override
	public ArrayList<MultiPolygon> getG2GBounds(String tableAlias,
			String columnName, String columnValue) {
		return G2GBoundsProvider.getG2GBounds(tableAlias, columnName, columnValue);
	}

	@Override
	public boolean isRfcLoaded() {
		return RfcProvider.isRfcLoaded();
	}

	@Override
	public List<String> loadContWatchNum() {
		// TODO Auto-generated method stub
		List<String> watchNum = new ArrayList<String>();
		
		try {
			watchNum = ContinuingWatch.loadContWatchNum();
		} catch (VizException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return watchNum;
	}

}
