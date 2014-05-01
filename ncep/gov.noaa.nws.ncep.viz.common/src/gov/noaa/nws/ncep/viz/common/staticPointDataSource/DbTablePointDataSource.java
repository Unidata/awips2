/**
 * example for cities station table mapping
 * 
 * This java class defines the getters and setters for the 
 *      
 * 
 * HISTORY
 *
 * Date     	Author		Description
 * ------------	----------	-----------	--------------------------
 * 01/2010		Uma Josyula	Initial creation	
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.viz.common.staticPointDataSource;


import gov.noaa.nws.ncep.viz.common.dbQuery.NcDirectDbQuery;
import gov.noaa.nws.ncep.viz.common.staticPointDataSource.IStaticPointDataSource.StaticPointDataSourceType;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.index.strtree.STRtree;


// this is for the ncep stns tables but we could/should either combine or replace this with 
// a source to read from the maps db.

public class DbTablePointDataSource extends AbstractPointDataSource {
		
	private final String dbName = "ncep";

	private String dbTableName = null;

	private String dbFieldName = null;
	
	private List<LabeledPoint> stationPoints = new ArrayList<LabeledPoint>();

	public DbTablePointDataSource( String srcName, String dbfld ) { 
//		dbName = dbn;
		dbTableName = srcName;
		dbFieldName = dbfld;
	}
	
	@Override
	public StaticPointDataSourceType getSourceType() {
		return StaticPointDataSourceType.NCEP_DB_TABLE;
	} 

	// TODO : we could add a flag to not load all the data in memory and instead
	// query the database each time.
	@Override
	public void loadData( ) throws VizException {

		// Order is important here since it determines order in the
		// queryResults
		//
		String query = "select latitude,longitude," +
						dbFieldName + " from " + dbTableName;
		
		QueryResult queryList;
		QueryResultRow[] queryResult = null;		

		//query the DB
		try {				
			queryList = NcDirectDbQuery.executeMappedQuery( query, dbName, QueryLanguage.SQL);
			queryResult= queryList.getRows();				
		}
		catch (Exception e ){
			System.out.println("___ Error: populateTrees() of LoadPointData: "+e.getMessage());	return;
		}

		Object[] os = null;			

		// populate Tree/pointList
		for( QueryResultRow rows : queryResult ){

			os = rows.getColumnValues();	
			//System.out.println( rows.getColumn(2).toString() );
			
			if(os == null || os.length == 0) 
				continue;

			if(  !(os[0] instanceof Double) ||
				 !(os[1] instanceof Double) || 
				 !(os[2] instanceof String) ) {
				System.out.println("sanity check: PointDataSource query has unexpected class?");
				continue;
			}
			
			LabeledPoint namedPoint = new LabeledPoint(
					(String)os[2], (Double)os[0], (Double)os[1] );
			
			insertPoint( namedPoint );
			stationPoints.add( namedPoint );
		}
	}


	@Override
	public List<LabeledPoint> getPointData() {
		return stationPoints;
	}

	@Override
	public Map<String, LabeledPoint> getPointDataByLabel() {
		return null;
	}

}
