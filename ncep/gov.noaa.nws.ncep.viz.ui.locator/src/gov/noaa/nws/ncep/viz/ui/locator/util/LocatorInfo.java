package gov.noaa.nws.ncep.viz.ui.locator.util;

import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.*;

import org.geotools.referencing.GeodeticCalculator;

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.index.strtree.STRtree;

import gov.noaa.nws.ncep.viz.common.dbQuery.NcDirectDbQuery;
import gov.noaa.nws.ncep.viz.localization.impl.LocalizationManager;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;
import gov.noaa.nws.ncep.viz.ui.locator.LocatorEditDialog;
import gov.noaa.nws.ncep.viz.ui.locator.db.LocatorQueryBuilder;
import gov.noaa.nws.ncep.viz.ui.locator.resource.*;


public class LocatorInfo {	
	
	public static List<Locator> testList = null;
	
	public static Map<String, Locator> NAME_LOCATOR_MAP = new HashMap<String, Locator>();
	
	public static final Map<String, STRtree> LOCATORNAME_STRTREE_MAP = new HashMap<String,STRtree>();
	
	static{ 		
		try{			
			testList = new LocatorTableReader(
					LocalizationManager.getInstance().getFilename("locatorTable")).getLocatorTable();

			for(Locator ll : testList)				
				NAME_LOCATOR_MAP.put(ll.getLocatorName().trim(), ll);			
		
		}catch(Exception e){	
			System.out.println("---Error in LocatorInfo static block: "+e.getMessage());		
		}
		
		new LoadEnvelopes();
		VizApp.runAsync(new Runnable(){	public void run(){	LoadPointData.fillMap(testList);}});
	}
	
	/**
	 * empty method just for get the static block called
	 */
	public static void init(){
		
	}
	
	public static class LoadPointData  {
		
		private static final double DIST = 1.0;
		
		public static void fillMap(List<Locator> list){
			if(list == null || list.isEmpty()) return;
			LocatorQueryBuilder lqb = new LocatorQueryBuilder();
			String query = "";
			
			for(Locator l : list){
				query = lqb.getQueryWithoutWhere(l);
				
				if(query==null || query.length()==0)
					continue;
				STRtree locatorTree = new STRtree();
				populateTrees(query, locatorTree);
				
				LOCATORNAME_STRTREE_MAP.put(l.getLocatorName(),locatorTree);				
			}
		}
		
		private static void populateTrees(String query, STRtree locatorTree){
			
			//preparation		
			QueryResult queryList;
			QueryResultRow[] queryResult = null;		
			
			//query the DB
			try {				
				queryList = NcDirectDbQuery.executeMappedQuery(query, "ncep", QueryLanguage.SQL);
				queryResult= queryList.getRows();				
			}
			catch (Exception e ){
				System.out.println("___ Error: populateTrees() of LoadPointData: "+e.getMessage());
			}
			Object[] os = null;			
			//populate Tree/pointList
			for(QueryResultRow rows:queryResult){
				os = rows.getColumnValues();	
				if(os == null || os.length == 0) 
					continue;
				
				LocatorPointData pointData = new LocatorPointData();
				//if not String/Double, call setters with default values (""/0.0) so pointData values NOT null
				if( !(os[0] instanceof String)){ os[0] = "";}	pointData.setStateID((String)os[0]);//rows.getColumn(0));
				if( !(os[1] instanceof String)){ os[1] = "";}	pointData.setName((String)os[1]); 	//rows.getColumn(1));
				if( !(os[2] instanceof Double)){ os[2] = 0.0;}	pointData.setLat((Double)os[2]);//rows.getColumn(2));
				if( !(os[3] instanceof Double)){ os[3] = 0.0;}	pointData.setLon((Double)os[3]);//rows.getColumn(3));
				
				Coordinate c = new Coordinate(pointData.getLon(), pointData.getLat());
				Envelope env = new Envelope(c.x-DIST, c.x+DIST, c.y-DIST, c.y+DIST);
				locatorTree.insert(env, pointData);
			}			
		}	
	}
}
