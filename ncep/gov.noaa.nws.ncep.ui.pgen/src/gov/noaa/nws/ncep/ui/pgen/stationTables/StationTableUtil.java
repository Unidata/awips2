package gov.noaa.nws.ncep.ui.pgen.stationTables;

import java.util.HashMap;
import java.util.List;

import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;

import gov.noaa.nws.ncep.edex.common.stationTables.StationTable;
import gov.noaa.nws.ncep.viz.common.dbQuery.NcDirectDbQuery;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;

public class StationTableUtil {

	//surface stations
	private static StationTable sfstnTbl;

	//anchor points
	private static StationTable anchorTbl;
	
	//vor points
	private static StationTable vorTbl;

	//merged cluster tables
    static private HashMap<String,String> clstTbl;
    
    //volcano table
	private static StationTable volcanoTbl;    
    
	/**
	 * Return the anchor table
	 * @return
	 */
	public static StationTable getSfstnTbl(){
		
		if ( sfstnTbl == null ){
			
			sfstnTbl = new StationTable(
					NcPathManager.getInstance().getStaticFile(
	            			NcPathConstants.SFSTNS_TBL ).getAbsolutePath());

		}
		
		return sfstnTbl;
	}
	
	/**
	 * Return the anchor table
	 * @return
	 */
	public static StationTable getAnchorTbl(){
		
		if ( anchorTbl == null ){
			
			anchorTbl = new StationTable(
					NcPathManager.getInstance().getStaticFile( 
							NcPathConstants.PGEN_SPC_ANCHOR_TBL).getAbsolutePath() );

		}
		
		return anchorTbl;
	}

	/**
	 * Return the vor table
	 * @return
	 */
	public static StationTable getVorTbl(){
		if ( vorTbl == null ){
			vorTbl = new StationTable(
					NcPathManager.getInstance().getStaticFile( 
							NcPathConstants.PGEN_SPC_ANCHOR_TBL).getAbsolutePath() );
		}
		return vorTbl;
	}
	
	/**
	 * Return the volcano table
	 * @return
	 */
	public static StationTable getvolcanoTbl(){
		if ( volcanoTbl == null ){
			volcanoTbl = new StationTable(
					NcPathManager.getInstance().getStaticFile( 
							NcPathConstants.VOLCANO_STN_TBL).getAbsolutePath() );
		}
		return volcanoTbl;
	}
	
	   /**
     * Read and merge the clustering tables.
     */
    public static HashMap<String,String> getClstTbl(){
    	
    	if ( clstTbl == null ){

    		clstTbl = new HashMap<String, String>();

    		List<Object[]> rows1 = null;
    		List<Object[]> rows2 = null;
    		List<Object[]> rows3 = null;

    		String queryWfo = "Select cntyfipscode FROM stns.countyclustwfo";
    		String querySt = "Select cntycitifipscode FROM stns.countycluststate";
    		String queryPerm = "Select cntyfipscode FROM stns.permclust";

    		try {
    			rows1 = NcDirectDbQuery.executeQuery(
    					queryWfo, "ncep", QueryLanguage.SQL);
    			rows2 = NcDirectDbQuery.executeQuery(
    					querySt, "ncep", QueryLanguage.SQL);
       			rows3 = NcDirectDbQuery.executeQuery(
    					queryPerm, "ncep", QueryLanguage.SQL);

    		}
    		catch (Exception e ){
    			System.out.println("Read clustering table exception!");
    			e.printStackTrace();

    		}

    		if ( rows1 != null ){
    			if (rows2 != null ) rows1.addAll(rows2);
    			if ( rows3 != null ) rows1.addAll(rows3);
    			for ( Object[] obj : rows1){
    				String fipsStr = (String)obj[0];
    				String strNoSpace = fipsStr.replaceAll(" ", "");
    				int index = 0;
    				do {
    					//	System.out.println(obj[0]);
    					//	System.out.println(((String)obj[0]).substring(index, index+5));
    					String key = (strNoSpace).substring(index, index+5);
    					String value = clstTbl.get(key);
    					if ( value != null ){		//make sure no overwrite
    						clstTbl.put(key, value + "+" + strNoSpace);
    					}
    					else {
    						clstTbl.put(key, strNoSpace);
    					}
    					index += 6;
    				}while (index+5 <= strNoSpace.length());
    			}

    		}
    	}
    	
    	return clstTbl;
    }
    
}
