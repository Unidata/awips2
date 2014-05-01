/*
 * gov.noaa.nws.ncep.common.staticDataProvider.StationTableProvider
 * 
 * 12 March 2012
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.staticdataprovider;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.StringTokenizer;
//import java.awt.Color;
import java.io.File;

import org.dom4j.Document;
import org.dom4j.Node;
import org.dom4j.io.SAXReader;

//import com.vividsolutions.jts.geom.Coordinate;

//import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;

//import gov.noaa.nws.ncep.common.staticdata.SPCCounty;
import gov.noaa.nws.ncep.edex.common.stationTables.StationTable;
import gov.noaa.nws.ncep.ui.pgen.PgenStaticDataProvider;
//import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
//import gov.noaa.nws.ncep.viz.common.dbQuery.NcDirectDbQuery;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;

/**
 * Class to load station tables required by NCEP.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 02/12		?			B. Yin   	Moved from PGEN 
 * 06/13		T1000		J. Wu   	Move clusters from DB to Localization. 
 *
 * </pre>
 * 
 * @author	B. Yin
 */

public class StationTableProvider {


	//surface stations
	private static StationTable sfstnTbl;

	//anchor points
	private static StationTable anchorTbl;

	//vor points
	private static StationTable vorTbl;

	//merged cluster tables
	static private HashMap<String,Set<String>> clstTbl;

	//Cluster table names
	private static String PERM_CLUSTER_TABLE = "WatchCountyPermCluster.xml";
	private static String STATE_CLUSTER_TABLE = "WatchCountyStateCluster.xml";
	private static String WFO_CLUSTER_TABLE = "WatchCountyWFOCluster.xml";
	
	//Cluster tables
	static private Set<String> permClstTbl;
	static private Set<String> stateClstTbl;
	static private Set<String> wfoClstTbl;

	//volcano table
	private static StationTable volcanoTbl;    

    /*
     *  The 15 marine zones "State" names and its 2-digits IDs in NMAP2.
     */
    private static HashMap<String, String> marineZoneStates;

	/**
	 * Return the anchor table
	 * @return
	 */
	public static StationTable getSfStnTbl(){

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
							NcPathConstants.VORS_STN_TBL).getAbsolutePath() );
		}
		return vorTbl;
	}

	/**
	 * Return the volcano table
	 * @return
	 */
	public static StationTable getVolcanoTbl(){
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
	public static HashMap<String,Set<String>> getClstTbl(){

		if ( clstTbl == null ){

			// The key of the map is a fips string, the value is a string that contains all fips
			// in the cluster including the key itself in the format of fips1+fips2+...
			clstTbl = new HashMap<String, Set<String>>();

			/*
			 * We move the cluster tables from NCEP DB to Localization - so we retrieve those
			 * tables from PGEN localization now.
			 */
			/*
			List<Object[]> rows1 = null;
			List<Object[]> rows2 = null;
			List<Object[]> rows3 = null;

			String queryWfo = "Select cntyfipscode FROM stns.countyclustwfo";
			String querySt = "Select cntycitifipscode FROM stns.countycluststate";
			String queryPerm = "Select cntyfipscode FROM stns.permclust";

			// Query ncep database to load the optional and permanent cluster tables.
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
			*/
           
			/*
			 * Retrieve cluster tables from PGEN localization.
			 */
			List<Object[]> rows1 = new ArrayList<Object[]>();
			List<Object[]> rows2 = new ArrayList<Object[]>();
			List<Object[]> rows3 = new ArrayList<Object[]>();

			for ( String ss : getWFOClusterTbl() ) {			    
				String clusters = ss.substring( ss.lastIndexOf('|') + 1 );
				if ( clusters != null && clusters.length() > 0 ) {
					rows1.add( new Object[]{ new String( clusters ) } );
				}
			}
			
			for ( String ss : getStateClusterTbl() ) {
				String clusters = ss.substring( ss.lastIndexOf('|') + 1 );
				if ( clusters != null && clusters.length() > 0 ) {
					rows2.add( new Object[]{ new String( clusters ) } );
				}
			}
			
			for ( String ss : getPermClusterTbl() ) {
				String clusters = ss.substring( ss.lastIndexOf('|') + 1 );
				if ( clusters != null && clusters.length() > 0 ) {
					rows3.add( new Object[]{ new String( clusters ) } );
				}
			}

			//For optional cluster A+B, two entries are needed. "A: A+B" and "B: A+B"
			List<Object[]> optional = new ArrayList< Object[] >();
			if ( rows1 != null ) optional.addAll( rows1 );
			if ( rows2 != null ) optional.addAll( rows2 );

			for ( Object[] obj : optional ){
				String fipsStr = (String)obj[0];
				String strNoSpace = fipsStr.replaceAll(" ", "");
				
				StringTokenizer token = new StringTokenizer(strNoSpace, "+" );
				
				HashSet<String> set = new HashSet<String>();

				while ( token.hasMoreTokens() ){
					set.add( token.nextToken() );
				}
				
				for ( String fips : set ){
					clstTbl.put( fips , (Set<String>) set.clone());
				}
			}
			
			//For permanent cluster A+C and B+C, three entries are needed.
			// "A: A+C", "B: B+C", and "C: A+B+C"
			if ( rows3 != null ){
				for ( Object[] obj : rows3){
					String fipsStr = (String)obj[0];
					String strNoSpace = fipsStr.replaceAll(" ", "");
					
					StringTokenizer token = new StringTokenizer(strNoSpace, "+" );
					
					HashSet<String> set = new HashSet<String>();
					
					if ( token.hasMoreTokens() ) {
						String firstFips = token.nextToken();
						set.add( firstFips);
						
						while ( token.hasMoreTokens() ){
							set.add( token.nextToken() );
						}
						
						//First FIPS is already in optional cluster table, merge with the permanent table
						if ( clstTbl.get( firstFips ) != null ){
							 clstTbl.get(firstFips).addAll(set);
						}
						else {	//new entry
							clstTbl.put(firstFips, (Set<String>) set.clone());
						}
						
						//add the remaining FIPS in the set
						for ( String fips : set ){
							if ( !fips.equals( firstFips )){ 	
								if ( clstTbl.get( fips ) != null ){ //already in the map
									 clstTbl.get( fips ).addAll(set);
								}
								else {	//new entry
									clstTbl.put(fips, (Set<String>) set.clone());
								}
							}
						}
					}
				}
			}
			
//			writeClusters();
			
		}	

		return clstTbl;
	}


    /**
     *  The equivalent state names and numeric IDs for marine zones.
     *  These name/numeric IDs match those used in NMAP2.
     */
    private static HashMap<String, String> getMarineZoneStateIDs() {
    	if ( marineZoneStates == null ) {
    		marineZoneStates = new HashMap<String, String>();
    		marineZoneStates.put( "60", "LC" );
    		marineZoneStates.put( "61", "PZ" );
    		marineZoneStates.put( "62", "LO" );
    		marineZoneStates.put( "63", "LE" );
    		marineZoneStates.put( "64", "LM" );
    		marineZoneStates.put( "65", "LS" );
    		marineZoneStates.put( "66", "AM" );
    		marineZoneStates.put( "67", "AN" );
    		marineZoneStates.put( "68", "GM" );
    		marineZoneStates.put( "69", "PK" );
    		marineZoneStates.put( "70", "PH" );
    		marineZoneStates.put( "71", "PM" );
    		marineZoneStates.put( "72", "PS" );
    		marineZoneStates.put( "73", "SL" );
    		marineZoneStates.put( "74", "LH" );
	    }
    	
    	return marineZoneStates;
    }
    
    /**
     *  Make FIPS for marine zones to match those in Legacy NMAP2.
     *  
     *  Each FIPS is a 6-digit number string as following:
     *  
     *  The first two number is the numeric ID of a marine zones "State"
     *  see getMarineZoneStates() above.
     *  
     *  The next three digits is the marine zone's assigned id - the 
     *  last three digits in its ID (e. g., 123 in "PKZ123" ).
     *  
     *  The last digit is always "0".
     *  
     *  06/10/2013 - Decided to use the marine zone's ID directly as FIPS.
     *  
     */
    private static String makeMarineZoneIDs( String fips ) {
	    
		String id = new String( fips );
		if ( fips != null && fips.trim().length() >= 4 ) {
			String stateID;
			if ( fips.trim().length() == 4 ) {
				stateID = fips.substring(0, 1);
		        fips = new String( getMarineZoneStateIDs().get( stateID ) + "Z" + fips.substring(1, 4) );
		    }
			else {
				stateID = fips.substring(0, 2);				
		        fips = new String( getMarineZoneStateIDs().get( stateID ) + "Z" + fips.substring(2, 5) );
			}			
       }
		
   	    return id;
    }
    
	/**
	 * Read a clustering table.
	 */
	public static Set<String> loadClstTbl( String tblName ){
		
		HashSet<String> clusters = new HashSet<String>();
			
		File clusterFile = PgenStaticDataProvider.getProvider().getStaticFile( 
				PgenStaticDataProvider.getProvider().getPgenLocalizationRoot() + 
				tblName );   	    

		Document clstDoc = null;

		try {
			SAXReader reader = new SAXReader();
			clstDoc= reader.read( clusterFile .getAbsoluteFile());
		} catch (Exception e) {
			e.printStackTrace();
		}

		if ( clstDoc != null ) {
			List<Node> clstNodes = clstDoc.selectNodes("/root/countyCluster");
			for ( Node n : clstNodes) {
				clusters.add( n.getText() );				
			}

		}
					
		return clusters;
	}

	
    /**
	 * Return permanent cluster table
	 * @return
	 */
	public static Set<String> getPermClusterTbl(){

		if ( permClstTbl == null ){
			permClstTbl = loadClstTbl( PERM_CLUSTER_TABLE );
		}

		return permClstTbl;
	}
	
    /**
	 * Return state cluster table
	 * @return
	 */
	public static Set<String> getStateClusterTbl(){

		if ( stateClstTbl == null ){
			stateClstTbl = loadClstTbl( STATE_CLUSTER_TABLE );
		}

		return stateClstTbl;
	}

	/**
	 * Return WFO cluster table
	 * @return
	 */
	public static Set<String> getWFOClusterTbl(){

		if ( wfoClstTbl == null ){
			wfoClstTbl = loadClstTbl( WFO_CLUSTER_TABLE );
		}

		return wfoClstTbl;
	}

	/**
	 *  Write out all clusters to a PGEN file (symbol) for testing.
	 * @return
	 */
/*	private static void writeClusters() {
		 HashSet<String>  fips = new HashSet<String>();
		 List<Coordinate> pts = new ArrayList<Coordinate>();
         for ( String ss : clstTbl.keySet() ) {
        	 for ( String sn : clstTbl.get( ss ) ) {
        	     if ( !fips.contains( sn ) ) {
        	    	 fips.add( sn );
        	    	 SPCCounty spc = SPCCountyProvider.findCounty( sn );
        	    	 if ( spc != null ) {
        	    		 pts.add( spc.getCentriod() );
        	    	 }
        	     }
        	 }
         }
		         
         PgenUtil.writePgenFile( "/export/cdbsrv/jwu/workbak/pgen/cluster", "watch_cluster.xml", 
        		 "DIAMOND", Color.green, 1.0, pts );
	}
*/
}
