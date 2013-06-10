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

import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;

import gov.noaa.nws.ncep.edex.common.stationTables.StationTable;
import gov.noaa.nws.ncep.viz.common.dbQuery.NcDirectDbQuery;
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

	//volcano table
	private static StationTable volcanoTbl;    

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
			
		}	

		return clstTbl;
	}

}
