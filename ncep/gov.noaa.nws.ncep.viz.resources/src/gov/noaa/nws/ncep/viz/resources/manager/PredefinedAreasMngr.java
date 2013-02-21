package gov.noaa.nws.ncep.viz.resources.manager;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Map;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Vector;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.viz.core.exception.VizException;

import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;
import gov.noaa.nws.ncep.viz.ui.display.PredefinedArea;

/**
 *  Read and create PredefinedAreas.
 *  
 *  TODO : add methods to save, delete and edit PredefinedAreas at the USER localization level.
 *  
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 07/28/11       #450      Greg Hull    Created / broke out from NmapResourceUtils. Use NcPathManager
 * 07/10/12       #646      Greg Hull    createPredefinedArea
 * 11/14/12       #630      Greg Hull    implement ILocalizationFileObserver
 * 11/18/12       #630      Greg Hull   
 * 
 * </pre>
 * 
 * @author 
 * @version 1
 */
public class PredefinedAreasMngr {	

	static Map<String,LocalizationFile> predefinedAreasMap = null;
	
	// make sure that the default map is first.
	//
	public static String[] getAvailPredefinedAreas() {
 		if( predefinedAreasMap == null ) {

			predefinedAreasMap = NcPathManager.getInstance().listFiles(
					NcPathConstants.PREDEFINED_AREAS_DIR, new String[]{".xml"}, false, true);
			
			if( predefinedAreasMap.isEmpty() ) {
				System.out.println("Could not find any Predefined Areas?");
				return new String[]{};
			}

			boolean dfltFound = false;
			
			// to a validity check by unmarshalling each of the files.
			// Also replace the localizationName (with the PredefinedArea directory prefix
			// with the name of the area.
			for( String lAreaName : new ArrayList<String>( predefinedAreasMap.keySet() ) ) {
				String areaName = lAreaName.substring(
						lAreaName.lastIndexOf( File.separator )+1, lAreaName.length()-".xml".length() );
				
				try {
					if( getPredefinedArea(areaName) == null ) {
						System.out.println("Error parsing Predefined Area "+ lAreaName );
						predefinedAreasMap.remove( lAreaName );
					}
					else if( areaName.equals( NmapCommon.getDefaultMap() ) ) {
						dfltFound = true;
					}
				} catch( VizException ve ) {
					System.out.println("Error parsing Predefined Area "+ lAreaName );
					predefinedAreasMap.remove( lAreaName );
				}				
			}
			
			if( !dfltFound ) {
				System.out.println("Could not find valid Default Predefined Area "+ 
						NmapCommon.getDefaultMap() );
			}
		}

		ArrayList<String> lAreaNamesList = new ArrayList<String>( predefinedAreasMap.keySet() );
		String lAreaNames[] = lAreaNamesList.toArray( new String[0] );
		   		
   		Arrays.sort( lAreaNames );

   		Vector<String> areaNamesVect = new Vector<String>();
   		areaNamesVect.add( NmapCommon.getDefaultMap() );

   		for( int i=0 ; i< lAreaNames.length ; i++ ) {
   			String lAreaName = lAreaNames[i];
			String areaName = lAreaName.substring(
					lAreaName.lastIndexOf( File.separator )+1, lAreaName.length()-".xml".length() );
   			
   			if( !areaName.equals( NmapCommon.getDefaultMap() ) ) {
   				areaNamesVect.add( areaName );   				
   			}
   		}
   		
   		return areaNamesVect.toArray(new String[0]);
   	}

//	// it might be nice create a class specifically to store the predefined 
//	// area but for now this will just be the Display (we need the zoomLevel and
//	// mapCenter as well as the gridGeometry)
//	// 
	public static PredefinedArea getPredefinedArea( String areaName ) throws VizException {
		if( predefinedAreasMap == null )  {
			getAvailPredefinedAreas();
		}
		
		String key = NcPathConstants.PREDEFINED_AREAS_DIR + File.separator + areaName +".xml";
		
		if( !predefinedAreasMap.containsKey( key ) ) {
			throw new VizException("Unable to find Predefined Area : "+ areaName );
		}
		
		try {
			LocalizationFile lFile = predefinedAreasMap.get( key );
			
			Object xmlObj = SerializationUtil.jaxbUnmarshalFromXmlFile( lFile.getFile() );

			if( !(xmlObj instanceof PredefinedArea) ) {				
   	   			VizException ve = new VizException( 
   	   					"Error unmarshaling PredefinedArea: "+areaName );
   	   			throw ve;
			}
			if( ((PredefinedArea)xmlObj).getAreaName().isEmpty() ) {
				VizException ve = new VizException( 
   	   					"Error unmarshaling PredefinedArea: " +areaName  );
				throw ve;
			}
			
			// just want to keep one instance of the observer. 
			lFile.removeFileUpdatedObserver( localizationObserver );
			lFile.addFileUpdatedObserver( localizationObserver );
			
			return (PredefinedArea)xmlObj;
			
		} catch (SerializationException e) {
			throw new VizException( e );
		}
	}
	
	public static PredefinedArea clonePredefinedArea( PredefinedArea pArea ) throws VizException { 

		try {
			File tempRbdFile = File.createTempFile("tempArea-", ".xml");

			SerializationUtil.jaxbMarshalToXmlFile( pArea, 
									tempRbdFile.getAbsolutePath() );
			String s = null;
			FileReader fr = new FileReader( tempRbdFile );
			char[] b = new char[ (int)tempRbdFile.length() ];
			fr.read(b);
			fr.close();
			s = new String(b);

			pArea = (PredefinedArea)SerializationUtil.unmarshalFromXml( s );
			tempRbdFile.delete();
			
			return pArea;
			
		} catch (SerializationException e) {
			throw  new VizException( e );
		} catch (IOException e) { // from createTempFile
			throw  new VizException( e ); 
		} catch (JAXBException e) {
			throw  new VizException( e ); 
		}
	}
	
//	public static Boolean areAreasEqual( PredefinedArea p1, PredefinedArea p2 ) {
//		if( Math.abs( p1.getZoomLevel() - p2.getZoomLevel() ) > .00001 ) {
//			return false;
//		}
//		if( p1.getMapCenter().length != p2.getMapCenter().length ) {
//			return false;
//		}
//		
//		for( int i=0 ; i<p1.getMapCenter().length ; i++ ) {
//			if( Math.abs( p1.getMapCenter()[i]-p2.getMapCenter()[i] ) > .00001 ) {
//				return false;
//			}			
//		}
//		
//		return p1.getGridGeometry().equals( p2.getGridGeometry() );
//	}

	private static ILocalizationFileObserver localizationObserver = new ILocalizationFileObserver() {
		@Override
		public void fileUpdated( FileUpdatedMessage fumsg) {
			// just force the map to be recreated next time
			if( predefinedAreasMap != null ) {
				predefinedAreasMap.clear();
				predefinedAreasMap = null;
			}
			//		String fName = fumsg.getFileName();
			//		LocalizationFile lFile;
			//		
			//		// if the file had been deleted 
			//		if( fumsg.getChangeType() == FileChangeType.DELETED ) {
			//			predefinedAreasMap.remove( fName );
			//			// if reverted. (ie DELETED and there is a lower level file available)
			//			lFile = NcPathManager.getInstance().getStaticLocalizationFile( fumsg.getFileName() );
			//		}
			//		else {
			//		// get the ADDED, UPDATED file  
			//			lFile = NcPathManager.getInstance().getLocalizationFile( 
			//							fumsg.getContext(), fumsg.getFileName() );
			//		}
			//
			//		// update the map with the new file 
			//		if( lFile != null ) {
			//			predefinedAreasMap.put( fName, lFile );			
			//		}
		}
	};
}
