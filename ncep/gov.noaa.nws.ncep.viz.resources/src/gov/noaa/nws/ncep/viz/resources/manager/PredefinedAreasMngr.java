package gov.noaa.nws.ncep.viz.resources.manager;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Map;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Vector;
import java.io.File;
import java.io.FileReader;
import java.io.FilenameFilter;
import java.io.IOException;

import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.viz.core.exception.VizException;

import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;
import gov.noaa.nws.ncep.viz.ui.display.NCMapRenderableDisplay;
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

//		TreeMap<String> 
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

//	static public File getPredefinedAreaFile( String predefinedArea ) {
//		
//	}
	
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
			return (PredefinedArea)xmlObj;
			
		} catch (SerializationException e) {
			throw new VizException( e );
		}
	}
	
	public static PredefinedArea createPredefinedArea( 
			NCMapRenderableDisplay display ) throws VizException {
		PredefinedArea pArea = new PredefinedArea();
		pArea.setPredefinedArea( display );
		
		return clonePredefinedArea( pArea );
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
}
