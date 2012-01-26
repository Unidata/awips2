package gov.noaa.nws.ncep.viz.ui.locator.resource;

import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;
import gov.noaa.nws.ncep.viz.ui.locator.resource.LocatorDataSource.SourceType;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 12/2008  	22    		M. Li      Initial Creation
 * 11/25/2009    138        G. Hull    add writeLocatorTable (from lost to10 change)
 * 12/03/2011    #561     
 * 12/04/2011    #561       G. Hull    use SerializationUtil; NcPathManager
 *                       
 * </pre>
 * 
 * @author M. Li
 * @version 1
 */
public class LocatorDataSourceMngr {
    	
	private HashMap<String, LocatorDataSource> dataSourcesMap = null;
	
    private static LocatorDataSourceMngr instance = null;
    
    public static LocatorDataSourceMngr getInstance() {
    	//instance = null;
    	if( instance == null ) {
    		instance = new LocatorDataSourceMngr();
    	}
    	return instance;
    }

    private  LocatorDataSourceMngr() {
    	dataSourcesMap = new HashMap<String, LocatorDataSource>();
    	
    	readLocatorDataSources();
    }
    
    public void readLocatorDataSources() {
    	
		Map<String, LocalizationFile> dataSourceLFiles = 
					NcPathManager.getInstance().listFiles(        		
						NcPathConstants.LOCATOR_SOURCE_DIR, new String[]{".xml"}, false, true );
			
		for( LocalizationFile lfile : dataSourceLFiles.values() ) {
		
			try {
				LocatorDataSource lds = (LocatorDataSource)SerializationUtil.jaxbUnmarshalFromXmlFile( 
						lfile.getFile().getAbsolutePath() );
				lds.setLocalizationFile( lfile );
				dataSourcesMap.put( lds.getSourceName(), lds );
			
			} catch (SerializationException e) {
				System.out.println("Error unmarshalling LocatorDataSource File:"+lfile.getFile().getAbsolutePath());
			}
		} 
	}
	
    public Set<String> getLocatorDataSources() {
    	return dataSourcesMap.keySet();
    }
    
	public LocatorDataSource getLocatorDataSource( String sourceName ) throws VizException {
		if( sourceName == null ||
			sourceName.isEmpty() || 
			sourceName.equalsIgnoreCase("NONE" ) ) {
			return null;
		}
		else if( !dataSourcesMap.containsKey( sourceName ) ) {
			throw new VizException("Error finding LocatorDataSource: "+ sourceName );
		}
		
		return dataSourcesMap.get( sourceName );
	}

	// This could be used by nsharp if it needs to use the sfcstations data source
	// currently it queries the sfc stations on its own.
//	public LocatorDataSource getSurfaceStationLocatorDataSource() {
//		for( LocatorDataSource ds : dataSourcesMap.values() ) {
//			if( ds.getSourceType() == SourceType.SFC_STATIONS ) {
//				return ds;
//			}
//		}
//		return null;
//	}
	
	public HashMap<String, LocatorDataSource> getAvailLocatorDataSources() {
		return dataSourcesMap;
	}
	
}