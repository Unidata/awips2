package gov.noaa.nws.ncep.viz.common.area;

import gov.noaa.nws.ncep.viz.common.area.AreaName.AreaSource;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;

import com.raytheon.uf.viz.core.exception.VizException;

/**
 * read the gov.noaa.nws.ncep.viz.common.areaProvider extention point and create all
 * factories for the specified areaProviders. 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 05/15/13       #862       G. Hull     Created
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class NcAreaProviderMngr {
	
	private static Map<String,INcAreaProviderFactory> areaProviders = new HashMap<String,INcAreaProviderFactory>();
		
    private static final String AREA_PROV_EXT_PT = "gov.noaa.nws.ncep.viz.common.areaProvider";
    private static final String AREA_SOURCE_NAME_TAG  = "areaSourceName";
    private static final String FACTORY_CLASS_TAG = "sourceFactory";
    private static final String LOCATION_TAG = "sourceLocation"; // May be ignored
    private static final String IMPL_CONFIG_DATA_TAG = "configSourceData"; // may be ignored

	public static List<VizException> initialize() throws VizException {
		
		// TODO : loop thru extension point and add areaProviders.
		
        IExtensionRegistry registry = Platform.getExtensionRegistry();
        IExtensionPoint    extPoint = registry.getExtensionPoint( AREA_PROV_EXT_PT );
        
        if( extPoint == null ) {
        	throw new VizException("Unable to get Extension Point : " + AREA_PROV_EXT_PT );        	
        }

        for( IExtension ext : extPoint.getExtensions() ) { 

            for( IConfigurationElement cfgElem : ext.getConfigurationElements() ) { 
                String areaSourceName      = cfgElem.getAttribute( AREA_SOURCE_NAME_TAG );
                String factoryClassName = cfgElem.getAttribute( FACTORY_CLASS_TAG );
                String locStr = cfgElem.getAttribute( LOCATION_TAG );
                String cfgDataStr = cfgElem.getAttribute( IMPL_CONFIG_DATA_TAG );
                
                INcAreaProviderFactory factObj=null;
				try {
					factObj = (INcAreaProviderFactory)cfgElem.createExecutableExtension(FACTORY_CLASS_TAG);
 
					factObj.initialize( areaSourceName, locStr, cfgDataStr );
					
					areaProviders.put( areaSourceName, factObj );
					System.out.println("Area Provider, "+areaSourceName+" created.");
				}            		
				catch( CoreException e1 ) {
            		throw new VizException("Unable to instantiate area factory, "+
            				factoryClassName+", for source "+areaSourceName+":"+ 
            				e1.getMessage() );        				                
				}
                catch( VizException e ) {
                	System.out.println( e.getMessage() );
                	// if a critical source then throw an error.
                }
            }
        }
        		        
		List<VizException> warningsList = new ArrayList<VizException>();
		for( INcAreaProviderFactory areaProv : areaProviders.values() ) {
			List<VizException> provWarnings = areaProv.getInitializationExceptions();
			if( provWarnings != null ) {
				warningsList.addAll( provWarnings );
			}
		}
	
		return warningsList;
	}
	
	public static INcAreaProviderFactory getSourceProviderFactory( AreaSource areaSource ) {
		return areaProviders.get( areaSource.toString() );
	}
	
	public static IGridGeometryProvider createGeomProvider( AreaName areaName ) throws VizException {
		INcAreaProviderFactory apf = getSourceProviderFactory(areaName.getSource()); 
		return ( apf == null ? null : apf.createGeomProvider(areaName.getName()) );
	}
	
	// reinit each of the areaProviderFactories based on possible changes to data files.
	//
	public static List<VizException> reinitialize() {
		
		List<VizException> warningsList = new ArrayList<VizException>();
		
		for( INcAreaProviderFactory areaProv : areaProviders.values() ) {
			
//			try {
//				areaProv.initialize( null, null, null ); // use existing values for source/location
//			
//				List<VizException> provWarnings = areaProv.getInitializationExceptions();
//				if( provWarnings != null ) {
//					warningsList.addAll( provWarnings );
//				}
//			} catch (VizException e) {
//				warningsList.add( e );
//			}
		}
	
		return warningsList;
	}
}
