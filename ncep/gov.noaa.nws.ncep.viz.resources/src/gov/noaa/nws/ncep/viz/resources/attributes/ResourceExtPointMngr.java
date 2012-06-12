package gov.noaa.nws.ncep.viz.resources.attributes;

import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefnsMngr;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.exception.VizException;
/**
 * 
 *  
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 *      	 	     		Greg Hull    created
 *  06/10/10     #273	    Greg Hull    read bundleFile location
 *  06/22/10     #273       Greg Hull    map attributes with rscName key instead of rscDataClass
 *  10/13/11                Xilin Guo    Enable "WOU"
 *  04/12/2012   #615       Shova Gurung Added ConditionalFilter in readResourceAttributesExtPoint()
 *  
 * </pre>
 * 
 * @author 
 * @version 1
 */

public class ResourceExtPointMngr {
	
    private static final String NC_RESOURCE_EXT = "gov.noaa.nws.ncep.viz.resources.NC-Resource";
    private static final String RESOURCE_NAME_TAG  = "name";
    private static final String RESOURCE_CLASS_TAG = "class";
    private static final String EDIT_DLG_CLASS_TAG = "editDialogClass";
    private static final String BUNDLE_FILE_TAG    = "bundleFile";
    
    private static final String NC_RESOURCE_ATTR_EXT = "gov.noaa.nws.ncep.viz.resources.NC-ResourceAttribute";
    private static final String RESOURCE_NAME_REF_TAG = "ncResourceName";
    private static final String ATTR_NAME_TAG   = "attributeName";
    private static final String ATTR_CLASS_TAG = "attributeClass";
    private static final String DFLT_VAL_TAG = "defaultValue";

    ResourceDefnsMngr rscCfgMngr;
    
    public static class ResourceAttrInfo {
    	private String   attrName;
    	private Class<?> attrClass;
    	private Object   dfltVal; // not implemented
    	
    	ResourceAttrInfo( String n, Class<?> clz ) {
    		attrName = n;
    		attrClass = clz;
    	}
    	public String getAttrName() {
    		return attrName;
    	}
    	public Class<?> getAttrClass() {
    		return attrClass;
    	}
    }
   
    private static class ResourceExtPointInfo {
    	String   rscName;  // 
    	Class<?> rscDataClass;
    	Class<?> rscDialogClass;
    	
    	HashMap<String, ResourceAttrInfo> rscAttributes = null;
    	
    	ResourceExtPointInfo( String name, Class<?> rcls, Class<?> rdlgcls) {
    		rscName = new String( name );
        	rscAttributes = new HashMap<String, ResourceAttrInfo>();
        	rscDataClass = rcls;
        	rscDialogClass = rdlgcls;
        }
    	
    	void addAttribute( String attrName, Class<?> attrClass ) { // , Object dfltVal ) {

    		rscAttributes.put( attrName, new ResourceAttrInfo( attrName, attrClass ) );
    	}
    }
    
    //  map from the resource name to the resource info containing the rscData class,
    // the class that implements the edit dialog and a list of attributes from the 
    // attribute extension points
    //
    static HashMap<String, ResourceExtPointInfo> rscExtPointInfoMap = null;
    
    static ResourceExtPointMngr instance = null;

    public static ResourceExtPointMngr getInstance() {
    	if( instance == null ) {
    		instance = new ResourceExtPointMngr();
    	}
    	else if( instance.rscExtPointInfoMap.size() == 0 ) {
    		instance = new ResourceExtPointMngr();
    	}
    	return instance;
    }
    
    private ResourceExtPointMngr() {

    	rscExtPointInfoMap = new HashMap<String, ResourceExtPointInfo>();

        readResourceExpPoint();
        readResourceAttributesExtPoint();
    }
    
	private static void readResourceExpPoint( ) {
        IExtensionRegistry registry = Platform.getExtensionRegistry();
        IExtensionPoint    extPoint = registry.getExtensionPoint( NC_RESOURCE_EXT );
        
        if( extPoint == null ) {
        	System.out.println("Unable to get Extension Point : " + NC_RESOURCE_EXT );
        	return;
        }

        IExtension[] extensions = extPoint.getExtensions();

        for( int i = 0; i < extensions.length; i++ ) {
            IConfigurationElement[] config = extensions[i].getConfigurationElements();

            for( int j = 0; j < config.length; j++ ) {
                String rscName      = config[j].getAttribute(RESOURCE_NAME_TAG);
                String rscClassName = config[j].getAttribute(RESOURCE_CLASS_TAG);
                String editDlgClassName = config[j].getAttribute(EDIT_DLG_CLASS_TAG);
                String bndlFileName = config[j].getAttribute(BUNDLE_FILE_TAG);

                if( rscClassName == null ) {
                	System.out.println( " "+rscClassName );
                	continue;
                }
//                if( rscName.contains("WOU") ) {
//                	System.out.println( " "+rscName );
//                	continue;
//                }
                // for now this is optional since we can have NC resources that do not have 
                // editable attributes.
                if( editDlgClassName == null ) {
                	continue;
                }
                
                IConfigurationElement[] fields = config[j].getChildren();
                
                Class<?> rscClass = null;
                Class<?> editDlgClass = null;
                
                try {
                	rscClass = Class.forName( rscClassName );
                	editDlgClass = Class.forName( editDlgClassName );
                	
                	rscExtPointInfoMap.put( rscName, 
                			     new ResourceExtPointInfo( rscName, rscClass, editDlgClass ) );
                } catch ( ClassNotFoundException e ) {
                	System.out.println("Unable to load class "+rscClassName +" or "+ editDlgClassName );
                }
            }
        }
    }

    
	// init the resourceAttrsMap from the NC-ResourceAttributes ext point
	private static void readResourceAttributesExtPoint( ) {
	
        IExtensionRegistry registry = Platform.getExtensionRegistry();
        IExtensionPoint extPoint = registry.getExtensionPoint( NC_RESOURCE_ATTR_EXT );
        
        if( extPoint == null ) {
        	System.out.println("Unable to get Extension Point : "+ NC_RESOURCE_ATTR_EXT );
        	return;
        }

        IExtension[] extensions = extPoint.getExtensions();
		
        for( int i = 0; i < extensions.length; i++ ) {
            IConfigurationElement[] config = extensions[i].getConfigurationElements();

            for( int j = 0; j < config.length; j++ ) {
                String rscName  = config[j].getAttribute(RESOURCE_NAME_REF_TAG);
                String rscAttrName = config[j].getAttribute(ATTR_NAME_TAG);
                String rscAttrClassName = config[j].getAttribute(ATTR_CLASS_TAG);
                String dfltValue   = config[j].getAttribute(DFLT_VAL_TAG); // not implemented

                if( rscName == null || rscAttrName == null || rscAttrClassName == null ) {
                	System.out.println( "Invalid Extension of Ext Point: "+ NC_RESOURCE_ATTR_EXT );
                	continue;
                }
                
                IConfigurationElement[] fields = config[j].getChildren();
                
                // then test for one of the standard types or classes.
        		//
                if( rscAttrClassName.equalsIgnoreCase( "Integer" )) {
                	rscAttrClassName = "java.lang.Integer";
        	    } else if( rscAttrClassName.equalsIgnoreCase( "Boolean" )) {
        	    	rscAttrClassName = "java.lang.Boolean";
        	    } else if( rscAttrClassName.equalsIgnoreCase( "Double" )) {
        	    	rscAttrClassName = "java.lang.Double";
        	    } else if( rscAttrClassName.equalsIgnoreCase( "Float" )) {
        	    	rscAttrClassName = "java.lang.Float";
        	    } else if( rscAttrClassName.equalsIgnoreCase( "String" )) {
        	    	rscAttrClassName = "java.lang.String";
        	    } else if( rscAttrClassName.equalsIgnoreCase( "RGB" )) {
        	    	rscAttrClassName = RGB.class.getName();
        	    } else if( rscAttrClassName.equalsIgnoreCase( "ArrowStyle" )) {
                    rscAttrClassName = "gov.noaa.nws.ncep.viz.rsc.ncscat.rsc.NcscatResourceData$ArrowStyle";
                } else if( rscAttrClassName.equalsIgnoreCase( "LineStyle" )) {
        	    	rscAttrClassName = LineStyle.class.getName();
        	    } else if( rscAttrClassName.equalsIgnoreCase( "MarkerState" )) {
        	    	//className = MarkerState.class.getName();
        	    	rscAttrClassName = "gov.noaa.nws.ncep.viz.overlays.IPointOverlayResourceData$MarkerState";
        	    } else if( rscAttrClassName.equalsIgnoreCase( "MarkerType" )) {
        	    	rscAttrClassName = "gov.noaa.nws.ncep.viz.overlays.IPointOverlayResourceData$MarkerType";
        	    } else if( rscAttrClassName.equalsIgnoreCase( "MarkerTextSize" )) {
        	    	rscAttrClassName = "gov.noaa.nws.ncep.viz.overlays.IPointOverlayResourceData$MarkerTextSize";
        	    } else if( rscAttrClassName.equalsIgnoreCase("PlotModel") ) {
        	    	rscAttrClassName = "gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.elements.PlotModel";
        	    } else if( rscAttrClassName.equalsIgnoreCase("ColorBar") ) {
//        	    	rscAttrClassName = "gov.noaa.nws.ncep.viz.resources.attributes.ColorBar";
        	    	rscAttrClassName = "gov.noaa.nws.ncep.viz.ui.display.ColorBar";
        	    } else if( rscAttrClassName.equalsIgnoreCase("ConditionalFilter") ) {
        	    	rscAttrClassName = "gov.noaa.nws.ncep.viz.rsc.plotdata.conditionalfilter.ConditionalFilter";
        	    }

            	ResourceExtPointInfo repi = rscExtPointInfoMap.get( rscName );
            	
            	if( repi == null ) {
            		System.out.println("Error reading "+NC_RESOURCE_ATTR_EXT + " Ext Point: The Resource " +
            		   rscName + " is not defined." );
            		continue;
            	}

                try {
                	Class<?> rscAttrClass = Class.forName( rscAttrClassName );

                	repi.addAttribute(rscAttrName, rscAttrClass );
                } catch (ClassNotFoundException e) {
                	System.out.println("Unable to load class: "+rscAttrClassName );
                }
            }
        }

	}

	public Class<?> getResourceDialogClass( ResourceName rscName ) {
		// get the rsc Impl for this  
    	if( rscCfgMngr == null ) {
    		try {
    			rscCfgMngr = ResourceDefnsMngr.getInstance();
    		} catch (VizException e) {
    			e.printStackTrace();
    		}
    	}

		String rscImplName = rscCfgMngr.getResourceImplementation( rscName.getRscType() );
		
		ResourceExtPointInfo repi = rscExtPointInfoMap.get( rscImplName );
		
		return ( repi == null ? null : repi.rscDialogClass );
	}

//	public String getResourceNameForClass( Class<?> rscDataClass ) {
//		ResourceExtPointInfo repi = rscExtPointInfoMap.get( //rscDataClass );
//
//		return ( repi == null ? null : repi.rscName );
//	}
	
	public HashMap<String,ResourceAttrInfo> getResourceAttributes( ResourceName rscName ) {
    	if( rscCfgMngr == null ) {
    		try {
    			rscCfgMngr = ResourceDefnsMngr.getInstance();
    		} catch (VizException e) {
    			e.printStackTrace();
    		}
    	}

		String rscImplName = rscCfgMngr.getResourceImplementation( rscName.getRscType() );
		
		ResourceExtPointInfo repi = rscExtPointInfoMap.get( rscImplName );
		
		return ( repi == null ? null : repi.rscAttributes );		
	}

	public ArrayList<String> getAvailResources( ) {
		ArrayList<String> availResources = new ArrayList<String>();
		for( ResourceExtPointInfo rscInfo : rscExtPointInfoMap.values() ) {
			availResources.add( rscInfo.rscName );
		}
		return availResources;
	}
	
	public Class<?> getResourceDataClass( String rscName ) {
		for( ResourceExtPointInfo rscInfo : rscExtPointInfoMap.values() ) {
			if( rscInfo.rscName.equals( rscName ) ) {
				return rscInfo.rscDataClass;
			}
		}
//		if( rscExtPointInfoMap.containsKey( rscName ) ) {
//			return rscExtPointInfoMap.get( rscName ).rscDataClass;
//		}
		return null;
	}
}
