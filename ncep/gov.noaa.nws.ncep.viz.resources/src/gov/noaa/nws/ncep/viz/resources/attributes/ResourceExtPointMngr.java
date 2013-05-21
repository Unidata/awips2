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
 *  01/15/12     #606       Greg Hull    add resource parameters
 *  04/19/12     #606       Greg Hull    add attributeName attribute for resource parameters 
 *  04/23/12     #606       Greg Hull    add ConditionalFilter as a shortcut for the classname.
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
    
    private static final String NC_RESOURCE_PARAMS_EXT = "gov.noaa.nws.ncep.viz.resources.NC-ResourceParameter";
    private static final String RESOURCE_NAME_REF_TAG = "ncResourceName";
    private static final String RSC_PARAM_ELEMENT_NAME = "nc-resourceParameter";
    private static final String PARAM_TYPE_TAG = "paramType";
    private static final String PARAM_NAME_TAG   = "paramName";
    private static final String ATTR_NAME_TAG   = "attributeName";
    private static final String PARAM_CLASS_TAG = "paramClass";
    private static final String CONSTRAINT_NAME_TAG = "constraintName";
//    private static final String PARAM_REF_LOC_TAG   = "paramRefLocation";
    private static final String DFLT_VAL_TAG = "defaultValue";


    public static enum ResourceParamType {
    	EDITABLE_ATTRIBUTE("EDITABLE_ATTRIBUTE"),                         
    	IMPLEMENTATION_PARAM("IMPLEMENTATION_PARAM"),    // used to instantiate the ResourceImplementation  
    	REQUEST_CONSTRAINT("REQUEST_CONSTRAINT"),      //  instantiates a Resource Implementation and also used as a request constraint
    	NON_EDITABLE_ATTRIBUTE("NON_EDITABLE_ATTRIBUTE");
    	
    	private String name;
    	private ResourceParamType( String n ) {
    		name = n;
    	}
    	
    	public static ResourceParamType getResourceParamType( String n ) {
    		if( n.equalsIgnoreCase( REQUEST_CONSTRAINT.name ) ) {
    			return REQUEST_CONSTRAINT;
    		}
    		else if( n.equalsIgnoreCase( EDITABLE_ATTRIBUTE.name ) ) {
    			return EDITABLE_ATTRIBUTE;
    		}
    		else if( n.equalsIgnoreCase( NON_EDITABLE_ATTRIBUTE.name ) ) {
    			return NON_EDITABLE_ATTRIBUTE;
    		}
    		else if( n.equalsIgnoreCase( IMPLEMENTATION_PARAM.name ) ) {
    			return IMPLEMENTATION_PARAM;
    		}
    		return IMPLEMENTATION_PARAM;
    	}
    }
           
    public static class ResourceParamInfo {
    	private String   paramName;
    	private String   constraintName; // usually the same as the paramName except for 
    	                                 // GDFILE which is modelName
    	// the name of the member variable in the java resourceData class. 
    	// This will default to the paramName which will be the case for everything
    	// except the Grid resource which will be the same except lower case.
    	//
    	private String   attributeName;
    	private Class<?> paramClass;
    	private ResourceParamType paramType;

    	private String   dfltVal; // not implemented
//    	private String   paramRefLocation; // for ref'd colorBars

    	ResourceParamInfo( String n, String c, String a, Class<?> clz, ResourceParamType ptype,
    			String dval ) { //, String prmRefLoc) {
    		paramName = n;
    		constraintName = c;
    		attributeName = a;
    		paramClass = clz;
    		paramType = ptype;
    		dfltVal = dval;
//    		paramRefLocation = prmRefLoc;
    	}
    	public String getParamName() {
    		return paramName;
    	}
    	public String getConstraintName() {
    		return constraintName;
    	}
    	public String getAttributeName() {
    		return attributeName;
    	}
    	public Class<?> getParamClass() {
    		return paramClass;
    	}
    	public ResourceParamType getParamType() {
    		return paramType;
    	}
    	public String getDefaultValue() {
    		return dfltVal;
    	}
    }
   

    private static class ResourceExtPointInfo {
    	String   rscName;  // 
    	Class<?> rscDataClass;
    	Class<?> rscDialogClass;
    	
    	HashMap<String, ResourceParamInfo> rscParameters = null;
    	
    	ResourceExtPointInfo( String name, Class<?> rcls, Class<?> rdlgcls) {
    		rscName = new String( name );
        	rscParameters = new HashMap<String, ResourceParamInfo>();
        	rscDataClass = rcls;
        	rscDialogClass = rdlgcls;
        }
    	    	
    	void addParameter( String prmName, String cName, String aName,
    				       Class<?> prmClass, ResourceParamType pType, String dflt) { // , Object dfltVal ) {

    		rscParameters.put( prmName, 
    				new ResourceParamInfo( prmName, cName, aName, prmClass, pType, dflt ) );
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
        readResourceParameterssExtPoint();
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

                if( rscClassName == null ) {
                	System.out.println( " "+rscClassName );
                	continue;
                }
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
	private static void readResourceParameterssExtPoint( ) {
	
        IExtensionRegistry registry = Platform.getExtensionRegistry();
        IExtensionPoint extPoint = registry.getExtensionPoint( NC_RESOURCE_PARAMS_EXT );
        
        if( extPoint == null ) {
        	System.out.println("Unable to get Extension Point : "+ NC_RESOURCE_PARAMS_EXT );
        	return;
        }

        IExtension[] extensions = extPoint.getExtensions();
		
        for( int i = 0; i < extensions.length; i++ ) {
            IConfigurationElement[] config = extensions[i].getConfigurationElements();

            for( int j = 0; j < config.length; j++ ) {
            	if( config[j].getName().equalsIgnoreCase( RSC_PARAM_ELEMENT_NAME ) ) {
            		String rscName  = config[j].getAttribute(RESOURCE_NAME_REF_TAG);
            		String prmName = config[j].getAttribute(PARAM_NAME_TAG);
            		String attrName = config[j].getAttribute(ATTR_NAME_TAG);
            		String prmClassName = config[j].getAttribute(PARAM_CLASS_TAG);
            		String constraintName = config[j].getAttribute(CONSTRAINT_NAME_TAG);
            		String prmType   = config[j].getAttribute(PARAM_TYPE_TAG);
            		String dfltVal   = config[j].getAttribute(DFLT_VAL_TAG);
//            		String refLoc   = config[j].getAttribute(PARAM_REF_LOC_TAG);
            		
            		// sanity check ; these are required
            		if( rscName == null || prmName == null || prmClassName == null || prmType == null) {
            			System.out.println( "Invalid Extension of Ext Point: "+ NC_RESOURCE_PARAMS_EXT );
            			continue;
            		}

            		// then test for one of the standard types or classes.
            		//
            		prmClassName = getFullClassName( prmClassName );

            		ResourceExtPointInfo repi = rscExtPointInfoMap.get( rscName );

            		if( repi == null ) {
            			System.out.println("Error reading "+NC_RESOURCE_PARAMS_EXT + " Ext Point: The Resource " +
            					rscName + " is not defined." );
            			continue;
            		}

            		if( constraintName == null ) {
            			constraintName = prmName;
            		}
            		
            		// this only exists for Grids where the parameter name is upper case
            		// and the attribute name (ie the name of the member variable in the 
            		// java class) is lowercase
            		//
            		if( attrName == null ) {
            			attrName = prmName;
            		}
            		try {
            			Class<?> rscPrmClass = Class.forName( prmClassName );
            			ResourceParamType rscPrmType = ResourceParamType.getResourceParamType( prmType );
            			
            			repi.addParameter( prmName, constraintName, attrName,
            							   rscPrmClass, rscPrmType, dfltVal );
            		} catch (ClassNotFoundException e) {
            			System.out.println("Unable to load class: "+prmClassName );
            		}
            	}
            }
        }

	}

	public Class<?> getResourceDialogClass( ResourceName rscName ) {
		// get the rsc Impl for this  
		String rscImplName = null;

//		if( rscCfgMngr == null ) {
    		try {
    			rscImplName = ResourceDefnsMngr.getInstance().getResourceImplementation( rscName.getRscType() );
    		} catch (VizException e) {
    			e.printStackTrace();
    		}
//    	}

		
		ResourceExtPointInfo repi = rscExtPointInfoMap.get( rscImplName );
		
		return ( repi == null ? null : repi.rscDialogClass );
	}

	private static String getFullClassName( String clzName ) {
		// then test for one of the standard types or classes.
		//
		if( clzName.equalsIgnoreCase( "Integer" )) {
			return "java.lang.Integer";
		} else if( clzName.equalsIgnoreCase( "Boolean" )) {
			return "java.lang.Boolean";
		} else if( clzName.equalsIgnoreCase( "Double" )) {
			return "java.lang.Double";
		} else if( clzName.equalsIgnoreCase( "Float" )) {
			return "java.lang.Float";
		} else if( clzName.equalsIgnoreCase( "String" )) {
			return "java.lang.String";
		} else if( clzName.equalsIgnoreCase( "RGB" )) {
			return RGB.class.getName();
		} else if( clzName.equalsIgnoreCase( "ArrowStyle" )) {
			return "gov.noaa.nws.ncep.viz.rsc.ncscat.rsc.NcscatResourceData$ArrowStyle";
		} else if( clzName.equalsIgnoreCase( "LineStyle" )) {
			return LineStyle.class.getName();
		} else if( clzName.equalsIgnoreCase( "MarkerState" )) {
			//className = MarkerState.class.getName();
			return "gov.noaa.nws.ncep.viz.overlays.IPointOverlayResourceData$MarkerState";
		} else if( clzName.equalsIgnoreCase( "MarkerType" )) {
			return "gov.noaa.nws.ncep.viz.overlays.IPointOverlayResourceData$MarkerType";
		} else if( clzName.equalsIgnoreCase( "MarkerTextSize" )) {
			return "gov.noaa.nws.ncep.viz.overlays.IPointOverlayResourceData$MarkerTextSize";
		} else if( clzName.equalsIgnoreCase("PlotModel") ) {
			return "gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.elements.PlotModel";
		} else if( clzName.equalsIgnoreCase("ColorBar") ) {
			return "gov.noaa.nws.ncep.viz.ui.display.ColorBar";
		} else if( clzName.equalsIgnoreCase("conditionalFilter") ) {
			return "gov.noaa.nws.ncep.viz.rsc.plotdata.conditionalfilter.ConditionalFilter";
		}
		return clzName;
	}
	
//	public String getResourceNameForClass( Class<?> rscDataClass ) {
//		ResourceExtPointInfo repi = rscExtPointInfoMap.get( //rscDataClass );
//
//		return ( repi == null ? null : repi.rscName );
//	}
	
//	public HashMap<String,ResourceAttrInfo> getResourceAttributes( ResourceName rscName ) {
//		String rscImplName = null;
//
////		if( rscCfgMngr == null ) {
//    		try {
//    			rscImplName = ResourceDefnsMngr.getInstance().getResourceImplementation( rscName.getRscType() );
//    		} catch (VizException e) {
//    			e.printStackTrace();
//    		}
////    	}
//		
//		ResourceExtPointInfo repi = rscExtPointInfoMap.get( rscImplName );
//		
//		return ( repi == null ? null : repi.rscAttributes );		
//	}

	public HashMap<String,ResourceParamInfo> getParameterInfoForRscImplementation( ResourceName rscName ) {
		try {
			String rscImpl = ResourceDefnsMngr.getInstance().getResourceImplementation( rscName.getRscType() );
			
			return getResourceParameters( rscImpl );
		} catch (VizException e) {
			e.printStackTrace();
			return new HashMap<String,ResourceParamInfo>();
		}		
	}
	
	public HashMap<String,ResourceParamInfo> getResourceParameters( String rscImpl ) {
		ResourceExtPointInfo repi = rscExtPointInfoMap.get( rscImpl );
		return ( repi == null ? null : repi.rscParameters );
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
