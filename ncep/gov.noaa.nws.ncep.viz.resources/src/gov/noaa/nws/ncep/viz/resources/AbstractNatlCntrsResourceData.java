package gov.noaa.nws.ncep.viz.resources;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Set;

import gov.noaa.nws.ncep.viz.common.RGBColorAdapter;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceAttrSet;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceExtPointMngr;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceAttrSet.RscAttrValue;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceExtPointMngr.ResourceAttrInfo;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName.ResourceNameAdapter;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapters;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;

/**
 * This is the abstract class for all Natl Cntrs (non-requestable) resources. It is very similar to 
 * the AbstractNatlCntrsRequestableResourceData class with the only real difference being that it
 * extends AbstractResourceData instead of AbstractRequestableResourceData.
 *    The main purpose of this class is to manage the resource's attributes which are stored in a
 * named ResourceAttrSet (currently the .prm files) The values in the attrSet can be loaded to and
 * from the ResourceData. The edit Attributes dialog uses this to get the attribute values from
 * the resource. When an RBD is being created the attribute values are stored in the .prm file and
 * in a ResourceData and both are written out to the RBD file along with a flag indicating whether
 * the attribute values were edited from the original named attribute set. When the RBD is loaded
 * the resource uses this flag to use either the values in the .prm file at load time (which may
 * have changed from when the RBD was created) or from the unmarshalled resource.   
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * July 20, 2009           mgao        Initial creation
 * Aug 03, 2009            ghull       rm 'Attr' getter methods
 * Aug 06, 2009            ghull       construct() -> constructResource()
 * Apr 5, 2010     259     ghull       add legendColor
 * 
 * </pre>
 *  * 
 * @author ghull
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
public abstract class AbstractNatlCntrsResourceData extends AbstractResourceData 
                          implements INatlCntrsResourceData, ISerializableObject{

	@XmlElement
	@XmlJavaTypeAdapter(RGBColorAdapter.class)
	protected RGB legendColor;
	
	// if true then the attribute values are stored in the member variables, if false then 
	// the attribute values are stored in the rscAttrSet.
	@XmlAttribute
	protected boolean isEdited;
	
	protected ResourceExtPointMngr rscExtPointMngr = null;
	
	// the full name/path of the resource with its category, sub-category...
	@XmlAttribute
	@XmlJavaTypeAdapter(ResourceNameAdapter.class)
	ResourceName resourceName = null;
		
	@XmlElement
	protected String resourceVersion="";  // TODO : not implemented
	
	private AbstractVizResource<?, ?> ncRsc;
	
    public AbstractNatlCntrsResourceData() {
        super();
        isEdited = false;
        resourceVersion = null;
        ncRsc = null;
        legendColor = new RGB(255,255,255);

        rscExtPointMngr = ResourceExtPointMngr.getInstance();
    }
        
    // Version can be used to test whether an RBD was created with an older version of the resource
    // Currently this is not enforced or implemented by any of the resources.
	public String getResourceVersion() {
		return resourceVersion;
	}

	public void setResourceVersion(String resourceVersion) {
		this.resourceVersion = resourceVersion;
	}
	
    // get/set for isEdited
    public void setIsEdited( boolean e ) {
    	isEdited = e; 
    }
    
    public boolean getIsEdited() {
    	return isEdited;
    }	

	public final ResourceName getResourceName() {
		return resourceName;
	}

	public void setResourceName( ResourceName rscName ) {		
		resourceName = new ResourceName( rscName );
	}

    @Override
    public AbstractVizResource<?, ?> construct( LoadProperties loadProperties, IDescriptor descriptor) 
                          throws VizException {
    	AbstractVizResource<?, ?> rsc = constructResource( loadProperties, descriptor );
    
    	// store off the resource. Currently this is done only to be able to update the color 
    	// capability when the color attribute is changed.
    	if( rsc instanceof INatlCntrsResource ) {
    		// The current design assumes that each ResourceData will only create one Resource. If this
    		// needs to change then we will either need to store a list of ncRscs or create a new
    		// AbstractNatlCntrsResource class and put the color update code in it.
    		if( ncRsc != null ) {
    			System.out.println("Sanity Check: ncRsc != null. A ResourceData is attempting to construct ");
    			System.out.println(" a resource that already exists. ");
    		}
    		ncRsc = rsc;
    	}
    	else {
    		System.out.println("A NatlCntrsResourceData is constructing a non-NatlCntrs Resource???");
    	}
    	
        // if this resource was not edited (ie attribute values changed from the original values in 
        // the rscAttrSet) then get the values from the rscAttrSet and set the member variables with them.
        // In other words, if the attribute values in the rscAttrSet were changed from the time when the 
        // RBD was created to now we will use the current values.) (This behaviour could be change to 
        // only apply to the default attrSet if we wanted to.)
        //
//		if( rscAttrSet == null && rscAttrSetName != null ) {
//			rscAttrSet = new ResourceAttrSet( rscAttrSetName );
//		}
//
//		if( rscAttrSet != null ) {
//			if( isEdited ) {
//				// if the attributes were edited then the values in rscAttrSet are different so update them
//				// here. (Note: currently this is not required. We could instead just not create an attrSet.)
//				// Or we might want to change the name to prevent inadvertant writing out of edited attributes to 
//				// the file.)
//				getResourceAttrValues( rscAttrSet );
//			}
//
//			// call setRscAttrSet instead of setResourceAttrValues since 
//			// setRscAttrSet may be overridden. This is the case for the overlayResourceGroup and
//			// this is how the component maps for the group are updated with the attributes for the group.
//			setRscAttrSet( rscAttrSet ); 
//		}

    	return rsc;
    }
    
    
	public abstract AbstractVizResource<?, ?>  constructResource( LoadProperties loadProperties,
			IDescriptor descriptor) throws VizException ;

	// get a list of the defined attributes for this resource and 
	// 
	public ResourceAttrSet getRscAttrSet() {
		
		HashMap<String,ResourceAttrInfo> attrSetInfo = 
			rscExtPointMngr.getResourceAttributes( getResourceName() );

		if( attrSetInfo == null ) {
			return null;
		}

		ResourceAttrSet rscAttrSet = new ResourceAttrSet( 
				resourceName.getRscAttrSetName() );

		for( ResourceAttrInfo attrInfo : attrSetInfo.values() ) {
			Method[] mthds = this.getClass().getDeclaredMethods();
			String attrName = attrInfo.getAttrName();

			String getMthdName = "get"+attrName.substring(0,1).toUpperCase() +
			attrName.substring(1);

			for( Method m : mthds ) {
				if( m.getName().equals( getMthdName ) ) {
					Class<?>[] params = m.getParameterTypes();
					Class<?> rtype = m.getReturnType();

//					 This would be a nice sanity check but I would have to go back and change all ints and booleans
//					 in the getters and setters for old resources even though they are compatible with the defined classes
//											if( rtype != attrInfo.getAttrClass() ) {
//												System.out.println("Warning: Attribute "+attrName +" is not defined\n"+
//														"as correct type:" +rtype.getName() + " != " +
//														attrInfo.getAttrClass().getName() );								
//											}

					if( params.length == 0 ) {
						Object attrVal=null;
						try {            					
							attrVal = m.invoke( this );

							Constructor<?>  cc = rtype.getConstructor( rtype );
							if( cc != null ) {
								attrVal = cc.newInstance( attrVal );
							}

							rscAttrSet.setAttrValue( attrName, attrVal );

						} catch (NoSuchMethodException e) {
							// if there is no copy constructor go ahead and set
							// the attribute value
							rscAttrSet.setAttrValue( attrName, attrVal );

						} catch( IllegalAccessException iae ) {
							System.out.println(iae.getMessage());
						} catch( IllegalArgumentException iae ) {
							System.out.println(iae.getMessage());
						} catch( InvocationTargetException ite ) {
							System.out.println(ite.getMessage());
						} catch( ClassCastException cce ) {
							System.out.println(cce.getMessage());
						} catch (SecurityException e) {
							System.out.println(e.getMessage());
						} catch (InstantiationException e) {
							System.out.println(e.getMessage());
						}
					}
				}
			}
		}
		
		return rscAttrSet;
	}


	// the rscAttrSet should only contain attributes defined for this resource.
	//
	public boolean setRscAttrSet( ResourceAttrSet newRscAttrSet ) {
		if( newRscAttrSet == null ) { 
			return false;
		}		

		HashMap<String,ResourceAttrInfo> attrSetInfo = 
		     rscExtPointMngr.getResourceAttributes( getResourceName() );
	
		if( attrSetInfo == null ) {
			return false;
		}

		// loop thru the attributes and use Java Bean utils to set the attributes on the resource    	
		for( ResourceAttrInfo attrInfo : attrSetInfo.values() ) {
			String attrName = attrInfo.getAttrName();
			
			// make sure that this attrSet has this attributeName
			if( !newRscAttrSet.hasAttrName(attrName) ) {
				continue;
			}
			
			RscAttrValue rscAttr = newRscAttrSet.getRscAttr( attrName );
			Object attrValue = rscAttr.getAttrValue();
			Class<?> attrClass = rscAttr.getAttrClass();

			if( attrClass != attrInfo.getAttrClass() ) {
				System.out.println("Unable to set Attribute "+attrName+" because it is defined as "+
						" the wrong type: "+attrClass.getName()+" != "+
						attrInfo.getAttrClass().getName() );
				continue;
			}
			else if( attrValue == null ) {
				continue;
			}

			String setMthdName = "set"+attrName.substring(0,1).toUpperCase() +
						attrName.substring(1);

			Method[] mthds = this.getClass().getDeclaredMethods();

			for( Method m : mthds ) {
				if( m.getName().equals( setMthdName ) ) {
					Class<?>[] params = m.getParameterTypes();
					Class<?> rtype = m.getReturnType();
					
					// This would be a nice sanity check but I would have to go back and change all ints and booleans
					// in the getters and setters for old resources even though they are compatible with the defined classes
//					if( params[0].getClass() != attrInfo.getAttrClass() ||
//							params.length != 1) {
//						System.out.println("Error setting rsc attr "+attrName+" : setter class " +
//						        "has incompatible argument.");
//						System.out.println("Warning: Attribute "+attrName +" is not defined\n"+
//								"as correct type:" +rtype.getName() + " != " +
//								attrInfo.getAttrClass().getName() );
//						continue;
//					}

					try {            					
						m.invoke( this, attrValue );
					} catch( IllegalAccessException iae ) {
						System.out.println(iae.getMessage());
					} catch( IllegalArgumentException iae ) {
						System.out.println(iae.getMessage());
					} catch( InvocationTargetException ite ) {
						System.out.println(ite.getMessage());
					} catch( ClassCastException cce ) {
						System.out.println(cce.getMessage());
					}					
				}
			}
		}
		
		if( ncRsc != null ) {
			((INatlCntrsResource)ncRsc).resourceAttrsModified();
		}

		return true;
	}
	
    @Override
    public void update(Object updateData) {
        // Do nothing version for those resources that don't need this method
    }

    public void setLegendColor( RGB legClr ) {
    	legendColor = legClr;
    }
    
    public RGB getLegendColor() {
    	return legendColor;
    }
    
    @Override
    public boolean equals(Object obj) {
        if( obj == null ) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }

        AbstractNatlCntrsRequestableResourceData other = 
        	   (AbstractNatlCntrsRequestableResourceData) obj;

        if( (legendColor == null && other.legendColor != null) ||
        	(legendColor != null && other.legendColor == null) ) {
        	return false;
        }
        if( !legendColor.toString().equals( other.legendColor.toString() ) ) {
        	return false;
        }
        if( isEdited != other.isEdited ) {
        	return false;
        }
        if( (resourceName == null && other.resourceName != null) ||
            (resourceName != null && other.resourceName == null) ) {
            	return false;
        }
    	if( !resourceName.toString().equals( other.resourceName.toString() ) ) {
    		return false;
    	}
    	if( !resourceVersion.equals( other.resourceVersion ) ) {
    		return false;
    	}

        // Compare the attributes here to avoid having to write code in all of the resource classes.
        //       
        ResourceAttrSet thisAttrSet = this.getRscAttrSet();
        ResourceAttrSet otherAttrSet = other.getRscAttrSet();
        
        if( thisAttrSet == null && otherAttrSet != null ) {
        	return false;
        }
        else if( thisAttrSet != null && otherAttrSet == null ) {
        	return false;
        }
        else if( thisAttrSet != null ) {
        	return thisAttrSet.equals( otherAttrSet );
        }
        
        return true;
    }

}
