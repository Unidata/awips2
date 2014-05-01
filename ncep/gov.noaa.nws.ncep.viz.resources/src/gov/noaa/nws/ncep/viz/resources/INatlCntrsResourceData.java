/**
 * 
 */
package gov.noaa.nws.ncep.viz.resources;

import java.util.ArrayList;

import org.eclipse.swt.graphics.RGB;

import gov.noaa.nws.ncep.viz.common.display.NcDisplayType;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceAttrSet;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;

/**
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 29 May 2009     #115      Greg Hull    Initial Creation.
 * 06 Aug 2009               Greg Hull    Migrate to to11
 * 01 Sep 2009     #148      Greg Hull    Add get/setQualifiedResourceName
 * 04 Apr 2010     #259      Greg Hull    add getLegendColor() 
 * 22 Jun 2010     #273      Greg Hull    use ResourceName
 * 11 Feb 2013     #972      Greg Hull    getSupportedDisplayTypes()
 *
 * </pre>
 *
 * @author ghull
 * @version 1
 */
public interface INatlCntrsResourceData {
	
    public abstract boolean setRscAttrSet( ResourceAttrSet attrSet );
    public abstract ResourceAttrSet getRscAttrSet();

    public abstract ResourceName getResourceName();
	public void     setResourceName( ResourceName rscName );

//    public abstract boolean getResourceAttrValues( ResourceAttrSet newRscAttrSet );
//    public abstract boolean setResourceAttrValues( ResourceAttrSet newRscAttrSet );
            
    // if edited then the values in the member variables have been modified from those 
    // set in the ResourceAttrSet.
    public abstract boolean getIsEdited();
    public abstract void setIsEdited( boolean e );
    
    // Version can be used to test whether an RBD was created with an older version of the resource
    // Currently this is not enforced or implemented by any of the resources.
    public abstract void setResourceVersion( String v );
    public abstract String getResourceVersion();
    
    public abstract RGB getLegendColor();
    
    public abstract NcDisplayType[] getSupportedDisplayTypes();
}
