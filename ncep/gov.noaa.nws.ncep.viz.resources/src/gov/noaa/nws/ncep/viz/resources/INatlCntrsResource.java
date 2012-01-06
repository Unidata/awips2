/**
 * 
 */
package gov.noaa.nws.ncep.viz.resources;

import java.util.ArrayList;

import gov.noaa.nws.ncep.viz.resources.attributes.ResourceAttrSet;

/**
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 29 May 2009     #115      Greg Hull    Initial Creation.
 * 
 * </pre>
 *
 * @author ghull
 * @version 1
 */
public interface INatlCntrsResource {
	/*
    public void setRscAttrSetName( String attrSetName );
    public String getRscAttrSetName();

    public abstract String getResourceName();
    
    public abstract void init();
    public abstract ResourceAttrSet getResourceAttrSet();

    public abstract boolean getResourceAttrValues( ResourceAttrSet newRscAttrSet );
    public abstract boolean setResourceAttrValues( ResourceAttrSet newRscAttrSet );
        
    // if edited then the values in the member variables have been modified from those 
    // set in the ResourceAttrSet.
    public abstract boolean getIsEdited();
    public abstract void setIsEdited( boolean e );
    
    // Version can be used to test whether an RBD was created with an older version of the resource
    // Currently this is not enforced or implemented by any of the resources.
    public abstract void setResourceVersion( String v );
    public abstract String getResourceVersion();
    */
    //  modified thru the contextual menu. // not from the RBD mngr dialog.
    public abstract INatlCntrsResourceData getResourceData();

    public abstract void resourceAttrsModified();
}
