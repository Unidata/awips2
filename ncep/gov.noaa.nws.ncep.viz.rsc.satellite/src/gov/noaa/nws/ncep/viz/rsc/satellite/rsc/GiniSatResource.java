package gov.noaa.nws.ncep.viz.rsc.satellite.rsc;


import org.eclipse.jface.action.IMenuManager;
import org.geotools.coverage.grid.GridGeometry2D;

import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;

/**
 * Provides satellite raster rendering support 
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  05/24/2010    #281        ghull       Initial creation 
 *  
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
 
public class GiniSatResource extends AbstractSatelliteResource 
		   implements ICloudHeightCapable, INatlCntrsResource {

	private static final String MER_PROJ = "MER";
	private static final String STR_PROJ = "STR";
	private static final String LCC_PROJ = "LCC";
	
	public GiniSatResource(SatelliteResourceData data, LoadProperties props) {
        super(data, props);
        satRscData = data;
        
        // set the legend from the satellite name and imageType
        // NOTE : this assumes that the request type of EQUALS (ie only one kind of imageType and satellite name)
        //
        if( satRscData.getMetadataMap().containsKey("physicalElement") && 
        	satRscData.getMetadataMap().containsKey("creatingEntity") ) {
        	legendStr = satRscData.getMetadataMap().get("creatingEntity").getConstraintValue() + " " +
        	            satRscData.getMetadataMap().get("physicalElement").getConstraintValue();
        	        	
        	legendStr = legendStr.replace('%', ' ');        	        	
        }
	}

	public boolean isCloudHeightCompatible() {
    	RequestConstraint physElemConstraint = satRscData.getMetadataMap().
    	        get("physicalElement");
    	if( physElemConstraint != null ) {
    		// TODO : Not sure if we could handle a constraint type like 'IN' or not?
    		// The Image may or may not be cloudHeightCompatible if some image types are
    		// IR and some aren't.
    		if( physElemConstraint.getConstraintType() == ConstraintType.EQUALS && 
    				physElemConstraint.getConstraintValue().equals("Imager 11 micron IR") ) {
    			return true;
    		}
    	}
    	return false;
	}

    String getImageTypeFromRecord( PluginDataObject pdo ) {
    	return ((SatelliteRecord)pdo).getPhysicalElement();
    }

    String getDataUnitsFromRecord( PluginDataObject pdo ) {
    	return ((SatelliteRecord)pdo).getUnits();
    }
    
    String getCreatingEntityFromRecord( PluginDataObject pdo ) {
    	return ((SatelliteRecord)pdo).getCreatingEntity();
    }

	@Override
	String getProjectionFromRecord(PluginDataObject pdo) {
		int proj = ((SatelliteRecord)pdo).getCoverage().getProjection();
		if ( proj == 1 ) return MER_PROJ;
		else if ( proj == 3 ) return LCC_PROJ;
		else if ( proj == 5 ) return STR_PROJ;
		else return new String("Unknown");
	}

	@Override
	public GridGeometry2D createNativeGeometry(PluginDataObject pdo) {
		return null;   //can't do
	}

	@Override
	public void resourceChanged(ChangeType type, Object object) {
		// TODO Auto-generated method stub
		
	}


}
