package gov.noaa.nws.ncep.viz.rsc.satellite.rsc;


import java.util.List;

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.Separator;
import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;

import gov.noaa.nws.ncep.common.dataplugin.mcidas.McidasMapCoverage;
import gov.noaa.nws.ncep.common.dataplugin.mcidas.McidasRecord;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.attributes.EditResourceAttrsAction;
import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;



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
public class McidasSatResource extends AbstractSatelliteResource 
		   implements ICloudHeightCapable, INatlCntrsResource {

	public McidasSatResource(SatelliteResourceData data, LoadProperties props) {
        super(data, props);
        satRscData = data;
        
        // set the legend from the satellite name and imageType
        // NOTE : this assumes that the request type of EQUALS (ie only one kind of imageType and satellite name)
        //
        if( satRscData.getMetadataMap().containsKey("imageType") && 
        	satRscData.getMetadataMap().containsKey("satelliteName") ) {
        	legendStr = satRscData.getMetadataMap().get("satelliteName").getConstraintValue() + " " +
        	            satRscData.getMetadataMap().get("imageType").getConstraintValue();
        }

	}
    

	public boolean isCloudHeightCompatible() {
    	RequestConstraint imgTypeConstraint = satRscData.getMetadataMap().get("imageType");
    	if( imgTypeConstraint != null ) {
    		// TODO : Not sure if we could handle a constraint type like 'IN' or not?
    		// The Image may or may not be cloudHeightCompatible if some image types are
    		// IR and some aren't.
    		if( imgTypeConstraint.getConstraintType() == ConstraintType.EQUALS && 
    			imgTypeConstraint.getConstraintValue().equals("IR") ) {
    			return true;
    		}
    	}
    	return false;
    	// can't do this since imageTypes is not set til a record is processed and
    	// we need the dataUnits before that.
//    	for( String physElmt : getImageTypes() ) {
//			if( !physElmt.equals("IR") ) {
//				return false;
//			}
//		}
	}

    String getImageTypeFromRecord( PluginDataObject pdo ) {
    	return ((McidasRecord)pdo).getImageType();
    }

    String getDataUnitsFromRecord( PluginDataObject pdo ) {
    	// TODO: workaround until the units is put into the DB.
    	return (isCloudHeightCompatible() ? "IRPixel" : "BRIT" );
    	//"";//((McidasRecord)pdo).getUnits();
    }
    
    String getCreatingEntityFromRecord( PluginDataObject pdo ) {
    	return ((McidasRecord)pdo).getSatelliteName();
    }


	@Override
	String getProjectionFromRecord(PluginDataObject pdo) {
		return ((McidasRecord)pdo).getProjection();
	}
	
	/*
	 * Create GridGeometry base on Satellite image's spatial info
	 * (non-Javadoc)
	 * @see gov.noaa.nws.ncep.viz.rsc.satellite.rsc.AbstractSatelliteResource#createNativeGeometry(com.raytheon.uf.common.dataplugin.PluginDataObject)
	 */
	@Override
	public
	GridGeometry2D createNativeGeometry(PluginDataObject pdo) {
		
		if ( ! (pdo instanceof McidasRecord) ) return null;
		
		McidasRecord satRec = (McidasRecord)pdo;
		McidasMapCoverage cov = satRec.getCoverage();
		
	    GeneralEnvelope env = new GeneralEnvelope(2);
	    env.setCoordinateReferenceSystem( satRec.getCoverage().getCrs() );
	    
	    int minX = cov.getUpperLeftElement();
	    int maxX = cov.getUpperLeftElement() + ( cov.getNx() * cov.getElementRes() );
	    int minY = cov.getUpperLeftLine() + ( cov.getNy() * cov.getLineRes() );
	    minY = -minY;
	    int maxY = -1 * cov.getUpperLeftLine();
	    env.setRange(0, minX, maxX);
	    env.setRange(1, minY, maxY);
	    
	    GridGeometry2D mapGeom = new GridGeometry2D(new GeneralGridEnvelope(new int[] {
                 0, 0 }, new int[] { cov.getNx(), cov.getNy() }, false),
                 env);
		
	       return mapGeom;
	}



}
