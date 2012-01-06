package gov.noaa.nws.ncep.viz.rsc.pgen.rsc;

import gov.noaa.nws.ncep.ui.pgen.display.AbstractElementContainer;
import gov.noaa.nws.ncep.ui.pgen.display.DisplayProperties;
import gov.noaa.nws.ncep.ui.pgen.display.ElementContainerFactory;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.Layer;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;
import gov.noaa.nws.ncep.ui.pgen.file.ProductConverter;
import gov.noaa.nws.ncep.ui.pgen.file.Products;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource.DfltRecordRscDataObj;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource.IRscDataObject;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefnsMngr;

import java.awt.Color;
import java.io.File;
import java.util.Iterator;
import java.util.List;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.map.MapDescriptor;

import gov.noaa.nws.ncep.ui.pgen.file.FileTools;

/**
 * PgenResource - Resource for Display of PGEN Products loaded from XML.
 * 
 *  This code has been developed by the SIB for use in the AWIPS2 system.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 Dec 2009  202         B. Hebbard  Initial creation.
 * 18 Aug 2010  273         G. Hull     get full filename from rscMngr
 * 
 * </pre>
 * 
 * @author bhebbard 
 * @version 1.0
 */
public class PgenResource extends AbstractNatlCntrsResource<PgenResourceData, IMapDescriptor>
                                implements INatlCntrsResource {
	
	private PgenResourceData pgenResourceData;

    // TODO : if/when we decide to make PGEN resources 'animatible' then 
    // we will need to move this to the FrameData class. Currently this is 
    // ignored in paint() and prds is used for every frame.
    //
    List<gov.noaa.nws.ncep.ui.pgen.elements.Product> prds;
    
	DisplayProperties dprops = new DisplayProperties();

    private class FrameData extends AbstractFrameData {  

		public FrameData(DataTime frameTime, int timeInt) {
			super( frameTime, timeInt );
    	}

        public boolean updateFrameData(IRscDataObject rscDataObj ) {
        	return true;
        }
    }
    
    //  ------------------------------------------------------------
    
    /**
     * Create a PGEN XML resource.
     * 
     * @throws VizException
     */
    public PgenResource(PgenResourceData resourceData,
    		LoadProperties loadProperties) throws VizException {
    	super(resourceData, loadProperties);
    	pgenResourceData = (PgenResourceData) resourceData;
    }

    protected AbstractFrameData createNewFrame( DataTime frameTime, int timeInt ) {
    	return (AbstractFrameData) new FrameData( frameTime, timeInt );
    }
    
    public void initResource(IGraphicsTarget grphTarget) throws VizException {
        
    	// set initial display values from resource attributes (as if after modification)
    	resourceAttrsModified();

		File pgenDataDir = ResourceDefnsMngr.getInstance().getPgenDataDir( 
				                                 pgenResourceData.getResourceName() );
		if( pgenDataDir == null || !pgenDataDir.exists() ) {
			throw new VizException( "Resource Mngr can't find data dir for: " + 
					pgenResourceData.getResourceName().toString() );
		}
		
		// the group name is actually the name of the .prm file which contains the 
		// name of the .xml file.
		String pgenProdFilename = pgenDataDir.getAbsolutePath() + File.separator + 
		                           pgenResourceData.getFileName();

		// get the PGEN product data, and convert into format ready for display during paint

		Products products = FileTools.read( pgenProdFilename );        
        prds = ProductConverter.convert( products );
    }
    
    public void paintFrame(AbstractFrameData frameData, IGraphicsTarget target, PaintProperties paintProps) throws VizException {

    	// FrameData currFrameData = (FrameData) frameData;
		
		if ( paintProps != null && prds != null ) {
			
			//  Loop through all products in the PGEN drawing layer,
			//  drawing the display elements
			
			for ( Product prod : prds ) {
				if ( prod.isOnOff() ) {
					for ( Layer layer : prod.getLayers() ) {					
					    if ( layer.isOnOff() ) {

					        Iterator<DrawableElement> iterator = layer.createDEIterator();
					        AbstractElementContainer container;
					        while ( iterator.hasNext()) {    
					        	DrawableElement el = iterator.next();
	                            container = ElementContainerFactory.createContainer(el, (MapDescriptor) descriptor, target);
	                            container.draw(target, paintProps, dprops);					        
					        }				
					    }
					}				
				}		
			}
		}
    }                        

	public void resourceAttrsModified() {
    	dprops.setLayerMonoColor( pgenResourceData.monoColorEnable );
    	dprops.setLayerColor( new Color (pgenResourceData.monoColor.red,
    			                         pgenResourceData.monoColor.green,
    			                         pgenResourceData.monoColor.blue) );
    	dprops.setLayerFilled( false /* pgenResourceData.fillModeEnable */ );
	}
}