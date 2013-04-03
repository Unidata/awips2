package gov.noaa.nws.ncep.viz.overlays.resources;


import gov.noaa.nws.ncep.ui.pgen.display.AbstractElementContainer;
import gov.noaa.nws.ncep.ui.pgen.display.DisplayProperties;
import gov.noaa.nws.ncep.ui.pgen.display.ElementContainerFactory;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.Layer;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;
import gov.noaa.nws.ncep.ui.pgen.file.FileTools;
import gov.noaa.nws.ncep.ui.pgen.file.ProductConverter;
import gov.noaa.nws.ncep.ui.pgen.file.Products;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;

import java.awt.Color;
import java.io.File;
import java.io.IOException;
import java.util.Iterator;
import java.util.List;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 12-20-11                 ghull        Initial creation
 * 12-23-11	    579         jzeng        Implementation
 * 12-18-12     861         ghull        allow display of original element colors from pgen product.
 * 
 * </pre>
 *  
 * @author 
 * @version 1.0
 */
public class PgenStaticOverlayResource extends AbstractVizResource<PgenStaticOverlayResourceData, IMapDescriptor> 
implements INatlCntrsResource {
	private PgenStaticOverlayResourceData pgenOverlayRscData; 
	
	/** Whether the resource is ready to be drawn */

	/** The list of points */
    List<Product> prds=null;

    DisplayProperties dprops = new DisplayProperties();
        
    /**
     * Create a PGEN XML Overlay resource.
     * 
     * @throws VizException
     */
    protected PgenStaticOverlayResource(PgenStaticOverlayResourceData resourceData,
            LoadProperties loadProperties) throws VizException {
        super(resourceData, loadProperties);
        pgenOverlayRscData = (PgenStaticOverlayResourceData) resourceData;
    }
    
	public void initInternal( IGraphicsTarget target ) throws VizException {
		
        String lFileName =  pgenOverlayRscData.getPgenStaticProductLocation() + File.separator +
                            pgenOverlayRscData.getPgenStaticProductName();
        
        //NcPathConstants.PGEN_XML_OVERLAYS + File.separator + pgenOverlayRscData.getPgenProductName();
        File productFile = null;
        
        if( lFileName.startsWith( NcPathConstants.NCEP_ROOT ) ) {
        	productFile = NcPathManager.getInstance().getStaticFile( lFileName );
        }
        else {
            // TODO : this should be considered temporary since soon the PGEN Files will be stored on the server and
            // this option will not work.
        	productFile = new File( lFileName );
        }
        
        if( productFile == null || !productFile.exists() ) {
                throw new VizException("Error. PGEN product: "+
                                lFileName + ", doesn't exist" );
        }
        
        // get the PGEN product data, and convert into format ready for display during paint
        //
        try {
        	Products products = FileTools.read( productFile.getCanonicalPath() );
        	prds = ProductConverter.convert( products );
        	        
        	resourceAttrsModified();
//        	dprops.setLayerMonoColor( pgenOverlayRscData.monoColorEnable );
//        	RGB rgb = pgenOverlayRscData.getColor();    	
//        	dprops.setLayerColor( new Color( rgb.red, rgb.green, rgb.blue ) );
        } 
        catch (IOException e) {
        	throw new VizException("Error reading PGEN Product, "+ productFile.getAbsolutePath() );
        }        
	}

	public void paintInternal(IGraphicsTarget target, PaintProperties paintProps)
			throws VizException, NullPointerException{
		
		if( paintProps != null && prds != null ) {			
			//  Loop through all products in the PGEN drawing layer,
			//  drawing the display elements
			for( Product prod : prds ) {
				if( prod.isOnOff() ) {
					for( Layer layer : prod.getLayers() ) {					
					    if( layer.isOnOff() ) {
					        Iterator<DrawableElement> iterator = layer.createDEIterator();
					        AbstractElementContainer container;
					        while ( iterator.hasNext()) {    
					        	DrawableElement el = iterator.next();
					        	container = ElementContainerFactory.createContainer(el, (MapDescriptor) descriptor, target);
					        	container.draw(target, paintProps, dprops);		
					        	container.dispose();
					        }				
					    }
					}				
				}
			}
		}
	}

	protected void disposeInternal() {
	}

	@Override
	public void resourceAttrsModified() {
    	dprops.setLayerMonoColor( pgenOverlayRscData.monoColorEnable );
    	RGB rgb = pgenOverlayRscData.getColor();    	
    	dprops.setLayerColor( new Color( rgb.red, rgb.green, rgb.blue ) );
//    	dprops.setLayerFilled( pgenOverlayRscData.isFillEnable() );
    	
    	// if monoColorEnable is turned off then set the legend color to the color of the first layer in the product
    	if( !pgenOverlayRscData.monoColorEnable ) {
    		Color legendCol = prds.get(0).getLayer(0).getColor();
    		pgenOverlayRscData.setLegendColor( new RGB( legendCol.getRed(), legendCol.getGreen(), legendCol.getBlue() ) );
    	}    	
	}
}
