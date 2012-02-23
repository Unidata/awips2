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

import java.io.File;
import java.io.IOException;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.PaintStatus;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
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
 * </pre>
 * 
 * This class is copied over from com.raytheon.viz.core.rsc.DbMapResource
 * 
 * @author mgao
 * @version 1.0
 */

public class PgenXmlOverlayResource extends AbstractVizResource<PgenXmlOverlayResourceData, IMapDescriptor> 
implements INatlCntrsResource {
	private PgenXmlOverlayResourceData pgenXmlResourceData; 
	
	/** Whether the resource is ready to be drawn */

	/** The list of points */
    List<gov.noaa.nws.ncep.ui.pgen.elements.Product> prds;

    DisplayProperties dprops = new DisplayProperties();
    //private ConcurrentHashMap<DrawableElement,AbstractElementContainer> displayMap;
	
    //  ------------------------------------------------------------
    
    /**
     * Create a PGEN XML Overlay resource.
     * 
     * @throws VizException
     */
    protected PgenXmlOverlayResource(PgenXmlOverlayResourceData resourceData,
            LoadProperties loadProperties) throws VizException {
        super(resourceData, loadProperties);
        pgenXmlResourceData = (PgenXmlOverlayResourceData) resourceData;
        //displayMap = new ConcurrentHashMap<DrawableElement,AbstractElementContainer>();        
    }
    
    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#init(com.raytheon.uf
     * .viz.core.IGraphicsTarget)
     */
	public void initInternal( IGraphicsTarget target ) throws VizException {
		
    	resourceAttrsModified();
        String locName = NcPathConstants.PGEN_XML_OVERLAYS + File.separator + pgenXmlResourceData.getPgenProductName();
        
        File productFile = NcPathManager.getInstance().getStaticFile( locName );
        
        if( productFile == null || !productFile.exists() ) {
                throw new VizException("Error. PGEN product: "+
                                pgenXmlResourceData.getPgenProductName() + ", doesn't exist" );
        }
        // get the PGEN product data, and convert into format ready for display during paint
        Products products;
        try {
                products = FileTools.read( productFile.getCanonicalPath() );
                prds = ProductConverter.convert( products );
        } catch (IOException e) {
                e.printStackTrace();
        }        

	}
	

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.viz.core.rsc.IVizResource#paint(com.raytheon.viz.core.IGraphicsTarget,
	 *      com.raytheon.viz.core.PixelExtent, double, float)
	 */	
	public void paintInternal(IGraphicsTarget target, PaintProperties paintProps)
			throws VizException, NullPointerException{

		
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
		// Nothing to do. The modified color, linewidth.... will be picked up on the next paint
		
	}
}
