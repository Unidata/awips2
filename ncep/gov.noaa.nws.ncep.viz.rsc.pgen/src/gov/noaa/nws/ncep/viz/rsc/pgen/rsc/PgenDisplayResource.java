package gov.noaa.nws.ncep.viz.rsc.pgen.rsc;

import gov.noaa.nws.ncep.common.dataplugin.pgen.PgenRecord;
import gov.noaa.nws.ncep.ui.pgen.display.AbstractElementContainer;
import gov.noaa.nws.ncep.ui.pgen.display.DisplayProperties;
import gov.noaa.nws.ncep.ui.pgen.display.ElementContainerFactory;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.Layer;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;
import gov.noaa.nws.ncep.ui.pgen.file.ProductConverter;
import gov.noaa.nws.ncep.ui.pgen.file.Products;
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource.IRscDataObject;
import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;

import java.awt.Color;
import java.io.File;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.map.MapDescriptor;

import gov.noaa.nws.ncep.ui.pgen.file.FileTools;
import gov.noaa.nws.ncep.ui.pgen.store.PgenStorageException;
import gov.noaa.nws.ncep.ui.pgen.store.StorageUtils;

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
 * 09 Aug 2011  450         G. Hull     add pgen directory with the filename
 * 14 Dec 2012  861         G. Hull     ignore onOff Layer flag
 * 25 Jun 2013  1011        G. Hull     read from new pgen plugin/db
 * 
 * </pre>
 * 
 * @author bhebbard 
 * @version 1.0
 */
public class PgenDisplayResource extends AbstractNatlCntrsResource<PgenDisplayResourceData, NCMapDescriptor>
                                implements INatlCntrsResource {
	
	private PgenDisplayResourceData pgenResourceData;

	private DisplayProperties dprops = new DisplayProperties();
	
    private class TimeTaggedPgenProduct implements IRscDataObject {
    	
    	Product pgenProduct;
    	DataTime productTime;
    	
    	// break up the Products list into individually time tagged
    	// Products for time matching. (I know this isn't necessary now since
    	// all products will have the same time. Just demonstrating one option 
    	// for later...)
    	public TimeTaggedPgenProduct( Product pPrd, DataTime dt ) {
    		pgenProduct = pPrd;
    		productTime = dt;
    	}
    
		@Override
		public DataTime getDataTime() {
			return productTime;
		}
    	public Product getProduct() {
    		return pgenProduct;
    	}
    }

    private class FrameData extends AbstractFrameData {  

    	// Products after being timeMatched.
    	public List<Product> productList = new ArrayList<Product>();
    	
    	// TODO : if Products can actually have different times here then 
    	// will need another way to determine the latest product to time match. 
    	// (ie loop through products list for the 'same' product and choose the 
    	// one with the latest time, and put back in the list.)
    	private DataTime productTime;

		public FrameData(DataTime frameTime, int timeInt) {
			super( frameTime, timeInt );
    	}

		// note that if the same record time matches to 
        public boolean updateFrameData(IRscDataObject rscDataObj ) {

        	if( !(rscDataObj instanceof TimeTaggedPgenProduct) ) {
        		// if a pgen record then this is probably called from the autoupdate code.
        		// This currently can't happen since the pgen plugin doesn't issue dataURINotifications.
        		//
        		if( rscDataObj instanceof PgenRecord ) {
        			System.out.println("PgenDisplayResource not designed to work with auto update");
        		}
        		else {
        			System.out.println("sanity check: PgenDisplayResource.updateFrameData is not a TimeTaggedPgenProduct object??" );
        }
        		return false;
    }
    
        	TimeTaggedPgenProduct prodWithTime = (TimeTaggedPgenProduct)rscDataObj; 
        	
        	// TODO : if this is ever changed so that more than one record passes the query
        	// then we will need to write code to determine which times/products to display. 
        	// 
//        	if( !productList.isEmpty() ) {        		
//        		if( productTime.getValidTime().getTimeInMillis() < 
//        		        prodWithTime.getDataTime().getValidTime().getTimeInMillis() ) {
//        			return false;
//        		}
// .....        	
//        	}
        	
        	productTime = ((TimeTaggedPgenProduct)rscDataObj).productTime;
        	
        	productList.add( prodWithTime.pgenProduct );
        	
            return true;
        }
        
        public String getLegend() {
			
        	// TODO : should we use the label, Type, subType???
        	return pgenResourceData.getResourceName().getRscType()+" " +
        		   pgenResourceData.getResourceName().getRscGroup();
        	//        	if( )
//        	legendStr = pgenResourceData.getResourceName().getRscType()+" ";
//        	return (productList.isEmpty() ? legendStr+"(No PGEN Products)" : 
//        		legendStr + NmapCommon.getTimeStringFromDataTime(productTime, "/") );
        }
    }
    
    public PgenDisplayResource(PgenDisplayResourceData resourceData,
    		LoadProperties loadProperties) throws VizException {
    	super(resourceData, loadProperties);
    	pgenResourceData = (PgenDisplayResourceData) resourceData;
    }

    protected AbstractFrameData createNewFrame( DataTime frameTime, int timeInt ) {
    	return (AbstractFrameData) new FrameData( frameTime, timeInt );
    }
    
    // Get the ProductList from the PgenRecord and return individually wrapped Products.
    // The dataTime will be the same as the record but we could change this later.
    //
	// if all the constraints are set right, this should be unique and there
	// should be only one record. 
    @Override
	protected IRscDataObject[] processRecord( Object pdo ) {
    	if( !(pdo instanceof PgenRecord) ) {
    		System.out.println("sanity check: PgenDisplayResource.processRecord is not a PgenRecord??" );
    		return null;
    	}

    	List<IRscDataObject> wrappedProducts = new ArrayList<IRscDataObject>();
    	
    	PgenRecord pgenRec = (PgenRecord)pdo;
    	
    	String uri = pgenRec.getDataURI();
    	
        try {
        	List<Product> recProds = StorageUtils.retrieveProduct( uri );
        	
        	if( recProds.isEmpty() ) {
        		System.out.println("Error: No products retrieved for PgenRecord:"+ uri );
        	
        	}
        	else {
        		for( Product pgenProd : recProds ) {
        			// ...determine dataTime for product
        			wrappedProducts.add( new TimeTaggedPgenProduct( pgenProd, pgenRec.getDataTime() ));
        		}
        	}
        } 
        catch( PgenStorageException e ) {
        	StorageUtils.showError(e);
        }
        
        return wrappedProducts.toArray( new IRscDataObject[0] );
	}

    public void initResource(IGraphicsTarget grphTarget) throws VizException {
        
    	// set initial display values from resource attributes (as if after modification)
    	resourceAttrsModified();

    	// current design will have the dataTime as a request constraint so only 1 record should
    	// be found.
    	
    	if( !pgenResourceData.getMetadataMap().containsKey("dataTime") ) {
    	
    	}
    	
    	queryRecords();
    	
    	// preProcessFrameUpdate() will be called to remove all but the latest time.
    	// or we could have done it here.
    }
		
	// implement this to only process the latest times (could do this in updateFrameData but
    // this is easier.
    //
    @Override
	protected boolean preProcessFrameUpdate() {
    	long latestTimeMs = 0;
    	
		for( IRscDataObject rscDataObj : newRscDataObjsQueue ) {
			long dataTimeMs = rscDataObj.getDataTime().getValidTime().getTimeInMillis();
			if( latestTimeMs < dataTimeMs ) {
				latestTimeMs = dataTimeMs;
			}
		}
		List<IRscDataObject> latestDataObjs = new ArrayList<IRscDataObject>( newRscDataObjsQueue );
				 
		newRscDataObjsQueue.clear();
		
		for( IRscDataObject dataObj : latestDataObjs ) {
			if( dataObj.getDataTime().getValidTime().getTimeInMillis() == latestTimeMs ) {
				newRscDataObjsQueue.add( dataObj );
			}
		}

		return true;
    }
    
    public void paintFrame(AbstractFrameData fd, IGraphicsTarget target, PaintProperties paintProps) throws VizException {

    	FrameData frameData = (FrameData) fd;
		
		if ( paintProps != null && frameData.productList != null ) {
			
			//  Loop through all products in the PGEN drawing layer,
			//  drawing the display elements
			
			for ( Product prod : frameData.productList ) {
				if ( prod.isOnOff() ) {
					for ( Layer layer : prod.getLayers() ) {					
//					    if ( layer.isOnOff() ) {
					        Iterator<DrawableElement> iterator = layer.createDEIterator();
					        AbstractElementContainer container;
					        while ( iterator.hasNext()) {    
					        	DrawableElement el = iterator.next();
	                            container = ElementContainerFactory.createContainer(el, (MapDescriptor) descriptor, target);
	                            container.draw(target, paintProps, dprops);					        
					        }				
//					    }
					}				
				}		
			}
		}
    }                        

    @Override
    public String getName() {
    	FrameData fd = (FrameData)getCurrentFrame();
    	if( fd == null ) {
    		return "No Data";
    	}
    	return fd.getLegend();
    }
    
	public void resourceAttrsModified() {
    	dprops.setLayerMonoColor( pgenResourceData.monoColorEnable );
    	dprops.setLayerColor( new Color (pgenResourceData.monoColor.red,
    			                         pgenResourceData.monoColor.green,
    			                         pgenResourceData.monoColor.blue) );
    	dprops.setLayerFilled( false /* pgenResourceData.fillModeEnable */ );
	}
}