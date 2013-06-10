/*
 * AbstractElementContainer
 * 
 * Date created: 08 DECEMBER 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.display;

import java.util.List;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.map.IMapDescriptor;

import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.tcm.ITcm;
import gov.noaa.nws.ncep.ui.pgen.elements.tcm.ITcmFcst;
import gov.noaa.nws.ncep.ui.pgen.gfa.IGfa;
import gov.noaa.nws.ncep.ui.pgen.tca.ITca;
import gov.noaa.nws.ncep.ui.pgen.sigmet.ISigmet;

/**
 * This Element Container is the base class for all Element Containers.  It's function
 * is to hold a PGEN DrawableElement along with associated renderable objects that depict the 
 * DrawableElement on the graphics target.  
 * 
 * Subclasses' implementation of the draw method should determine when the IDisplayables for the 
 * Drawable Element should be recreated.  IDisplayables can be created using the 
 * createDisplayables() method.
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 12/09     	#160        G. Zhang    Added ISigmet for Sigmet support
 * 03/10		#223		M.Laryukhin	Gfa added. 
 * 04/11		#?			B. Yin		Re-factor IAttribute
 * 09/12					B. Hebbard  Merge RTS changes from OB12.9.1
 * </pre>
 * 
 * @author sgilbert
 */
public abstract class AbstractElementContainer {

	/*
	 * The PGEN Drawable Element to be rendered.
	 */
	protected DrawableElement element;
	protected IMapDescriptor mapDescriptor;
	protected IGraphicsTarget target;
	protected DisplayElementFactory def;
	
	/*
	 * Objects that can be rendered on the graphics target depicting the DrawableElement.
	 */
	protected List<IDisplayable> displayEls = null;

	/**
	 * @param element
	 * @param mapDescriptor
	 */
	public AbstractElementContainer(DrawableElement element, IMapDescriptor mapDescriptor, IGraphicsTarget target) {
		this.element = element;
		this.mapDescriptor = mapDescriptor;
		this.target = target;
		def = new DisplayElementFactory( target, mapDescriptor);
	}

	/**
	 * Sets a new mapDescriptor.  All IDisplayables will be recreated.
	 * @param mapDescriptor the mapDescriptor to set
	 */
	public void setMapDescriptor(IMapDescriptor mapDescriptor) {
		this.mapDescriptor = mapDescriptor;
		def = new DisplayElementFactory( target, mapDescriptor);
		dispose();
		displayEls = null;
	}
	
	/**
	 * Draws to the given graphics target.  Recreates the IDisplayable objects, if
	 * necessary.
	 * @param target
	 * @param paintProps 
	 * @param dprops PGEN Layer properties
	 */
	public abstract void draw(IGraphicsTarget target, PaintProperties paintProps, 
			DisplayProperties dprops);
	
	/**
	 * Draws to the given graphics target.  Recreates the IDisplayable objects, if
	 * necessary.
	 * @param target
	 * @param paintProps 
	 * @param dprops PGEN Layer properties
	 * @param needsCreate
	 */
	public abstract void draw(IGraphicsTarget target, PaintProperties paintProps, 
			DisplayProperties dprops, boolean needsCreate);
	
	/**
	 * Uses a DisplayElementFactory to create IDisplayable objects from the Drawable Element
	 * @param paintProps
	 */
	protected void createDisplayables(PaintProperties paintProps) {
		
		//Cleanup first
		if ( (displayEls!=null) && !displayEls.isEmpty() ) reset();
		if ( element instanceof IAvnText ) {
			displayEls = def.createDisplayElements( (IAvnText) element, paintProps );		   			
		}
		else if ( element instanceof IMidCloudText ) {
			displayEls = def.createDisplayElements( (IMidCloudText) element, paintProps );		   			
		}		
		else if ( element instanceof IText ) {
			displayEls = def.createDisplayElements( (IText) element, paintProps );
		}
		else if ( element instanceof IVector ) {
			displayEls = def.createDisplayElements( (IVector) element, paintProps );		   			
		}
		else if ( element instanceof ICombo ) {
			displayEls = def.createDisplayElements( (ICombo) element, paintProps );		   			
		}
		else if ( element instanceof ITca ) {
			displayEls = def.createDisplayElements( (ITca) element, paintProps );		   			
		}
		else if ( element instanceof ISigmet ) { 
			displayEls = def.createDisplayElements( (ISigmet) element, paintProps );
		}
		else  if ( element instanceof ISymbol ){
		    displayEls = def.createDisplayElements( (ISymbol) element, paintProps );
	    } 
		else if ( element instanceof ITrack ) {
	    	displayEls = def.createDisplayElements( (ITrack) element, paintProps );
	    }
	    else if ( element instanceof IWatchBox ){
	    	displayEls = def.createDisplayElements( (IWatchBox) element, paintProps );
	    } 
	    else if ( element instanceof ITcm ){
	    	displayEls = def.createDisplayElements( (ITcm) element, paintProps );
	    } 
	    else if ( element instanceof IMultiPoint ){
		    if ( element instanceof IKink ){
			    displayEls = def.createDisplayElements( (IKink) element, paintProps );
		    }
		    else if ( element instanceof IArc ) {
		    	displayEls = def.createDisplayElements( (IArc) element, paintProps );
		    }
		    else if ( element instanceof IGfa ){
		    	displayEls = def.createDisplayElements( (IGfa) element, paintProps );
		    }
		    else if ( element instanceof ILine ){
		    	displayEls = def.createDisplayElements( (ILine) element, paintProps, true );
		    }
	    }
	}
	
	private void reset() {
		def.reset();
	}

	/**
	 * Releases the resources held by any of the IDisplayables
	 */
	public void dispose() {
		
		if ( displayEls == null ) 	return;
		
		for ( IDisplayable each : displayEls ) {
	         each.dispose();
		}
		displayEls.clear();
	}
	
	public void setElement(DrawableElement el) {
		this.element = el;
	}
	
	
}
