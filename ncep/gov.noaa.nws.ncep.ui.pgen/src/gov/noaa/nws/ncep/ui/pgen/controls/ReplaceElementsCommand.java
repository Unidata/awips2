/*
 * gov.noaa.nws.ncep.ui.pgen.controls.ReplaceElementsCommand
 * 
 * Date created: August 5th 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.controls;

import java.util.ArrayList;
import java.util.List;

import gov.noaa.nws.ncep.ui.pgen.PGenException;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DECollection;

/**
 * Replace a set of existing elements in a layer (product) with a set of new elements.  
 * The original elements can be restored as an undo feature.  
 * 
 * Note: This command works as "add" if no existing elements are provided. And it works
 *       as "delete" if no new elements are provided.       
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 08/09		#141		Jun Wu  	Initial Creation.
 * 03/10		#159		B. Yin		Set the parent of output elements to that of
 * 										input elements, instead of active layer. 
 * 04/13		#874		B. Yin		If the parent is null, loop through every element,
 * 										find its parent, remove the old DE and add the new DE.
 * 
 * </pre>
 * 
 * @author	J. Wu
 */
public class ReplaceElementsCommand extends PgenCommand {
	
	/*
	 * Layer that contains the element to be replaced
	 */
	private DECollection parent;
	
	/*
	 * The drawables to be replaced
	 */
	private List<AbstractDrawableComponent> oldElements = null;
	
	/*
	 * The new drawable elements
	 */
	private List<AbstractDrawableComponent> newElements = null;
	
	/**
	 * Constructor used to specify the product layer as well as the old and new drawable element.
	 * @param layer 	 The layer contains the elements
	 * @param oldElement Drawable element to replace
	 * @param newElement New drawable element
	 */
	public ReplaceElementsCommand( DECollection parent, AbstractDrawableComponent oldElement, 
			AbstractDrawableComponent newElement) {
		
		this.parent = parent;
		
		if ( this.oldElements == null ) {
			this.oldElements = new ArrayList<AbstractDrawableComponent>();
		}
		else {
			this.oldElements.clear();
		}
		
		this.oldElements.add( oldElement );	

		if ( this.newElements == null ) {
			this.newElements = new ArrayList<AbstractDrawableComponent>();
		}
		else {
			this.newElements.clear();
		}
		
		this.newElements.add( newElement );	
		
				
	}
	/**
	 * Constructor used to specify the product layer as well as the old and new drawable element.
	 * @param The layer contains the elements
	 * @param oldElements Drawable elements to replace
	 * @param newElements New drawable elements
	 */
	public ReplaceElementsCommand( DECollection parent, List<AbstractDrawableComponent> oldElements, 
			List<AbstractDrawableComponent> newElements) {
		
		this.parent = parent;	
		this.oldElements = oldElements;
		this.newElements = newElements;
		
	}

	/**
	 * Replaces drawable elements with new elements in a layer
	 * @see gov.noaa.nws.ncep.ui.pgen.controls.PgenCommand#execute()
	 */
	@Override
	public void execute() throws PGenException {

		if ( parent != null ){
			if ( oldElements != null ) {	
				for ( AbstractDrawableComponent ade : oldElements ) {
					parent.removeElement( ade );
				}
			}

			if ( newElements != null ) {
				parent.add( newElements );			
			}				
		}	
		else if ( oldElements.size() == newElements.size() ){
			for ( int ii = 0; ii < oldElements.size(); ii++ ) {
				AbstractDrawableComponent ade  = oldElements.get(ii);
				if ( ade.getParent() != null && ade.getParent() instanceof DECollection ){
					DECollection dec = (DECollection) ade.getParent();
					dec.removeElement( ade );
					dec.add( newElements.get( ii ));
				}
			}
		}
	}

	/**
	 * Restores the original elements back in the layer
	 * @see gov.noaa.nws.ncep.ui.pgen.controls.PgenCommand#undo()
	 */
	@Override
	public void undo() throws PGenException {

		if ( parent != null ){
			if ( newElements != null ) {
				for ( AbstractDrawableComponent ade : newElements ) {
					parent.removeElement( ade );
				}	    	
			}

			if ( oldElements != null ) {
				parent.add( oldElements );
			}
		}
		else if ( oldElements.size() == newElements.size() ){
			for ( int ii = 0; ii < newElements.size(); ii++ ) {
				AbstractDrawableComponent ade  = newElements.get(ii);
				if ( ade.getParent() != null && ade.getParent() instanceof DECollection ){
					DECollection dec = (DECollection) ade.getParent();
					dec.removeElement( ade );
					dec.add( oldElements.get( ii ));
				}
			}
		}
	}

}

