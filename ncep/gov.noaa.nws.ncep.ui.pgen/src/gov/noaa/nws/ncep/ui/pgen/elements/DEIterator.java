/*
 * gov.noaa.nws.ncep.ui.pgen.rsc.PgenSinglePointDrawingTool
 * 
 * 20 June 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.elements;

import java.util.Iterator;
import java.util.ListIterator;
import java.util.Stack;

/**
 * Implementation of an iterator for DECollection 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 06/09		#116			B. Yin   	Initial Creation.
 * 03/12		#711			B. Yin		Handle empty DECollection
 * 
 * </pre>
 * 
 * @author	B. Yin
 */

public class DEIterator implements Iterator<DrawableElement> {

	private Stack<ListIterator<AbstractDrawableComponent>> stack = new Stack<ListIterator<AbstractDrawableComponent>>();
	
	/**
	 * Public constructor. 
	 * @param iterator - iterator of the list in the DECollection
	 */
	public DEIterator( ListIterator<AbstractDrawableComponent> iterator ){
		
		stack.push(iterator);
		
	}
	
	@Override
	public boolean hasNext() {
		boolean status = false;
		
		if ( !stack.empty()){

			ListIterator<AbstractDrawableComponent> iterator = stack.peek();
			if( !iterator.hasNext()){
				stack.pop();
				return hasNext();
			}
			else {
		//		ListIterator<AbstractDrawableComponent> it2 = iterator;
				int pIdx = iterator.previousIndex();
				while ( iterator.hasNext() ){
					AbstractDrawableComponent adc = iterator.next();
					if ( adc instanceof DrawableElement ){ 
						status = true;
						break;
					}
					else if ( adc instanceof DECollection ){ 
						status = hasDE((DECollection)adc);
						if ( status ) break;
					}
				}
				
				//reset iterator
				while( iterator.hasPrevious() ){
					if ( iterator.previousIndex() == pIdx ) break;
					else iterator.previous();
				}
				
				//if the last item is an empty DEC,
				//or if the current iterator is empty
				if ( !status && !stack.isEmpty()){
					stack.pop();
					return hasNext();
				}
				
			}
		}
		
		return status;
	}

	@Override
	public DrawableElement next() {
		if ( hasNext()) {
			return (DrawableElement)getNext();
		}
		else {
			return null;
		}
	}
	
	private AbstractDrawableComponent getNext() {
			Iterator<AbstractDrawableComponent> iterator = stack.peek();
			AbstractDrawableComponent component = (AbstractDrawableComponent) iterator.next();
			if ( component instanceof DECollection ){
				if ( hasDE((DECollection)component)){
					stack.push(((DECollection) component).getComponentListIterator());
				}
		//		Iterator<AbstractDrawableComponent> it = stack.peek();
		//		return it.next();
				return getNext();
			}
			else{
				return component;
			}
	}	

	@Override
	public void remove() {
		// TODO Auto-generated method stub
	}
	
	/**
	 * Check if the input collection contains a DE
	 * @param dec
	 * @return
	 */
	private boolean hasDE(DECollection dec){
		boolean status = false;
		Iterator<AbstractDrawableComponent> it = dec.getComponentIterator();
		while( it.hasNext() ){
			AbstractDrawableComponent adc = it.next();
			if ( adc instanceof DrawableElement ) {
				status = true;
				break;
			}
			else if ( adc instanceof DECollection ){
				status = hasDE((DECollection)adc);
				if ( status ) break;
			}
		}
		
		return status;
	}
	
}
