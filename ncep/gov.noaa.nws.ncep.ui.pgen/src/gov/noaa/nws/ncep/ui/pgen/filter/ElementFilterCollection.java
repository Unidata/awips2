/*
 * ElementFilterCollection
 * 
 * Date created 03 FEBRUARY 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.filter;

import java.util.HashSet;
import java.util.Set;

import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;

/**
 * An ElementFilter that evaluates a set of ElementFilters.
 * @author sgilbert
 *
 */
public class ElementFilterCollection implements ElementFilter {

	Set<ElementFilter> filters;
	
	public ElementFilterCollection() {
		filters = new HashSet<ElementFilter>();
	}
	
	/**
	 * @param filters
	 */
	public ElementFilterCollection(ElementFilter filter) {
		this();
		addFilter(filter);
	}

	public void addFilter (ElementFilter filter) {
		filters.add(filter);
	}

	public void removeFilter(ElementFilter filter ){
		if( filters.contains(filter)){
			filters.remove(filter);
		}
	}
	
	@Override
	public boolean accept(AbstractDrawableComponent adc) {
		//System.out.println("filters =" + filters.size());

		for ( ElementFilter f : filters ) {
			if ( ! f.accept(adc) ) return false;
		}
		return true;
	}
	
	/**
	 * Check if any filter accept the element
	 * @param adc
	 * @return
	 */
	public boolean acceptOnce( AbstractDrawableComponent adc ){
		//This method is different from the accept(adc), which accepts adc only if  
		//all filters accept it.
		//This method return true if any of the filters accepts the input element adc.
		
		if ( filters.isEmpty()) return true;
		
		for ( ElementFilter f : filters ) {
			if ( f.accept(adc) ) return true;
		}
		
		return false;
	}

}
