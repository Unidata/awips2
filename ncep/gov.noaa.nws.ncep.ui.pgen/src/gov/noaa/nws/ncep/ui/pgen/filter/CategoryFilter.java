/*
 * CategoryFilter
 * 
 * Date created 03 FEBRUARY 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.filter;

import gov.noaa.nws.ncep.ui.pgen.contours.Contours;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DECollection;
import gov.noaa.nws.ncep.ui.pgen.elements.Jet;
import gov.noaa.nws.ncep.ui.pgen.elements.Outlook;
import gov.noaa.nws.ncep.ui.pgen.elements.Spenes;
import gov.noaa.nws.ncep.ui.pgen.elements.Text;
import gov.noaa.nws.ncep.ui.pgen.elements.labeledlines.LabeledLine;
import gov.noaa.nws.ncep.ui.pgen.elements.tcm.Tcm;
import gov.noaa.nws.ncep.ui.pgen.gfa.Gfa;
import gov.noaa.nws.ncep.ui.pgen.tca.TCAElement;

/**
 * Filter used to accept only the AbstractDrawableComponents that are part
 * of a given PGEN Category.
 * @author sgilbert
 *
 */
public class CategoryFilter implements ElementFilter {

	String category;
	
	/**
	 * @param category a PGEN category
	 */
	public CategoryFilter(String category) {
		this.category = category;
	}

	@Override
	public boolean accept(AbstractDrawableComponent adc) {


		if ( category.equalsIgnoreCase("MET") ) {
			if ( adc instanceof DECollection ){
				if ( ((DECollection)adc).getCollectionName().equalsIgnoreCase("Watch") ||
					 ((DECollection)adc).getCollectionName().equalsIgnoreCase("Outlook") ||
					 adc instanceof LabeledLine ||
					 adc instanceof Jet ||
					 adc instanceof Contours ) {
					return true;
				}
			}
			else if ( adc instanceof Jet.JetHash ||
					  adc instanceof Jet.JetBarb ||
					  adc instanceof Jet.JetText ){
				//exclude jet hash/text/barb
				return false;
			}
			else if ( adc instanceof Text && 
					  adc.getParent().getParent() != null &&
					  adc.getParent().getParent() instanceof Outlook ){
				return true;
			}
			else if	( adc.getParent().getPgenCategory() != null &&
					  adc.getParent().getPgenCategory().equalsIgnoreCase("MET") ) { 
				
					return true;
				
			}
			else if ( adc.getParent().getParent() != null && adc.getParent().getParent().getPgenCategory() != null && 
					adc.getParent().getParent().getPgenCategory().equalsIgnoreCase("MET" )){
				//for contours
				return true;
			}
			else if ( adc instanceof Gfa ||
					  adc instanceof TCAElement ||
					  adc instanceof Tcm ||
					  adc instanceof Spenes ){
				return true;
			}
			
		}
		else if(category.equalsIgnoreCase("SIGMET") ) {//CCFP is SIGMET but parent LabeledLine 
			return true; 
		}
		else if ( adc.getParent() != null && 
			      (adc.getParent() instanceof LabeledLine ||
			       adc.getParent().getParent() instanceof LabeledLine )) {
				//cannot select individual from LabeledLine collection
			 	if ( category.equalsIgnoreCase("ANY")){
			 		return true;
			 	}
			 	else {
			 		return false;
			 	}
		}
		else if ( category.equalsIgnoreCase("any") || 
				adc.getPgenCategory().equalsIgnoreCase(category) ) {
			if ( category.equalsIgnoreCase("Lines") ) {
				if ( 	adc.getParent() instanceof Jet ||
						adc.getParent().getParent() instanceof Jet ||
						adc.getParent() instanceof Outlook ||
						adc.getParent().getParent() instanceof Outlook ||
						adc.getParent().getParent() instanceof Contours ){
					//return false for lines that belong to a outlook/jet/contour
					return false;

				}
			}
			else if ( category.equalsIgnoreCase("Text")){
				if ( adc.getParent().getParent()!= null && 
					 adc.getParent().getParent() instanceof Contours ){
					if ( adc.getParent().getParent() instanceof Outlook ){
						return true;
					}
					else {
						return false;
					}
				}
			}
			
			return true;
			
		}
		
		return false;
	}

	public String getCategory() {
		return category;
	}

	public void setCategory(String category) {
		this.category = category;
	}

}
