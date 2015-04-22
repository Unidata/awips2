/*
 * gov.noaa.nws.ncep.ui.pgen.elements.labeledLines.Label
 * 
 * 5 September 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.elements.labeledlines;

import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DECollection;
import gov.noaa.nws.ncep.ui.pgen.elements.Line;
import gov.noaa.nws.ncep.ui.pgen.elements.SinglePointElement;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * Implements a label class for PGEN labeled lines
 * 
 * A Label can have a text(or avnText, symbol, etc.) and a number of 
 * arrow lines.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 09/10		#304			B. Yin   	Initial Creation.
 * 
 * </pre>
 * 
 * @author	B. Yin
 */

public class Label extends DECollection {
	
	// text, avnText, or symbol
	SinglePointElement spe;

	/**
	 * constructor
	 */
	public Label(){
		super("Label");
	}
	
	/**
	 * Constructor
	 * @param spe - text, avnText, or Symbol
	 */
	public Label(SinglePointElement spe){
		super("Label");
		this.spe = spe;
		add(spe);
	}

	/**
	 * Get the label content
	 * @return
	 */
	public SinglePointElement getSpe() {
		return spe;
	}
	
	/**
	 * Set label
	 * @param spe
	 */
	public void setSpe(SinglePointElement spe) {
		if ( spe != null ){
			if ( this.spe != null ){
				this.remove(this.spe);
			}
		
			this.spe = spe;
			this.add(spe);
		}
	}
	
	/**
	 * Add a arrow line for the label
	 * @param arrow
	 */
	public void addArrow( Line arrow ){
		if ( arrow != null ) add(arrow);
	}
	
	/**
	 * Get all arror lines for the label
	 * @return
	 */
	public List<Line> getArrows(){
		List<Line> arrows = new ArrayList<Line>();
		Iterator<AbstractDrawableComponent> it = this.getComponentIterator();
		while( it.hasNext() ){
			AbstractDrawableComponent adc = it.next();
			if ( adc instanceof Line ){
				arrows.add((Line)adc);
			}
		}
		
		return arrows;
	}
	
	/**
	 * Deep copy
	 */
	public Label copy(){
		Label lbl = new Label( (SinglePointElement)this.spe.copy() );
		lbl.setPgenCategory(pgenCategory);
		lbl.setPgenType(pgenType);
		lbl.setParent(parent);
		
		for( Line ln : getArrows() ){
			lbl.addArrow((Line)ln.copy());
		}
		
		return lbl;
	}
}
