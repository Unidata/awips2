/*
 * gov.noaa.nws.ncep.ui.pgen.elements.labeledLines.LabeledLine
 * 
 * 5 September 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.elements.labeledlines;

import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DECollection;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.Layer;
import gov.noaa.nws.ncep.ui.pgen.elements.Line;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Implements a base class for PGEN labeled lines(cloud, turbulence, etc.).
 * 
 * A LabeledLin can have multiple lines and multiple labels.
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

public class LabeledLine extends DECollection {

	/**
	 * Constructor
	 * @param name
	 */
	public LabeledLine( String name ){
		super( name );
	}
	
	/**
	 * Constructor
	 * @param name	- name string
	 * @param ln 	- line
	 * @param lbl	- label
	 */
	public LabeledLine( String name, Line ln, Label lbl ){
		super( name );
		if ( ln != null ) add(ln);
		if ( lbl != null ) add(lbl);
	}
	
	/**
	 * Add a line into the labeledLine
	 * @param ln
	 */
	public void addLine( Line ln ){
		if ( ln != null ) add(ln);
	}
	
	/**
	 * Remove a line from the labeledLine
	 * @param ln
	 */
	public void rmLine( Line ln ){
		Iterator<AbstractDrawableComponent> it  = getComponentIterator();
		while( it.hasNext() ){
			if ( it.next() == ln ){
				it.remove();
				break;
			}
		}
	}
	
	/**
	 * Add a label to the labeledLine
	 * @param lbl
	 */
	public void addLabel( Label lbl ){
		if ( lbl != null ) add(lbl);
	}
	
	/**
	 * Remove a label from the labeledLine
	 * @param lbl
	 */
	public void rmLabel( Label lbl ){
		Iterator<AbstractDrawableComponent> it  = getComponentIterator();
		while( it.hasNext() ){
			if ( it.next() == lbl ){
				it.remove();
				break;
			}
		}
	}

	/**
	 * Get all lines in the labeledLine
	 * @return
	 */
	public List<Line> getLines(){
		ArrayList<Line> lines = new ArrayList<Line>();
		Iterator<AbstractDrawableComponent> it  = getComponentIterator();
		while( it.hasNext() ){
			AbstractDrawableComponent adc = it.next();
			if ( adc instanceof Line ){
				lines.add((Line)adc);
			}
		}
		return lines;
	}
	
	/**
	 * Get all labels in the labeledLine
	 * @return
	 */
	public List<Label> getLabels(){
		ArrayList<Label> labels = new ArrayList<Label>();
		Iterator<AbstractDrawableComponent> it  = getComponentIterator();
		while( it.hasNext() ){
			AbstractDrawableComponent adc = it.next();
			if ( adc instanceof Label ){
				labels.add((Label)adc);
			}
		}
		return labels;
	}
	
	/**
	 * Deep copy
	 */
	public LabeledLine copy(){
		LabeledLine ll = new LabeledLine( this.getName());
		ll.setParent( this.parent);
		ll.setPgenCategory(pgenCategory);
		ll.setPgenType(pgenType);
		
		Iterator<AbstractDrawableComponent> it = this.getComponentIterator();
		while( it.hasNext() ){
			ll.add(it.next().copy());
		}
		
		return ll;
	}
	
	@Override
	public DrawableElement getPrimaryDE(){
		
		for ( AbstractDrawableComponent adc : compList ){
			if ( adc instanceof Line ) return (Line)adc;
		}
		
		return null;
	}
	
	public Label getLabelAt( Coordinate loc ){
		Iterator<AbstractDrawableComponent> it = getComponentIterator();
		while( it.hasNext() ){
			AbstractDrawableComponent adc =it.next();
			if ( adc instanceof Label ){
				Label lbl = (Label)adc;
				if ( Math.abs(lbl.getSpe().getLocation().x - loc.x) < 0.0001 &&
						Math.abs(lbl.getSpe().getLocation().y - loc.y) < 0.0001 ) {

					//get the label at loc
					return lbl;
				}
			}
		}
		return null;
	}
	
}
