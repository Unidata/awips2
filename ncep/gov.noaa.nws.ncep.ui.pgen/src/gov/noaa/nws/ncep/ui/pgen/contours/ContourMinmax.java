/*
 * gov.noaa.nws.ncep.ui.pgen.contours.ContourMinMax
 * 
 * June 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.contours;

import gov.noaa.nws.ncep.ui.pgen.display.IText.DisplayType;
import gov.noaa.nws.ncep.ui.pgen.display.IText.FontStyle;
import gov.noaa.nws.ncep.ui.pgen.display.IText.TextJustification;
import gov.noaa.nws.ncep.ui.pgen.display.IText.TextRotation;
import gov.noaa.nws.ncep.ui.pgen.elements.ComboSymbol;
import gov.noaa.nws.ncep.ui.pgen.elements.DECollection;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.Symbol;
import gov.noaa.nws.ncep.ui.pgen.elements.Text;

import java.awt.Color;
import java.util.Iterator;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Class for a ContourMinmax element - simple DECollection with one Symbol, and
 * one Text label.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 06/10		#215		J. Wu   	Initial Creation.
 * 
 * </pre>
 * 
 * @author	J. Wu
 */
public class ContourMinmax extends DECollection {		
	
	/**
	*  Default constructor
	*/
	public ContourMinmax() {			
	    super( "ContourMinmax");	
	}
						
	/**
	* public constructor
	*/		
	public ContourMinmax ( Coordinate loc, String pgenCat, String pgenType, String[] text ) {
    					
		super( "ContourMinmax");
	    
		DrawableElement cmm = null;
		// Create a Symbol or a ComboSymbol		            
        if ( pgenCat.equals("Combo") ) {
        	 cmm = new ComboSymbol( null, new Color[] {Color.green},
        			2.0F, 1.0, true, loc, pgenCat,  pgenType );
        }
        else {
       	     cmm = new Symbol( null, new Color[] {Color.green},
     			2.0F, 2.0, true, loc, pgenCat,  pgenType );     	
        }

    	cmm.setParent( this );

		add( cmm );
		
		// Create a Text under the Symbol		
		Text lbl = new Text( null, "Courier", 14.0f, TextJustification.CENTER,
	                    null, 0.0, TextRotation.SCREEN_RELATIVE, text,
	                    FontStyle.REGULAR, Color.GREEN, 0, 0, true, DisplayType.NORMAL,
	                    "Text", "General Text" );
		Coordinate p = new Coordinate( loc.x, loc.y - 2.5 );		
		lbl.setLocation( p );
	    lbl.setAuto( true );	
		lbl.setParent( this );
				
		add( lbl );
			
	}
		
			
	@Override
	/**
	* make a deep copy of the contour Min/max
	*/
	public ContourMinmax copy() {		
		
		ContourMinmax cmm = new ContourMinmax();
				
        Iterator<DrawableElement> iterator = this.createDEIterator();
        
        while ( iterator.hasNext() ) {   
        	DrawableElement de = (DrawableElement)(iterator.next().copy());
        	de.setParent( cmm );
	        cmm.add( de );
        }            
		
		return cmm;			
	}

	/**
	* Get the label of the Contour Min/max.
	*/
	public Text getLabel() {		
				
        Iterator<DrawableElement> iterator = this.createDEIterator();
        
        Text label = null;
        while ( iterator.hasNext() ){    				        					        					        	
        	DrawableElement de = iterator.next();
        	if ( de instanceof Text ) {
        	    label = (Text)de;
        	    break;
            }
        }            
		
		return label;			
	}
	
	/**
	* Get the label string of the contour Min/Max.
	*/
	public String[] getLabelString() {		
		Text label = getLabel();
		if ( label != null ) {
			return label.getText();
		}
		else {
			return null;	
		}
	}


	/**
	 * Updates the label string for the contour Min/Max.
	 */
	public void updateLabelString( String[] text ) {		
		
		Text label = getLabel();
		if ( label != null ) {
        	label.setText( text );
        }
        
	}


	/**
	* Get the Symbol for the contour Min/Max.
	*/
	public DrawableElement getSymbol() {		
				
        Iterator<DrawableElement> iterator = this.createDEIterator();
        
        DrawableElement csym = null;
        while ( iterator.hasNext() ){    				        					        					        	
        	DrawableElement de = iterator.next();
        	if ( de instanceof Symbol || de instanceof ComboSymbol ) {
        	    csym = de;
        		break;
            }
        } 
        
        return csym;
		
	}
				
}

