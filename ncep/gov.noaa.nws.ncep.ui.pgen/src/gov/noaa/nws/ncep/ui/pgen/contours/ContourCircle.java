/*
 * gov.noaa.nws.ncep.ui.pgen.contours.ContourCircle
 * 
 * June 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.contours;

import gov.noaa.nws.ncep.ui.pgen.display.FillPatternList.FillPattern;
import gov.noaa.nws.ncep.ui.pgen.display.IText.DisplayType;
import gov.noaa.nws.ncep.ui.pgen.display.IText.FontStyle;
import gov.noaa.nws.ncep.ui.pgen.display.IText.TextJustification;
import gov.noaa.nws.ncep.ui.pgen.display.IText.TextRotation;
import gov.noaa.nws.ncep.ui.pgen.elements.Arc;
import gov.noaa.nws.ncep.ui.pgen.elements.DECollection;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.Text;

import java.awt.Color;
import java.util.Iterator;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Class for a ContourCircle element - simple DECollection with one Circle, and
 * one Text label.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 11/10		#345		J. Wu   	Initial Creation.
 * 
 * </pre>
 * 
 * @author	J. Wu
 */
public class ContourCircle extends DECollection {		
	
	/**
	*  Default constructor
	*/
	public ContourCircle() {			
	    super( "ContourCircle");	
	}
						
	/**
	* public constructor
	*/		
	public ContourCircle ( Coordinate center, Coordinate circum, String[] text, boolean hide ) {
    					
		super( "ContourCircle");
	    
		DrawableElement cArc = null;
		// Create an Arc		            
		cArc= new Arc( null, Color.red, 3.0F, 1.0, false, false, 2, FillPattern.SOLID, 
			 "Circle", center, circum, "Arc", 1.0, 0.0, 360.0 );
		
    	cArc.setParent( this );

		add( cArc );
		
		// Create a Text to label the circle		
		Text lbl = new Text( null, "Courier", 14.0f, TextJustification.CENTER,
	                    null, 0.0, TextRotation.SCREEN_RELATIVE, text,
	                    FontStyle.REGULAR, Color.GREEN, 0, 0, true, DisplayType.NORMAL,
	                    "Text", "General Text" );

		lbl.setLocation( circum );
		
		lbl.setAuto( true );
	    	
		lbl.setParent( this );
		
		lbl.setHide( hide );
				
		add( lbl );
			
	}
		
			
	@Override
	/**
	* make a deep copy of the contour circle
	*/
	public ContourCircle copy() {		
		
		ContourCircle cmm = new ContourCircle();
				
        Iterator<DrawableElement> iterator = this.createDEIterator();
        
        while ( iterator.hasNext() ) {   
        	DrawableElement de = (DrawableElement)(iterator.next().copy());
        	de.setParent( cmm );
	        cmm.add( de );
        }            
		
		return cmm;			
	}

	/**
	* Get the label of the Contour circle.
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
	* Get the label string of the contour Circle.
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
	 * Updates the label string for the contour Circle.
	 */
	public void updateLabelString( String[] text ) {		
		
		Text label = getLabel();
		if ( label != null ) {
        	label.setText( text );
        }
        
	}


	/**
	* Get the circle for the contour Circle.
	*/
	public DrawableElement getCircle() {		
				
        Iterator<DrawableElement> iterator = this.createDEIterator();
        
        DrawableElement csym = null;
        while ( iterator.hasNext() ){    				        					        					        	
        	DrawableElement de = iterator.next();
        	if ( de instanceof Arc ) {
        	    csym = de;
        		break;
            }
        } 
        
        return csym;
		
	}
				
}

