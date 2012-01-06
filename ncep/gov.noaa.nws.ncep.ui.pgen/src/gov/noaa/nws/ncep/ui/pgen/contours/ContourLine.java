/*
 * gov.noaa.nws.ncep.ui.pgen.contours.ContourLine
 * 
 * october 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.contours;

import gov.noaa.nws.ncep.ui.pgen.display.FillPatternList.FillPattern;
import gov.noaa.nws.ncep.ui.pgen.display.IText.DisplayType;
import gov.noaa.nws.ncep.ui.pgen.display.IText.FontStyle;
import gov.noaa.nws.ncep.ui.pgen.display.IText.TextJustification;
import gov.noaa.nws.ncep.ui.pgen.display.IText.TextRotation;
import gov.noaa.nws.ncep.ui.pgen.elements.DECollection;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.elements.Line;
import gov.noaa.nws.ncep.ui.pgen.elements.Text;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Class for a ContourLine element - simple DECollection with one line, and
 * one or more labels.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 10/09		#167		J. Wu   	Initial Creation.
 * 
 * </pre>
 * 
 * @author	J. Wu
 */
public class ContourLine extends DECollection {		
	
	private String[] labelString;
	private int	numOfLabels;
	
	/**
	*  Default constructor
	*/
	public ContourLine() {			
	    super( "ContourLine");	
	    numOfLabels = 0;
	    labelString = null;
	}
						
	/**
	* public constructor
	*/		
	public ContourLine ( ArrayList<Coordinate> linePoints, boolean closed, String[] text ) {
    					
		super( "ContourLine");
			
    	Line cline = new Line( null, new Color[]{ Color.red }, 2.0f, 1.0, closed, 
    				false, linePoints, 2, FillPattern.SOLID, "Lines", "LINE_SOLID" );
		
    	cline.setParent( this );

		add( cline );
			
		for ( String str : text ) {
	           
			Text lbl = new Text( null, "Courier", 14.0f, TextJustification.CENTER,
	                    null, 0.0, TextRotation.SCREEN_RELATIVE, new String[]{str},
	                    FontStyle.REGULAR, Color.GREEN, 0, 0, true, DisplayType.NORMAL,
	                    "Text", "General Text" );	
			lbl.setLocation( linePoints.get( linePoints.size()/2 ) );
	    	lbl.setAuto( true );
			lbl.setParent( this );
				
			add( lbl );
			
		}			
		
	}

	/**
	* public constructor
	*/		
	public ContourLine ( ArrayList<Coordinate> linePoints, boolean closed, String[] text, int nlabels ) {
    					
		super( "ContourLine");
		
		numOfLabels = nlabels;
		
		labelString = new String[ text.length ];
		for ( int ii = 0; ii < text.length; ii++ ) {
			labelString[ ii ] = new String ( text[ii] );
		}
		
    	Line cline = new Line( null, new Color[]{ Color.red }, 2.0f, 1.0, closed, 
    				false, linePoints, 2, FillPattern.SOLID, "Lines", "LINE_SOLID" );
		
    	cline.setParent( this );

		add( cline );
			
		for ( int ii = 0; ii < nlabels; ii++  ) {
	            
			Text lbl = new Text( null, "Courier", 14.0f, TextJustification.CENTER,
	                    null, 0.0, TextRotation.SCREEN_RELATIVE, labelString,
	                    FontStyle.REGULAR, Color.GREEN, 0, 0, true, DisplayType.NORMAL,
	                    "Text", "General Text" );
				
			lbl.setLocation( linePoints.get( linePoints.size()/2 ) );
	    	lbl.setAuto( true );    	
			lbl.setParent( this );
				
			add( lbl );
			
		}			
		
	}
		
	/**
	* public constructor
	*/		
	public ContourLine ( Line line, Text text, int nlabels ) {
    					
		super( "ContourLine");
		
		numOfLabels = nlabels;
		
		if ( line != null ) {
    	    Line cline = (Line)line.copy();
		
    	    cline.setParent( this );

		    add( cline );
		
		    if ( text != null && nlabels > 0 ) {
		    
			    labelString = new String[ text.getText().length ];
			    for ( int ii = 0; ii < text.getText().length; ii++ ) {
			        labelString[ ii ] = new String ( text.getText()[ii] );
			    }
			
			    for ( int ii = 0; ii < nlabels; ii++  ) {
	            
			        Text lbl = (Text)text.copy();
				    
			        ArrayList<Coordinate> pts = cline.getPoints();
			        
			        lbl.setLocation( pts.get( pts.size()/2 ) );
			    	lbl.setAuto( true );
			        lbl.setParent( this );
				
			        add( lbl );
			    }
		    }
			
		}			
		
	}
			
	@Override
	/**
	* make a deep copy of the contour line
	*/
	public ContourLine copy() {		
		
		ContourLine cline = new ContourLine();
		
		cline.numOfLabels = this.numOfLabels;
		cline.labelString = new String[ this.labelString.length ];
		for ( int ii = 0; ii < this.labelString.length; ii++ ) {
			cline.labelString[ ii ] = new String ( this.labelString[ ii ] );
		}

		
        Iterator<DrawableElement> iterator = this.createDEIterator();
        
        while ( iterator.hasNext() ) {   
        	DrawableElement de = (DrawableElement)(iterator.next().copy());
        	de.setParent( cline );
	        cline.add( de );
        }            
		
		return cline;			
	}

	/**
	* Get the labels of the contour line.
	*/
	public ArrayList<Text> getLabels() {		
				
        Iterator<DrawableElement> iterator = this.createDEIterator();
        
        ArrayList<Text> labels = new ArrayList<Text>();
        while ( iterator.hasNext() ){    				        					        					        	
        	DrawableElement de = iterator.next();
        	if ( de instanceof Text ) {
        	    labels.add( ((Text) de) );
            }
        }            
		
		return labels;			
	}
	
	/**
	* Get the label string of the contour line.
	* Note: all the labels on a contour line have the same text string.
	*/
	public String[] getLabelString() {		
		return labelString;			
	}

	/**
	* Get the number of labels on the original contour line.
	*/
	public int getNumOfLabels() {						
		return numOfLabels;			
	}

	/**
	 * Updates the label string for the contour line.
	 * Note: all the labels on a contour line have the same text string.
	 */
	public void updateLabelString( String[] label ) {		
		
        setLabelString( label );
        
        for ( Text lbl : getLabels() ){    				        					        					        	
        	lbl.setText( labelString );
        }
        
	}

	/**
	 *  Update the number of labels and create more labels if necessary.
	 */
	public void updateNumOfLabels( int nlabels ) {			
		
		if ( nlabels == getNumOfLabels() ) {
			return;
		}
		
		if ( getNumOfLabels() > 0 ) {
    		
			Text oldLabel = (Text)( getLabels().get(0).copy() );
		    
			for ( Text lbl : getLabels() ) {
				this.removeElement( lbl );
			}		    
					    
			for ( int ii = 0; ii < nlabels; ii++  ) {
	            
				Text lbl = (Text)( oldLabel.copy() );
				
				lbl.setAuto( true );
				
				lbl.setParent( this );
					
				add( lbl );
				
			}

		}
		else {
			
			for ( int ii = 0; ii < nlabels; ii++  ) {
	            
				Text lbl = new Text( null, "Courier", 14.0f, TextJustification.CENTER,
		                    null, 0.0, TextRotation.SCREEN_RELATIVE, labelString,
		                    FontStyle.REGULAR, Color.GREEN, 0, 0, true, DisplayType.NORMAL,
		                    "Text", "General Text" );
				
				lbl.setLocation( this.getLine().getPoints().get(0) );
				
				lbl.setAuto( true );
							    	
				lbl.setParent( this );
					
				add( lbl );
				
			}			

		}
		
		setNumOfLabels( nlabels );
		        
	}

	/**
	* Get the line of the contour line.
	*/
	public Line getLine() {		
				
        Iterator<DrawableElement> iterator = this.createDEIterator();
        
        Line cline = null;
        while ( iterator.hasNext() ){    				        					        					        	
        	DrawableElement de = iterator.next();
        	if ( de instanceof Line ) {
        	    cline = (Line)de;
        		break;
            }
        } 
        
        return cline;
		
	}
	
	/**
	 *  Remove a part from the contour line.
	 */
	public ArrayList<ContourLine> split( int start, int end ) {		
		
		ArrayList<ContourLine> newContourlines = new ArrayList<ContourLine>();
        
		ArrayList<Line> newLines = splitLine( getLine(), start, end );

		for ( Line ln : newLines ) {
			ContourLine ncline = this.copy();
			ncline.getLine().setClosed( ln.isClosedLine() );
			ncline.getLine().setLinePoints( ln.getPoints() );
			newContourlines.add( ncline );
		}
		        
        return newContourlines;
		
	}

	/**
	 * Removes a part from a line.  
	 */
	private ArrayList<Line> splitLine( Line element, int start, int end ) {
		
		ArrayList<Line> newlns;
		
	    if (element.isClosedLine()){						
			newlns = removePartFromClosedLine( element, start, end );						
		}
		else {						
			newlns =  removePartFromOpenLine( element, start, end);
		}
	    
	    return newlns;
		
	}

	/**
	 * Removes part from an open line
	 */
	private ArrayList<Line> removePartFromOpenLine( Line line, int pt1Index, int pt2Index ){
	    
		ArrayList<Line>  newLines = new ArrayList<Line>();
		Line element1 = null, element2 = null;
		
		List<Coordinate> points = line.getPoints();
    			
		if ( Math.abs( pt2Index - pt1Index ) + 1 == points.size() ){			
			//remove whole element			
		}
		else if ( pt1Index == 0 || pt2Index == points.size() - 1 ) {
			
			//remove part from one end
			element1 = (Line)line.copy();
			ArrayList<Coordinate> newPts = new  ArrayList<Coordinate>(points);
			
           if ( pt1Index == 0 ){
				newPts.subList(pt1Index, pt2Index).clear();
			}
			else if ( pt2Index == points.size() - 1 ){
				newPts.subList(pt1Index+1, pt2Index+1).clear();
			}
			
			element1.setPoints( newPts );
			newLines.add( element1 );
			
		}
		else {
			
			//remove part in the middle						
			element1 = (Line)line.copy();
			element1.setPoints( new ArrayList<Coordinate>(points.subList(0, pt1Index+1 )));
			
			element2 = (Line)line.copy();
			element2.setPoints(new ArrayList<Coordinate>(points.subList(pt2Index, points.size() )));
			
			newLines.add( element1 );
			newLines.add( element2 );						
		}
		
		return newLines;
	}
	
	/**
	 * Removes part from a closed line
	 */	
	private ArrayList<Line> removePartFromClosedLine( Line element, int pt1Index, int pt2Index ){
		
		ArrayList<Line>  newLines = new ArrayList<Line>();
		Line element1 = null;

		List<Coordinate> points = element.getPoints();

		element1 = (Line)element.copy();
		element1.setClosed( false );
		element1.getPoints().clear();

		if ( pt2Index - pt1Index + 1 >( points.size() -(pt2Index - pt1Index + 1) + 2 ) ){			
			//if there are more points between pt1 and pt2, remove the other part.
			element1.getPoints().addAll(points.subList(pt1Index, pt2Index + 1));						
		}
		else {
			element1.getPoints().addAll(points.subList(pt2Index, points.size()));
			element1.getPoints().addAll(points.subList(0, pt1Index+1));						
		}
		
		newLines.add( element1 );
		
		return newLines;
		
	}
	
	
   /*
    *  Sets the number of labels
    */
	public void setNumOfLabels ( int nlabels ) {
		numOfLabels = nlabels;
	}


	/*
	 *  Sets the label string
	 */
	public void setLabelString ( String[] label ) {		

		if ( label != null ) { 

			labelString = new String[ label.length ];
			for ( int ii = 0; ii < label.length; ii++ ) {
				labelString[ ii ] = new String ( label[ ii ] );
			}
		}
		else {
			labelString = new String[ 1 ];
			labelString[0] = "";
		}
	}
	
}
