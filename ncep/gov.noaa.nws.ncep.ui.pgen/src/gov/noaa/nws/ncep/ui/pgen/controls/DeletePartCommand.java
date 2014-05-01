/*
 * DeletePartCommand
 * 
 * Date created: 7 May 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.controls;

import java.util.List;
import java.util.ArrayList;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateList;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.linearref.LinearLocation;
import com.vividsolutions.jts.linearref.LocationIndexedLine;

import gov.noaa.nws.ncep.ui.pgen.PGenException;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DECollection;
import gov.noaa.nws.ncep.ui.pgen.elements.Jet;
import gov.noaa.nws.ncep.ui.pgen.elements.Line;
import gov.noaa.nws.ncep.ui.pgen.elements.MultiPointElement;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;
import gov.noaa.nws.ncep.ui.pgen.gfa.Gfa;

/**
 * Implements a PgenCommand to delete part of a MultiPointElement.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 05/09			79		B. Yin   	Initial Creation.
 * 02/11					J. Wu   	Make sure GFA is always closed.
 * 02/11			?		B. Yin		Makd it work for Outlook
 * 04/11			?		B. Yin		Changed element from MulitPoint to Line
 * 										(Re-factor IAttribute)
 * 05/11			#808	J. Wu		Update Gfa vor text
 * 06/12			TTR102	B. Yin		Make it work for Jet
 *
 * </pre>
 * 
 * @author	B. Yin
 */
public class DeletePartCommand extends PgenCommand {

	/*
	 * product list from which element should be modified 
	 */
	private List<Product> list;
	
	/*
	 * layer from which element should be modfified
	 */
//	private Layer layer;
	private DECollection parent;
	
	/*
	 * drawable element to modified
	 */
	private AbstractDrawableComponent element;
	
	/*
	 * elements after deleting
	 */
	private AbstractDrawableComponent element1 = null;
	private AbstractDrawableComponent element2 = null;

	/*
	 * Two end point and locations of the deleting part 
	 */
	private Coordinate firstPt;
	private Coordinate secondPt;
	LocationIndexedLine lil;
	private LinearLocation firstLoc, secondLoc;
	
	/*
	 * flags of deleting types
	 */
	private boolean removeAll;
	private boolean removeOneEnd;
	private boolean removeMiddle;

	/**
	 * Constructor used to specify the element, part to delete and product list.
	 * @param list Product list from which element should be deleted.
	 * @param element - drawable element to delete.
	 * @param point1 - the first point of the deleting part
	 * @param point2 - the second point of the deleting part
	 */
	public DeletePartCommand(List<Product> list, Line element,
			Coordinate point1, Coordinate point2) {
		this.list = list;
		this.element = element;

 		GeometryFactory gf = new GeometryFactory();

 		/*
 		 * For each given point, find the location of its closest point on the line.
 		 * Save order of points along line.
 		 */
 		CoordinateList clist = new CoordinateList( element.getLinePoints() );
 		if ( element.isClosedLine() ) clist.closeRing();
 		LineString ls = gf.createLineString( clist.toCoordinateArray() );
 		lil = new LocationIndexedLine(ls);
 		LinearLocation loc1 = lil.project(point1);
 		LinearLocation loc2 = lil.project(point2);
 		if ( loc1.compareTo(loc2) <= 0 ) {
 			firstLoc = loc1;
 			secondLoc = loc2;
 			this.firstPt = point1;
 			this.secondPt = point2;
 		}
 		else {
 			firstLoc = loc2;
 			secondLoc = loc1;
 			this.firstPt = point2;
 			this.secondPt = point1;
 		}

	}

	/**
	 * Removes the part to be deleted.  Saves the layer for possible undo
	 * @see gov.noaa.nws.ncep.ui.pgen.controls.PgenCommand#execute()
	 * @throws PGenException if the element could not be found in the list
	 */
	@Override
	public void execute() throws PGenException {

	//	for ( Product currProd : list ) {
			
	//		for ( Layer currLayer : currProd.getLayers() ) {
			
	//			if ( currLayer.getDrawables().contains(element) ) {
		//			layer = currLayer;
		
				parent = (DECollection)element.getParent();
					if (((Line)element).isClosedLine()){
						
						deleteClosedPart();
						
					}
					else{
						
						deleteOpenPart();
						
					}
					return;
	//			}
	//		}
			
	//	}
		
	//	throw new PGenException("Could not find specified element in current product list");

	}

	/**
	 * adds the part to the drawable element back to the original layer
	 * @see gov.noaa.nws.ncep.ui.pgen.controls.PgenCommand#undo()
	 */
	@Override
	public void undo() throws PGenException {

		if ( !(element instanceof Jet) && ((Line)element).isClosedLine() ){
			undeleteClosedPart();
		}
		else {
			undeleteOpenPart();
		}
		
	}
	
	/**
	 * removes part from an open MultiPointElement
	 */
	private void deleteOpenPart(){
	
		List<Coordinate> points = element.getPoints();
		
		if ( lil.getStartIndex().compareTo( firstLoc ) == 0 &&
			 lil.getEndIndex().getSegmentIndex() == secondLoc.getSegmentIndex() ){
			
			/*
			 * Both points selected were endpoints, remove whole element
			 */
			removeAll = true;
			parent.removeElement( element );
			
		}
		else if ( lil.getStartIndex().compareTo( firstLoc ) == 0 ||
				 lil.getEndIndex().getSegmentIndex() == secondLoc.getSegmentIndex() ) {
			
			/*
			 * One point selected was an endpoint, remove part from one end
			 */
			removeOneEnd = true;
			element1 = (MultiPointElement)element.copy();
			ArrayList<Coordinate> newPts = new  ArrayList<Coordinate>();
			
			if ( lil.getStartIndex().compareTo( firstLoc ) == 0 ){
				newPts.add(secondPt);
				newPts.addAll (points.subList( secondLoc.getSegmentIndex()+1, points.size() ));
			}
			else if ( lil.getEndIndex().getSegmentIndex() == secondLoc.getSegmentIndex() ){
				newPts.addAll( points.subList( 0, firstLoc.getSegmentIndex()+1 ));
				newPts.add(firstPt);
			}
			
			((MultiPointElement)element1).setPoints(newPts);
			
			parent.addElement(element1);
			parent.removeElement(element);

		}
		else {
			
			//remove part in the middle of line
			removeMiddle = true;
			
			if ( element1 == null && element2 == null ) {  // make sure this part does not execute when Re-Do
				element1 = (MultiPointElement)element.copy();
				ArrayList<Coordinate> new1 = new ArrayList<Coordinate>( points.subList(0, firstLoc.getSegmentIndex()+1 ));
				new1.add( firstPt );
				((MultiPointElement)element1).setPoints( new1 );

				element2 = (MultiPointElement)element.copy();
				ArrayList<Coordinate> new2 = new ArrayList<Coordinate>();
				new2.add( secondPt );
				new2.addAll( points.subList( secondLoc.getSegmentIndex()+1, points.size()));
				((MultiPointElement)element2).setPoints( new2 );
			}
			
			if ( parent instanceof Jet ){
				Jet jet1 = new Jet();
				jet1.clear();
				jet1.addElement(element1);
				jet1.setPgenCategory(parent.getPgenCategory());
				jet1.setPgenType(parent.getPgenType());

				jet1.setSnapTool(((Jet)parent).getSnapTool());
				
				Jet jet2 = new Jet();
				jet2.clear();
				jet2.addElement(element2);
				jet2.setSnapTool(((Jet)parent).getSnapTool());
				jet2.setPgenCategory(parent.getPgenCategory());
				jet2.setPgenType(parent.getPgenType());
				
				jet1.getSnapTool().addBarbHashFromAnotherJet(jet1, (Jet)parent);
				jet2.getSnapTool().addBarbHashFromAnotherJet(jet2, (Jet)parent);
				
				jet1.getSnapTool().snapJet(jet1);
				jet2.getSnapTool().snapJet(jet2);

				element = parent;
				parent = (DECollection) parent.getParent();
				element1 = jet1;
				element2 = jet2;
			}
			
			parent.addElement( element1 );
			parent.addElement( element2 );
			parent.removeElement(element);
			
		}
	}
	
	/**
	 * removes part from a closed MultiPointElement
	 */	
	private void deleteClosedPart(){
		
		List<Coordinate> points = element.getPoints();
		int pointsBetween = secondLoc.getSegmentIndex() - firstLoc.getSegmentIndex();
		
		if ( pointsBetween >  points.size() - pointsBetween ){
			
			//if there are more points between pt1 and pt2, remove the other part.
			element1 = (MultiPointElement)element.copy();
			if ( element1 instanceof Gfa ) {
				((MultiPointElement)element1).setClosed( true );
			}
			else {
				((MultiPointElement)element1).setClosed( false);				
			}
			element1.getPoints().clear();
			element1.getPoints().add( firstPt );
			element1.getPoints().addAll(points.subList( firstLoc.getSegmentIndex()+1, secondLoc.getSegmentIndex()+1 ));
			element1.getPoints().add( secondPt );
			
			parent.addElement(element1);
			parent.removeElement(element);
			
		}
		else {
			
			element1 = (MultiPointElement)element.copy();
			if ( element1 instanceof Gfa ) {
				((MultiPointElement)element1).setClosed( true );
			}
			else {
				((MultiPointElement)element1).setClosed( false);				
			}
			element1.getPoints().clear();
			element1.getPoints().add( secondPt );
			element1.getPoints().addAll(points.subList( secondLoc.getSegmentIndex()+1, points.size() ));
			element1.getPoints().addAll(points.subList(0, firstLoc.getSegmentIndex()+1 ));
			element1.getPoints().add( firstPt );
			
			parent.addElement(element1);
			parent.removeElement(element);
			
		}
		
		if ( element1 instanceof Gfa ) {
			((Gfa)element1).setGfaVorText( Gfa.buildVorText( (Gfa)element1 ));
		}

		
	}
	
	/**
	 * un-deletes part from an open MultiPointElement
	 */	
	private void undeleteOpenPart(){
		
		if ( removeAll ){

			parent.addElement(element);
			removeAll = false;
			
		}
		else if ( removeOneEnd ) {
			
			parent.addElement(element);
			parent.removeElement(element1);
			removeOneEnd = false;
			
		}
		else if ( removeMiddle ){
			
			parent.removeElement(element1);
			parent.removeElement(element2);
			parent.addElement(element);
			
			if ( element instanceof Jet ){
				parent = (Jet) element;
				element = ((Jet)parent).getJetLine();
				element1 = ((Jet)element1).getJetLine();
				element2 = ((Jet)element2).getJetLine();
			}
			
			removeMiddle = false;
			
		}
	}
	
	/**
	 * un-deletes part from an open MultiPointElement
	 */	
	private void undeleteClosedPart(){
		
		parent.removeElement( element1);
		parent.addElement( element );
			
	}

}

