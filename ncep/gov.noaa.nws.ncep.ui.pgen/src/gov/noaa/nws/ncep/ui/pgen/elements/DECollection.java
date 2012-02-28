/*
 * gov.noaa.nws.ncep.ui.pgen.rsc.PgenSinglePointDrawingTool
 * 
 * 20 June 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.elements;

import gov.noaa.nws.ncep.ui.pgen.annotation.ElementOperations;
import gov.noaa.nws.ncep.ui.pgen.annotation.Operation;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

import org.geotools.referencing.GeodeticCalculator;
import org.geotools.referencing.datum.DefaultEllipsoid;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Implementation of collections of DrawableElement
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 06/09		#116			B. Yin   	Initial Creation.
 * 08/09		#135			B. Yin   	Added a parent field
 * 12/10		#366			B. Yin		Added getNearestDE  
 *
 * </pre>
 * 
 * @author	B. Yin
 */

@ElementOperations ( {Operation.COPY_MOVE, Operation.EXTRAPOLATE} )
public class DECollection extends AbstractDrawableComponent {
	
	protected String collectionName;
	protected List<AbstractDrawableComponent> compList;

	/**
	 * Public constructor
	 * @param name
	 */
	public DECollection( String name ){
		collectionName = name;
		compList = new ArrayList<AbstractDrawableComponent>();
	}
	
	public DECollection(){
		collectionName = "collection";
		compList = new ArrayList<AbstractDrawableComponent>();
	}
	/**
	 * Get the collection name
	 * @return
	 */
	public String getCollectionName(){
		return collectionName;
	}
	
	/**
	 * Create an iterator for DrawableElements in the collection.
	 * A collection may contain collections. This iterator will loop 
	 * over all DrawableElements regardless their depths. Collections 
	 * themselves are not included in the loop. 
	 */
	public Iterator<DrawableElement> createDEIterator(){
			return new DEIterator(compList.listIterator());
	}
	
	
	/**
	 * Get the iterator of the collection. This iterator will loop
	 * over only the direct children(DrawableElements and DECollections)
	 *  of the collection.  
	 * @return
	 */
	public Iterator<AbstractDrawableComponent> getComponentIterator(){
		return compList.iterator();
	}
	
	/**
	 * Get the list iterator(can be reset, sort of)
	 */
	public ListIterator<AbstractDrawableComponent> getComponentListIterator(){
		return compList.listIterator();
	}
	
	/**
	 * Add a component to the collection
	 * @param adc
	 */
	public void addElement(AbstractDrawableComponent adc){
		this.add(adc);
	}
	
	/**
	 * Add a component to the collection
	 * @param adc
	 */	
	public void add(AbstractDrawableComponent adc){
		adc.setParent(this);
		compList.add(adc);
	}
	
	public void add(int index, AbstractDrawableComponent adc){
		adc.setParent(this);
		compList.add(index, adc);
	}
	
	/**
	 * Add a list of components to the collection
	 * @param adc
	 */
	public void add(List<? extends AbstractDrawableComponent> adcList){
		for (AbstractDrawableComponent adc : adcList ){
			adc.setParent(this);
		}
		compList.addAll(adcList);
	}
	
	/**
	 * Remove a component from the collection
	 * @param adc
	 */
	public void remove(AbstractDrawableComponent adc){
		compList.remove(adc);
	}
	
	/**
	 * Remove a component from the collection
	 * @param adc
	 */	
	public void removeElement(AbstractDrawableComponent comp){
		this.remove(comp);
	}
	
	@Override
	/**
	 * Get ALL points of every DrawableElement in the collection
	 */
	public List<Coordinate> getPoints() {
		ArrayList<Coordinate> points = new ArrayList<Coordinate>();
		Iterator<DrawableElement> iterator = createDEIterator();
		while ( iterator.hasNext()){
			points.addAll(iterator.next().getPoints());
		}
		return points;
	}
	
	@Override
	/**
	 * Deep copy of the collection
	 */
	public DECollection copy(){
		DECollection dec = new DECollection(collectionName);
		Iterator<AbstractDrawableComponent> iterator = getComponentIterator();

		while ( iterator.hasNext()){
			dec.addElement(iterator.next().copy());
		}

		iterator = dec.getComponentIterator();
		while ( iterator.hasNext()){
			iterator.next().setParent(dec);
		}
		
		dec.setPgenCategory(this.getPgenCategory());
		dec.setPgenType(this.getPgenType());
		dec.setParent(this.getParent());
		return dec;
	}

	/**
	 * Set colors for all DrawableElements
	 */
	@Override
	public void setColors(Color[] colors) {
		Iterator<DrawableElement> iterator = createDEIterator();
		while ( iterator.hasNext()){
			iterator.next().setColors(colors);
		}

	}
	
	/**
	 * Check if the collection is empty
	 */
	@Override
	public boolean isEmpty() {
		return compList.isEmpty();
	}
	
	/**
	 * Removes all Elements from the collection
	 */
	public void clear() {
		compList.clear();
	}
	
	/**
	 * Deep search for the input component. Return the component's parent 
	 * if search succeeds. Otherwise return null.
	 * @param component
	 * @return
	 */
	public DECollection search( AbstractDrawableComponent component){
		if ( compList.contains(component)) {
			return this;
		}
		else{
			for (AbstractDrawableComponent adc:compList){
				if ( adc instanceof DECollection ){
					DECollection dec = ((DECollection)adc).search(component);
					if (dec != null){
						return dec;
					}
				}
			}
		}
		
		return null;
	}
	
	/**
	 * Replace the old component with the new one from the compList 
	 * @param oldEl
	 * @param newEl
	 * @return
	 */
	private boolean replaceComponent( AbstractDrawableComponent oldEl, AbstractDrawableComponent newEl){
		if ( compList.contains(oldEl)) {
			int idx = compList.indexOf(oldEl);
			compList.set(idx, newEl);
			newEl.setParent(this);
			return true;
		}
		else {
			return false;
		}
	}
	
	/**
	 * Deep search and replace the old component with the new one.
	 * @param oldEl
	 * @param newEl
	 * @return
	 */
	public boolean replace( AbstractDrawableComponent oldEl, AbstractDrawableComponent newEl){
		DECollection dec = search(oldEl);
		if ( dec != null ){
			return dec.replaceComponent(oldEl, newEl);
		}
		else{
			return false;
		}
	}
	
	/**
	 * Return the primary DrawableElement
	 */
	public DrawableElement getPrimaryDE(){
		AbstractDrawableComponent el = compList.get(0);
		if ( el instanceof DrawableElement ){
			return (DrawableElement)el; 
		}
		else {
			return el.getPrimaryDE();
		}
	}
	/**
	 * Finds the nearest component(DE/DECollection) in the collection to the input point.
	 * @param point
	 * @return	the nearest component
	 */	
	public AbstractDrawableComponent getNearestComponent( Coordinate point ){
		
		AbstractDrawableComponent nearestComponent = null;
		double	minDistance = -1;

       	GeodeticCalculator gc = new GeodeticCalculator(DefaultEllipsoid.WGS84);
       	
		gc.setStartingGeographicPoint(point.x, point.y);
		
		Iterator<AbstractDrawableComponent> iterator = getComponentIterator();

		while ( iterator.hasNext()){
			AbstractDrawableComponent comp = iterator.next();

			for ( Coordinate pts : comp.getPoints() ){

				gc.setDestinationGeographicPoint(pts.x, pts.y);
				double dist = gc.getOrthodromicDistance();

				if ( minDistance <  0 || dist < minDistance ) {

					minDistance = dist;
					nearestComponent = comp; 

				}
			}					

		}
		
		return nearestComponent;
		
	}
	
	/**
	 * Finds the nearest DE in the collection to the input point.
	 * @param point
	 * @return	the nearest component
	 */	
	public AbstractDrawableComponent getNearestDE( Coordinate point ){
		
		DrawableElement nearestDE = null;
		double	minDistance = -1;

       	GeodeticCalculator gc = new GeodeticCalculator(DefaultEllipsoid.WGS84);
       	
		gc.setStartingGeographicPoint(point.x, point.y);
		
		Iterator<DrawableElement> iterator = createDEIterator();

		while ( iterator.hasNext()){
			DrawableElement de = iterator.next();

			for ( Coordinate pts : de.getPoints() ){

				gc.setDestinationGeographicPoint(pts.x, pts.y);
				double dist = gc.getOrthodromicDistance();

				if ( minDistance <  0 || dist < minDistance ) {

					minDistance = dist;
					nearestDE = de; 

				}
			}					

		}
		
		return nearestDE;
		
	}
	/**
	 * return collection name
	 */
	public String getName(){
		return getCollectionName();
	}
	
	/**
	 * return size of the component list
	 * @return
	 */
	public int size(){
		return compList.size();
	}
	
	/**
	 * Return true if the DEColloection contains the input component.
	 * The input component can be a DrawableElement or a DECollection
	 * @param adc
	 * @return
	 */
	public boolean contains( AbstractDrawableComponent adc ){
		
		if ( this.compList.contains(adc) ) return true;
		else {
			Iterator<AbstractDrawableComponent> it = getComponentIterator();
			while (it.hasNext() ){
				AbstractDrawableComponent item = it.next();
				if ( item instanceof DECollection ){
					if ( ((DECollection)item).contains(adc) ) return true;
				}
			}
		}
		
		return false;
	}

	public void setCollectionName(String collectionName) {
		this.collectionName = collectionName;
	}
}
