/*
 * DeleteSelectedElementsCommand
 * 
 * Date created: 22 May 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.controls;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import gov.noaa.nws.ncep.ui.pgen.PGenException;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DECollection;
import gov.noaa.nws.ncep.ui.pgen.elements.Layer;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;

/**
 * Implements a PgenCommand to delete selected elements.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 05/09			79		B. Yin   	Initial Creation.
 * 06/09			116		B. Yin		Changed DrawableElement to 
 * 										AbstractDrawableComponent
 *
 * </pre>
 * 
 * @author	B. Yin
 */

public class DeleteSelectedElementsCommand extends PgenCommand {

	/*
	 * The Product list.
	 */
	private List<Product> prodList;
	
	/*
	 * Elements selected
	 */
	private List<AbstractDrawableComponent> elSelected;
	
	/*
	 * Saved version of the elements selected
	 */
	private List<AbstractDrawableComponent> saveEl;
	
	/*
	 * Element-layer map for selected elements
	 */
	private HashMap<AbstractDrawableComponent, DECollection> elMap;

	/**
	 * Constructor
	 * @param list
	 * @param elements
	 */
	public DeleteSelectedElementsCommand(List<Product> list, 
			List<AbstractDrawableComponent>elements) {
		
		this.prodList = list;
		elSelected = elements;
		saveEl = new ArrayList<AbstractDrawableComponent>(elements);
		elMap = new HashMap<AbstractDrawableComponent, DECollection>();

	}

	/**
	 * Remove selected elements and save the layer info for each removed el to a map. 
	 * @see gov.noaa.nws.ncep.ui.pgen.controls.PgenCommand#execute()
	 */
	@Override
	public void execute() throws PGenException {
		
		for ( AbstractDrawableComponent comp : saveEl ){
			
			for ( Product prod : prodList ) {

				for ( Layer layer : prod.getLayers() ) {

					DECollection dec = layer.search(comp);
					if ( dec != null ){
						
						dec.removeElement(comp);
						elMap.put(comp,dec);
					}
				}
			}
		}

		elSelected.clear();
	}

	/**
	 * Add back all removed elements into the layers they belong to. 
	 * @see gov.noaa.nws.ncep.ui.pgen.controls.PgenCommand#undo()
	 */
	@Override
	public void undo() throws PGenException {
		
		for ( AbstractDrawableComponent comp : saveEl ){
			
			DECollection dec = elMap.get(comp);
			
			if (dec != null ){
				dec.addElement(comp);
			}
			else {
				throw new PGenException("Coulnd't find the collection when restoring objects!");
			}
			
		}
		
	}

}
