/*
 * AddElementCommand
 * 
 * Date created: 14 APRIL 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.controls;

import java.util.List;

import gov.noaa.nws.ncep.ui.pgen.PGenException;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.Layer;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;

/**
 * This class contains the implementation needed to add a new Drawable Element to an
 * existing PGEN Resource.
 * @author sgilbert
 *
 */
public class AddElementCommand extends PgenCommand {

	/*
	 *  Product list associated with a PGEN Resource
	 */
	private List<Product> list;
	
	/*
	 * A specific Product in the list
	 */
	private Product product;
	
	/*
	 * A specific Layer in the Product
	 */
	private Layer layer;
	
	/*
	 * The new Drawable element to add  
	 */
	private AbstractDrawableComponent element;
	
	/*
	 * indicates whether element was added to new layer or an existing one.
	 */
	private boolean layerExisted;
	
	/*
	 * indicates whether element was added to a new product or existing one.
	 */
	private boolean productExisted;
	
	/**
	 * Constructor specifying the list, product and layer to which the element should
	 * be added.
	 * @param list add element to this product list
	 * @param product add element to this product
	 * @param layer add element to this layer
	 * @param element drawable element to add.
	 */
	public AddElementCommand(List<Product> list, Product product, Layer layer,
			AbstractDrawableComponent element) {
		this.list = list;
		this.product = product;
		this.layer = layer;
		this.element = element;
	}

	/**
	 * Adds an element to the list, product and layer specified in constructor.
	 * Keeps track if layer and product is new.	 *
	 * @see gov.noaa.nws.ncep.ui.pgen.controls.PgenCommand#execute()
	 * @throws PGenException if list, product, layer or element is null
	 */
	@Override
	public void execute() throws PGenException {
		
		/*
		 * check if any items passed to constructor were null
		 */
		if ( list == null ) throw new PGenException("AddElementCommand must be given a non null Product List");
		if ( product == null ) throw new PGenException("AddElementCommand must be given a non null Product");
		if ( layer == null ) throw new PGenException("AddElementCommand must be given a non null Layer");
		if ( element == null || element.isEmpty() ) throw new PGenException("AddElementCommand must be given a non null Element");
		
		/*
		 * Add product to list, if product is new.
		 */
		if ( list.contains(product) ) {
			productExisted = true;
		}
		else {
			productExisted = false;
			list.add(product);
		}
		
		/*
		 * Add layer to product, if layer is new.
		 */
		if ( product.contains(layer) ) {
			layerExisted = true;
		}
		else {
			layerExisted = false;
			product.addLayer(layer);
		}

		/*
		 * add element
		 */
		layer.addElement(element);

	}

	/**
	 * Removes the element from the list, product, and layer specified in the 
	 * constructor.  If product and layer were new, they are also removed.
	 * @see gov.noaa.nws.ncep.ui.pgen.controls.PgenCommand#undo()
	 */
	@Override
	public void undo() throws PGenException {

		/*
		 * remove element from layer
		 */
		layer.removeElement(element); 
		
		/*
		 * remove layer, if it was new
		 */
		if ( layer.isEmpty() && !layerExisted ) product.removeLayer(layer);
		
		/*
		 * removep product, if it was new
		 */
		if ( product.isEmpty() && !productExisted ) list.remove(product);
	}

}
