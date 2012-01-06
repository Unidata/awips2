/*
 * DeleteAllCommand
 * 
 * Date created: 14 APRIL 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.controls;

import java.util.ArrayList;
import java.util.List;

import gov.noaa.nws.ncep.ui.pgen.PGenException;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;

/**
 * This class contains the implementation needed to remove all products, layers and elements
 * from a Product list.  The elements can be re-added for an undo feature.
 * @author sgilbert
 *
 */
public class DeleteAllCommand extends PgenCommand {

	/*
	 * The Product list.
	 */
	private List<Product> list;
	
	/*
	 * saved version of the product list
	 */
	private ArrayList<Product> saveList;
	
	/**
	 * Constructor used to specify product list to empty
	 * @param list
	 */
	public DeleteAllCommand(List<Product> list) {
		this.list = list;
	}

	/**
	 * Saves the product list, and then empties it.
	 * @see gov.noaa.nws.ncep.ui.pgen.controls.PgenCommand#execute()
	 */
	@Override
	public void execute() throws PGenException {

		saveList = new ArrayList<Product>(list);
		list.clear();

	}

	/**
	 * Re-adds the saved product list to the original list
	 * @see gov.noaa.nws.ncep.ui.pgen.controls.PgenCommand#undo()
	 */
	@Override
	public void undo() throws PGenException {

		list.addAll(saveList);
	}

}
