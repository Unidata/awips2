package gov.noaa.nws.ncep.ui.pgen.productManage;

import gov.noaa.nws.ncep.ui.pgen.rsc.PgenResource;

public class ProductDialogStarter implements Runnable {

	PgenResource pgen = null;
	
	/**
	 * @param pgen
	 */
	public ProductDialogStarter(PgenResource pgen) {
		this.pgen = pgen;
	}

	@Override
	public void run() {
		pgen.startProductManage();
	}

}
