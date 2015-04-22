package gov.noaa.nws.ncep.ui.pgen;

import org.osgi.framework.ServiceReference;

import gov.noaa.nws.ncep.common.staticdata.IStaticDataProvider;

public class PgenStaticDataProvider {

	static private IStaticDataProvider provider;

	static public IStaticDataProvider getProvider(){
		if ( provider == null ){

			ServiceReference ref = Activator.getDefault().getBundle().getBundleContext().getServiceReference(
					IStaticDataProvider.class.getName());
			IStaticDataProvider isdp = null;
			if(ref != null
					&& (isdp = (IStaticDataProvider)Activator.getDefault().getBundle().getBundleContext().getService(ref) )!= null ) {
				provider = isdp;
			}
			else throw new PGenRuntimeException("PGEN static data provider: NULL!");
		}

		return provider;
	}

}
