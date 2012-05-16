package gov.noaa.nws.ncep.ui.pgen;

import org.osgi.framework.ServiceReference;

import gov.noaa.nws.ncep.common.staticdata.IStaticDataProvider;
import gov.noaa.nws.ncep.staticdataprovider.StaticDataProvider;

public class PgenStaticDataProvider {

	static private IStaticDataProvider provider;

	static public IStaticDataProvider getProvider(){
		if ( provider == null ){

			ServiceReference ref = Activator.getDefault().getBundle().getBundleContext().getServiceReference(
					IStaticDataProvider.class.getName());
            if (ref == null) {
                // cause the classloader to load StaticDataProvided and thus to
                // call the Activator for gov.noaa.nws.ncep.staticdataprovider.
                StaticDataProvider.getInstance();
                ref = Activator.getDefault().getBundle().getBundleContext().getServiceReference(IStaticDataProvider.class.getName());
            }
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
