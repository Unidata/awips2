/**
 *
 * BufrSshaDescriptorDelegate
 *
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * -------		-------		--------	-----------
 * 01/26/11		209			F. J. Yen	Initial coding for R1G1-6
 * </pre>
 * 
 * @author F. J. Yen
 * @version 1.0
 */
package gov.noaa.nws.ncep.edex.plugin.bufrssha.util;

import java.util.HashMap;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.edex.decodertools.bufr.descriptors.DescriptorFactory;
import com.raytheon.uf.edex.decodertools.bufr.descriptors.IDescriptorFactoryDelegate;
import com.raytheon.uf.edex.decodertools.bufr.descriptors.IDescriptorFactorySelector;

public class BufrSshaDescriptorDelegate implements IDescriptorFactoryDelegate {

	private static Log logger = LogFactory.getLog(BufrSshaDescriptorDelegate.class);
    IDescriptorFactorySelector factorySelector = null;
    
    private static Map<String, DescriptorFactory> factories;
    static {
        factories = new HashMap<String, DescriptorFactory>();
        factories.put("SSHA", createFactory("SSHABufrTable_B", "SSHABufrTable_D"));
    }
    
    public BufrSshaDescriptorDelegate(IDescriptorFactorySelector selector) {
    	factorySelector = selector;
    }
    
    @Override
    public DescriptorFactory getInstance() {  	
    	DescriptorFactory factory = null; // defaultFactory;
    	
        String selector = null;
        if (factorySelector != null) {
            selector = factorySelector.getSelector();
        }
        logger.debug ("Getting DescriptorFactory for type " + selector);

        factory = factories.get(selector);
        if (factory == null) {
            factory = new DescriptorFactory();
        }
        return factory;
    }

    @Override
    public void setDescriptorFactorySelector(IDescriptorFactorySelector selector) {
        factorySelector = selector;
    }

    /**
     * 
     * @param resourceClassReference
     *            Class reference table resources will be loaded from.
     * @param tableB
     *            Name of BUFR table B.
     * @param tableD
     *            Name of BUFR table D.
     * @return
     */
    private static final DescriptorFactory createFactory(String tableB,
            String tableD) {
        DescriptorFactory factory = null;
        logger.debug ("Creating table " + tableB + " " + tableD);
        factory = new DescriptorFactory(BufrSshaDescriptorDelegate.class, tableB,
                tableD);
        logger.debug ("Table created = [" + (factory != null) + "]");
        return factory;
    }
    

}
