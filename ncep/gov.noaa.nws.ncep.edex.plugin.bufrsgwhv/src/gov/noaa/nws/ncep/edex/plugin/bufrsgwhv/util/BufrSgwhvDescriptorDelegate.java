/**
 *
 * BufrSgwhvDescriptorDelegate
 *
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * -------		-------		--------	-----------
 * 11/12/10		208			F. J. Yen	Initial coding from Tamdar (R1G1-4)
 * </pre>
 * 
 * @author F. J. Yen
 * @version 1.0
 */
package gov.noaa.nws.ncep.edex.plugin.bufrsgwhv.util;

import com.raytheon.uf.edex.decodertools.bufr.descriptors.DescriptorFactory;
import com.raytheon.uf.edex.decodertools.bufr.descriptors.IDescriptorFactoryDelegate;
import com.raytheon.uf.edex.decodertools.bufr.descriptors.IDescriptorFactorySelector;

public class BufrSgwhvDescriptorDelegate implements IDescriptorFactoryDelegate {

    private DescriptorFactory factory = new DescriptorFactory();   
    IDescriptorFactorySelector factorySelector = null;
    
    public BufrSgwhvDescriptorDelegate(IDescriptorFactorySelector selector) {
        factorySelector = selector;
    }
    
    @Override
    public DescriptorFactory getInstance() {
        return factory;
    }

    @Override
    public void setDescriptorFactorySelector(IDescriptorFactorySelector selector) {
        factorySelector = selector;
    }

}
