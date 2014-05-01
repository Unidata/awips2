/**
 *
 * SgwhvDescriptorDelegate
 *
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * -------		-------		--------	-----------
 * 08/23/11					Chin J Chen	Initial coding from BufrSgwhvDescriptorDelegate
 * </pre>
 * 
 * @author Chin J. Chen 
 * @version 1.0
 */
package gov.noaa.nws.ncep.edex.plugin.sgwhv.util;

import com.raytheon.uf.edex.decodertools.bufr.descriptors.DescriptorFactory;
import com.raytheon.uf.edex.decodertools.bufr.descriptors.IDescriptorFactoryDelegate;
import com.raytheon.uf.edex.decodertools.bufr.descriptors.IDescriptorFactorySelector;

public class SgwhvDescriptorDelegate implements IDescriptorFactoryDelegate {

    private DescriptorFactory factory = new DescriptorFactory();   
    IDescriptorFactorySelector factorySelector = null;
    
    public SgwhvDescriptorDelegate(IDescriptorFactorySelector selector) {
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
