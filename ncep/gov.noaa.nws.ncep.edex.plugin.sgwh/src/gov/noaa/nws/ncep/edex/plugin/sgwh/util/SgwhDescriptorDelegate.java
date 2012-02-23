/**
*
* SgwhDescriptorDelegate
*
* <pre>
* 
* SOFTWARE HISTORY
* 
* Date         Ticket#     Engineer    Description
* -------		-------		--------	-----------
* 08/18/2011               Chin Chen	Initial coding from BufrSgwhDescriptorDelegate
* </pre>
* 
* @author Chin J. Chen
* @version 1.0
*/
package gov.noaa.nws.ncep.edex.plugin.sgwh.util;

import com.raytheon.uf.edex.decodertools.bufr.descriptors.DescriptorFactory;
import com.raytheon.uf.edex.decodertools.bufr.descriptors.IDescriptorFactoryDelegate;
import com.raytheon.uf.edex.decodertools.bufr.descriptors.IDescriptorFactorySelector;

public class SgwhDescriptorDelegate implements IDescriptorFactoryDelegate {

   private DescriptorFactory factory = new DescriptorFactory();   
   IDescriptorFactorySelector factorySelector = null;
   
   public SgwhDescriptorDelegate(IDescriptorFactorySelector selector) {
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
