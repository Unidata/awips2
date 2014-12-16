/**
 * This code has unlimited rights, and is provided "as is" by the National Centers 
 * for Environmental Prediction, without warranty of any kind, either expressed or implied, 
 * including but not limited to the implied warranties of merchantability and/or fitness 
 * for a particular purpose.
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 **/
package gov.noaa.nws.ncep.viz.rtkp;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.xy.graph.IGraph;
import com.raytheon.uf.viz.xy.timeseries.display.TimeSeriesDescriptor;

/**
 * RTKP Time Series descriptor, needed so loading bundles know what editor to
 * load with this descriptor.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 5, 2014   1122       sgurung     Initial creation
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
public class GeoMagRTKpDescriptor extends TimeSeriesDescriptor {

    public GeoMagRTKpDescriptor() {
        super();
    }

    public GeoMagRTKpDescriptor(PixelExtent pixelExtent) {
        super(pixelExtent);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.xy.graph.XyGraphDescriptor#constructGraph()
     */
    @Override
    public IGraph constructGraph() {
        return new GeoMagRTKpGraph(this);
    }

}
