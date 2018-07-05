package gov.noaa.gsd.viz.ensemble.display.control.contour;

import com.raytheon.uf.viz.core.rsc.capabilities.AbstractCapability;

/**
 * 
 * The capability to pop up the "Contour Control" dialog on the legend by right
 * clicking menu of a grid product or a ensemble product.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 14, 2017  19598   polster     Initial creation
 * Mar 14, 2017  19598   jing        removed stuff no longer to be used
 * Jun 27, 2017  19325   jing        Upgrade to 17.3.1
 *
 * </pre>
 *
 * @author poster
 */

public class ContourControlCapability extends AbstractCapability {

    public ContourControlCapability() {

    }

    @Override
    public AbstractCapability clone() {
        return new ContourControlCapability();
    }
}
