package gov.noaa.nws.ncep.viz.ui.display;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.viz.ui.input.InputAdapter;
import com.raytheon.viz.ui.input.preferences.MousePreferenceManager;

/**
 * Abstract legend input handler for NC. Has a mouse preference manager, and a
 * NCLegendResource. Extends InputAdapter.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/03/2012              S. Gurung     Initial creation
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */

public class AbstractNCLegendInputHandler extends InputAdapter {

    protected static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractNCLegendInputHandler.class);

    protected MousePreferenceManager prefManager = MousePreferenceManager
            .getInstance();

    protected NCLegendResource resource;

    protected AbstractNCLegendInputHandler(NCLegendResource resource) {
        this.resource = resource;
    }

}
