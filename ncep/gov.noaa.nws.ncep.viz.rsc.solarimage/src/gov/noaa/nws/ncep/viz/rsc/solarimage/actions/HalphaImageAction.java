package gov.noaa.nws.ncep.viz.rsc.solarimage.actions;

import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.rsc.solarimage.SolarImageBundleLoader;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import com.raytheon.uf.viz.core.exception.VizException;

/**
 * 
 * @version 1.0
 */
public class HalphaImageAction extends AbstractHandler {

    public static final String INSTRUMENT = "instrument";
    
    public static final String SITE = "site";

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(final ExecutionEvent arg0) throws ExecutionException {

        if (arg0.getCommand() == null) {
            return null;
        }

        String instrument = arg0.getParameter(INSTRUMENT);
        String site = arg0.getParameter(SITE);
        if (instrument == null || site == null) {
            return null;
        }

        File bundle = NcPathManager.getInstance().getStaticFile(
                "ncep/Bundles/HalphaImage.xml");

        Map<String, String> vars = new HashMap<String, String>();
        vars.put(INSTRUMENT, instrument);
        vars.put(SITE, site);

        try {
            SolarImageBundleLoader.loadTo(bundle, vars);
        } catch (VizException e) {
            e.printStackTrace();
            // statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
            // e);
        }
        
        return null;
    }
}