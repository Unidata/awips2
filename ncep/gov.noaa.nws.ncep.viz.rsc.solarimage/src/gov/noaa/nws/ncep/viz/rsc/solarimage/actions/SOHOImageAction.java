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
public class SOHOImageAction extends AbstractHandler {

    public static final String WAVELENGTH = "wavelength";

    public static final String INSTRUMENT = "instrument";

    public static final String DETECTOR = "detector";

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
        if (instrument == null) {
            return null;
        }

        Map<String, String> vars = new HashMap<String, String>();
        File bundle = null;

        if (instrument.equals("EIT")) {
            bundle = NcPathManager.getInstance().getStaticFile(
                    "ncep/Bundles/SOHOEitImage.xml");
            String wavelength = arg0.getParameter(WAVELENGTH);

            vars.put(WAVELENGTH, wavelength);
            vars.put(INSTRUMENT, instrument);
        } else { //if (instrument.equals("LASCO")) {
            bundle = NcPathManager.getInstance().getStaticFile(
                    "ncep/Bundles/SOHOLascoImage.xml");
            
            vars.put(INSTRUMENT, instrument);
            
        } 


        try {
            // LoadSerializedXml.loadTo(bundle, vars);
            SolarImageBundleLoader.loadTo(bundle, vars);
        } catch (VizException e) {
            e.printStackTrace();
            // statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
            // e);
        }

        return null;
    }
}