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
public class STEREOImageAction extends AbstractHandler {

    public static final String WAVELENGTH = "wavelength";

    public static final String INSTRUMENT = "instrument";

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

        String wavelength = arg0.getParameter(WAVELENGTH);
        String instrument = arg0.getParameter(INSTRUMENT);
//        String detector = arg0.getParameter(DETECTOR);
//        if (detector == null || instrument == null) {
//            return null;
//        }
        if (wavelength == null)
            wavelength = "0";

        File bundle = NcPathManager.getInstance().getStaticFile(
                "ncep/Bundles/STEREOImage.xml");
        System.out.println("GOTIT!!!!!!!!!!!!!!  " + bundle.getAbsolutePath()
                + " for " + wavelength);

        Map<String, String> vars = new HashMap<String, String>();
        vars.put(WAVELENGTH, wavelength);
        vars.put(INSTRUMENT, instrument);
        //vars.put(DETECTOR, detector);

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