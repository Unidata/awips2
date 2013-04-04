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
public class SXIImageAction extends AbstractHandler {

    public static final String WAVELENGTH = "wavelength";

    public static final String INTTIME = "inttime";

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
        String inttime = arg0.getParameter(INTTIME);
        if (wavelength == null || inttime == null) {
            return null;
        }

        File bundle = NcPathManager.getInstance().getStaticFile(
                "ncep/Bundles/GoesSXIImage.xml");
        System.out.println("GOTIT!!!!!!!!!!!!!!  " + bundle.getAbsolutePath()
                + " for " + wavelength);

        Map<String, String> vars = new HashMap<String, String>();

        if (wavelength.contains("|")) {
            bundle = NcPathManager.getInstance().getStaticFile(
                    "ncep/Bundles/GoesSXIImageMultipane.xml");

            int numPanels = 4;
            String[] wavelens = wavelength.split("\\|", numPanels);
            String[] times = inttime.split("\\|", numPanels);
            for (int n = 0; n < numPanels; n++) {
                vars.put(WAVELENGTH + String.valueOf(n), wavelens[n]);
                vars.put(INTTIME + String.valueOf(n), times[n]);
                System.out.println(String.valueOf(n) + " : " + wavelens[n]
                        + " : " + times[n]);
            }
        } else {
            vars.put(WAVELENGTH, wavelength);
            vars.put(INTTIME, inttime);
        }

        // vars.put("tabTitle", "GOES-15/SXI");
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