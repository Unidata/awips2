package gov.noaa.nws.ncep.viz.timeseries.actions;

import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.timeseries.TimeSeriesBundleLoader;

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
public class TimeSeriesAction extends AbstractHandler {

    public static final String INSTRUMENT = "instrument";

    public static final String STATION = "stationCode";

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(final ExecutionEvent arg0) throws ExecutionException {

        // String bundleName;

        // if (arg0.getCommand() == null) {
        // return null;
        // }
        //
        // String instrument = arg0.getParameter(INSTRUMENT);
        // if (instrument == null) {
        // return null;
        // }

        File bundle = NcPathManager.getInstance().getStaticFile(
                "ncep/Bundles/TimeSeriesExample3.xml");

        Map<String, String> vars = new HashMap<String, String>();
        // vars.put(STATION, "OTT");
        vars.put(STATION, "BOU");

        try {
            // vars = null;
            TimeSeriesBundleLoader.loadTo(bundle, vars);
        } catch (VizException e) {
            e.printStackTrace();
            // statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
            // e);
        }

        // final NCMapEditor editor = NmapUiUtils.getActiveNatlCntrsEditor();
        // if( editor == null )
        // return null;
        //
        // Job j = new Job("Loading Map Overlays...") {
        // @SuppressWarnings("unchecked")
        // @Override
        // protected IStatus run(IProgressMonitor monitor) {
        // long t0 = System.currentTimeMillis();
        // String overlayName = arg0.getParameter("overlayName");
        //
        // if (overlayName == null) {
        // return new Status(IStatus.ERROR, UiPlugin.PLUGIN_ID,
        // "bundleName was null");
        // }
        //
        // try {
        // IDisplayPane displayPane = editor.getActiveDisplayPane();
        // IDescriptor existingMD = displayPane.getDescriptor();
        //
        // // get the name of the default attr set and create the overlay
        // resource
        // //String qualRscName = NmapCommon.OverlaysRscDir + bundleName;
        // ResourceName fullRscName = new ResourceName(
        // ResourceName.OverlayRscCategory, overlayName, null );
        //
        // ResourceSelection rbt = ResourceFactory.createResource( fullRscName
        // );
        // ResourcePair rscPair = rbt.getResourcePair();
        // ResourceProperties props = rscPair.getProperties();
        // AbstractVizResource ovrlyRsc = rscPair.getResource();
        //
        // IDisplayPane[] seldPanes = ((NCMapEditor)editor).getSelectedPanes();
        //
        // if( seldPanes.length == 0 ) {
        // System.out.println("There are no Selected Panes to load to?");
        // }
        //
        // // this assumes a map bundle has only a single display
        // for (IDisplayPane pane : seldPanes ) {
        // existingMD = pane.getRenderableDisplay().getDescriptor();
        //
        // if(ovrlyRsc == null) {
        // AbstractResourceData resourceData = rscPair.getResourceData();
        // if(resourceData != null)
        // ovrlyRsc = resourceData.construct(
        // rscPair.getLoadProperties(), existingMD);
        // }
        // ResourceList resourceList = existingMD.getResourceList();
        // resourceList.add(ovrlyRsc, props);
        // ovrlyRsc.setDescriptor(existingMD);
        // ovrlyRsc.init( pane.getTarget() );
        // }
        //
        // editor.refresh();
        // } catch (VizException e) {
        // return new Status(IStatus.ERROR, UiPlugin.PLUGIN_ID,
        // "Error loading bundle", e);
        // }
        // long t2 = System.currentTimeMillis();
        // System.out.println("Total bundle retrieval: " + (t2 - t0));
        // return Status.OK_STATUS;
        // }
        // };
        //
        // j.schedule();

        return null;
    }
}