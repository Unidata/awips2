package gov.noaa.nws.ncep.viz.ui.display;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.ui.AbstractSourceProvider;
import org.eclipse.ui.ISources;

// 
// 12/12/2012     #630     ghull     moved from zoom/tools since now set from NcMapEditor refreshPanes()
//
//
public class ZoomStateSourceProvider extends AbstractSourceProvider {
    public final static String ZOOM_STATE = "gov.noaa.nws.ncep.viz.tools.zoomState";

    private final static String SUSPEND_ZOOM = "suspended";

    private final static String ZOOM_NOT_SUSPENDED = "notSuspended";

    private boolean isZoomSuspended = false;

    public ZoomStateSourceProvider() {
    }

    @Override
    public void dispose() {
    }

    @Override
    public Map<String, String> getCurrentState() {
        Map<String, String> currentStateMap = new HashMap<String, String>(1);
        String currentState = isZoomSuspended ? SUSPEND_ZOOM
                : ZOOM_NOT_SUSPENDED;
        currentStateMap.put(ZOOM_STATE, currentState);
        return currentStateMap;
    }

    @Override
    public String[] getProvidedSourceNames() {
        return new String[] { ZOOM_STATE };
    }

    public void setZoomSuspended(boolean _isZoomSuspended) {
        if (this.isZoomSuspended == _isZoomSuspended)
            return; // no change
        this.isZoomSuspended = _isZoomSuspended;
        String currentState = this.isZoomSuspended ? SUSPEND_ZOOM
                : ZOOM_NOT_SUSPENDED;
        fireSourceChanged(ISources.WORKBENCH, ZOOM_STATE, currentState);
    }
}
