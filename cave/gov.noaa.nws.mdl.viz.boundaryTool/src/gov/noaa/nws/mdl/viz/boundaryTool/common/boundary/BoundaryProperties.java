package gov.noaa.nws.mdl.viz.boundaryTool.common.boundary;

import com.raytheon.uf.viz.core.drawables.PaintProperties;

/**
 * 
 * @author Mamoudou Ba
 * @version 1.0
 * 
 *          Entirely reused of A2 "StormTrackProperties" Class by renaming it
 *          and its methods
 */

public class BoundaryProperties extends PaintProperties {

    private BoundaryState state;

    public BoundaryProperties(PaintProperties props, BoundaryState state) {
        super(props);
        this.state = state;
        state.distanceThreshold = (props.getView().getExtent().getWidth() / props
                .getCanvasBounds().width) * 10;
    }

    public BoundaryState getState() {
        return state;
    }

    public void setState(BoundaryState state) {
        this.state = state;
    }

}
