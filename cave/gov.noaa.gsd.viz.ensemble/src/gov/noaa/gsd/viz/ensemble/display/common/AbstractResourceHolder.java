package gov.noaa.gsd.viz.ensemble.display.common;

import gov.noaa.gsd.viz.ensemble.display.rsc.GeneratedEnsembleGridResource;
import gov.noaa.gsd.viz.ensemble.display.rsc.histogram.HistogramResource;
import gov.noaa.gsd.viz.ensemble.display.rsc.timeseries.GeneratedTimeSeriesResource;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.xy.timeseries.rsc.TimeSeriesResource;
import com.raytheon.viz.grid.rsc.GridNameGenerator;

/**
 * This class represents the abstract container which encapulates an
 * AbstractVizResource (that happens to be associated with the Ensemble Tool).
 * It provides a poor-man's approach at defining how to parse and extract common
 * resource metadata, an interface that doesn't already exist in the viz
 * resource class. Similar approaches have been attempted in other resource data
 * classes (e.g. getMetaDataMap) but were not necessarily reusable. This class
 * exists to attempt to fill an unusual void in the hierarchy of an
 * AbstractVizResource as there is no convenient way to extract commonly known
 * meteorological metadata information from the resource (needs verification).
 * 
 * This abstact class forces derived classes to define the equals() and hashCode
 * methods, among other notable getters.
 * 
 * In order to create a class of this type call the factory method (see
 * createResourceHolder).
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 17, 2014    5056     polster     Initial creation
 * Jan 17, 2016    13211    polster     Renamed class from GenericResourceHolder
 *                                      to AbstractResourceHolder
 * 
 * </pre>
 * 
 * @author polster
 * @author jing
 * @version 1.0
 */

public abstract class AbstractResourceHolder extends
        AbstractLegendComponentsProvider {

    protected AbstractVizResource<?, ?> rsc;

    protected boolean isSelected = false; // either selected or unselected

    protected boolean isGenerated = false; // either generated or a basic
                                           // (normally loaded) resource

    public static AbstractResourceHolder createResourceHolder(
            AbstractVizResource<?, ?> rsc, boolean isSelected) {

        AbstractResourceHolder genericRsc = null;

        if (GeneratedEnsembleGridResource.class
                .isAssignableFrom(rsc.getClass())) {
            genericRsc = new GeneratedGridResourceHolder(rsc, isSelected);
        } else if (GeneratedTimeSeriesResource.class.isAssignableFrom(rsc
                .getClass())) {
            genericRsc = new GeneratedTimeSeriesResourceHolder(rsc, isSelected);
        } else if ((rsc instanceof AbstractVizResource<?, ?>)
                && (rsc instanceof GridNameGenerator.IGridNameResource)) {
            genericRsc = new GridResourceHolder(rsc, isSelected);
        } else if (TimeSeriesResource.class.isAssignableFrom(rsc.getClass())) {
            genericRsc = new TimeSeriesResourceHolder(rsc, isSelected);
        } else if (HistogramResource.class.isAssignableFrom(rsc.getClass())) {
            genericRsc = new HistogramGridResourceHolder(rsc, isSelected);
        } else if (rsc instanceof AbstractVizResource<?, ?>) {
            genericRsc = new GenericResourceHolder(rsc, isSelected);
        }
        return genericRsc;
    }

    @Override
    public boolean equals(Object o) {
        boolean equals = false;
        if (o instanceof AbstractResourceHolder) {
            AbstractResourceHolder gr = (AbstractResourceHolder) o;
            equals = this.getSpecificName().equals(gr.getSpecificName());
        } else {
            equals = false;
        }
        return equals;
    }

    public int hashCode() {
        return getSpecificName().hashCode();
    }

    protected AbstractResourceHolder() {

    }

    protected AbstractResourceHolder(AbstractVizResource<?, ?> rsc,
            boolean isSelected) {

        this.rsc = rsc;
        this.isSelected = isSelected;
    }

    public AbstractVizResource<?, ?> getRsc() {
        return rsc;
    }

    public void setRsc(AbstractVizResource<?, ?> rsc) {
        this.rsc = rsc;
    }

    public boolean isSelected() {
        return isSelected;
    }

    public void setSelected(boolean isSelected) {
        this.isSelected = isSelected;
    }

    public boolean isGenerated() {
        return isGenerated;
    }

    public void setGenerated(boolean isEnsGenerated) {
        this.isGenerated = isEnsGenerated;
    }

    @Override
    public boolean isLoadedAtFrame() {

        if (getRsc() == null) {
            return false;
        }

        // Use the current resource to directly get the current FramesInfo
        // for avoid asynchronous issue.
        FramesInfo currentFrameInfo = getRsc().getDescriptor().getFramesInfo();
        if (currentFrameInfo == null) {
            return false;
        }

        // Get the frame index by the frame time
        DataTime[] frameTimes = currentFrameInfo.getFrameTimes();
        if (frameTimes == null || frameTimes.length < 1) {
            return false;
        }

        // Get the resource data time in current frame.
        DataTime resourceTime = currentFrameInfo.getTimeForResource(getRsc(),
                currentFrameInfo.getFrameIndex());
        if (resourceTime == null) {
            return false;
        }

        return true;

        // TODO: Verify if the data loaded at local.
        // D2DGridResource gridResource = null;
        // For none D2D grid resource do nothing to verify data currently
        // if (getRsc() instanceof D2DGridResource) {
        // gridResource = (D2DGridResource) getRsc();
        // } else {
        // return true;
        // }
        // Whether the resource grid data is in the current frame.
        // Refer to the loaded data in current frame, otherwise will do an extra
        // data request
        // to verify.It will not hurt any, just inefficient. because the
        // requestData(resourceTime)
        // is the only interface we can use. It will be best to added a
        // interface
        // "public boolean isDataLoaded(DataTime resourceTime)" in the
        // AbstractGrideResource class.
        // List<GeneralGridData> data = gridResource.requestData(resourceTime);
        // if (data != null ||data.isEmpty() ) {
        // return true;
        // }
        // return false;
    }

    public boolean isLoadedAtFrameInternal(FramesInfo info) {

        if (getRsc() == null) {
            return false;
        }

        // Use the current resource to directly get the current FramesInfo
        // for avoid asynchronous issue.
        FramesInfo currentFrameInfo = getRsc().getDescriptor().getFramesInfo();
        if (currentFrameInfo == null) {
            return false;
        }

        AbstractVizResource<?, ?> resource = getRsc();
        String name = resource.getName();
        if (name == null) {
            return false;
        }

        if (!resource.isTimeAgnostic()) {
            boolean hasTimes = false;
            DataTime[] times = info.getTimeMap().get(resource);
            if (times != null) {
                for (DataTime dt : times) {
                    if (dt != null) {
                        hasTimes = true;
                        break;
                    }
                }
            }
            if (!hasTimes) {
                return false;
            }

            DataTime time = getDataTimeForResource(getRsc(), getRsc()
                    .getDescriptor(), info);

            if (time == null) {
                return false;
            }

            // Get the frame index by the frame time
            DataTime[] frameTimes = currentFrameInfo.getFrameTimes();
            if (frameTimes == null || frameTimes.length < 1) {
                return false;
            }

            // Check if this resource is loaded in any frame
            // by checking matched data time
            for (int i = 0; i < frameTimes.length; i++) {
                // Any matched resource data time for a frame
                DataTime memberTime = getRsc().getDescriptor().getFramesInfo()
                        .getTimeForResource(getRsc(), i);
                if (memberTime != null) {
                    return true;
                }
            }

        }
        return false;
    }

    private DataTime getDataTimeForResource(AbstractVizResource<?, ?> rsc,
            IDescriptor descriptor, FramesInfo info) {
        return info.getTimeForResource(rsc);
    }

    public abstract boolean requiresLoadCheck();

}
