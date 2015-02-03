package gov.noaa.gsd.viz.ensemble.display.common;

import gov.noaa.gsd.viz.ensemble.display.rsc.GeneratedEnsembleGridResource;
import gov.noaa.gsd.viz.ensemble.display.rsc.histogram.HistogramResource;
import gov.noaa.gsd.viz.ensemble.display.rsc.timeseries.GeneratedTimeSeriesResource;

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
 * 
 * </pre>
 * 
 * @author polster
 * @author jing
 * @version 1.0
 */

public abstract class GenericResourceHolder extends
        AbstractLegendComponentsProvider {

    AbstractVizResource<?, ?> rsc;

    boolean isSelected = false; // either selected or unselected

    boolean isGenerated = false; // either generated or a basic (normally
                                 // loaded) resource

    public static GenericResourceHolder createResourceHolder(
            AbstractVizResource<?, ?> rsc, boolean isSelected) {

        GenericResourceHolder genericRsc = null;

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
        }
        return genericRsc;
    }

    @Override
    public boolean equals(Object o) {
        boolean equals = false;
        if (o instanceof GenericResourceHolder) {
            GenericResourceHolder gr = (GenericResourceHolder) o;
            equals = this.getUniqueName().equals(gr.getUniqueName());
        } else {
            equals = false;
        }
        return equals;
    }

    public abstract int hashCode();

    protected GenericResourceHolder() {

    }

    protected GenericResourceHolder(AbstractVizResource<?, ?> rsc,
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

}
