package gov.noaa.gsd.viz.ensemble.display.common;

import gov.noaa.gsd.viz.ensemble.display.calculate.Calculation;
import gov.noaa.gsd.viz.ensemble.display.rsc.histogram.HistogramResource;
import gov.noaa.gsd.viz.ensemble.util.Utilities;

import java.util.StringTokenizer;

import com.raytheon.uf.viz.core.rsc.AbstractVizResource;

/**
 * Concrete resolution of accessors of histogram resource attributes.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 17, 2014    5056     jing     Initial creation
 * 
 * </pre>
 * 
 * @author jing
 * @author polster
 * @version 1.0
 */
public class HistogramGridResourceHolder extends GenericResourceHolder {

    private HistogramResource<?> currRsc = null;

    protected HistogramGridResourceHolder(AbstractVizResource<?, ?> rsc,
            boolean isSelected) {

        super(rsc, isSelected);
        currRsc = (HistogramResource<?>) rsc;
    }

    @Override
    public String getModel() {
        String model = "";
        return model;
    }

    @Override
    public String getLevel() {
        String level;

        if ((currRsc.getName() == null) || (currRsc.getName().length() == 0)) {
            level = "<level missing>";
        } else {
            StringTokenizer st = new StringTokenizer(currRsc.getName());
            level = st.nextToken();
        }
        return level;
    }

    @Override
    public String getParameter() {
        String parameter = "";

        return parameter;
    }

    @Override
    public String getDataTime() {
        String datatime = "";
        return datatime;
    }

    @Override
    public String getUnits() {
        String units = "";
        if ((currRsc.getName() == null) || (currRsc.getName().length() == 0)) {
            units = "<level missing>";
        } else {
            StringTokenizer st = new StringTokenizer(currRsc.getName());
            st.nextToken();
            units = st.nextToken();
        }
        return units;
    }

    @Override
    public Calculation getCalculation() {
        // return "color histogram";
        if (((HistogramResource<?>) this.rsc).getMode() == HistogramResource.DisplayMode.SAMPLING)
            return Calculation.HISTOGRAM_SAMPLING;
        else
            return Calculation.HISTOGRAM_TEXT;
    }

    public String getGroupName() {

        return getUniqueName();
    }

    public String getUniqueName() {

        String s = currRsc.getName();
        String nodeLabel = Utilities.removeExtraSpaces(s);

        return nodeLabel;
    }

    @Override
    public String getLocation() {
        return "";
    }

    @Override
    public String getType() {
        return "";
    }

    @Override
    public String getEnsembleId() {
        return "";
    }

    @Override
    public String getEnsembleIdRaw() {
        return "";
    }

    @Override
    public String getStationId() {
        return "";
    }

    @Override
    public int hashCode() {
        return getUniqueName().hashCode();
    }

}
