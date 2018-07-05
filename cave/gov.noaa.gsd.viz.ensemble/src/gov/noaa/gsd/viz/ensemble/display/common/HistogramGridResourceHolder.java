package gov.noaa.gsd.viz.ensemble.display.common;

import java.util.StringTokenizer;

import com.raytheon.uf.viz.core.rsc.AbstractVizResource;

import gov.noaa.gsd.viz.ensemble.display.calculate.Calculation;
import gov.noaa.gsd.viz.ensemble.display.rsc.histogram.HistogramResource;
import gov.noaa.gsd.viz.ensemble.util.Utilities;

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
 * Jan 15, 2016    12301    jing     Added distribution feature
 *                                   by using GRAPHIC_HISTGRAM display mode.
 * </pre>
 * 
 * @author jing
 * @author polster
 * @version 1.0
 */
public class HistogramGridResourceHolder extends GenericResourceHolder {

    private HistogramResource<?> currRsc = null;

    protected HistogramGridResourceHolder(AbstractVizResource<?, ?> rsc) {

        super(rsc);
        currRsc = (HistogramResource<?>) rsc;
        isGenerated = true;

    }

    @Override
    public HistogramResource<?> getRsc() {
        return (HistogramResource<?>) rsc;
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
        if (((HistogramResource<?>) this.rsc)
                .getMode() == HistogramResource.DisplayMode.POINT_SAMPLING) {
            return Calculation.HISTOGRAM_SAMPLING;
        } else if (((HistogramResource<?>) this.rsc)
                .getMode() == HistogramResource.DisplayMode.GRAPHIC_HISTOGRAM) {
            return Calculation.HISTOGRAM_GRAPHICS;
        } else {
            return Calculation.VALUE_SAMPLING;
        }
    }

    @Override
    public String getGeneralName() {
        return getSpecificName();
    }

    public String getSpecificName() {

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
    public boolean requiresLoadCheck() {
        return false;
    }

}
