package gov.noaa.gsd.viz.ensemble.display.common;

import gov.noaa.gsd.viz.ensemble.display.calculate.Calculation;

/**
 * Force implementers of derived concrete classes to provide the metadata
 * getters for all the common meteorological components of a resource.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 17, 2014   5056     polster     Initial creation
 * 
 * </pre>
 * 
 * @author jing
 * @author polster
 * @version 1.0
 */
public abstract class AbstractLegendComponentsProvider {

    public AbstractLegendComponentsProvider() {
    }

    /* this will be a root name e.g. for an ensemble group */
    abstract public String getGroupName();

    /*
     * this will be a less specific name than the unique name though in certain
     * cases may be identical to the unique name
     */
    abstract public String getGeneralName();

    /*
     * this will be unique between resources within a group or it will be
     * identical to the getGroupName() method.
     */
    abstract public String getSpecificName();

    abstract public String getModel();

    abstract public String getLocation();

    abstract public String getLevel();

    abstract public String getParameter();

    abstract public String getUnits();

    abstract public String getDataTime();

    abstract public String getType();

    abstract public String getEnsembleId();

    abstract public String getEnsembleIdRaw();

    abstract public String getStationId();

    abstract public Calculation getCalculation();

    abstract public boolean isLoadedAtFrame();

    /*
     * An abstract place to put the SREF prettifier.
     */
    protected String srefPerturbationPrettyfied(String pert) {

        String niceName = "<undef>";

        if (pert == null) {
            return niceName;
        }

        String pertPrefix = pert.replaceAll("[0-9]", "");
        String pertNumStr = pert.replaceAll("[^0-9]", "");

        int pertNumber = Integer.parseInt(pertNumStr);

        if ((pertNumber >= 1) && (pertNumber <= 7)) {

            if (pertPrefix.startsWith("ctll")) {
                niceName = "nmm ctrl-1";
            } else if (pertPrefix.startsWith("n")) {
                niceName = "nmm n-" + pertNumber;
            } else if (pertPrefix.startsWith("p")) {
                niceName = "nmm p-" + pertNumber;
            }
        }

        if ((pertNumber >= 8) && (pertNumber <= 14)) {

            if (pertPrefix.startsWith("ctll")) {
                niceName = "nmb ctrl-1";
            } else if (pertPrefix.startsWith("n")) {
                niceName = "nmb n-" + (pertNumber - 7);
            } else if (pertPrefix.startsWith("p")) {
                niceName = "nmb p-" + (pertNumber - 7);
            }

        }

        if ((pertNumber >= 15) && (pertNumber <= 21)) {

            if (pertPrefix.startsWith("ctll")) {
                niceName = "em ctrl-1";
            } else if (pertPrefix.startsWith("n")) {
                niceName = "em n-" + (pertNumber - 14);
            } else if (pertPrefix.startsWith("p")) {
                niceName = "em p-" + (pertNumber - 14);
            }

        }

        niceName = String.format("%-10s", niceName);

        return niceName;
    }

}
