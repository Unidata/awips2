package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix;

/***
 * 
 * This enumeration contains a list of model sources that are available to be
 * associated with certain model families.
 * 
 * TODO: Storing configuration information into enumerations is not acceptable
 * for the longer term. Since the model family raw files are stable for now, we
 * will plan to change this in the next release.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 20, 2016   12371     polster     Initial creation
 * 
 * </pre>
 * 
 * @author polster
 * @version 1.0
 */
public enum ModelSources {

    ARW_EAST("ARWmodel1", "ARW East", "HiResW-ARW-East", false), //
    ARW_WEST("ARWmodel2", "ARW West", "HiResW-ARW-West", false), //
    CANADIAN_NH("NO_VARIABLE_NEEDED", "Canadian-NH", "Canadian-NH", false), //
    CANADIAN_REG("NO_VARIABLE_NEEDED", "Canadian-Reg", "Canadian-Reg", false), //
    DGEX("DGEXmodel", "DGEX", "DGEX185", true), //
    ECMWF_HIRES("NO_VARIABLE_NEEDED", "ECMWF-HiRes", "ECMWF-HiRes", true), //
    GEM_NH("GEMNHmodel", "GEM-NHem", "Canadian-NH", false), //
    GEM_REG("GEMRegmodel", "GEM-Regional", "Canadian-Reg", false), //
    GFS20("NO_VARIABLE_NEEDED", "GFS20", "GFS215", true), //
    GFS40("NO_VARIABLE_NEEDED", "GFS40", "GFS212", true), //
    GFS_GLOBAL("GFSmodel", "GFS Global", "GFS229", true), //
    HRRR("NO_VARIABLE_NEEDED", "HRRR", "HRRR", true), //
    LAPS("NO_VARIABLE_NEEDED", "LAPS", "LAPS", true), //
    AK_NAM12("AK-NAM12model", "AK-NAM12", "ETA242", true), //
    NAM12("NAM12model", "NAM12", "ETA218", true), //
    AK_NAM40("AK-NAM40model", "AK-NAM40", "mesoEta216", true), //
    NAM40("NAM40model", "NAM40", "mesoEta212", true), //
    NAM80("NO_VARIABLE_NEEDED", "NAM80", "ETA", true), //
    NAM_Nest("NAMNestmodel", "NAMNest 4km", "CR-NAMNest", false), //
    NMM_EAST("MMMmodel1", "HiResW-NMM-East", "HiResW-NMM-East", false), //
    NMM_WEST("MMMmodel2", "HiResW-NMM-West", "HiResW-NMM-West", false), //
    RAP13("RAP13model", "RAP13", "RUC130", true), //
    RAP40("RAPmodel", "RAP40", "RUC236", true), //
    SREF("SREFmodel", "SREF", "SREF212", true);

    private String variableName = null;

    private String modelName = null;

    private String modelId = null;

    private boolean active = false;

    ModelSources(String varName, String name, String id, boolean visibleInMenu) {
        setVariableName(varName);
        setModelId(id);
        setModelName(name);
        setActive(visibleInMenu);
    }

    public boolean isActive() {
        return active;
    }

    private void setActive(boolean active) {
        this.active = active;
    }

    public String getModelId() {
        return modelId;
    }

    private void setModelId(String modelId) {
        this.modelId = modelId;
    }

    public String getModelName() {
        return modelName;
    }

    private void setModelName(String modelName) {
        this.modelName = modelName;
    }

    public String getVariableName() {
        return variableName;
    }

    private void setVariableName(String variableName) {
        this.variableName = variableName;
    }

    public String toString() {
        return modelName;
    }

    public boolean isSameAs(ModelSources operand) {
        boolean isEqual = false;
        if (getModelId().equals(operand.getModelId())
                && getModelName().equals(operand.getModelName())) {
            isEqual = true;
        }
        return isEqual;
    }
}
