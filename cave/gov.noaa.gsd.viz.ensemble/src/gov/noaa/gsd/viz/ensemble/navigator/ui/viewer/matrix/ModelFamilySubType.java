package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix;

/***
 * 
 * This enumeration contains a list of model family subtypes and is analogous to
 * the decomposition of the Model Family submenu options in the CAVE Volume
 * menu.
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
public enum ModelFamilySubType {

    /* Surface Family */
    BASIC_SFC(ModelFamilyType.SURFACE, "Basic Surface Family"), //

    /* Winter Model Families */
    BASIC(ModelFamilyType.WINTER_MODEL, "Basic Winter Family"), //

    /* Convection Families */
    HAIL(ModelFamilyType.CONVECTION, "Hail Family"), //
    HEAVY_RAIN(ModelFamilyType.CONVECTION, "Heavy Rain Family"), //
    MCS(ModelFamilyType.CONVECTION, "MCS Family"), //
    NONSUPERCELL_TORNADO(ModelFamilyType.CONVECTION,
            "Non-Supercell Tornado Family"), //
    QLCS_WIND(ModelFamilyType.CONVECTION, "QLCS/Wind Family"), //
    STORM_INITIATION(ModelFamilyType.CONVECTION, "Storm Initiation Family"), //
    STORM_TYPE(ModelFamilyType.CONVECTION, "Storm Type Family"), //
    SUPERCELL(ModelFamilyType.CONVECTION, "Supercell Family"), //
    SUPERCELL_TORNADO(ModelFamilyType.CONVECTION, "Supercell Tornado Family"); //

    private String subTypeName = null;

    private ModelFamilyType parentType = null;

    ModelFamilySubType(ModelFamilyType type, String menuText) {
        subTypeName = menuText;
        parentType = type;
    }

    public String getSubtypeName() {
        return subTypeName;
    }

    public ModelFamilyType getParentType() {
        return parentType;
    }

    public String toString() {
        return subTypeName;
    }
}
