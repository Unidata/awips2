package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix;

/***
 * 
 * This enumeration contains a list of model family subtypes and is analogous to
 * the top-level of the Model Family menu options in the CAVE Volume menu.
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
public enum ModelFamilyType {

    CONVECTION("Convection Families"), //

    SURFACE("Surface Family"), //

    WINTER_MODEL("Winter Families"); //

    private String menuName = null;

    ModelFamilyType(String menuText) {
        menuName = menuText;
    }

    public String getName() {
        return menuName;
    }

}
