/*
 * PgenPrefernces
 * 
 * Date created 31 DECEMBER 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen;

import gov.noaa.nws.ncep.ui.pgen.PgenUtil.PgenMode;
import gov.noaa.nws.ncep.ui.pgen.gfa.GfaClip;

import java.io.File;

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.ComboFieldEditor;
import org.eclipse.jface.preference.DirectoryFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.IntegerFieldEditor;
import org.eclipse.jface.preference.RadioGroupFieldEditor;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

/**
 * Specifies PGEN preferences page
 * 
 * <pre>
 * 
 *     SOFTWARE HISTORY
 *    
 *     Date       	Ticket#		Engineer	Description
 *     ------------	----------	-----------	--------------------------
 *     12/31/09     #158        sgilbert    Initial Creation.
 *     02/10/11		?			B. Yin		Add maximum distance to be selected
 *     02/11	    #405        J. Wu    	Added P_WORKING_DIR.
 *     05/11	    ?        	J. Wu    	Added P_COMP_COORD
 *     08/11	    #335        J. Wu    	Added P_BASE_DIR
 *     04/12        #977        S. Gilbert  PGEN Database support
 *     11/13        TTR752      J. Wu       Added P_AUTOPLACE_TEXT
 *     12/13        TTR776      J. Wu       Added P_LAYER_MERGE
 * 
 * </pre>
 * 
 * @author sgilbert
 * 
 */
public class PgenPreferences extends FieldEditorPreferencePage implements
        IWorkbenchPreferencePage {

    public final static String P_RECOVERY_DIR = "PGEN_RECOVERY_DIR";

    public final static String V_RECOVERY_DIR = "/tmp";

    public final static String P_AUTO_FREQ = "PGEN_AUTOSAVE_FREQ";

    public final static String P_MAX_DIST = "PGEN_MAX_DISTANCE_TO_SELECT";

    public final static String P_PGEN_MODE = "PGEN_MODE";

    public final static String P_LAYER_LINK = "PGEN_LAYER_LINK";

    public final static String P_WORKING_DIR = "PGEN_WORKING_DIR";

    public final static String V_WORKING_DIR = System.getProperty("user.home");

    // Preference for the operational directory to store PGEN product file.
    public final static String P_OPR_DIR = "PGEN_BASE_DIR";

    public final static String V_OPR_DIR = (System.getenv("PGEN_OPR") != null) ? System
            .getenv("PGEN_OPR") : System.getProperty("user.home");

    // Preference for PGEN computational coordinates
    public final static String P_COMP_COORD = "PGEN_COMP_COORD";

    public final static String CED_COMP_COORD = "ced/0;0;0|18.00;-137.00;58.00;-54.00";

    // public final static String CED_COMP_COORD = "ced/0;0;0|KS+";
    public final static String STR_COMP_COORD = "str/90;-97;0|19.00;-119.00;47.00;-56.00";

    // Preference to place text box automatically (CCFP);
    public final static String P_AUTOPLACE_TEXT = "PGEN_AUTOPLACE_TEXT";

    private BooleanFieldEditor autoPlaceText;

    public final static String P_LAYER_MERGE = "P_LAYER_MERGE";

    private BooleanFieldEditor layerLink;

    public PgenPreferences() {
        super(GRID);
        setPreferenceStore(Activator.getDefault().getPreferenceStore());
        setDescription("Specify PGEN preferences");
    }

    @Override
    public void createFieldEditors() {

        this.addField(new DirectoryFieldEditor(P_OPR_DIR,
                "&PGEN Base Directory:", getFieldEditorParent()));

        this.addField(new DirectoryFieldEditor(P_WORKING_DIR,
                "&PGEN Working Directory:", getFieldEditorParent()));

        this.addField(new DirectoryFieldEditor(P_RECOVERY_DIR,
                "&PGEN Recovery Directory:", getFieldEditorParent()));

        IntegerFieldEditor freqEd = new IntegerFieldEditor(P_AUTO_FREQ,
                "&Auto Save frequency (min):", getFieldEditorParent(), 2);
        freqEd.setValidRange(1, 99);
        freqEd.setValidateStrategy(IntegerFieldEditor.VALIDATE_ON_FOCUS_LOST);
        this.addField(freqEd);

        addField(new IntegerFieldEditor(P_MAX_DIST,
                "&Maximum Distance to be Selected (pixel):",
                getFieldEditorParent(), 6));

        String[][] modeOptions = new String[][] {
                {
                        "Single PGEN visible on all Map Editors (Legacy NMAP behavior)",
                        PgenMode.SINGLE.toString() },
                { "Separate PGEN Data for each Map Editor",
                        PgenMode.MULTIPLE.toString() } };

        RadioGroupFieldEditor modeEditor = new RadioGroupFieldEditor(
                P_PGEN_MODE, "&PGEN Mode:", 1, modeOptions,
                getFieldEditorParent(), true);
        this.addField(modeEditor);

        layerLink = new BooleanFieldEditor(
                P_LAYER_LINK,
                "&Link Pgen Layers with Editor? (option valid in PGEN SINGLE mode only)",
                BooleanFieldEditor.DEFAULT, getFieldEditorParent());
        this.addField(layerLink);

        ComboFieldEditor projCombo = new ComboFieldEditor(P_COMP_COORD,
                "&PGEN Computational Coordinate:", new String[][] {
                        { CED_COMP_COORD, CED_COMP_COORD },
                        { STR_COMP_COORD, STR_COMP_COORD } },
                getFieldEditorParent());
        this.addField(projCombo);

        autoPlaceText = new BooleanFieldEditor(P_AUTOPLACE_TEXT,
                "&Text Auto Placement (where applicable)",
                BooleanFieldEditor.DEFAULT, getFieldEditorParent());
        this.addField(autoPlaceText);

        ComboFieldEditor layerMerge = new ComboFieldEditor(P_LAYER_MERGE,
                "&Default Action for PGEN Layer Merge:", new String[][] {
                        { "Take no action", "0" },
                        { "Add all as new layers", "2" },
                        { "Replace all like name layers", "3" },
                        { "Merge all like name layers", "4" },
                        { "Merge all into active layer", "5" }, },
                getFieldEditorParent());
        this.addField(layerMerge);

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
     */
    public void init(IWorkbench workbench) {

    }

    @Override
    public void propertyChange(PropertyChangeEvent event) {

        if (event.getSource() instanceof RadioGroupFieldEditor) {

            String value = event.getNewValue().toString();
            if (value.equalsIgnoreCase(PgenMode.SINGLE.toString())) {
                layerLink.setEnabled(true, getFieldEditorParent());
            } else if (value.equalsIgnoreCase(PgenMode.MULTIPLE.toString())) {
                layerLink.setEnabled(false, getFieldEditorParent());
            }
        } else if (event.getSource() instanceof DirectoryFieldEditor) {
            String ovalue = event.getOldValue().toString();
            String prefname = ((DirectoryFieldEditor) event.getSource())
                    .getPreferenceName();
            String opref = Activator.getDefault().getPreferenceStore()
                    .getString(prefname);
            if (ovalue.equals(opref)) {
                String nvalue = event.getNewValue().toString();
                File nfile = new File(nvalue);
                if (nfile.exists() && nfile.isDirectory() && nfile.canWrite()) {
                    Activator.getDefault().getPreferenceStore()
                            .setValue(prefname, nvalue);
                }
            }
        } else if (event.getSource() instanceof ComboFieldEditor) {
            String prefname = ((ComboFieldEditor) event.getSource())
                    .getPreferenceName();
            if (prefname.equals(P_COMP_COORD)) {
                GfaClip.getInstance().updateGfaBoundsInGrid();
            } else if (prefname.equals(P_LAYER_MERGE)) {
                String nvalue = event.getNewValue().toString();
                Activator.getDefault().getPreferenceStore()
                        .setValue(prefname, Integer.parseInt(nvalue));
            }
        } else if (event.getSource() instanceof IntegerFieldEditor) {
            IntegerFieldEditor iField = (IntegerFieldEditor) event.getSource();
            if (iField.getPreferenceName().equals(P_AUTO_FREQ)) {
                if (!iField.isValid())
                    iField.setStringValue(getPreferenceStore()
                            .getDefaultString(P_AUTO_FREQ));
            }
        }
    }

}
