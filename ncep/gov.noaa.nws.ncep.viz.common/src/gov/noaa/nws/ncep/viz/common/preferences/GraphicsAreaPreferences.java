package gov.noaa.nws.ncep.viz.common.preferences;

import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;

import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

/**
 * Specifies Data Area preferences page
 * 
 * <pre>
 * 
 *     SOFTWARE HISTORY
 *    
 *     Date       	Ticket#		Engineer	Description
 *     ------------	----------	-----------	--------------------------
 *     Aug. 2011             	M. Li    	Initial Creation.
 *     May 2012     #809        S. Gurung   Adjust size of FieldEditors used for getting lats/lons
 * 
 * </pre>
 * 
 * @author mli
 * 
 */
public class GraphicsAreaPreferences extends FieldEditorPreferencePage implements
		IWorkbenchPreferencePage {

	public final static String LLLAT = "LLLAT";
	public final static String LLLON = "LLLON";
	public final static String URLAT = "URLAT";
	public final static String URLON = "URLON";

	private StringFieldEditor llLatText;
	private StringFieldEditor llLonText;
	private StringFieldEditor urLatText;
	private StringFieldEditor urLonText;
	
   	public GraphicsAreaPreferences() {
		super(GRID);
		setPreferenceStore( NmapCommon.getNcepPreferenceStore() );
//		setDescription("Select Data Area preference");
	}

   	
    @Override
    public void createFieldEditors() {
    	
    	Composite composite = getFieldEditorParent();
    	
        llLatText = new StringFieldEditor(LLLAT, "&Lower Left Latitude: ", 10,
        		composite);
        this.addField(llLatText);
        llLonText = new StringFieldEditor(LLLON, "&Lower Left Longitude: ", 10,
        		composite);
        this.addField(llLonText);
        urLatText = new StringFieldEditor(URLAT, "&Upper Right Latitude: ", 10,
        		composite);
        this.addField(urLatText);
        urLonText = new StringFieldEditor(URLON, "&Upper Right Longitude: ", 10,
        		composite);
        this.addField(urLonText);
        
        
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
     */
    public void init(IWorkbench workbench) {
    	
    }
    
    /*
    @Override
    public void propertyChange(PropertyChangeEvent event) {

    	garea.setEnabled(customArea.getBooleanValue(), getFieldEditorParent());
    	proj.setEnabled(customArea.getBooleanValue(), getFieldEditorParent());
//    	customArea.setEnabled(false, getFieldEditorParent());
//    	customArea.setEnabled(true, getFieldEditorParent());
    	
    }	
*/
    
 
}
