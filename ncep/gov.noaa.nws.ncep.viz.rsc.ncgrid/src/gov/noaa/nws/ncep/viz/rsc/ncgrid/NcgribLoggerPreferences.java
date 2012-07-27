package gov.noaa.nws.ncep.viz.rsc.ncgrid;

import gov.noaa.nws.ncep.viz.common.Activator;

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

public class NcgribLoggerPreferences extends FieldEditorPreferencePage implements
		IWorkbenchPreferencePage{
	
	public final static String ENABLE_ALL_LOGGER = "ENABLE_ALL_LOGGER";
	
	public final static String ENABLE_RSC_LOGGER = "ENABLE_RSC_LOGGER";
	
	public final static String ENABLE_DGD_LOGGER = "ENABLE_DGD_LOGGER";

	public final static String ENABLE_CNTR_LOGGER = "ENABLE_CNTR_LOGGER";
	
	public final static String ENABLE_FINAL_LOGGER = "ENABLE_FINAL_LOGGER";

	private BooleanFieldEditor enableAll;
	
	private BooleanFieldEditor enableRsc;
	
	private BooleanFieldEditor enableDgdrv;
	
	private BooleanFieldEditor enableCntr;
	
	private BooleanFieldEditor enableFinal;
	
   	public NcgribLoggerPreferences() {
		super(GRID);
		setPreferenceStore(Activator.getDefault().getPreferenceStore());
//		setDescription("Select Clipping Area preference");
	}

   	
    @Override
    public void createFieldEditors() {
    	
    	Composite composite = getFieldEditorParent();
    	
    	enableAll = new BooleanFieldEditor(ENABLE_ALL_LOGGER, "&Enable All logger",
    			BooleanFieldEditor.DEFAULT,composite);
        this.addField(enableAll);
        
        enableRsc = new BooleanFieldEditor(ENABLE_RSC_LOGGER, "&Enable Resource logger",
        		BooleanFieldEditor.DEFAULT,composite);
        this.addField(enableRsc);
        
        enableDgdrv = new BooleanFieldEditor(ENABLE_DGD_LOGGER, "&Enable Diagnostic logger",
        		BooleanFieldEditor.DEFAULT,composite);
        this.addField(enableDgdrv);
        
        enableCntr = new BooleanFieldEditor(ENABLE_CNTR_LOGGER, "&Enable Contour logger",
        		BooleanFieldEditor.DEFAULT,composite);
        this.addField(enableCntr);
            
        enableFinal = new BooleanFieldEditor(ENABLE_FINAL_LOGGER, "&Enable Total Time logger",
        		BooleanFieldEditor.DEFAULT,composite);
        this.addField(enableFinal);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
     */
    public void init(IWorkbench workbench) {
    	
    }
    
}
