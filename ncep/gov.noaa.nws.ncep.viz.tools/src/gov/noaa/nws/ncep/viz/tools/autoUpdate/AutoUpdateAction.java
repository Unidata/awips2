package gov.noaa.nws.ncep.viz.tools.autoUpdate;

import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;
import gov.noaa.nws.ncep.viz.resources.time_match.NCTimeMatcher;
import gov.noaa.nws.ncep.viz.tools.Activator;
import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;
import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;

import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.commands.IElementUpdater;
import org.eclipse.ui.menus.UIElement;
import org.eclipse.ui.plugin.AbstractUIPlugin;

import com.raytheon.viz.ui.UiPlugin;

/**
 * Enable/Disable Auto Update
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 09/01/10      #307        Greg Hull    created
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class AutoUpdateAction extends AbstractHandler implements IElementUpdater {

	private static boolean autoUpdate = false;
	private static boolean autoUpdateNotApplicable = false;
	
	private ImageDescriptor autoUpdateOnIcon = null;
	private ImageDescriptor autoUpdateOffIcon = null;
	private ImageDescriptor autoUpdateDisabledIcon = null;
	
	static boolean getAutoUpdate() {
		return autoUpdate;
	}

	public AutoUpdateAction() {
		autoUpdateOnIcon = AbstractUIPlugin.imageDescriptorFromPlugin(
        		Activator.PLUGIN_ID, "icons/auto_on.gif");
		autoUpdateOffIcon = AbstractUIPlugin.imageDescriptorFromPlugin(
        		Activator.PLUGIN_ID, "icons/auto_off.gif");
		autoUpdateDisabledIcon = AbstractUIPlugin.imageDescriptorFromPlugin(
        		Activator.PLUGIN_ID, "icons/blank.gif");;
	}

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		
		if( autoUpdateNotApplicable ) {
			return null;
		}
		
		NCMapEditor currEditor = NmapUiUtils.getActiveNatlCntrsEditor();
				
		setAutoUpdate( !autoUpdate );            
		currEditor.setAutoUpdate( autoUpdate );
		
		// calls updateElement()
		currEditor.refreshGUIElements();
		currEditor.refresh();
		
		return null;
	}

	public void setAutoUpdate( boolean a ) {
		autoUpdate = a;
	}

	@Override
	public void updateElement(UIElement element, Map parameters) {
		NCMapEditor activeDisplay = NmapUiUtils.getActiveNatlCntrsEditor();

		autoUpdateNotApplicable = true;

		if( activeDisplay != null ) {
			NCMapDescriptor descr = (NCMapDescriptor)activeDisplay.getDescriptor();	
			NCTimeMatcher timeMatcher = (NCTimeMatcher)descr.getTimeMatcher();
			INatlCntrsResourceData rscData = (timeMatcher == null ? null : 
											    timeMatcher.getDominantResource() );			
			if( rscData != null && rscData instanceof AbstractNatlCntrsRequestableResourceData ) {				
				autoUpdateNotApplicable = 
					!((AbstractNatlCntrsRequestableResourceData)rscData).isAutoUpdateable(); 
			}			
		}

		if( autoUpdateNotApplicable ) {
			//element.setDisabledIcon(autoUpdateDisabledIcon);
			autoUpdate = false;
			element.setChecked( false );
			element.setIcon( autoUpdateDisabledIcon );
		}
		else {			
			autoUpdate = activeDisplay.getAutoUpdate();
		
			element.setChecked( autoUpdate );
			element.setIcon( (autoUpdate ? autoUpdateOnIcon : autoUpdateOffIcon) );			 
		}
	}
}
