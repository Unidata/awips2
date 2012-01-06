package gov.noaa.nws.ncep.viz.ui.perspectives.menus;

import gov.noaa.nws.ncep.viz.localization.impl.LocalizationManager;
import gov.noaa.nws.ncep.viz.localization.impl.LocalizationResourcePathConstants;
import gov.noaa.nws.ncep.viz.tools.predefinedArea.PredefinedAreaAction;

import java.io.File;
import java.io.FilenameFilter;

import org.eclipse.jface.action.ContributionItem;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;

/**
 * Create the Menu Items for the Predefined Area menu
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 4/15/11                  G. Hull      created.
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class PredefinedAreaMenu extends ContributionItem {
	
	@Override
	public void fill(Menu menu, int index) {
		File areasDir = LocalizationManager.getInstance().getLocalizationFileDirectory(
				LocalizationResourcePathConstants.PREDEFINED_AREAS_DIR );

		if( !areasDir.exists() ) {
			return;
		}
		String predefinedAreas[] = areasDir.list( new FilenameFilter() {			
			@Override
			public boolean accept(File dir, String name) {
				return name.endsWith(".xml");
			}
		});

			
		int indx=0;
		for( String areaName : predefinedAreas ) {		
			areaName = areaName.substring( 0,areaName.indexOf(".xml") );
			MenuItem areaMenuItem = new MenuItem( menu, SWT.PUSH, indx++ );
			areaMenuItem.setText( areaName );
//			areaMenuItem.setData( areaName );

			areaMenuItem.addSelectionListener(new SelectionAdapter() {
				public void widgetSelected(SelectionEvent e) {
					PredefinedAreaAction.setGeographicArea( ((MenuItem)e.widget).getText() );
				}
			});
		}
	}

}

