package gov.noaa.nws.ncep.viz.ui.perspectives.menus;

import gov.noaa.nws.ncep.viz.resources.manager.PredefinedAreasMngr;
import gov.noaa.nws.ncep.viz.tools.predefinedArea.PredefinedAreaAction;

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
 *  4/15/11                  G. Hull      created.
 * 07/28/11       450        G. Hull      Use PredefinedAreasMngr
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */
public class PredefinedAreaMenu extends ContributionItem {
	
	@Override
	public void fill(Menu menu, int index ) {
		
		String predefinedAreas[] = PredefinedAreasMngr.getAvailPredefinedAreas();

		int indx=0;
		for( String areaName : predefinedAreas ) {		
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

