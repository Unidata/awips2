/*
 * MarkerSelectionPanel
 * 
 * Date created (November 08, 2010)
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system. 
 */
package gov.noaa.nws.ncep.viz.rsc.wstm.rsc;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;

import gov.noaa.nws.ncep.viz.rsc.wstm.Activator;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.window.IShellProvider;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

/**
 * Creates a marker selection panel to select a marker for the WSTM resource
 * 
  * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 08-Nov-2010    247      Archana     Initial creation.
 *                                                
 * @author archana
 *</pre>
 */
public class MarkerSelectionPanel extends Dialog {
	
	/** List of symbolic names for the icons */
	private static List<String> markerNames;
	
	/**The symbolic name of the icon currently selected by the user*/
	private String selectedType;
	
	/**The button witht he marker icon clicked by the user*/
	private Button activator;
	
	/**The location of the marker icons*/
	private final static String ICON_DIR = "icons/marker";

	/**A map of the symbolic names for the marker icons to their actual location on the file-system*/
	private static Map<String, String> imageMap = new HashMap<String, String>(0);
	/**
	 * @return the imageMap
	 */
	protected static Map<String, String> getImageMap() {
		return imageMap;
	}


	private Composite top = null;
	/**
	 * OverLoaded Constructor
	 */
	public MarkerSelectionPanel ( Shell parent,  
			Button activator ) {
		super(parent);
		this.activator = activator;
		this.selectedType = activator.getData().toString();
		
		markerNames = new ArrayList<String>(Arrays.asList("OCTAGON", 
				                                                               "TRIANGLE",
				                                                               "SQUARE",
				                                                               "DIAMOND",
				                                                               "STAR"));
		populateMap();
	}
	/**
	 * Updates the selected type's data string and image icon.
	 */
@Override
	public void okPressed() {
	     changeIconForActivatorAndClosePanel();
	}

/**
 * Changes the icon on the button, to the icon selected 
 * by the user
 */
private void changeIconForActivatorAndClosePanel(){
	activator.setData( selectedType );
	activator.setImage( getButtonIcon( selectedType ) );	
	activator.setSelection(true);
	close();
}

	/**
	 * 
	 * @param iconName
	 * @return
	 */
    protected Image getButtonIcon( String iconName ) {    
    	Image buttonImage = getFillPatternIcon(this.imageMap.get(iconName));
    	return buttonImage;
    }

    /**
     * 
     * @param iconLocation
     * @return
     */
	private Image getFillPatternIcon( String iconLocation ){
		
		ImageDescriptor id = Activator.imageDescriptorFromPlugin(
				Activator.PLUGIN_ID, iconLocation );
		Image icon = null;
		if ( id != null ) {
			icon = id.createImage();
		}
			
		return icon;
	}   
	/**
	 * @param activator the activator to set
	 */
	public void setActivator(Button activator) {
		this.activator = activator;
	}
	
	public Button getActivator(){
		return this.activator;
	}
	
	/**
	 * Creates a row of 5 buttons with marker icons.
	 * 
	 */
	@Override
	public Control createDialogArea(Composite parent) {
		top = (Composite) super.createDialogArea(parent);
		GridLayout gridLayout = new GridLayout(5,  false );
		gridLayout.horizontalSpacing = 0;
		gridLayout.verticalSpacing = 0;
		gridLayout.marginHeight = 3;
		gridLayout.marginWidth = 3;
		top.setLayout(gridLayout);
		int numMarkers = markerNames.size();
		if(numMarkers > 0){
			for(String imageAlias: markerNames){
				Button markerBtn = new Button(top, SWT.PUSH);
				markerBtn.setData(imageAlias);
				markerBtn.setImage(getButtonIcon(imageAlias));
				markerBtn.addMouseListener(new MouseAdapter(){
					@Override
					public void mouseDown(MouseEvent mouseEvent) {
						selectedType =  new String(mouseEvent.widget.getData().toString()); 
					}		
					
					@Override
					public void mouseDoubleClick(MouseEvent e) {
						  changeIconForActivatorAndClosePanel();
					}
					
				});
				markerBtn.setSelection(false);
			}
		}
		return top;
		
	}

	
	/**
	 * Maps the symbolic name of the icon to its corresponding  file location
	 */
	public static void populateMap(){
		Integer ii = new Integer(17);
		for(String imageAlias: markerNames){
		    imageMap.put( imageAlias,new String(ICON_DIR + ii.toString() +".gif"));
		    ii = new Integer(ii.intValue()+1);
		}
	}
}
