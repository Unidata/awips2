package gov.noaa.nws.ncep.viz.rsc.satellite.rsc;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.viz.core.exception.VizException;

import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.attributes.AbstractEditResourceAttrsDialog;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceAttrSet.RscAttrValue;
import gov.noaa.nws.ncep.viz.resources.colorBar.ColorBarEditor;
import gov.noaa.nws.ncep.viz.ui.display.ColorBarFromColormap;
import gov.noaa.nws.ncep.viz.common.ColorMapUtil;

/**
 *  An interface to edit Mcidas resource attributes. 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 03/24/10      #259        Greg Hull    Initial Creation.
 * 04/27/2010    #245        Greg Hull    Added Apply Button
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1
 */

public class EditSatelliteAttrsDialog extends AbstractEditResourceAttrsDialog { 
   
    public EditSatelliteAttrsDialog(Shell parentShell, INatlCntrsResourceData r, Boolean apply) {
		super(parentShell, r, apply);
		// TODO Auto-generated constructor stub
	}

    private RscAttrValue colorMapNameAttr = null;
    private RscAttrValue colorBarAttr = null;
    
    private ColorBarFromColormap  editedColorBar = null;
    private ColorBarEditor colorBarEditor = null;

    private String cmapCategory="Satellite";
    // 
	@Override
	public Composite createDialog( Composite topComp ) {
		
        colorMapNameAttr = editedRscAttrSet.getRscAttr( "colorMapName" );
        colorBarAttr = editedRscAttrSet.getRscAttr( "colorBar" );

        if( colorMapNameAttr == null || colorMapNameAttr.getAttrClass() != String.class ) {
        	System.out.println("colorMapName is null or not of expected class String?");
        	return null;
        }
        if( colorBarAttr == null || colorBarAttr.getAttrClass() != ColorBarFromColormap.class ) {
        	System.out.println("colorBar is null or not of expected class ColorBarFromColormap?");
        	return null;
        }
        
        // The resource category is the category of the colormaps
        cmapCategory = rscData.getResourceName().getRscCategory();

        FormLayout layout0 = new FormLayout();
        topComp.setLayout(layout0);

        // the resource category is the category for the colormaps
        final String availColorMaps[] = ColorMapUtil.listColorMaps( cmapCategory );
        
        final Combo colorMapNamesCombo = new Combo( topComp, SWT.READ_ONLY | SWT.DROP_DOWN );
        FormData fd = new FormData();        
        fd.left = new FormAttachment( 50, -30 );
        fd.top = new FormAttachment( 0, 20 );
        colorMapNamesCombo.setLayoutData( fd );
        
        colorMapNamesCombo.setItems( availColorMaps );
        int seldColorMapIndx = -1;
        
        for( int c=0 ; c<availColorMaps.length ; c++ ) {
        	if( availColorMaps[c].equals( colorMapNameAttr.getAttrValue().toString() ) ) {
        		colorMapNamesCombo.select(c);
        		seldColorMapIndx = c;
        	}
        }
        
        if( seldColorMapIndx == -1 ) {
        	System.out.println("The current colorMap '"+colorMapNameAttr.getAttrValue().toString()+"' was not found.");
        	seldColorMapIndx = 0;
    		colorMapNamesCombo.select(0);
        }
        
        colorMapNamesCombo.addSelectionListener( new SelectionAdapter() {
        	public void widgetSelected(SelectionEvent e) {
        		// update the selected color map name in the attrSet and in the colorbar editor.
        		colorMapNameAttr.setAttrValue(  
        				 availColorMaps[ colorMapNamesCombo.getSelectionIndex() ] );
        		setColorBarFromColorMap( availColorMaps[ colorMapNamesCombo.getSelectionIndex() ] );

        		colorBarEditor.reset( editedColorBar );
        	}
        });

        Label colorMapLbl = new Label( topComp, SWT.None );
        fd = new FormData();        
        fd.right = new FormAttachment( colorMapNamesCombo, -5, SWT.LEFT );
        fd.top = new FormAttachment( colorMapNamesCombo, 3, SWT.TOP );
        colorMapLbl.setLayoutData( fd );
        colorMapLbl.setText( "Color Map" );

        Group colorBarGrp = new Group( topComp, SWT.NONE );
        colorBarGrp.setText("Edit Color Bar Options");
		fd = new FormData();//400,300);        
        fd.left = new FormAttachment( 0, 15 );
        fd.right = new FormAttachment( 100, -15 );
        fd.top = new FormAttachment( colorMapNamesCombo, 15, SWT.BOTTOM );
        fd.bottom = new FormAttachment( 100, -20 );
        colorBarGrp.setLayoutData( fd );
        
        colorBarGrp.setLayout( new FormLayout() );
        
        // usually just the options (length, orientation..) are initialized in the colorbar and a colormap
        // is used to initialize the color values.
        editedColorBar = (ColorBarFromColormap)colorBarAttr.getAttrValue();        
        
        setColorBarFromColorMap( colorMapNameAttr.getAttrValue().toString() );
        
        colorBarEditor = new ColorBarEditor( colorBarGrp, editedColorBar );

        return topComp;
    }

	public void setColorBarFromColorMap( String cmapName ) {
		ColorMap colorMap;
		try {
			colorMap = (ColorMap) ColorMapUtil.loadColorMap( cmapCategory, cmapName );
		} catch (VizException e) {
			System.out.println(e.getMessage());
			return;
		}

		((ColorBarFromColormap)editedColorBar).setColorMap( colorMap );
	}
	
	@Override
	public void initWidgets() {
		// done in createDialog		
	}    
	
	@Override 
	protected void dispose() {
		super.dispose();
		colorBarEditor.dispose();
	}
}