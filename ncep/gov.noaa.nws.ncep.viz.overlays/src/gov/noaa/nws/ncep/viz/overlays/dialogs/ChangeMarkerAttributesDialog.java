package gov.noaa.nws.ncep.viz.overlays.dialogs;


import java.io.File;
import java.text.DecimalFormat;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Slider;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;

import com.raytheon.viz.ui.editor.AbstractEditor;

import gov.noaa.nws.ncep.viz.common.ui.color.ColorMatrixSelector;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;
import gov.noaa.nws.ncep.viz.overlays.Activator;
import gov.noaa.nws.ncep.viz.overlays.IPointOverlayResourceData.MarkerState;
import gov.noaa.nws.ncep.viz.overlays.IPointOverlayResourceData.MarkerTextSize;
import gov.noaa.nws.ncep.viz.overlays.IPointOverlayResourceData.MarkerType;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.attributes.AbstractEditResourceAttrsDialog;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceAttrSet.RscAttrValue;
import gov.noaa.nws.ncep.viz.ui.display.AbstractNcEditor;
import gov.noaa.nws.ncep.viz.ui.display.NcEditorUtil;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;

/**
 * Provides an interface to modify the point overlay parameters
 * 
 * This dialog must serve two purposes:  Allow 'live' editing
 * of an already loaded resource, and selection of preferences
 * for an RBD for future use.  For this reason, the dialog
 * (1) allows a caller to set preselected values (say, from
 * current resource settings, or from tables of defaults for
 * a particular resource type), (2) stores selected values in
 * private member variables, and (3) allows the caller to
 * retrieve selected values after the dialog has been closed.
 * 
 * (Design note:  Item (2) above is required because we can't
 * interrogate the settings of widgets after the dialog has
 * been closed, because they may have been destroyed by then.)
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 06 Feb 2009  53          bhebbard    Initial Creation.
 * 15 Apr 2009  53B         bhebbard    Revise marker type selection
 *                                      to show icons; strengthen error
 *                                      checking in setAttributesFromMap;
 *                                      various layout and other cleanups.
 * 21 Apr 2009  90          bhebbard    Factor out color selection; use ColorMatrixSelector.
 * 17 Jun 2009  115         Greg Hull   Integrated with AbstractEditResourceAttrsDialog
 * 31 Jul 2009              ghull       Migrate to to11
 * 27 Apr 2010   #245       Greg Hull   Added Apply Button
 * 23 Apr 2012   #744       sgurung     Display Marker Text based on user specified zoom level
 * 
 * </pre>
 * 
 * @author bhebbard
 * @version 1
 */

public class ChangeMarkerAttributesDialog extends AbstractEditResourceAttrsDialog {
	//  Current attribute values.
	
	/*  Just initialize each to a 'neutral' default state on dialog
	    creation; typically the caller will later use setter methods
	    to apply particular defaults before opening the dialog.  */
	
    private RscAttrValue markerState    = null;    
    private RscAttrValue markerType     = null;    
    private RscAttrValue markerSize     = null;    
    private RscAttrValue markerWidth    = null;   
    private RscAttrValue markerTextSize = null;    
    private RscAttrValue markerColor    = null; 
    private RscAttrValue markerTextAppearZoomLevel = null; 
    
    private MarkerType     selMarkerType = null; // working copy
    private MarkerTextSize selMarkerTextSize = null; // working copy
    
    private File applyZoomImageFile = NcPathManager.getInstance().getStaticFile("ncep/icons/icon_zoom.gif");
   
    public ChangeMarkerAttributesDialog( Shell parentShell, INatlCntrsResourceData rd, Boolean apply ) {
        super(parentShell, rd, apply);
    }

	@Override
	public Composite createDialog(Composite composite) {
        FormLayout layout0 = new FormLayout();
        composite.setLayout(layout0);
        
        // Get the set of attributes from AbstractEditResourceAttrsDialog and check that all the 
        // attributes that we expect are there.
        markerState = editedRscAttrSet.getRscAttr("markerState");
        markerType    = editedRscAttrSet.getRscAttr("markerType");
        markerTextSize = editedRscAttrSet.getRscAttr("markerTextSize");
        markerSize  = editedRscAttrSet.getRscAttr("markerSize");
        markerWidth  = editedRscAttrSet.getRscAttr("markerWidth");
        markerColor  = editedRscAttrSet.getRscAttr("color");
        markerTextAppearZoomLevel = editedRscAttrSet.getRscAttr("markerTextAppearanceZoomLevel");
        
        selMarkerType =     (MarkerType)markerType.getAttrValue();
        selMarkerTextSize = (MarkerTextSize)markerTextSize.getAttrValue();
        
        // Confirm that the types are the expected classes

        if( markerState == null || 
        		markerState.getAttrClass() != MarkerState.class ) {
        	System.out.println("markerState is null or not of expected class?");
        }
                
        //  Lay out the various groups within the dialog
        
        Group selectMarkerTypeGroup = new Group ( composite, SWT.SHADOW_NONE );
        selectMarkerTypeGroup.setText("Marker Type");
        GridLayout markerTypeGridLayout = new GridLayout();
        markerTypeGridLayout.numColumns = 1;
        markerTypeGridLayout.marginLeft = 28;
        markerTypeGridLayout.marginHeight = 10;
        selectMarkerTypeGroup.setLayout(markerTypeGridLayout);
        
        FormData formData1 = new FormData();
        formData1.top = new FormAttachment(2,0);
        formData1.left = new FormAttachment(2,0);
        formData1.right  = new FormAttachment(47, 0);
        formData1.height = 60;
        selectMarkerTypeGroup.setLayoutData(formData1);

        Group selectMarkerStateGroup = new Group ( composite, SWT.SHADOW_NONE );
        selectMarkerStateGroup.setText("State");
        GridLayout markerStateGridLayout = new GridLayout();
        markerStateGridLayout.numColumns = 1;
        markerStateGridLayout.marginLeft = 12;
        selectMarkerStateGroup.setLayout(markerStateGridLayout);
        
        FormData formData2 = new FormData();
        formData2.top = new FormAttachment(selectMarkerTypeGroup, 10);
        formData2.left = new FormAttachment(2,0);
        formData2.right  = new FormAttachment(47, 0);
        formData2.height = 96;
        selectMarkerStateGroup.setLayoutData(formData2);

        Group selectMarkerSizeGroup = new Group ( composite, SWT.SHADOW_NONE );
        selectMarkerSizeGroup.setText("Marker Size");
        GridLayout markerSizeGridLayout = new GridLayout();
        markerSizeGridLayout.numColumns = 1;
        selectMarkerSizeGroup.setLayout(markerSizeGridLayout);
        
        FormData formData3 = new FormData();
        formData3.top = new FormAttachment(2,0);
        formData3.left  = new FormAttachment(selectMarkerStateGroup, 10);
        formData3.right  = new FormAttachment(98, 0);
        selectMarkerSizeGroup.setLayoutData(formData3);

        Group selectMarkerWidthGroup = new Group ( composite, SWT.SHADOW_NONE );
        selectMarkerWidthGroup.setText("Marker Width");
        GridLayout markerWidthGridLayout = new GridLayout();
        markerWidthGridLayout.numColumns = 1;
        selectMarkerWidthGroup.setLayout(markerWidthGridLayout);
        
        FormData formData4 = new FormData();
        formData4.top   = new FormAttachment(selectMarkerSizeGroup, 4);
        formData4.left  = new FormAttachment(selectMarkerStateGroup, 10);
        formData4.right = new FormAttachment(98,0);
        selectMarkerWidthGroup.setLayoutData(formData4);

        Group selectMarkerTextSizeGroup = new Group ( composite, SWT.SHADOW_NONE );
        selectMarkerTextSizeGroup.setText("Text Size");
        GridLayout markerTextSizeGridLayout = new GridLayout();
        markerTextSizeGridLayout.numColumns = 1;
        markerTextSizeGridLayout.marginLeft = 12;
        selectMarkerTextSizeGroup.setLayout(markerTextSizeGridLayout);        

        FormData formData5 = new FormData();
        formData5.top = new FormAttachment(selectMarkerWidthGroup, 4);
        formData5.left  = new FormAttachment(selectMarkerStateGroup, 10);
        formData5.right = new FormAttachment(98, 0);
        selectMarkerTextSizeGroup.setLayoutData(formData5);
        
        Group selectMarkerTxtAppZoomLvlGroup = new Group ( composite, SWT.SHADOW_NONE );
        selectMarkerTxtAppZoomLvlGroup.setText("Text Appearance Zoom Level");
        
        GridLayout markerTxtAppZoomLvlGridLayout = new GridLayout();
        markerTxtAppZoomLvlGridLayout.numColumns = 2; 
        markerTxtAppZoomLvlGridLayout.marginLeft = 12;
        markerTxtAppZoomLvlGridLayout.marginRight = 12;
        selectMarkerTxtAppZoomLvlGroup.setLayout(markerTxtAppZoomLvlGridLayout);
        
        FormData formData6 = new FormData();
        formData6.top   = new FormAttachment(selectMarkerTextSizeGroup, 10);
        formData6.left  = new FormAttachment(2, 0);
        formData6.right = new FormAttachment(98,0);        
        selectMarkerTxtAppZoomLvlGroup.setLayoutData(formData6);
    
        Group selectMarkerColorGroup = new Group ( composite, SWT.SHADOW_NONE );
        selectMarkerColorGroup.setText("Color");
        
        FormData formData7 = new FormData();
        formData7.top = new FormAttachment(selectMarkerTxtAppZoomLvlGroup, 4);
        formData7.left  = new FormAttachment(2, 0);
        formData7.right = new FormAttachment(98, 0);
        formData7.height = 170;
        selectMarkerColorGroup.setLayoutData(formData7);
          
        //  Marker Type
	    
        /* Use a toolbar with a single drop-down button item that pops up
           a menu, to simulate a combo that works for images. */
        final ToolBar tb = new ToolBar(selectMarkerTypeGroup, SWT.HORIZONTAL);
    	final ToolItem ti = new ToolItem(tb, SWT.DROP_DOWN);
    	final Menu mu = new Menu(shell, SWT.POP_UP);
        for (MarkerType mt : MarkerType.values())
        {
            MenuItem mi = new MenuItem(mu, SWT.PUSH);
            mi.setData(mt);
        	mi.setText(mt.getDesignator());
        	//  TODO:  Use PGEN icons via extension points; avoid ordinal        	
        	Integer ord1 = mt.ordinal()+1;
        	String iconString = "icons/marker" + ord1.toString() + ".gif";
    		ImageDescriptor id = Activator.imageDescriptorFromPlugin(
    				Activator.PLUGIN_ID, iconString);
    		if ( id != null ) {
    			Image icon = id.createImage();
            	mi.setImage(icon);
    		}
            mi.addListener(SWT.Selection, new Listener() {
            	/* A new marker type has been chosen off the pop-up menu:
            	   Remember it AND set its icon back on the main button. */
            	public void handleEvent(Event event)
            	{
                    selMarkerType = (MarkerType) event.widget.getData();
                    markerType.setAttrValue( selMarkerType );
                    Image icon = mu.getItem( selMarkerType.ordinal()).getImage();
                    ti.setImage(icon);
                	ti.setToolTipText(selMarkerType.getDesignator());
            	}
            });

        }
        ti.addListener(SWT.Selection, new Listener() {
        	/* Main button clicked:  Pop up the menu showing all the symbols. */
        	public void handleEvent(Event event)
        	{
        		Rectangle bounds = ti.getBounds();
        		Point point = tb.toDisplay(bounds.x, bounds.y + bounds.height);
        		mu.setLocation(point);
        		mu.setVisible(true);
        	}
        });
        
        //  Set initial state

        Image icon = mu.getItem(selMarkerType.ordinal()).getImage();
        ti.setImage(icon);
    	ti.setToolTipText(selMarkerType.getDesignator());
    	
    	//  Marker Size
	    
	    final Label selectMarkerSizeSliderText = new Label(selectMarkerSizeGroup, SWT.NONE);
	    GridData gridData1 = new GridData();
	    gridData1.horizontalIndent = 50;
	    selectMarkerSizeSliderText.setLayoutData(gridData1);
	    final Slider selectMarkerSizeSlider = new Slider(selectMarkerSizeGroup, SWT.HORIZONTAL);
	    selectMarkerSizeSlider.setMinimum(5);
	    selectMarkerSizeSlider.setMaximum(31);
	    selectMarkerSizeSlider.setIncrement(1);
	    selectMarkerSizeSlider.setThumb(1);
	    selectMarkerSizeSlider.setSelection(10);
	    float mSize = ((Float)markerSize.getAttrValue()).floatValue();
	    selectMarkerSizeSlider.setSelection((int)(mSize * 10f + 0.5));
	    selectMarkerSizeSlider.addSelectionListener(new SelectionAdapter() {
	        public void widgetSelected(SelectionEvent event) {
	        	markerSize.setAttrValue( (Float)((float)selectMarkerSizeSlider.getSelection() / 10 ));
	        	selectMarkerSizeSliderText.setText(markerSize.getAttrValue().toString());
	        	selectMarkerSizeSliderText.redraw();
	        	selectMarkerSizeSliderText.update();
	        }
	    });
    	selectMarkerSizeSliderText.setText( markerSize.getAttrValue().toString() );
    	
	    //  Marker Width
	    
	    final Label selectMarkerWidthSliderText = new Label(selectMarkerWidthGroup, SWT.NONE);
	    GridData gridData2 = new GridData();
	    gridData2.horizontalIndent = 55;
	    selectMarkerWidthSliderText.setLayoutData(gridData2);
    	final Slider selectMarkerWidthSlider = new Slider(selectMarkerWidthGroup, SWT.HORIZONTAL);
	    selectMarkerWidthSlider.setMinimum(1);
	    selectMarkerWidthSlider.setMaximum(6);
	    selectMarkerWidthSlider.setIncrement(1);
	    selectMarkerWidthSlider.setThumb(1);
	    selectMarkerWidthSlider.setSelection(1);
	    selectMarkerWidthSlider.setSelection((Integer)markerWidth.getAttrValue());
	    selectMarkerWidthSlider.addSelectionListener(new SelectionAdapter() {
	        public void widgetSelected(SelectionEvent event) {
	        	selectMarkerWidthSliderText.setText(new Integer(selectMarkerWidthSlider.getSelection()).toString());
	            markerWidth.setAttrValue( (Integer)selectMarkerWidthSlider.getSelection() );
	        }
	    });
    	selectMarkerWidthSliderText.setText(new Integer(selectMarkerWidthSlider.getSelection()).toString());

    	
	    //  Marker Text Size
	    
	    final Combo selectMarkerTextSizeCombo = new Combo(selectMarkerTextSizeGroup, SWT.DROP_DOWN | SWT.READ_ONLY);
        for (MarkerTextSize mts : MarkerTextSize.values())
        {
        	selectMarkerTextSizeCombo.add(mts.getDisplayName());
        }
        String markerTextSizeMenuEntry = selMarkerTextSize.getDisplayName();
        selectMarkerTextSizeCombo.setText(markerTextSizeMenuEntry);
        selectMarkerTextSizeCombo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
            	selMarkerTextSize = MarkerTextSize.values()[selectMarkerTextSizeCombo.getSelectionIndex()];
            	markerTextSize.setAttrValue( (MarkerTextSize)selMarkerTextSize );
            }
        });
        
        // Marker Text Appearance Zoom Level
                
	    final Label selectMarkerTxtAppZoomLvlSliderText = new Label(selectMarkerTxtAppZoomLvlGroup, SWT.NONE);
	    GridData gridData3 = new GridData();
	    gridData3.horizontalIndent = 100;
	    gridData3.horizontalAlignment = GridData.FILL;	                 
	    gridData3.grabExcessHorizontalSpace = true;   
	    selectMarkerTxtAppZoomLvlSliderText.setLayoutData(gridData3);
	    
	    Button applyCurZoomLvlBtn = new Button(selectMarkerTxtAppZoomLvlGroup, SWT.PUSH);
        gridData3 = new GridData();	
        gridData3.heightHint = 26;
        gridData3.widthHint = 25;
        gridData3.horizontalIndent = 10;       
        applyCurZoomLvlBtn.setImage(new Image(Display.getCurrent(), applyZoomImageFile.getAbsolutePath()));
        applyCurZoomLvlBtn.setToolTipText("Use Current Zoom Level");
        applyCurZoomLvlBtn.setLayoutData(gridData3);
        
	    final Slider selectMarkerTxtAppZoomLvlSlider = new Slider(selectMarkerTxtAppZoomLvlGroup, SWT.HORIZONTAL);
    	gridData3 = new GridData(GridData.HORIZONTAL_ALIGN_CENTER);
	    gridData3.horizontalAlignment = GridData.FILL;	                 
	     selectMarkerTxtAppZoomLvlSlider.setLayoutData(gridData3);
    	selectMarkerTxtAppZoomLvlSlider.setMinimum(1);
    	selectMarkerTxtAppZoomLvlSlider.setMaximum(101);
    	selectMarkerTxtAppZoomLvlSlider.setIncrement(1);
    	selectMarkerTxtAppZoomLvlSlider.setThumb(1);
    	selectMarkerTxtAppZoomLvlSlider.setSelection(50);
    	float mDSize = ((Float)markerTextAppearZoomLevel.getAttrValue()).floatValue();
    	selectMarkerTxtAppZoomLvlSlider.setSelection((int)(mDSize * 10f + 0.5));
	    selectMarkerTxtAppZoomLvlSlider.addSelectionListener(new SelectionAdapter() {
	        public void widgetSelected(SelectionEvent event) {
	        	markerTextAppearZoomLevel.setAttrValue( (Float) ((float)selectMarkerTxtAppZoomLvlSlider.getSelection() / 10 ) );
	        	selectMarkerTxtAppZoomLvlSliderText.setText(markerTextAppearZoomLevel.getAttrValue().toString());
	        	selectMarkerTxtAppZoomLvlSliderText.redraw();
	        	selectMarkerTxtAppZoomLvlSliderText.update();
	        }
	    });
	    selectMarkerTxtAppZoomLvlSliderText.setText(markerTextAppearZoomLevel.getAttrValue().toString());
	   	
	    applyCurZoomLvlBtn.addSelectionListener( new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				AbstractEditor activeEd = NcDisplayMngr.getActiveNatlCntrsEditor();
				float currentZoomLvl = (float)NcEditorUtil.getSelectedPanes(activeEd)[0].getZoomLevel() * 10;
				currentZoomLvl = Math.round(currentZoomLvl * 100.0f) / 100.0f;
				markerTextAppearZoomLevel.setAttrValue( (Float) (currentZoomLvl) );
	        	selectMarkerTxtAppZoomLvlSliderText.setText(markerTextAppearZoomLevel.getAttrValue().toString());
	        	selectMarkerTxtAppZoomLvlSliderText.redraw();
	        	selectMarkerTxtAppZoomLvlSliderText.update();
	        	selectMarkerTxtAppZoomLvlSlider.setSelection((int)(currentZoomLvl * 10f + 0.5));				
			}
		});
      	    
        //  Marker State

	    Button markerOnlyButton = new Button(selectMarkerStateGroup, SWT.RADIO);
    	markerOnlyButton.setText("marker only");
    	markerOnlyButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
            	markerState.setAttrValue( MarkerState.MARKER_ONLY );
	        	selectMarkerTextSizeCombo.setEnabled(false);
        		selectMarkerSizeSliderText.setEnabled(true);
        		selectMarkerSizeSlider.setEnabled(true);
        		selectMarkerWidthSliderText.setEnabled(true);
        		selectMarkerWidthSlider.setEnabled(true);
        		selectMarkerTxtAppZoomLvlSliderText.setEnabled(false);
        		selectMarkerTxtAppZoomLvlSlider.setEnabled(false);
            }
        });
    	
	    Button markerPlusTextButton = new Button(selectMarkerStateGroup, SWT.RADIO);
		markerPlusTextButton.setText("marker + text");
		markerPlusTextButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
            	markerState.setAttrValue( MarkerState.MARKER_PLUS_TEXT );
	        	selectMarkerTextSizeCombo.setEnabled(true);
        		selectMarkerSizeSliderText.setEnabled(true);
        		selectMarkerSizeSlider.setEnabled(true);
        		selectMarkerWidthSliderText.setEnabled(true);
        		selectMarkerWidthSlider.setEnabled(true);
        		selectMarkerTxtAppZoomLvlSliderText.setEnabled(true);
        		selectMarkerTxtAppZoomLvlSlider.setEnabled(true);
            }
        });
        
	    Button textOnlyButton = new Button(selectMarkerStateGroup, SWT.RADIO);
	    textOnlyButton.setText("text only");	    
	    textOnlyButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
            	markerState.setAttrValue( MarkerState.TEXT_ONLY );
	        	selectMarkerTextSizeCombo.setEnabled(true);
        		selectMarkerSizeSliderText.setEnabled(false);
        		selectMarkerSizeSlider.setEnabled(false);
        		selectMarkerWidthSliderText.setEnabled(false);
        		selectMarkerWidthSlider.setEnabled(false);
        		selectMarkerTxtAppZoomLvlSliderText.setEnabled(true);
        		selectMarkerTxtAppZoomLvlSlider.setEnabled(true);
            }
        });

	    // might move this to initWidgets?
	    switch ( (MarkerState)markerState.getAttrValue() )  //  set initial state
	    {
	        case MARKER_ONLY:
	        {
	        	markerOnlyButton.setSelection(true);
	        	selectMarkerTextSizeCombo.setEnabled(false);
	        	break;
	        }
	        case MARKER_PLUS_TEXT:
        	{
        		markerPlusTextButton.setSelection(true);
        		break;
        	}
	        case TEXT_ONLY:
        	{
        		textOnlyButton.setSelection(true);
        		selectMarkerSizeSliderText.setEnabled(false);
        		selectMarkerSizeSlider.setEnabled(false);
        		selectMarkerWidthSliderText.setEnabled(false);
        		selectMarkerWidthSlider.setEnabled(false);
        		break;
        	}
	    }

        
        //  Marker and/or Text Color
	    
		final ColorMatrixSelector cms = new ColorMatrixSelector(selectMarkerColorGroup, false, true,
				22, 88, 18, 22, 28, 86, 0, 4, 5);
	    cms.setColorValue((RGB)markerColor.getAttrValue());
	    cms.addListener(new IPropertyChangeListener() {
	    	public void propertyChange(PropertyChangeEvent event) {
	    		markerColor.setAttrValue( cms.getColorValue() );
	    	}
	    });
	    
        //applyDialogFont(composite);
        return composite;
    }
	@Override
	public void initWidgets() {
		
	}
}

