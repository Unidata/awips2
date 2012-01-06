package gov.noaa.nws.ncep.viz.tools.locatorOptions;


import gov.noaa.nws.ncep.viz.ui.locator.LocatorDisplay;
import gov.noaa.nws.ncep.viz.ui.locator.resource.DisplayOptions;
import gov.noaa.nws.ncep.viz.ui.locator.resource.Locator;
import gov.noaa.nws.ncep.viz.ui.locator.resource.LocatorTool;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

/**
 * This class displays the Edit Locator Default Options dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 07/13/09      138        Greg Hull    Initial creation 
 * 11/24/09                 Greg Hull    migrate to to11d6
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1.0
 */

public class LocatorOptionsDialog extends Dialog {

    private Shell shell;
    private Display display;
    private Group dispOptionsGrp;
    private Combo locatorCombo, roundingCombo, distanceCombo;
    
    private Combo directionCombo, displayCombo, latlonCombo;
    
    private Label roundingLabel, distanceLabel, directionLabel;
    
    private Label displayLabel, latlonLabel, multLabel;
    
    private Button multTglBtn;
    
    private List<Locator> locatorList = null;
    private Locator selLocator = null;
    private DisplayOptions selDispOptions = null;

    protected LocatorOptionsDialog(Shell parentShell ) {
    	super(parentShell);
    }

    public boolean isOpen() {
        return shell != null && !shell.isDisposed();
    }    

    public Object open() {
    	LocatorDisplay locatorDisplay = LocatorDisplay.getInstance();

    	// TODO ? : should we initialize with the current defaults list 
    	// or with the current display options (from the Edit Dialog)
    	locatorList = new ArrayList<Locator>();
    	for( Locator loc : locatorDisplay.getLocatorList() ) {
    		locatorList.add( loc.copy() );
    	}

        Shell parent = getParent();
        display = parent.getDisplay();
        shell = new Shell(parent, SWT.DIALOG_TRIM | SWT.RESIZE );
        shell.setText("Edit Locator Options Defaults");

        FillLayout shellLayout = new FillLayout();
        shell.setLayout(shellLayout);
                        
        createDialog();
               
        shell.pack();

        shell.setLocation( parent.getBounds().x+100 , parent.getBounds().y+100 )
;

        shell.open();

        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }

        return null;
    }

    private void createDialog() {
        Composite topComp = new Composite( shell, SWT.NONE );
        FormLayout layout = new FormLayout();
        topComp.setLayout(layout);

        locatorCombo = new Combo( topComp, SWT.DROP_DOWN | SWT.READ_ONLY );

        FormData fd = new FormData();
        fd.top = new FormAttachment( 0, 35 );
        fd.left  = new FormAttachment( 50, -80 );
        locatorCombo.setLayoutData( fd );

        Label label = new Label( topComp, SWT.NONE );
        label.setText("Locator");
        fd = new FormData();
        fd.left  = new FormAttachment( locatorCombo, 0, SWT.LEFT );
        fd.bottom = new FormAttachment( locatorCombo, -5, SWT.TOP );
        label.setLayoutData( fd );

        for( Locator itm : locatorList ){
        	locatorCombo.add( itm.getLocatorName() );
        }

        locatorCombo.select(0);
        selLocator = locatorList.get(0); 
        selDispOptions = selLocator.getDisplayOptions();
        
        locatorCombo.addSelectionListener( new SelectionAdapter() {
        	public void widgetSelected(SelectionEvent e) {
        		selLocator = locatorList.get( locatorCombo.getSelectionIndex() );
        		selDispOptions = selLocator.getDisplayOptions();
        		createDisplayOptionsWidgets( );
        	}
        });
        
        dispOptionsGrp = new Group( topComp, SWT.NONE );
        dispOptionsGrp.setText("Display Options");
        
        GridLayout gl = new GridLayout( 2, false );
        gl.marginLeft = 10;
        gl.marginRight = 10;
        gl.marginTop = 10;
        gl.marginBottom = 10;
        dispOptionsGrp.setLayout( gl );

        fd = new FormData();
        fd.top = new FormAttachment( locatorCombo, 15, SWT.BOTTOM );
        fd.left  = new FormAttachment( 0, 15 );
        fd.right  = new FormAttachment( 100, -15 );
        dispOptionsGrp.setLayoutData( fd );

        dispOptionsGrp.pack(true);

        createDisplayOptionsWidgets( );

        createCloseButton( topComp );
    }
    
    private void createDisplayOptionsWidgets( ) {
    	if( roundingLabel != null ) {
    		roundingLabel.dispose();
    		roundingLabel = null;
    		roundingCombo.dispose();
    		roundingCombo = null;
    	}

    	if( distanceLabel != null ) {
    		distanceLabel.dispose();
    		distanceLabel = null;
    		distanceCombo.dispose();
    		distanceCombo = null;
    	}

    	if( directionLabel != null ) {
    		directionLabel.dispose();
    		directionLabel = null;
    		directionCombo.dispose();
    		directionCombo = null;
    	}

    	if( displayLabel != null ) {
    		displayLabel.dispose();
    		displayLabel = null;
    		displayCombo.dispose();
    		displayCombo = null;
    	}

    	if( latlonLabel != null ) {
    		latlonLabel.dispose();
    		latlonLabel = null;
    		latlonCombo.dispose();
    		latlonCombo = null;
    	}

    	if( multLabel != null ) {
    		multLabel.dispose();
    		multLabel = null;
    		multTglBtn.dispose();
    		multTglBtn = null;
    	}

    	// without this there is a problem in some cases where the new widgets aren't visible.
    	dispOptionsGrp.pack(true); 

    	if( selDispOptions.getRoundingToNearest() != null ) {
    		roundingLabel = new Label( dispOptionsGrp, SWT.NONE) ;
    		roundingLabel.setText("Round To:");
    		roundingLabel.setLayoutData( new GridData() );

    		roundingCombo = new Combo(dispOptionsGrp, SWT.DROP_DOWN | SWT.READ_ONLY);
    		roundingCombo.setLayoutData(new GridData());
    		roundingCombo.setItems( LocatorTool.ROUNDING_OPTIONS );

    		roundingCombo.addSelectionListener(new SelectionAdapter() {
    			public void widgetSelected(SelectionEvent e) {
    				if (roundingCombo.getSelectionIndex() != -1) {
    					selDispOptions.setRoundingToNearest(
    							Integer.valueOf(roundingCombo.getText()) );
    				}
    			}
    		});
    		setCombo( roundingCombo, LocatorTool.ROUNDING_OPTIONS, 
    				selDispOptions.getRoundingToNearest().toString()
    		);
    	}

    	if( selDispOptions.getDistanceUnit() != null ) {
    		distanceLabel = new Label(dispOptionsGrp, SWT.NONE);
    		distanceLabel.setText("Distance Units:");
    		distanceLabel.setLayoutData(new GridData());

    		distanceCombo = new Combo(dispOptionsGrp, SWT.DROP_DOWN | SWT.READ_ONLY);
    		distanceCombo.setLayoutData(new GridData());
    		distanceCombo.setItems(LocatorTool.DISTANCEUNIT_OPTIONS);

    		distanceCombo.addSelectionListener(new SelectionAdapter() {
    			public void widgetSelected(SelectionEvent e) {
    				if( distanceCombo.getSelectionIndex() != -1 ) {
    					selDispOptions.setDistanceUnit( distanceCombo.getText() );
    				}
    			}
    		});

    		setCombo( distanceCombo, LocatorTool.DISTANCEUNIT_OPTIONS, 
    				selDispOptions.getDistanceUnit().toString() );
    	}

    	if( selDispOptions.getDirectionUnit() != null ) {
    		directionLabel = new Label(dispOptionsGrp, SWT.NONE);
    		directionLabel.setText("Direction Units:");
    		directionLabel.setLayoutData(new GridData());

    		directionCombo = new Combo(dispOptionsGrp, SWT.DROP_DOWN | SWT.READ_ONLY);
    		directionCombo.setLayoutData(new GridData());
    		directionCombo.setItems(LocatorTool.DIRECTIONUNIT_OPTIONS);

    		directionCombo.addSelectionListener( new SelectionAdapter() {
    			public void widgetSelected(SelectionEvent e) {
    				if (directionCombo.getSelectionIndex() != -1) {
    					selDispOptions.setDirectionUnit( directionCombo.getText() );
    				}
    			}
    		});

    		setCombo( directionCombo, LocatorTool.DIRECTIONUNIT_OPTIONS, 
    				selDispOptions.getDirectionUnit().toString() );
    	}

    	if( selDispOptions.getDisplayAttribute() != null ) {
    		displayLabel = new Label(dispOptionsGrp, SWT.NONE);
    		displayLabel.setText("Station:");
    		displayLabel.setLayoutData(new GridData());

    		displayCombo = new Combo(dispOptionsGrp, SWT.DROP_DOWN | SWT.READ_ONLY);
    		displayCombo.setLayoutData(new GridData());
    		displayCombo.setItems(LocatorTool.STATIONDISPLAY_OPTIONS);

    		displayCombo.addSelectionListener(new SelectionAdapter() {
    			public void widgetSelected(SelectionEvent e) {
    				if( displayCombo.getSelectionIndex() != -1 ) {
    					selDispOptions.setDisplayAttribute(displayCombo.getText());
    				}
    			}
    		});

    		// if this locator only has 1 available Shapefile attribute to select from 
    		// then disable the Combo and just show the selected attribute of Name or ID.
    		if( selLocator.getAttributeName() == null ||
    				selLocator.getAttributeID() == null )
    		{
    			displayCombo.setEnabled(false);
    		}
    		
    		// this assumes that the locators file is set up correctly and that the displayOption
    		// doesn't conflict with the Locator's attribute. 
    		setCombo( displayCombo, LocatorTool.STATIONDISPLAY_OPTIONS, 
    				selDispOptions.getDisplayAttribute().toString() );
    	}

    	if( selDispOptions.getLatLonUnit() != null ) {
    		latlonLabel = new Label(dispOptionsGrp, SWT.NONE);
    		latlonLabel.setText("LatLon Units:");
    		latlonLabel.setLayoutData(new GridData());

    		latlonCombo = new Combo(dispOptionsGrp, SWT.DROP_DOWN | SWT.READ_ONLY);
    		latlonCombo.setLayoutData(new GridData());
    		latlonCombo.setItems(LocatorTool.LATLONUNIT_OPTIONS);
    		latlonCombo.addSelectionListener(new SelectionAdapter() {
    			public void widgetSelected(SelectionEvent e) {
    				if (latlonCombo.getSelectionIndex() != -1) {
    					selDispOptions.setLatLonUnit(latlonCombo
    							.getText());
    				}
    			}
    		} );

    		setCombo( latlonCombo, LocatorTool.LATLONUNIT_OPTIONS,
    				selDispOptions.getLatLonUnit().toString() );
    	}

    	multLabel = new Label( dispOptionsGrp, SWT.NONE );
    	multLabel.setText("Multiple Locators:");
    	multLabel.setLayoutData(new GridData());

    	multTglBtn = new Button( dispOptionsGrp, SWT.CHECK );
    	multTglBtn.setText("");
    	multTglBtn.setLayoutData(new GridData());

    	multTglBtn.addSelectionListener(new SelectionAdapter() {
    		public void widgetSelected(SelectionEvent e) {
    			selDispOptions.setIsMultipleDefault( multTglBtn.
    					getSelection() );
    		}
    	} );

    	multTglBtn.setSelection( selDispOptions.isIsMultipleDefault() );

    	dispOptionsGrp.pack(true);
    	shell.pack(true);
    }

    private void createCloseButton( Composite parentComp ) {
    	Label sepLbl = new Label(parentComp, SWT.SEPARATOR | SWT.HORIZONTAL);
    	FormData fd = new FormData();
    	fd.left = new FormAttachment( 0, 3 );
    	fd.right = new FormAttachment( 100, -3 );
    	fd.top = new FormAttachment( dispOptionsGrp, 20, SWT.BOTTOM );
    	sepLbl.setLayoutData(fd);

    	Button saveBtn = new Button( parentComp, SWT.NONE);
    	saveBtn.setText(" Save ");
    	fd = new FormData();
    	fd.left = new FormAttachment( 15, 0 );
    	fd.top = new FormAttachment( sepLbl, 10, SWT.BOTTOM );
    	fd.bottom = new FormAttachment( 100, -10 );
    	saveBtn.setLayoutData(fd);

    	saveBtn.addSelectionListener(new SelectionAdapter() {
    		public void widgetSelected(SelectionEvent event) {
    			LocatorDisplay.getInstance().setLocatorDfltsList( locatorList );
    		}
    	});

    	Button closeBtn = new Button(parentComp, SWT.NONE);
    	closeBtn.setText(" Close ");
    	fd = new FormData();
    	fd.right = new FormAttachment( 85, 0 );
    	fd.top = new FormAttachment( sepLbl, 10, SWT.BOTTOM );
    	fd.bottom = new FormAttachment( 100, -10 );
    	closeBtn.setLayoutData(fd);

    	closeBtn.addSelectionListener(new SelectionAdapter() {
    		public void widgetSelected(SelectionEvent event) {
    			shell.dispose();
    		}
    	});
    }

    public boolean setCombo( Combo combo, String[] items, String selStr ) {
    	for( int i=0 ; i < items.length ; i++) {
    		if( selStr.equalsIgnoreCase( items[i] )) {
    			combo.select(i);
    			return true;
    		}
    	}
    	return false;
    }

}
