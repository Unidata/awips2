package gov.noaa.nws.ncep.viz.cloudHeight.ui;

import java.util.ArrayList;

import javax.measure.converter.UnitConverter;
import javax.measure.quantity.Length;
import javax.measure.quantity.Temperature;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import gov.noaa.nws.ncep.viz.cloudHeight.ui.CloudHeightDialog.ComputationalMethod;
import gov.noaa.nws.ncep.viz.cloudHeight.ui.CloudHeightDialog.PixelValueMethod;
import gov.noaa.nws.ncep.viz.cloudHeight.ui.CloudHeightDialog.SoundingDataSourceType;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;

/**
 * Cloud Height Options Dialog.
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 05/01/09		#106        Greg Hull   Initial creation
 * 06/01/11     #393        Archana     Added a spinner to select
 *                                      the maximum time range for the 
 *                                      station data.
 * 02/16/12     #524        B. Hebbard  Resolve TODOs to change defaults for
 *                                      sndDataSrc and useSinglePix (per legacy,
 *                                      to STATION_DATA and false, respectively,
 *                                      now that those modes are implemented)
 * 03/01/12     #524        B. Hebbard  Rename "Close" button to "OK" and add
 *                                      "Cancel" button (per legacy).
 *                                      Add "hours" label to specify units for
 *                                      max time range; other minor reformats.
 * </pre>
 * 
 * @author 
 * @version 1
 */
public class CloudHeightOptionsDialog extends Dialog {
    private Shell shell;
    private String dlgTitle = null;
    private CloudHeightDialog cldHghtDlg = null;
    
    private Button stn_src_btn = null;
    private Button std_atm_src_btn = null;
    private Text stn_dist_txt = null;
    private Label stn_dist_lbl = null;
    private Combo comp_mthd_combo = null;
    
    private Button sngl_pix_btn = null;
    private Button pix_area_btn = null;
    private Text  pix_area_txt = null;
    private Combo pix_val_combo = null;
    private Label spinnerLabel = null;
    private Spinner spinner = null;
    private final int MAX_TIME_INTERVAL_FOR_VALID_STATION_DATA_IN_HOURS = 36;
    
    // Store the units from the main cloud height dialog here since these are part of the
    // defaults that may be reset.
    static private Unit<? extends Length> sndDistUnits = null;
    static private Unit<? extends Temperature> tempUnits = null;
    static private Unit<? extends Length> heightUnits = null;
 
    // this is the units for maxDistance and which is set by CloudHeightProcessor
    static private Unit<? extends Length> maxDistWorkingUnits = null;
    static private UnitConverter maxDistConverter = null; 
    static private UnitConverter maxDistInvConverter = null; 
    
    static private boolean firstTime = true;
    static private SoundingDataSourceType sndDataSrc = SoundingDataSourceType.STATION_DATA;
    static private double maxDistance = 0; 
    static private ComputationalMethod compMethod = ComputationalMethod.STANDARD;
    static private boolean useSinglePix = false;
    static private int  pixArea = 10; 
    static private PixelValueMethod pixValMethod = PixelValueMethod.MAX_VALUE;
    static private int  maxTimeIntervalForValidStationDataInHrs;     
    // enable/disable pix_area_txt & pix_val_combo based on single pixel or pixel area
    private class SoundingSourceListener extends SelectionAdapter {
    	public void widgetSelected(SelectionEvent e) {
    		stn_dist_txt.setEnabled( e.widget == stn_src_btn );
    		comp_mthd_combo.setEnabled( e.widget == stn_src_btn );
    		spinner.setEnabled( e.widget == stn_src_btn);
    	}
    }
    
    private class PixelSelectionListener extends SelectionAdapter {
    	public void widgetSelected(SelectionEvent e) {
    		pix_area_txt.setEnabled( e.widget == pix_area_btn );
    		pix_val_combo.setEnabled( e.widget == pix_area_btn );
    	}
    }
    	
    public CloudHeightOptionsDialog( Shell parShell, String title, CloudHeightDialog chdlg )  {
        super(parShell);
        dlgTitle = title;
        cldHghtDlg = chdlg;
        
        if( firstTime ) {
        	setDefaults();
        	firstTime = false;
        }	
    }
  
    public void createDialog( Composite parent ) {
        Composite top_form = parent; 
        top_form.setLayout( new FormLayout() );
        
        Group snding_src_grp = new Group( top_form, SWT.SHADOW_NONE );
        snding_src_grp.setText("Sounding Data Source");
        snding_src_grp.setLayout( new FormLayout() );
        FormData fd = new FormData( );
        fd.top   = new FormAttachment( 0, 15 );
        fd.left  = new FormAttachment( 0, 15 );
        snding_src_grp.setLayoutData( fd );

        std_atm_src_btn = new Button( snding_src_grp, SWT.RADIO );
        std_atm_src_btn.setText("Standard Atmosphere" );
        fd = new FormData( );
        fd.top   = new FormAttachment( 0, 15 ); // 
        fd.left  = new FormAttachment( 0, 20 ); //
        std_atm_src_btn.setLayoutData( fd );

        stn_src_btn = new Button( snding_src_grp, SWT.RADIO );
        stn_src_btn.setText("Station Data"); 
        fd = new FormData( );
        fd.top   = new FormAttachment( std_atm_src_btn, 5, SWT.BOTTOM );
        fd.left  = new FormAttachment( std_atm_src_btn, 0, SWT.LEFT );
        stn_src_btn.setLayoutData( fd );
        
        stn_src_btn.addSelectionListener( new SoundingSourceListener() );
        std_atm_src_btn.addSelectionListener( new SoundingSourceListener() );
        
        stn_dist_txt = new Text( snding_src_grp, SWT.SINGLE | SWT.BORDER );
        fd = new FormData( 60, 20 );
        fd.top   = new FormAttachment( stn_src_btn, 30, SWT.BOTTOM );
        fd.left  = new FormAttachment( 0, 20 );
        stn_dist_txt.setLayoutData( fd );

        stn_dist_lbl = new Label( snding_src_grp, SWT.NONE );
        stn_dist_lbl.setText("Max Station Distance ("+sndDistUnits.toString()+") ");
        fd = new FormData( );
        fd.bottom = new FormAttachment( stn_dist_txt, -4, SWT.TOP );
        fd.left  = new FormAttachment( stn_dist_txt, 0, SWT.LEFT );
        fd.right  = new FormAttachment( 100, -10 );
        stn_dist_lbl.setLayoutData( fd );

        spinnerLabel = new Label( snding_src_grp, SWT.NONE );
        spinnerLabel.setText("Max Time Range for Stn Data ");
        fd = new FormData( );        
        fd.left  = new FormAttachment( stn_dist_txt, 0, SWT.LEFT );
        fd.top   = new FormAttachment( stn_dist_txt, 10, SWT.BOTTOM );
        spinnerLabel.setLayoutData(fd);
        
        Label spinnerRangeLabel1 = new Label(snding_src_grp, SWT.NONE );
        spinnerRangeLabel1.setText(" +");
        fd = new FormData( );
        fd.top   = new FormAttachment( spinnerLabel, 2, SWT.BOTTOM );
        fd.left  = new FormAttachment( stn_dist_txt, 0, SWT.LEFT );
        spinnerRangeLabel1.setLayoutData(fd);
 
        Label spinnerRangeLabel2 = new Label(snding_src_grp, SWT.NONE );
        spinnerRangeLabel2.setText(" _");
        fd = new FormData( );
        fd.top   = new FormAttachment( spinnerRangeLabel1, -8, SWT.BOTTOM );
        fd.left  = new FormAttachment( spinnerLabel, 2, SWT.LEFT );
        spinnerRangeLabel2.setLayoutData(fd);        
        
        spinner = new Spinner(snding_src_grp,  SWT.BORDER );
        spinner.setDigits(0);
        spinner.setMinimum(1);
        spinner.setMaximum(MAX_TIME_INTERVAL_FOR_VALID_STATION_DATA_IN_HOURS);
        spinner.setIncrement(1);
        spinner.setSelection(maxTimeIntervalForValidStationDataInHrs);
        fd = new FormData( );
        fd.top   = new FormAttachment( spinnerLabel,  4, SWT.BOTTOM );
        fd.left  = new FormAttachment( spinnerLabel, 20, SWT.LEFT );
        spinner.setLayoutData(fd);
        
        spinner.addModifyListener(new ModifyListener() {
            @Override
			public void modifyText(ModifyEvent e) {
            	maxTimeIntervalForValidStationDataInHrs = ( (Spinner) (e.widget)).getSelection();
			}
		});
        
        Label spinnerUnitsLabel = new Label( snding_src_grp, SWT.NONE );
        spinnerUnitsLabel.setText("hours");
        fd = new FormData();
        fd.bottom = new FormAttachment( spinner, -4, SWT.BOTTOM );
        fd.left  = new FormAttachment( spinner, 10, SWT.RIGHT );
        spinnerUnitsLabel.setLayoutData( fd );
        
        comp_mthd_combo = new Combo( snding_src_grp, SWT.READ_ONLY );
        fd = new FormData( );
        fd.top   = new FormAttachment( spinner, 30, SWT.BOTTOM );
        fd.left  = new FormAttachment( stn_dist_txt, 0, SWT.LEFT );
        fd.bottom  = new FormAttachment( 100, -20 );
        comp_mthd_combo.setLayoutData( fd );

        // Just hard code these for now
        comp_mthd_combo.add("Standard");
        comp_mthd_combo.add("Moist-Adiabatic");

        Label cm_lbl = new Label( snding_src_grp, SWT.NONE );
        cm_lbl.setText("Computational Method");
        fd = new FormData( );
        fd.bottom = new FormAttachment( comp_mthd_combo, -2, SWT.TOP );
        fd.left  = new FormAttachment( comp_mthd_combo, 0, SWT.LEFT );
        cm_lbl.setLayoutData( fd );
        
        Group loc_sel_grp = new Group( top_form, SWT.SHADOW_NONE );
        loc_sel_grp.setText("Location Selection");
        loc_sel_grp.setLayout( new FormLayout() );
        fd = new FormData( );
        fd.top   = new FormAttachment( snding_src_grp, 0, SWT.TOP );
        fd.left  = new FormAttachment( snding_src_grp, 15, SWT.RIGHT );
        fd.right  = new FormAttachment( 100, -15 );
        loc_sel_grp.setLayoutData( fd );

        sngl_pix_btn = new Button( loc_sel_grp, SWT.RADIO );
        sngl_pix_btn.setText("Single Pixel");
        fd = new FormData( );
        fd.top   = new FormAttachment( 0, 15 );
        fd.left  = new FormAttachment( 0, 20 );
        sngl_pix_btn.setLayoutData( fd );

        pix_area_btn = new Button( loc_sel_grp, SWT.RADIO );
        pix_area_btn.setText("Pixel Area");
        fd = new FormData( );
        fd.top   = new FormAttachment( sngl_pix_btn, 5, SWT.BOTTOM );
        fd.left  = new FormAttachment( sngl_pix_btn, 0, SWT.LEFT );
        pix_area_btn.setLayoutData( fd );
        
        pix_area_txt = new Text( loc_sel_grp, SWT.SINGLE | SWT.BORDER );
        fd = new FormData( 50, 20 );
        fd.top   = new FormAttachment( pix_area_btn, 30, SWT.BOTTOM );
        fd.left  = new FormAttachment( 0, 20 );
        pix_area_txt.setLayoutData( fd );

        Label pa_lbl = new Label( loc_sel_grp, SWT.NONE );
        pa_lbl.setText("Pixel Area Radius");
        fd = new FormData( );
        fd.bottom = new FormAttachment( pix_area_txt, -2, SWT.TOP );
        fd.left  = new FormAttachment( pix_area_txt, 0, SWT.LEFT );
        pa_lbl.setLayoutData( fd );

        pix_val_combo = new Combo( loc_sel_grp, SWT.READ_ONLY );
        fd = new FormData( );
        fd.top   = new FormAttachment( pix_area_txt, 30, SWT.BOTTOM );
        fd.left  = new FormAttachment( pix_area_txt, 0, SWT.LEFT );
        fd.bottom  = new FormAttachment( 100, -20 );
        fd.right  = new FormAttachment( 100, -20 );
        pix_val_combo.setLayoutData( fd );

        pix_val_combo.add("Max Value");
        pix_val_combo.add("Most Frequent");
        
        // enable/disable dependent options
        sngl_pix_btn.addSelectionListener( new PixelSelectionListener() );
        pix_area_btn.addSelectionListener( new PixelSelectionListener() );

        Label pv_lbl = new Label( loc_sel_grp, SWT.NONE );
        pv_lbl.setText("Pixel Value Method");
        fd = new FormData( );
        fd.bottom = new FormAttachment( pix_val_combo, -2, SWT.TOP );
        fd.left  = new FormAttachment( pix_val_combo, 0, SWT.LEFT );
        pv_lbl.setLayoutData( fd );
        
        Label sep_lbl = new Label( top_form, SWT.SEPARATOR | SWT.HORIZONTAL );
        fd = new FormData();
        fd.top = new FormAttachment( snding_src_grp, 10, SWT.BOTTOM );
        fd.left  = new FormAttachment( 0, 5 );
        fd.right  = new FormAttachment( 100, -5 );
        fd.bottom  = new FormAttachment( 100, -45 );
        sep_lbl.setLayoutData( fd );

        Button ok_btn = new Button( top_form, SWT.PUSH );
        fd = new FormData();
        ok_btn.setText("   OK   ");
        fd.top = new FormAttachment( sep_lbl, 10, SWT.BOTTOM );
        fd.right  = new FormAttachment( 100, -20 );
        ok_btn.setLayoutData( fd );

        ok_btn.addSelectionListener(new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {
       			saveSeldOptions();	
       			shell.dispose();
       		}
        });

        Button dflts_btn = new Button( top_form, SWT.PUSH );
        fd = new FormData();
        dflts_btn.setText(" Reset Defaults ");
        fd.top = new FormAttachment( ok_btn, 0, SWT.TOP );
        fd.right  = new FormAttachment( ok_btn, -20, SWT.LEFT );
        dflts_btn.setLayoutData( fd );

        // how to pas the shell to the implicit SelectionListener?
        dflts_btn.addSelectionListener(new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {
       			setDefaults();
       			setWidgets();
       			cldHghtDlg.setUnitComboBoxes();
       	}});

        Button cancel_btn = new Button( top_form, SWT.PUSH );
        fd = new FormData();
        cancel_btn.setText("  Cancel  ");
        fd.top = new FormAttachment( sep_lbl, 10, SWT.BOTTOM );
        //fd.right  = new FormAttachment( 100, -20 );
        fd.right  = new FormAttachment( dflts_btn, -20, SWT.LEFT );
        cancel_btn.setLayoutData( fd );

        cancel_btn.addSelectionListener(new SelectionAdapter() {
       		public void widgetSelected( SelectionEvent ev ) {
       			// DON'T saveSeldOptions();	
       			shell.dispose();
       		}
        });
        
        setWidgets();
    }
   
    // TODO : get the default units from the system defaults on disk somewhere
	// and set the defaults and then reset the gui controls.
    private void setDefaults() {
	    sndDistUnits = NonSI.NAUTICAL_MILE;
	    tempUnits = SI.CELSIUS;
	    heightUnits = NonSI.FOOT;
		sndDataSrc = SoundingDataSourceType.STATION_DATA;
		maxDistance  = 540*1852.0; // 540 nm in meters
		compMethod = ComputationalMethod.STANDARD;
		useSinglePix = false;
		pixArea = 10; 
		pixValMethod = PixelValueMethod.MAX_VALUE;
		maxTimeIntervalForValidStationDataInHrs = MAX_TIME_INTERVAL_FOR_VALID_STATION_DATA_IN_HOURS;
		}
    
    // initialize all of the gui controls from the member variables
    //
    private void setWidgets() {
		
        std_atm_src_btn.setSelection( sndDataSrc == SoundingDataSourceType.STANDARD_ATM );
        stn_src_btn.setSelection( sndDataSrc == SoundingDataSourceType.STATION_DATA );
        
        // the Units are the same as for displaying the snding distance. This is controlled from the main Dialog 
        double mxDist = maxDistConverter.convert( maxDistance );
        stn_dist_txt.setText( Double.toString(mxDist) );
  		//String.format("%.2f",maxDistance);
        
        stn_dist_txt.setEnabled( sndDataSrc == SoundingDataSourceType.STATION_DATA );
        spinner.setEnabled( sndDataSrc == SoundingDataSourceType.STATION_DATA );
        stn_dist_lbl.setText("Max Station Distance ("+sndDistUnits.toString()+") ");

        if( compMethod == ComputationalMethod.STANDARD ) {
        	comp_mthd_combo.select( 0 );
        }
        else if( compMethod == ComputationalMethod.MOIST_ADIABATIC ) {
        	comp_mthd_combo.select( 1 );
        }

        comp_mthd_combo.setEnabled( sndDataSrc == SoundingDataSourceType.STATION_DATA );
                
        sngl_pix_btn.setSelection( useSinglePix );
        pix_area_btn.setSelection( !useSinglePix );
        
        pix_area_txt.setText( Integer.toString( pixArea) );
        
		pix_area_txt.setEnabled( !sngl_pix_btn.getSelection() );

        if( pixValMethod == PixelValueMethod.MAX_VALUE ) {
        	pix_val_combo.select( 0 );
        }
        else if( pixValMethod == PixelValueMethod.MOST_FREQUENT ) {
        	pix_val_combo.select( 1 );
        }

        pix_val_combo.setEnabled( !sngl_pix_btn.getSelection() );
    }
    
    public Object open() {
    	Shell parent = getParent();
    	Display display = parent.getDisplay();
    	
    	shell = new Shell( parent, SWT.DIALOG_TRIM | SWT.RESIZE | SWT.MODELESS );
    	shell.setText(dlgTitle);
    	shell.setSize( 500, 500 ); // pack later

    	GridLayout mainLayout = new GridLayout(1, true);
    	mainLayout.marginHeight = 1;
    	mainLayout.marginWidth = 1;
    	shell.setLayout(mainLayout);
    	shell.setLocation(0, 0);

    	createDialog( shell );
    	
    	//shell.setLocation( parShell.getLocation() );
    	//shell.setMinimumSize(100, 100);

    	shell.pack();
    	shell.open();

    	while( !shell.isDisposed() ) {
    		if( !display.readAndDispatch() ) {
    			display.sleep();
    		}
    	}

    	return null;
    }
    
    public boolean isOpen() {
        return shell != null && !shell.isDisposed();
    }

	private void saveSeldOptions() {
		
		// units are set from the main dialog so don't need to save them here.
		
    	sndDataSrc = (stn_src_btn.getSelection() ? SoundingDataSourceType.STATION_DATA :
                                                   SoundingDataSourceType.STANDARD_ATM );
    	double mxDist = Double.parseDouble( stn_dist_txt.getText() );
    	// TODO? : this is an odd bug? (misleading behaviour) that can happen if the user changes the Sounding Source
    	// Dist units on the main dialog before saving the max Distance option. The max dist units lable will not change
    	// but the displayed number will be interpreted in the new source distance units. 
    	
    	// convert from the display units to the units set by the cloudHeightProcesser which uses this value
    	maxDistance = maxDistInvConverter.convert( mxDist );
    	
    	if( comp_mthd_combo.getSelectionIndex() == 0 ) {
    		compMethod = ComputationalMethod.STANDARD;
    	}
    	else if ( comp_mthd_combo.getSelectionIndex() == 1 ) {
    		compMethod = ComputationalMethod.MOIST_ADIABATIC;
    	}

    	useSinglePix = sngl_pix_btn.getSelection();
    	pixArea = Integer.parseInt( pix_area_txt.getText() );
    	
    	if( pix_val_combo.getSelectionIndex() == 0 ) {
    	   	pixValMethod = PixelValueMethod.MAX_VALUE;
    	}
    	else if( pix_val_combo.getSelectionIndex() == 1 ) {
    	   	pixValMethod = PixelValueMethod.MOST_FREQUENT;
    	}
 	}
    
	// get methods for the main dialog to access the options.
    public SoundingDataSourceType getSoundingDataSourceType( ) {
    	return sndDataSrc;
    }

    // the units which maxDistance will be stored in
    public void setDistWorkingUnits( Unit<? extends Length> u ) {
    	maxDistWorkingUnits = u;
		maxDistConverter = maxDistWorkingUnits.getConverterTo( getSndDistUnits() );
		maxDistInvConverter = getSndDistUnits().getConverterTo( maxDistWorkingUnits );
    }
    
    // this value is always stored in the working units set by the CloudHeightProcessor
    // 
    public double getMaxSoundingDist( ) {
    	return maxDistance;
    }
    
    public ComputationalMethod getComputationalMethod( ) {
    	return compMethod;
    }

    public PixelValueMethod getPixelValueMethod( ) {
    	return pixValMethod;
    }
    
    public Unit<? extends Length> getSndDistUnits() {
    	return sndDistUnits;
    }
    
    public Unit<? extends Temperature> getTemperatureUnits() {
    	return tempUnits;
    }
    
    public Unit<? extends Length> getCloudHeightUnits() {
    	return heightUnits;
    }
    
    // set methods for the units since these are set from the main dialog
    // Since the distance units are also used as the units for entering the maxDistance
    // we need to update the converter used when setting the maxDistance.
    public void setSndDistUnits( Unit<? extends Length> u ) {
    	sndDistUnits = u;
		maxDistConverter = maxDistWorkingUnits.getConverterTo( sndDistUnits );
		maxDistInvConverter = sndDistUnits.getConverterTo( maxDistWorkingUnits );
    }
    
    public void setTemperatureUnits( Unit<? extends Temperature> u ) {
    	tempUnits = u;
    }

    public void setCloudHeightUnits( Unit<? extends Length> u ) {
    	heightUnits = u;
    }

     protected static int getMaxIntervalForStationDataTime(){
    	 return maxTimeIntervalForValidStationDataInHrs;
     }
    
     protected static boolean isPixelValueFromSinglePixel(){
    	 return useSinglePix;
     }

	public int getPixelArea() {
		return pixArea;
		
	}
}
