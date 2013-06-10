/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.TrackAttrDlg
 * 
 * 111 May 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import java.util.ArrayList;
import java.util.Calendar;
import java.awt.Color;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;

import gov.noaa.nws.ncep.ui.pgen.display.ITrack;
import gov.noaa.nws.ncep.ui.pgen.display.IText.FontStyle;
import gov.noaa.nws.ncep.ui.pgen.display.TrackPoint;
import gov.noaa.nws.ncep.ui.pgen.display.FillPatternList.FillPattern;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.elements.Track;
import gov.noaa.nws.ncep.viz.common.ui.color.ColorButtonSelector;

/**
 * Singleton attribute dialog for Track.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 05/09					M. Gao   	Initial Creation.
 * 08/09		149			B. Yin		Modified OkPressed to handle MultiSelect
 * 03/10        231         Archana     Altered the dialog for track  
 *                                      to display only a button showing  
 *                                      the selected color instead of displaying 
 *                                      the complete color matrix.
 * 04/11		#?			B. Yin		Re-factor IAttribute
 * 08/11		#? TTR58	B. Yin		Made the time factors editable.
 * 02/12        TTR456      Q.Zhou      Added speed units combo, roundTo combo and roundDirTo combo.
 * 06/12        #777        Q.Zhou      Modified DEFAULT_NUMBER_OF_TIMES. 
 * 										Added isNewTrack flag to make the FirstTime(/SecondTime) current time when create a Track
 * 03/13		#928		B. Yin 		Added a separator above the button bar.
 * </pre>
 * 
 * @author	M. Gao
 */

public class TrackAttrDlg extends AttrDlg implements ITrack{
	
//	private final static org.apache.log4j.Logger log = 
//		org.apache.log4j.Logger.getLogger(TrackAttrDlg.class);
	
	public static enum FontSizeName { TINY, SMALL, MEDIUM, LARGE, HUGE, GIANT };
	public static int[] FontSizeValue =  { 10, 12, 14, 18, 24, 34 };
	//public static String[] FontName = new String[]{ "Courier", "Helvetica", "Times" };
	public static  String[] FontName = new String[]{ "Courier", "Nimbus Sans L", "Liberation Serif" };

	public static String[] BoxName = new String[]{ "Normal", "Boxed", "Blanked", "Outline" };
		
	private final int DEFAULT_NUMBER_OF_TIMES = 5; 
	private final int DEFAULT_HOUR_SHIFT_FOR_FIRST_TIME = 4; 
	private final int DEFAULT_HOUR_SHIFT_BEYOND_FIRST_TIME = 1; 
	
	private static String[] IntervalTimeValues = {"00:15", "00:30", "01:00", "02:00", "06:00", 
		"12:00", "Other"}; 
	private String previousIntervalTimeValue = ""; 

	private static String[] UnitValues = {"kts", "kph", "mph"};
	private static String[] RoundTo = {" ", "5", "10"};
	private static String[] RoundDirTo = {" ", "1", "5"};
	
	private Text firstTimeText; 
	private Text secondTimeText; 
	private boolean setTimeButtonSelected; 
	private Button frameTimeButton; 
	private Button setTimeButton; 
	
	private Text numberOfTimesText; 
	private Text intervalText; 

	private Text skipFactorText; 
	private ExtraPointTimeDisplayOption extraPointTimeDisplayOption; 
	
	//	private Composite top = null;
    private ColorButtonSelector initialCS;
    private ColorButtonSelector extrapCS;
    
    private Text text = null;

    private Combo intervalCombo; 
	
	private Combo unitCombo;
	private int unitComboSelectedIndex;
	private Button roundButton;
	private Combo roundCombo;
	private int roundComboSelectedIndex;
	private Button roundDirButton;
	private Combo roundDirCombo;
	private int roundDirComboSelectedIndex;
	
	private Combo fontSizeCombo;
	private int fontSizeComboSelectedIndex; 
    private Combo fontNameCombo;
	private int fontNameComboSelectedIndex; 
    private Combo fontStyleCombo;
	private int fontStyleComboSelectedIndex; 

	private Button skipFactorButton; 
    private Button showFirstLastButton; 
    private Button onHourButton; 
    private Button onHalfHourButton; 
    
	private Calendar firstTimeCalendar; 
    private Calendar secondTimeCalendar; 
    
    private int numberOfTimes = DEFAULT_NUMBER_OF_TIMES; //set the default to 5
    
    TrackExtrapPointInfoDlg trackExtrapPointInfoDlg; 
    
	private static TrackAttrDlg INSTANCE = null;
	
	public boolean isNewTrack = false;
    
    /**
	 * Private constructor
	 * @param parShell
	 * @throws VizException
	 */
	private TrackAttrDlg(Shell parShell) throws VizException {
        super(parShell);
    }
	
	/**
	 * Creates a track attribute dialog if the dialog does not exist 
	 * and returns the instance. If the dialog exists, return the instance.
	 *  
	 * @param parShell
	 * @return
	 */
	public static TrackAttrDlg getInstance( Shell parShell){
		if ( INSTANCE == null ){
			try {
				INSTANCE = new TrackAttrDlg( parShell );
			} catch (VizException e) {
				e.printStackTrace();
			}	
		}
		return INSTANCE;
	} 
	
	public void okPressed() {
		
		ArrayList<AbstractDrawableComponent> adcList = null;
		ArrayList<AbstractDrawableComponent> newList = new ArrayList<AbstractDrawableComponent>() ;

		//get the list of selected tracks
		if ( drawingLayer != null ) {
			adcList = (ArrayList<AbstractDrawableComponent>) drawingLayer.getAllSelected();
		}
		
		if ( adcList != null && !adcList.isEmpty() ){
			
			Track newEl = null;
			//loop through the list and update attributes
			for ( AbstractDrawableComponent adc : adcList){

				Track el = (Track)adc.getPrimaryDE();	

				if ( el != null ){
					// Create a copy of the currently selected element
					newEl = (Track)el.copy();
					// Update the new Element with these current attributes
					newEl.update(this);
					/*
					 * populate the TrackExtrapPointInofDlg object
					 */
					populateTrackExtrapPointInfoDlgWithNewTrackData(getTrackExtrapPointInfoDlg(), newEl, 
							unitComboSelectedIndex, roundComboSelectedIndex, roundDirComboSelectedIndex); 
					
					newList.add(newEl);

				}
			}
			
			if ( newEl != null ){
				AttrSettings.getInstance().setSettings( newEl );
			}
			
			ArrayList<AbstractDrawableComponent> oldList = new ArrayList<AbstractDrawableComponent>(adcList);
			drawingLayer.replaceElements(oldList, newList);
		}
		
		// set the new elements as selected.
		drawingLayer.removeSelected();
		for ( AbstractDrawableComponent adc : newList ){
			drawingLayer.addSelected(adc);
		}
		
		if ( mapEditor != null ) {
			mapEditor.refresh();
		}
	}

	public void cancelPressed(){
		
		if(trackExtrapPointInfoDlg != null) {
			trackExtrapPointInfoDlg.close(); 
			trackExtrapPointInfoDlg = null; 
		}
		super.cancelPressed();
		
	}
	
	/**
	 * Creates the dialog area
	 */
	@Override
	public Control createDialogArea(Composite parent) {
		
			Composite top = (Composite) super.createDialogArea(parent);

	        // Create the main layout for the shell.
	        GridLayout mainLayout = new GridLayout(2, false);
	        mainLayout.marginHeight = 3;
	        mainLayout.marginWidth = 3;
	        top.setLayout(mainLayout);

	        // Initialize all of the menus, controls, and layouts
	        initializeComponents(top);

	        return top;
	        
	}
	
	public void initializeTrackAttrDlg(Track track) {
		if(track == null)
			return; 
		/*
		 * 1. restore frameTimeButton and setTimeButton status
		 */
		if(track.isSetTimeButtonSelected()) {
			getSetTimeButton().setSelection(true); 
			getFrameTimeButton().setSelection(false); 
		} else {
			getFrameTimeButton().setSelection(true); 
			getSetTimeButton().setSelection(false); 
		}
		/*
		 * 2. restore first time and second time text values
		 */
		if (isNewTrack == true) {
			String[] firstSecondTimeValueArray = getFirstSecondTimeInitialTimeValueForSetTimeButton(); 
			getFirstTimeText().setText(firstSecondTimeValueArray[0]);
			getSecondTimeText().setText(firstSecondTimeValueArray[1]);
		}
		else {
        
			getFirstTimeText().setText(getFirstOrSecondTimeStringValue(track.getFirstTimeCalendar(), true, track.getInitialPoints())); 
			getSecondTimeText().setText(getFirstOrSecondTimeStringValue(track.getSecondTimeCalendar(), false, track.getInitialPoints())); 
		}
		
		//isNewTrack = false;
		/*
		 * 3. restore Number of times value
		 */
		if(track.getExtrapPoints() != null)
			numberOfTimesText.setText(String.valueOf(track.getExtrapPoints().length));
		
		/*
		 * 4. restore interval time settings
		 */
//		intervalCombo.select(track.getIntervalComboSelectedIndex()); 		
//		int intervalComboItemCount = intervalCombo.getItemCount();		
//		if ( (intervalComboItemCount - 1) == track.getIntervalComboSelectedIndex() )
//			intervalText.setText(track.getIntervalTimeString()); 
		
		setIntervalTimeString( track.getIntervalTimeString() );
        		
//		restoreIntervalTimeSettingByTrack(this, track); 
		
		/*
		 * 5. restore initial and extrap colors
		 */
		java.awt.Color initColor = track.getInitialColor(); 
		initialCS.setColorValue(new RGB(initColor.getRed(), initColor.getGreen(), initColor.getBlue())); 
		java.awt.Color extrapColor = track.getExtrapColor(); 
		extrapCS.setColorValue(new RGB(extrapColor.getRed(), extrapColor.getGreen(), extrapColor.getBlue())); 
		
		/*
		 * 6. restore label option settings
		 */
		setExtraPointTimeDisplayOption(track.getExtraPointTimeDisplayOption()); 
		makeTimeDisplayOptionSelected(track.getExtraPointTimeDisplayOption(), track.getSkipFactorTextString()); 
		
		/*
		 * 7. restore Font, Size, Style combo values
		 */
		getFontNameCombo().select(track.getFontNameComboSelectedIndex()); 
		getFontSizeCombo().select(track.getFontSizeComboSelectedIndex()); 
		getFontStyleCombo().select(track.getFontStyleComboSelectedIndex()); 
		
		/*
		 * 7. restore Unit combo values
		 */
		unitComboSelectedIndex = track.getUnitComboSelectedIndex();
		getUnitCombo().select(unitComboSelectedIndex);
		
		roundComboSelectedIndex = track.getRoundComboSelectedIndex();
		getRoundCombo().select(roundComboSelectedIndex); 
		if (roundComboSelectedIndex >0)
			roundButton.setSelection(true);
		else
			roundButton.setSelection(false);
		
		roundDirComboSelectedIndex = track.getRoundDirComboSelectedIndex();
		getRoundDirCombo().select(roundDirComboSelectedIndex); 
		if (roundDirComboSelectedIndex >0)
			roundDirButton.setSelection(true);
		else
			roundDirButton.setSelection(false);
	}
	
	private void populateTrackExtrapPointInfoDlgWithNewTrackData(TrackExtrapPointInfoDlg trackExtrapPointInfoDlgObject, 
			Track newTrackObject, int unitComboSelectedIndex, int roundComboSelectedIndex, int roundDirComboSelectedIndex) {
		if(trackExtrapPointInfoDlgObject != null && newTrackObject != null) {
			trackExtrapPointInfoDlgObject.close();
			
			trackExtrapPointInfoDlgObject.setBlockOnOpen( false );
			trackExtrapPointInfoDlgObject.open();
	       	
	   		trackExtrapPointInfoDlgObject.setTrack(newTrackObject, unitComboSelectedIndex, roundComboSelectedIndex, roundDirComboSelectedIndex);

	   		trackExtrapPointInfoDlgObject.setBlockOnOpen( true );
		}
	}
	
//	private void restoreIntervalTimeSettingByTrack(TrackAttrDlg targetTrackAttrDlg, Track track) {
//		if(targetTrackAttrDlg == null || track == null)
//			return; 
//		
//		targetTrackAttrDlg.getIntervalCombo().select(track.getIntervalComboSelectedIndex()); 
//		int intervalComboItemCount = targetTrackAttrDlg.getIntervalCombo().getItemCount(); 
//		if((intervalComboItemCount - 1) == track.getIntervalComboSelectedIndex()) {
//			if(track.getIntervalTimeString() != null) 
//				targetTrackAttrDlg.getIntervalText().setText(track.getIntervalTimeString()); 
//		} else {
////			targetTrackAttrDlg.getIntervalText().setText(targetTrackAttrDlg.getIntervalCombo().getText()); 
//			track.setIntervalTimeString(targetTrackAttrDlg.getIntervalCombo().getText()); 
//		}
//	}
	
	private void makeTimeDisplayOptionSelected(ExtraPointTimeDisplayOption extraPointTimeDisplayOption, String skipFactorTextString) {
		getSkipFactorButton().setSelection(false); 
		skipFactorText.setText(""); 
		if(extraPointTimeDisplayOption == ITrack.ExtraPointTimeDisplayOption.ON_HALF_HOUR)
			getOnHalfHourButton().setSelection(true); 
		else if(extraPointTimeDisplayOption == ITrack.ExtraPointTimeDisplayOption.ON_ONE_HOUR)
			getOnHourButton().setSelection(true); 
		else if(extraPointTimeDisplayOption == ITrack.ExtraPointTimeDisplayOption.SHOW_FIRST_LAST)
			getShowFirstLastButton().setSelection(true); 
		else {
			getSkipFactorButton().setSelection(true); 
			skipFactorText.setText(skipFactorTextString); 
		}
	}
	
	/**
	 * Creates buttons, menus, and other controls in the dialog area
	 * @param listener 
	 */
	private void initializeComponents(Composite topComposite) {
		isNewTrack = true;
		
	    int textWidth = 120; //160;      
	    int textHeight = 15; //40;    

        GridLayout childGridLayout = new GridLayout( 2, false );

        this.getShell().setText("Track Attributes");
//        log.info("===now it is inside initializeComponents(...)"); 

        /**
         * Draw Frame/Set time buttons 
         */
        Group timeRadioButtonGroup = new Group(topComposite, SWT.NONE);
        timeRadioButtonGroup.setLayout(childGridLayout);
        
        frameTimeButton  = new Button(timeRadioButtonGroup, SWT.RADIO);
//        Button frameTimeButton  = new Button(topComposite, SWT.RADIO);
        frameTimeButton.setText("Frame time");
        
//        Button setTimeButton = new Button(topComposite, SWT.RADIO);
        setTimeButton = new Button(timeRadioButtonGroup, SWT.RADIO);
        setTimeButton.setText( "Set Time" );     
//        setTimeButton.setSelection(true);
//        timeRadioButtonGroup.h
        Label emptyLabel = new Label( topComposite, SWT.LEFT );
        emptyLabel.setText("   ");

        /*
         * Draw First and Second Time text rows
         */
        String[] firstSecondTimeValueArray = getFirstSecondTimeInitialTimeValueForSetTimeButton(); 
        setFirstTimeText(createTextfieldWithLabel(topComposite, "First time:", 
        		SWT.SINGLE | SWT.BORDER, textWidth, textHeight, true));         
        getFirstTimeText().setText(firstSecondTimeValueArray[0]);
        getFirstTimeText().addModifyListener( new ModifyListener(){
        	public void modifyText(ModifyEvent e) {
        		Text txt = (Text)e.widget;
        		Calendar cal = gempakTM2Calendar(txt.getText());
        		if ( cal != null ) firstTimeCalendar = cal;
        		
        	}
        });

        
        setSecondTimeText(createTextfieldWithLabel(topComposite, "Second time:", 
        		SWT.SINGLE | SWT.BORDER, textWidth, textHeight, true));         
        getSecondTimeText().setText(firstSecondTimeValueArray[1]);
        
        getSecondTimeText().addModifyListener( new ModifyListener(){
        	public void modifyText(ModifyEvent e) {
        		Text txt = (Text)e.widget;
        		Calendar cal = gempakTM2Calendar(txt.getText());
        		if ( cal != null ) secondTimeCalendar = cal;
        		
        	}
        });
        
        setTimeButton.setSelection(true);
        setSetTimeButtonSelected(true); 
        frameAndSetTimeButtonSelectionListenerAction(frameTimeButton, getFirstTimeText(), 
        		getSecondTimeText()); 
        frameAndSetTimeButtonSelectionListenerAction(setTimeButton, getFirstTimeText(), 
        		getSecondTimeText()); 
        
        /*
         * Played with NMAP2, it seems directly change first/second time texts do not have any 
         * impact on the first/second starting points. Thus, now set both text fields are not 
         * editable 
         */
        getFirstTimeText().setEditable(true); 
        getSecondTimeText().setEditable(true); 
        
        /*
         * Draw Number of times text row
         */
        setNumberOfTimesText(createTextfieldWithLabel(topComposite, "Number of times:", 
        		SWT.SINGLE | SWT.BORDER, textWidth/3, textHeight, true));    
        numberOfTimesText.setText(String.valueOf(numberOfTimes));
        numberOfTimesText.addModifyListener(new ModifyListener() {
        	public void modifyText(ModifyEvent e) {
        		try {
        			numberOfTimes = Integer.parseInt(numberOfTimesText.getText()); 
        		} catch(NumberFormatException nfe) {
        			numberOfTimes = DEFAULT_NUMBER_OF_TIMES; //use the default value
        		//	log.error("The text value of number of times is invalid, input text="
        		//			+getNumberOfTimesText().getText()); 
        		}
        	}
        }); 
        
        /*
         * Draw Interval combo box row
         */
        Label intervalLabel = new Label(topComposite, SWT.LEFT); 
        intervalLabel.setText("Interval:"); 

        Group intervalRowGroup = new Group(topComposite, SWT.NONE);
        intervalRowGroup.setLayout(childGridLayout);
        
        intervalCombo = new Combo(intervalRowGroup, SWT.DROP_DOWN | SWT.READ_ONLY); 
        for ( String currentString : IntervalTimeValues ) {
        	intervalCombo.add(currentString);
        }
        intervalCombo.select(2);  //set default to 01:00
        setPreviousIntervalTimeValue(intervalCombo.getText()); 
        intervalText = new Text(intervalRowGroup, SWT.SINGLE | SWT.BORDER);                        
        intervalCombo.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				String lastItemString = intervalCombo.getItem(intervalCombo.getItemCount()-1); 
				if(intervalCombo.getText().equals(lastItemString)) {
					intervalText.setEditable(true); 
					intervalText.setText(getPreviousIntervalTimeValue());
					setIntervalTimeString(getPreviousIntervalTimeValue()); 
				} else {
					intervalText.setEditable(false);
					intervalText.setText(""); 
					setPreviousIntervalTimeValue(intervalCombo.getText()); 
					setIntervalTimeString(intervalCombo.getText()); 
				}
			}
        }); 

        intervalText.addModifyListener(new ModifyListener() {
        	public void modifyText(ModifyEvent e) {
        		if ( !((Text)e.widget).getText().isEmpty())
        			setIntervalTimeString(intervalText.getText()); 
        	}
        }); 
        
        /*
         * Draw two color choice options
         */
        Label colorLabel = new Label(topComposite, SWT.LEFT); 
        colorLabel.setText("Initial Color:"); 
        initialCS = new ColorButtonSelector ( topComposite );
        initialCS.setColorValue( new RGB( 0,0,255 ) ); // Blue???
        
        colorLabel = new Label(topComposite, SWT.LEFT); 
        colorLabel.setText("Extra Color:"); 
        extrapCS = new ColorButtonSelector( topComposite );
        extrapCS.setColorValue( new RGB( 0,192,0 ) ); // Green???
        
        /*
         * Draw speed unit selection  label, checkbox and combo
         */
        Label speedUnitLabel = new Label(topComposite, SWT.LEFT); 
        speedUnitLabel.setText("Speed Unit:"); 
       
        unitCombo = new Combo(topComposite, SWT.DROP_DOWN | SWT.READ_ONLY); 
        for (String unit : UnitValues) {
        	unitCombo.add(unit);
        }
        
        unitCombo.select(0);  // default to the 1st item of the list. the value is Courier
        setUnitComboSelectedIndex(0); 
        unitCombo.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				setUnitComboSelectedIndex(unitCombo.getSelectionIndex()); 
			}
        });

        /*
        * Draw speed Round To selection
        */
        roundButton  = new Button(topComposite, SWT.CHECK);
        String roundTo = "Round speed To:"; 
        roundButton.setText(roundTo); 
        roundButton.setSelection(false); 
        roundButton.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				if (!roundButton.getSelection())
					setRoundComboSelectedIndex(-1); 
			}
        });
        
        roundCombo = new Combo(topComposite, SWT.DROP_DOWN | SWT.READ_ONLY); 
        for (String round : RoundTo) {
        	roundCombo.add(round);
        }       
        roundCombo.select(0);  
        setRoundComboSelectedIndex(0);
        roundCombo.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				if (roundButton.getSelection())
					setRoundComboSelectedIndex(roundCombo.getSelectionIndex()); 
				else
					setRoundComboSelectedIndex(-1);
			}
        });
       
        /*
         * Draw Direction Round To selection
         */
         roundDirButton  = new Button(topComposite, SWT.CHECK);
         String roundDirTo = "Round Direction To:"; 
         roundDirButton.setText(roundDirTo); 
         roundDirButton.setSelection(false); 
         roundDirButton.addSelectionListener(new SelectionAdapter() {
 			public void widgetSelected(SelectionEvent e) {
 				if (!roundDirButton.getSelection())
 					setRoundDirComboSelectedIndex(-1); 
 			}
         });
         
         roundDirCombo = new Combo(topComposite, SWT.DROP_DOWN | SWT.READ_ONLY); 
         for (String round : RoundDirTo) {
         	roundDirCombo.add(round);
         }       
         roundDirCombo.select(0);  
         setRoundDirComboSelectedIndex(0);
         roundDirCombo.addSelectionListener(new SelectionAdapter() {
 			public void widgetSelected(SelectionEvent e) {
 				if (roundDirButton.getSelection())
 					setRoundDirComboSelectedIndex(roundDirCombo.getSelectionIndex()); 
 				else
 					setRoundDirComboSelectedIndex(-1);
 			}
         });
         
        /*
         * Draw a single line Label
         */
        Label optionLabel = new Label(topComposite, SWT.LEFT); 
        optionLabel.setText("Label Options:"); 
        //make the label to fill the two columns
        GridData gridData = new GridData(); 
        gridData.horizontalSpan = 2; 
        optionLabel.setLayoutData(gridData); 

        /*
         * Draw the four Buttons of Label Options
         */
        skipFactorButton  = new Button(topComposite, SWT.RADIO);
        String skipFactorButtonText = "Skip factor"; 
        skipFactorButton.setText(skipFactorButtonText); 
        skipFactorButton.setSelection(true); 
        skipFactorText = new Text(topComposite, SWT.SINGLE | SWT.BORDER);    
        skipFactorText.setLayoutData( new GridData( textWidth/4, textHeight ) );

        skipFactorText.setText("0"); 
        labelOptionButtonSelectionListenerAction(skipFactorButton, skipFactorText, true, 
        		skipFactorButtonText, 0); 
        
        showFirstLastButton = createButton(topComposite, "Show first&&last", true, 2);  
        labelOptionButtonSelectionListenerAction(showFirstLastButton, skipFactorText, false, 
				skipFactorButtonText, 0); 

        onHourButton = createButton(topComposite, "On hour", true, 2);  
        labelOptionButtonSelectionListenerAction(onHourButton, skipFactorText, false, 
        		skipFactorButtonText, 0); 

        onHalfHourButton = createButton(topComposite, "On half-hour", true, 2);  
        labelOptionButtonSelectionListenerAction(onHalfHourButton, skipFactorText, false, 
        		skipFactorButtonText, 0); 

        /*
         * Initialize the extra point display option as SKIP_FACTOR
         */
		setExtraPointTimeDisplayOption(ITrack.ExtraPointTimeDisplayOption.SKIP_FACTOR); 

        /*
         * Draw some Combo drop down lists
         */
        Label fontLabel = new Label(topComposite, SWT.LEFT); 
        fontLabel.setText("Font:"); 
        fontNameCombo = new Combo(topComposite, SWT.DROP_DOWN | SWT.READ_ONLY); 
        for (String fontName : FontName) {
        	fontNameCombo.add(fontName);
        }
        fontNameCombo.select(0);  // default to the 1st item of the list. the value is Courier
        setFontNameComboSelectedIndex(0); 
        fontNameCombo.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				setFontNameComboSelectedIndex(fontNameCombo.getSelectionIndex()); 
			}
        }); 
        
        Label fontSizeLabel = new Label(topComposite, SWT.LEFT); 
        fontSizeLabel.setText("Size:"); 
        fontSizeCombo = new Combo(topComposite, SWT.DROP_DOWN | SWT.READ_ONLY); 
        for ( FontSizeName fontSizeName : FontSizeName.values() ) {
        	fontSizeCombo.add(fontSizeName.name());
        }
        fontSizeCombo.select(2);  // default to the 3rd item of the list. The value is Medium
        setFontSizeComboSelectedIndex(2); 
        fontSizeCombo.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				setFontSizeComboSelectedIndex(fontSizeCombo.getSelectionIndex()); 
			}
        }); 
     
        Label fontStyleLabel = new Label(topComposite, SWT.LEFT); 
        fontStyleLabel.setText("Style:"); 
        fontStyleCombo = new Combo(topComposite, SWT.DROP_DOWN | SWT.READ_ONLY); 
        for ( FontStyle fontStyle : FontStyle.values() ) {
        	fontStyleCombo.add(fontStyle.name());
        }
        fontStyleCombo.select(2);  // default to the 3rd item of the list. The value is Bold
        setFontStyleComboSelectedIndex(2); 
        fontStyleCombo.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				setFontStyleComboSelectedIndex(fontStyleCombo.getSelectionIndex()); 
			}
        }); 
        
        addSeparator(topComposite.getParent());

	}	
	
	private void frameAndSetTimeButtonSelectionListenerAction(final Button button, 
			final Text firstTimeText, final Text secondTimeText) {  
		button.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				String[] firstAndSecondTimeValueArray = null; 
				String buttonText = button.getText(); 
				if(buttonText.indexOf("Frame") < 0) {
					firstAndSecondTimeValueArray = getFirstSecondTimeInitialTimeValueForSetTimeButton(); 
					setSetTimeButtonSelected(true); 
				} else {
					firstAndSecondTimeValueArray = getFirstSecondTimeInitialTimeValueForFrameTimeButton(); 
					setSetTimeButtonSelected(false); 
				}
				firstTimeText.setText(firstAndSecondTimeValueArray[0]); 
				secondTimeText.setText(firstAndSecondTimeValueArray[1]); 
			}
		}); 
	}
	
	private String[] getFirstSecondTimeInitialTimeValueForFrameTimeButton() {
		return getFirstSecondTimeInitialTimeValueForSetTimeButton(); 
	}
	
	private String[] getFirstSecondTimeInitialTimeValueForSetTimeButton() {
		String[] timeValueResult = new String[2]; 
		Calendar calendar = Calendar.getInstance(); 
		
		calendar.add(Calendar.HOUR_OF_DAY, DEFAULT_HOUR_SHIFT_FOR_FIRST_TIME); 
		setFirstTimeCalendar(calendar); 
		timeValueResult[0] = getDateTimeStringValue(calendar); 
		calendar.add(Calendar.HOUR_OF_DAY, DEFAULT_HOUR_SHIFT_BEYOND_FIRST_TIME); 
		setSecondTimeCalendar(calendar); 
		timeValueResult[1] = getDateTimeStringValue(calendar); 
		
//		Calendar calendar = Calendar.getInstance(TimeZone.getTimeZone("GMT")); 		
//		//calendar.add(Calendar.HOUR_OF_DAY, DEFAULT_HOUR_SHIFT_FOR_FIRST_TIME); 
//		setFirstTimeCalendar(calendar); 
//		timeValueResult[0] = getDateTimeStringValue(calendar); 
//		
//		String intervalStr = getPreviousIntervalTimeValue(); //intervalTimeValue;
//		System.out.println("intervalStr "+intervalStr);
//		
//		if (intervalStr == null || intervalStr.equalsIgnoreCase(""))
//			intervalStr = "1:00";
//		String[] intervalString = intervalStr.split(":");
//		
//		int day = 0;
//		int hr = 0;
//		int min = 0;
//		if (intervalString.length == 1){
//			try {
//				hr = Integer.parseInt(intervalString[0]);
//			}
//			catch (NumberFormatException e) {				
//			}
//		}
//		else if (intervalString.length == 2){
//			try {
//				hr = Integer.parseInt(intervalString[0]);
//				min = Integer.parseInt(intervalString[1]);
//			}
//			catch (NumberFormatException e) {				
//			}
//		}
//		
//		calendar.add(Calendar.HOUR_OF_DAY, hr); 
//		calendar.add(Calendar.MINUTE, min); 
//		setSecondTimeCalendar(calendar); 
//		timeValueResult[1] = getDateTimeStringValue(calendar); 
		
		return timeValueResult; 
	}

	private String getFirstOrSecondTimeStringValue(Calendar timeCalendar, boolean isFirstTimeCalendar, TrackPoint[] initTrackPoints) {
		String timeStringValue = ""; 
		if(timeCalendar != null) {
			timeStringValue = getDateTimeStringValue(timeCalendar); 
		} else {
			if(initTrackPoints != null && initTrackPoints.length >= 2) {
				 int trackPointArrayIndex = initTrackPoints.length - 1; 
				 if(isFirstTimeCalendar)
					 trackPointArrayIndex--; 
				 timeStringValue = getInitialPointsTimeStringValue(initTrackPoints, trackPointArrayIndex); 
			}
		}
		return timeStringValue; 
	}
	
	private String getInitialPointsTimeStringValue(TrackPoint[] trackPointArray, int pointArrayIndex) {
		String timeStringValue = ""; 
		if(pointArrayIndex < trackPointArray.length) {
			TrackPoint targetTrackPoint = trackPointArray[pointArrayIndex]; 
			if(targetTrackPoint != null && targetTrackPoint.getTime() != null)
				timeStringValue = getDateTimeStringValue(targetTrackPoint.getTime()); 
		}
		return timeStringValue; 
	}
	
	
	private String getDateTimeStringValue(Calendar calendar) {
		StringBuilder stringBuilder = new StringBuilder(11); 
		int year = calendar.get(Calendar.YEAR); 
		int month = calendar.get(Calendar.MONTH); 
		int day = calendar.get(Calendar.DAY_OF_MONTH); 
		int hour = calendar.get(Calendar.HOUR_OF_DAY); 
		int minute = calendar.get(Calendar.MINUTE); 
		
		String yearString = String.valueOf(year); 
		stringBuilder.append(yearString.substring(2)); 
		if((month+1) < 10)
			stringBuilder.append(0); 
		stringBuilder.append((month+1)); 
		if(day < 10)
			stringBuilder.append(0); 
		stringBuilder.append(day); 
		stringBuilder.append("/"); 
		if(hour < 10)
			stringBuilder.append(0); 
		stringBuilder.append(hour); 
		if(minute < 10)
			stringBuilder.append(0); 
		stringBuilder.append(minute); 
		
		return stringBuilder.toString(); 
	}
	
//	private Calendar getCalendarByParsingString(String dateString, String dateFormatPattern) {
//		SimpleDateFormat simpleDateFormat = new SimpleDateFormat(dateFormatPattern);  //a pattern is something like "yyMMdd/HHmm"
//		Calendar cal = null; 
//		if(dateString == null)
//			return cal; 
//		try {
//			Date date = simpleDateFormat.parse(dateString); 
//			cal = Calendar.getInstance(); 
//			cal.setTime(date); 
//		} catch(ParseException pe) {
//			log.error("The input of dateString is invalid, parse fails, dateString="+dateString); 
//		}
//		return cal; 
//	}
	
	
	/**
	 * a helper method to create an editable text with a text label
	 * @parentComposite, a parent Composite the text and label are built on
	 * @textLabel, the value of the labe ltext
	 * @textStyle, text style value
	 * @textWidth, text width
	 * @textHeight, text height
	 * @isEditable, a boolean to indicate if the text is editable 
	 * @return Text
	 */
	private Text createTextfieldWithLabel(Composite parentComposite, String textLabel, 
			int textStyle, int textWidth, int textHeight, boolean isEditable) {
        Label firstTimeLabel = new Label(parentComposite, SWT.NONE);
        firstTimeLabel.setText(textLabel);
        
        Text text = new Text(parentComposite, textStyle);                        
        text.setLayoutData( new GridData( textWidth, textHeight ) );
        text.setEditable(isEditable);   
        return text; 
 	}
	
	/**
	 * a helper method to create a button
	 * @parentComposite, a parent Composite the button is built on
	 * @buttonText, text value associated with the button
	 * @isHorizontalSpan, a boolean to indicate if the button should do a span
	 * @spanValue, this value decides how many columns are spaned 
	 * @return Button
	 */
	private Button createButton(Composite parentComposite, String buttonText, 
			boolean isHorizontalSpan, int spanValue) {
        Button button  = new Button(parentComposite, SWT.RADIO);
        button.setText(buttonText); 
        /*
         * check to see if the button needs to do horizontal span
         */
        if(isHorizontalSpan) {
        	GridData gridData = new GridData(); 
            gridData.horizontalSpan = spanValue; 
            button.setLayoutData(gridData); 
        }
        return button; 
	}
	
	private void labelOptionButtonSelectionListenerAction(final Button button, 
			final Text targetText, final boolean isTargetTextEditable, 
			final String skipFactorButtonText, final int defaultSkipFactorValue) {
		button.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				targetText.setEditable(isTargetTextEditable); 
				if(isTargetTextEditable)
					targetText.setText(String.valueOf(defaultSkipFactorValue));
				else
					targetText.setText(""); 
				String buttonTextString = button.getText(); 
				ExtraPointTimeDisplayOption extraPointTimeDisplayOption = 
					decideExtraPointTimeDisplayOptionByButtonText(buttonTextString); 
				setExtraPointTimeDisplayOption(extraPointTimeDisplayOption); 
			}
		}); 
	}
	// "Skip factor"  "Show first&&last"  "On hour"   "On half-hour"
	private ExtraPointTimeDisplayOption decideExtraPointTimeDisplayOptionByButtonText(String buttonTextString) {
//		ITrack.ExtraPointTimeDisplayOption[] testOptions = ITrack.ExtraPointTimeDisplayOption.values(); 
		
		if(buttonTextString.equalsIgnoreCase("Show first&&last"))
			return ITrack.ExtraPointTimeDisplayOption.SHOW_FIRST_LAST;  
		else if(buttonTextString.equalsIgnoreCase("On hour"))
			return ITrack.ExtraPointTimeDisplayOption.ON_ONE_HOUR; 
		else if(buttonTextString.equalsIgnoreCase("On half-hour"))
			return ITrack.ExtraPointTimeDisplayOption.ON_HALF_HOUR; 
		else
			return ITrack.ExtraPointTimeDisplayOption.SKIP_FACTOR; 
			
	}
	
	/**
	 * Gets the text
	 */
	public String[] getString(){

		return text.getText().split( "\n" );	  		

	}

	/**
	 * Return font size from the font size combo
	 */	
	public float getFontSize(){
		return ( FontSizeValue[ fontSizeCombo.getSelectionIndex() ] );
	}

	/**
	 * Return font name from the font combo
	 */	
	public String getFontName(){
		return FontName[fontNameCombo.getSelectionIndex()];
	}
	
	/**
	 * Return font style from the style combo
	 */	
	public FontStyle getStyle(){
		return FontStyle.values()[ fontStyleCombo.getSelectionIndex() ];
	}
	
	/**
	 * Set font size
	 */	
	public void setFontSize( float size ){
		
		int index = 0;
		for ( int ii = 0; ii < FontSizeValue.length; ii++ ) {
			if ( (int)size == FontSizeValue[ ii ] ) {
				index = ii;
				break;
			}
		}

		fontSizeCombo.select( index );

	}

	/**
	 * Set font name
	 */	
	public void setFontName( String name ){
		for ( String st:FontName ) {
			if ( st.equalsIgnoreCase( name ) ) {
				fontNameCombo.setText( st );
				break;
			}
		}		
	}
	
	/**
	 * set font style
	 */	
	public void setStyle( FontStyle style ){
        for ( FontStyle fs : FontStyle.values() ) {
            if ( fs == style ) {
            	fontStyleCombo.setText( fs.name() );
            	break;
            }
        }
	}
	
	/**
	 * Sets values of all attributes of the dialog.
	 */
	public void setAttrForDlg( IAttribute attr ){		
	}

	@Override
	public void setAttr(AbstractDrawableComponent adc){
		if ( adc instanceof Track){
			initializeTrackAttrDlg((Track)adc);
		}
	}
	
	/*
	 * All setters and getters start here
	 */
    public TrackExtrapPointInfoDlg getTrackExtrapPointInfoDlg() {
		return trackExtrapPointInfoDlg;
	}

	public void setTrackExtrapPointInfoDlg(
			TrackExtrapPointInfoDlg trackExtrapPointInfoDlg) {
		this.trackExtrapPointInfoDlg = trackExtrapPointInfoDlg;
	}

    public Button getSkipFactorButton() {
		return skipFactorButton;
	}

	public Button getShowFirstLastButton() {
		return showFirstLastButton;
	}

	public Button getOnHourButton() {
		return onHourButton;
	}

	public Button getOnHalfHourButton() {
		return onHalfHourButton;
	}

	public Combo getFontSizeCombo() {
		return fontSizeCombo;
	}

	public Combo getFontNameCombo() {
		return fontNameCombo;
	}

	public Combo getFontStyleCombo() {
		return fontStyleCombo;
	}


	public int getFontSizeComboSelectedIndex() {
		return fontSizeComboSelectedIndex;
	}

	public void setFontSizeComboSelectedIndex(int fontSizeComboSelectedIndex) {
		this.fontSizeComboSelectedIndex = fontSizeComboSelectedIndex;
	}

	public int getFontNameComboSelectedIndex() {
		return fontNameComboSelectedIndex;
	}

	public void setFontNameComboSelectedIndex(int fontNameComboSelectedIndex) {
		this.fontNameComboSelectedIndex = fontNameComboSelectedIndex;
	}

	public int getFontStyleComboSelectedIndex() {
		return fontStyleComboSelectedIndex;
	}

	public void setFontStyleComboSelectedIndex(int fontStyleComboSelectedIndex) {
		this.fontStyleComboSelectedIndex = fontStyleComboSelectedIndex;
	}

	public Combo getUnitCombo() {
		return unitCombo;
	}
	
	public int getUnitComboSelectedIndex() {
		return unitComboSelectedIndex;
	}

	public void setUnitComboSelectedIndex(int unitComboSelectedIndex) {
		this.unitComboSelectedIndex = unitComboSelectedIndex;
	}

	public Combo getRoundCombo() {
		return roundCombo;
	}
	
	public int getRoundComboSelectedIndex() {
		return roundComboSelectedIndex;
	}

	public void setRoundComboSelectedIndex(int roundComboSelectedIndex) {
		this.roundComboSelectedIndex = roundComboSelectedIndex;
	}

	public Combo getRoundDirCombo() {
		return roundDirCombo;
	}
	
	public int getRoundDirComboSelectedIndex() {
		return roundDirComboSelectedIndex;
	}

	public void setRoundDirComboSelectedIndex(int roundDirComboSelectedIndex) {
		this.roundDirComboSelectedIndex = roundDirComboSelectedIndex;
	}


	public boolean isSetTimeButtonSelected() {
		return setTimeButtonSelected;
	}

	public void setSetTimeButtonSelected(boolean setTimeButtonSelected) {
		this.setTimeButtonSelected = setTimeButtonSelected;
	}
	public Button getFrameTimeButton() {
		return frameTimeButton;
	}

	public void setFrameTimeButton(Button frameTimeButton) {
		this.frameTimeButton = frameTimeButton;
	}

	public Button getSetTimeButton() {
		return setTimeButton;
	}

	public void setSetTimeButton(Button setTimeButton) {
		this.setTimeButton = setTimeButton;
	}


	public ExtraPointTimeDisplayOption getExtraPointTimeDisplayOption() {
		return extraPointTimeDisplayOption;
	}

	public void setExtraPointTimeDisplayOption(ExtraPointTimeDisplayOption extraPointTimeDisplayOption) {
		this.extraPointTimeDisplayOption = extraPointTimeDisplayOption;
	}

	public String getSkipFactorText() {
		return skipFactorText.getText();
	}

	public void setSkipFactorText(String skipFactorText) {
		this.skipFactorText.setText(skipFactorText);
	}

	public Text getFirstTimeText() {
		return firstTimeText;
	}

	public void setFirstTimeText(Text firstTimeText) {
		this.firstTimeText = firstTimeText;
	}

	public Text getSecondTimeText() {
		return secondTimeText;
	}

	public void setSecondTimeText(Text secondTimeText) {
		this.secondTimeText = secondTimeText;
	}

	public int getExtraDrawingPointNumber() {
		int ret = 0;
		try {
			ret = Integer.parseInt(numberOfTimesText.getText());
		}
		catch ( Exception e ){
			ret = 0;
		}
		
		return ret ;
	}

	public void setNumberOfTimesText(Text numberOfTimesText) {
		this.numberOfTimesText = numberOfTimesText;
	}

	public String getPreviousIntervalTimeValue() {
		return previousIntervalTimeValue;
	}

	public void setPreviousIntervalTimeValue(String previousIntervalTimeValue) {
		this.previousIntervalTimeValue = previousIntervalTimeValue;
	}

    public Calendar getFirstTimeCalendar() {
		return firstTimeCalendar;
	}

	public String getIntervalTimeString() {
		if(intervalCombo.getSelectionIndex() == intervalCombo.getItemCount() - 1) {
			return intervalText.getText(); 
		}
		else 
			return intervalCombo.getText(); 
	}

	public void setIntervalTimeString(String intervalTimeString) {
		for ( int ii = 0; ii < intervalCombo.getItemCount(); ii++ ){
			if (intervalTimeString.equalsIgnoreCase( intervalCombo.getItem(ii) )){
				intervalCombo.select(ii);
				return;
			}
		}
		
		//set for 'Other'
		intervalCombo.select( intervalCombo.getItemCount() - 1 );
		intervalText.setText(intervalTimeString);

	}
    
	public void setFirstTimeCalendar(Calendar firstTimeCalendar) {
		Calendar cal = Calendar.getInstance(); 
		cal.set(Calendar.YEAR, firstTimeCalendar.get(Calendar.YEAR)); 
		cal.set(Calendar.MONTH, firstTimeCalendar.get(Calendar.MONTH)); 
		cal.set(Calendar.DAY_OF_MONTH, firstTimeCalendar.get(Calendar.DAY_OF_MONTH)); 
		cal.set(Calendar.HOUR_OF_DAY, firstTimeCalendar.get(Calendar.HOUR_OF_DAY)); 
		cal.set(Calendar.MINUTE, firstTimeCalendar.get(Calendar.MINUTE)); 
		this.firstTimeCalendar = cal;
	}

	public Calendar getSecondTimeCalendar() {
		return secondTimeCalendar;
	}

	public void setSecondTimeCalendar(Calendar secondTimeCalendar) {
		Calendar cal = Calendar.getInstance(); 
		cal.set(Calendar.YEAR, secondTimeCalendar.get(Calendar.YEAR)); 
		cal.set(Calendar.MONTH, secondTimeCalendar.get(Calendar.MONTH)); 
		cal.set(Calendar.DAY_OF_MONTH, secondTimeCalendar.get(Calendar.DAY_OF_MONTH)); 
		cal.set(Calendar.HOUR_OF_DAY, secondTimeCalendar.get(Calendar.HOUR_OF_DAY)); 
		cal.set(Calendar.MINUTE, secondTimeCalendar.get(Calendar.MINUTE)); 
		this.secondTimeCalendar = cal;
	}

	@Override
	public Color getExtrapColor() {
	      Color color =new java.awt.Color( extrapCS.getColorValue().red,
	    		  extrapCS.getColorValue().green, extrapCS.getColorValue().blue );         
        return color;
	}

	@Override
	public String getExtrapLinePattern() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getExtrapMarker() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public TrackPoint[] getExtrapPoints() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Color getInitialColor(){
	      Color color =new java.awt.Color( initialCS.getColorValue().red,
	    		  initialCS.getColorValue().green, initialCS.getColorValue().blue );         
        return color;
	}

	@Override
	public String getInitialLinePattern() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getInitialMarker() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public TrackPoint[] getInitialPoints() {
		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * The following methods are used in DrawableElementFactory class to set up all 
	 * necessary attributes for drawing a line. Since Track element dialogue GUI does
	 * not supply all of those attributes needed by a line. The methods below provide
	 * some default values for the attributes 
	 */
	/**
	 * Update the attributes
	 */	
    public int getSmoothFactor() {
    	return 0; 
    }
    
    public String getLinePattern() {
    	return "Solid Line"; 
    }
    
    public FillPattern getFillPattern() {
    	return FillPattern.FILL_PATTERN_0; 
    }
    
    public Boolean isClosedLine() {
    	return false; 
    }
    
    public Boolean isFilled() {
    	return false; 
    }
    
	public Color[] getColors() {
		Color[] colors = new Color[1]; 
		colors[0] = getInitialColor(); 
		return colors; 
	}
	
    public float getLineWidth() {
    	return (float)1.0; 
    }
    
    public double getSizeScale() {
    	return 2; 
    }

	@Override
	public FontStyle getFontStyle() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean[] getExtraPointTimeTextDisplayIndicator() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Coordinate[] getLinePoints() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getPatternName() {
		// TODO Auto-generated method stub
		return null;
	}

	
	/**
	 * Convert a String in gempak time format to a calendar.
	 * @param gempakTm - String in GEMPAK time format
	 * @return - Calendar
	 */
	private Calendar gempakTM2Calendar( String gempakTm ){
		Calendar cal = null;
		try {
			cal = Calendar.getInstance();
			int year = 2000 + Integer.valueOf(gempakTm.substring(0, 2));
			int month =  Integer.valueOf(gempakTm.substring(2, 4)) - 1;
			int day = Integer.valueOf(gempakTm.substring(4, 6));
			int hour = Integer.valueOf(gempakTm.substring(7, 9));
			int min = Integer.valueOf(gempakTm.substring(9));
			cal.set(Calendar.YEAR, year);
			cal.set(Calendar.MONTH, month);
			cal.set(Calendar.DAY_OF_MONTH, day);
			cal.set(Calendar.HOUR_OF_DAY, hour);
			cal.set(Calendar.MINUTE, min);
		}
		catch (Exception e){
			cal = null;
		}
		
		return cal;
	}
}
