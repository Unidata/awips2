/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.TrackAttrDlg
 * 
 * 111 May 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrDialog;

import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.Calendar;
import java.util.HashMap;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.exception.VizException;

import gov.noaa.nws.ncep.ui.pgen.display.TrackPoint;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.elements.Track;

/**
 * Singleton attribute dialog for Track.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 05/09					M. Gao   	Initial Creation.
 * 10/11					J. Wu   	Adjusted text width
 * 02/12        TTR456      Q.Zhou      Modified setTrack() and added roundToXX() functions.
 * </pre>
 * 
 * @author	M. Gao
 */

public class TrackExtrapPointInfoDlg extends AttrDlg {
	
	private final static org.apache.log4j.Logger log = 
		org.apache.log4j.Logger.getLogger(TrackExtrapPointInfoDlg.class);
	
	private static TrackExtrapPointInfoDlg INSTANCE = null;
       
	private final int DEFAULT_TEXT_WIDTH = 115; 
	private final int DEFAULT_TEXT_HEIGHT = 15; 
	private final int DEFAULT_SCOLL_TEXT_WIDTH = 320; 
	private final int DEFAULT_SCOLL_TEXT_HEIGHT = 60; 
    
	private final String THREE_DECIMAL_DIGIT_PATTERN = "#.###"; 
	private final String TWO_DECIMAL_DIGIT_PATTERN = "#.##"; 
	
    private Track track; 

	private Text speedText; 
    private Text directionText; 

	private Text firstInitPointTimeText; 
    private Text firstInitPointLatText; 
    private Text firstInitPointLonText; 
	private Text secondInitPointTimeText; 
    private Text secondInitPointLatText; 
    private Text secondInitPointLonText; 
    
    private Text extraPointInfoText; 
    
    public void setSpeedText(String speedText) {
		this.speedText.setText(speedText); 
	}

	public void setDirectionText(String directionText) {
		this.directionText.setText(directionText);
	}

	private Text getFirstInitPointTimeText() {
		return firstInitPointTimeText;
	}

	private Text getFirstInitPointLatText() {
		return firstInitPointLatText;
	}

	private Text getFirstInitPointLonText() {
		return firstInitPointLonText;
	}

	private Text getSecondInitPointTimeText() {
		return secondInitPointTimeText;
	}

	private Text getSecondInitPointLatText() {
		return secondInitPointLatText;
	}

	private Text getSecondInitPointLonText() {
		return secondInitPointLonText;
	}

	public Track getTrack() {
		return track;
	}

	public void setTrack(Track _track, int unitComboSelectedIndex, int roundComboSelectedIndex, int roundDirComboSelectedIndex) {
		this.track = _track;
		String unit = "";
		double speed = 0.0;
		double dir = 0.0;
		int roundSpeed = 0;
		int roundDir = 0;
		
		//speed
		if (unitComboSelectedIndex ==0){
			unit = " kts";
			speed = this.track.getSpeedInKnotOverHour();
		}
		else if (unitComboSelectedIndex ==1){
			unit = " kph";
			speed = this.track.getSpeedInKilometerOverHour();
		}
		else if (unitComboSelectedIndex ==2){
			unit = " mph";
			speed = this.track.getSpeedInMileOverHour();
		}
		
		if (roundComboSelectedIndex ==1)
			roundSpeed = roundTo5( (int)(speed + 0.5));	//if speed>=0.5, +1
		else if (roundComboSelectedIndex ==2)
			roundSpeed = roundTo10( (int)(speed + 0.5));	

		if (roundComboSelectedIndex >0)
			speedText.setText("Spd: " + roundSpeed + unit);
		else
			speedText.setText("Spd: " + doubleValurFormater(speed, 2) + unit); 
		
		//dir
		dir = this.track.getDirectionForExtraPoints();
		if (roundDirComboSelectedIndex ==1)
			roundDir = (int)(dir + 0.5);	//if speed>=0.5, +1
		else if (roundDirComboSelectedIndex ==2)
			roundDir = roundTo5( (int)(dir + 0.5));	

		if (roundDirComboSelectedIndex >0)
			directionText.setText("Dir: " + roundDir + " deg.");
		else
			directionText.setText("Dir: " + doubleValurFormater(dir, 2) + " deg."); 

		/*
		 * fill time, latitude and longitude info for the last two initial points
		 */
		fillLastTwoInitPointInfo(track.getInitialPoints()); 
		
		fillExtraPointInfo(track.getExtrapPoints()); 
	}

	private int roundTo5(int speed) {
		int remain = speed%10;
		int divid = speed/10;
		int roundSpeed = 0;
		
		if (remain >=1 && remain <=2)
			roundSpeed = divid*10;
		else if (remain >=3 && remain <=7)
			roundSpeed = divid*10 + 5;
		else if (remain >=8 && remain <=9)
			roundSpeed = divid*10 + 10;
		else 
			roundSpeed = speed;

		return roundSpeed;
	}
	
	private int roundTo10(int speed) {
		int remain = speed%10;
		int divid = speed/10;
		int roundSpeed = 0;
		
		if (remain >=1 && remain <=4)
			roundSpeed = divid*10;
		else if (remain >=5 && remain <=9)
			roundSpeed = divid*10 + 10;
		else 
			roundSpeed = speed;

		return roundSpeed;
	}

 	/**
	 * Private constructor
	 * @param parShell
	 * @throws VizException
	 */
	private TrackExtrapPointInfoDlg(Shell parShell) throws VizException {
        super(parShell);
    }
	
	/**
	 * Creates a track attribute dialog if the dialog does not exist 
	 * and returns the instance. If the dialog exists, return the instance.
	 *  
	 * @param parShell
	 * @return
	 */
	public static TrackExtrapPointInfoDlg getInstance( Shell parShell){
		if ( INSTANCE == null ){
			try {
				INSTANCE = new TrackExtrapPointInfoDlg( parShell );
			} catch (VizException e) {
				e.printStackTrace();
			}	
		}
		return INSTANCE;
	} 
	
	/**
	 * Creates the dialog area
	 */
	@Override
	public Control createDialogArea(Composite parent) {
		
			Composite top = (Composite) super.createDialogArea(parent);
			
			// Create the main layout for the shell.
	        GridLayout mainLayout = new GridLayout(3, false);
	        mainLayout.marginHeight = 3;
	        mainLayout.marginWidth = 3;
	        top.setLayout(mainLayout);

	        log.info("===right before calling initializeComponents(...) in TrackExtrapPointInfoDlg"); 
	        // Initialize all of the menus, controls, and layouts
	        initializeComponents(top, parent);

	        return top;
	}
	
	private void fillLastTwoInitPointInfo(TrackPoint[] trackPoints) {
		if(trackPoints == null || trackPoints.length < 2)
			return; 
		int length = trackPoints.length; 
		TrackPoint firstTrackPoint = trackPoints[length - 2]; 
		TrackPoint secondTrackPoint = trackPoints[length - 1];
		
		setInitPointTimeText(firstTrackPoint, getFirstInitPointTimeText()); 
		setInitPointLatText(firstTrackPoint, getFirstInitPointLatText()); 
		setInitPointLonText(firstTrackPoint, getFirstInitPointLonText()); 
		
		setInitPointTimeText(secondTrackPoint, getSecondInitPointTimeText()); 
		setInitPointLatText(secondTrackPoint, getSecondInitPointLatText()); 
		setInitPointLonText(secondTrackPoint, getSecondInitPointLonText()); 
	}
	
	private void fillExtraPointInfo(TrackPoint[] trackPoints) {
		/*
		 * The first point of the extra track points is the last point of the initial points
		 * thus, it needs to be skipped. 
		 */
		if(trackPoints == null || trackPoints.length < 2)
			return; 
		
		StringBuilder strBuilder = new StringBuilder(100); 
		for(int i=0; i<(trackPoints.length); i++) {
			strBuilder.append(getTimeStringByTrackPoint(trackPoints[i])); 
			strBuilder.append("\t\t\t\t"); 
			strBuilder.append(getLatStringByTrackPoint(trackPoints[i])); 
			strBuilder.append("\t\t"); 
			strBuilder.append(getLonStringByTrackPoint(trackPoints[i])); 
			strBuilder.append(Text.DELIMITER); 
		}
		extraPointInfoText.setText(strBuilder.toString().trim()); 
	}
	
    private void setInitPointTimeText(TrackPoint trackPoint, Text initTimeTextObject) {
    	if(trackPoint == null || trackPoint.getTime() == null)
    		return; 
    	String timeString = getHourMinuteTimeString(trackPoint.getTime()); 
    	initTimeTextObject.setText(timeString);
	}

    private void setInitPointLatText(TrackPoint trackPoint, Text initLatTextObject) {
    	if(trackPoint == null || trackPoint.getLocation() == null)
    		return; 
    	com.vividsolutions.jts.geom.Coordinate coordinate = trackPoint.getLocation(); 
    	initLatTextObject.setText(doubleValurFormater(coordinate.y, 3));
	}

	private void setInitPointLonText(TrackPoint trackPoint, Text initLonTextObject) {
    	if(trackPoint == null || trackPoint.getLocation() == null)
    		return; 
    	com.vividsolutions.jts.geom.Coordinate coordinate = trackPoint.getLocation(); 
    	initLonTextObject.setText(doubleValurFormater(coordinate.x, 3));
	}
	
	private String getTimeStringByTrackPoint(TrackPoint trackPoint) {
		return getHourMinuteTimeString(trackPoint.getTime()); 
	}
	
	private String getLatStringByTrackPoint(TrackPoint trackPoint) {
		com.vividsolutions.jts.geom.Coordinate coordinate = trackPoint.getLocation(); 
		if(coordinate == null)
			return ""; 
		return String.valueOf(doubleValurFormater(coordinate.y, 3)); 
	}
	
	private String getLonStringByTrackPoint(TrackPoint trackPoint) {
		com.vividsolutions.jts.geom.Coordinate coordinate = trackPoint.getLocation(); 
		if(coordinate == null)
			return ""; 
		return String.valueOf(doubleValurFormater(coordinate.x, 3)); 
	}
	
    private String getHourMinuteTimeString(Calendar cal) {
    	if(cal == null)
    		return ""; 
    	StringBuilder timeStringBuilder = new StringBuilder(4); 
    	if(cal.get(Calendar.HOUR_OF_DAY) < 10)
    		timeStringBuilder.append(0); 
    	timeStringBuilder.append(cal.get(Calendar.HOUR_OF_DAY)); 
    	if(cal.get(Calendar.MINUTE) < 10)
    		timeStringBuilder.append(0);
    	timeStringBuilder.append(cal.get(Calendar.MINUTE)); 
    	return timeStringBuilder.toString(); 
	}

    private String doubleValurFormater(double doubleValue, int decimalLength) {
    	NumberFormat formatter = null; 
    	if(decimalLength > 2)
    		formatter = new DecimalFormat(THREE_DECIMAL_DIGIT_PATTERN); 
    	else 
    		formatter = new DecimalFormat(TWO_DECIMAL_DIGIT_PATTERN); 
    		
    	return formatter.format(doubleValue); 
    }
	
	/**
	 * Creates buttons, menus, and other controls in the dialog area
	 * @param listener 
	 */
	private void initializeComponents(Composite topComposite, Composite parent) {
        this.getShell().setText("Track Results");

        speedText = createTextfieldWithoutLabel(topComposite, 
        		SWT.SINGLE | SWT.BORDER, DEFAULT_TEXT_WIDTH, DEFAULT_TEXT_HEIGHT, false);          
        directionText = createTextfieldWithoutLabel(topComposite,  
        		SWT.SINGLE | SWT.BORDER, DEFAULT_TEXT_WIDTH, DEFAULT_TEXT_HEIGHT, false);          
        
        Label emptyLabel = new Label( topComposite, SWT.LEFT );
        emptyLabel.setText("   ");
        
        /*
         * Draw Time, Latitude and Longitude titles
         */
        Label timeTitleLabel = new Label(topComposite, SWT.LEFT);
        timeTitleLabel.setText("Time"); 
        Label latitudeTitleLabel = new Label(topComposite, SWT.LEFT);
        latitudeTitleLabel.setText("Latitude"); 
        Label longitudeTitleLabel = new Label(topComposite, SWT.LEFT);
        longitudeTitleLabel.setText("Longitude"); 
        
        /*
         * Draw a single line Label
         */
        drawSingleLabelSpanColumns(topComposite, 3, "------------------------------------------------------------------------------");         
   
        /*
         * Draw the values of first initial point, time, Latitude and Longitude 
         */
        firstInitPointTimeText = createTextfieldWithoutLabel(topComposite, 
        		SWT.SINGLE | SWT.BORDER, DEFAULT_TEXT_WIDTH/2, DEFAULT_TEXT_HEIGHT, false);  
        firstInitPointLatText = createTextfieldWithoutLabel(topComposite, 
        		SWT.SINGLE | SWT.BORDER, DEFAULT_TEXT_WIDTH/2, DEFAULT_TEXT_HEIGHT, false);  
        firstInitPointLonText = createTextfieldWithoutLabel(topComposite, 
        		SWT.SINGLE | SWT.BORDER, DEFAULT_TEXT_WIDTH/2, DEFAULT_TEXT_HEIGHT, false);  
        
        /*
         * Draw the values of second initial point, time, Latitude and Longitude 
         */
        secondInitPointTimeText = createTextfieldWithoutLabel(topComposite, 
        		SWT.SINGLE | SWT.BORDER, DEFAULT_TEXT_WIDTH/2, DEFAULT_TEXT_HEIGHT, false);  
        secondInitPointLatText = createTextfieldWithoutLabel(topComposite, 
        		SWT.SINGLE | SWT.BORDER, DEFAULT_TEXT_WIDTH/2, DEFAULT_TEXT_HEIGHT, false);  
        secondInitPointLonText = createTextfieldWithoutLabel(topComposite, 
        		SWT.SINGLE | SWT.BORDER, DEFAULT_TEXT_WIDTH/2, DEFAULT_TEXT_HEIGHT, false);  

        /*
         * Draw a single line Label
         */
        drawSingleLabelSpanColumns(topComposite, 3, "------------------------------------------------------------------------------");         
   

        extraPointInfoText = new Text(topComposite, SWT.BORDER | SWT.MULTI | SWT.H_SCROLL |
        		SWT.V_SCROLL); 
        GridData gridData = new GridData(DEFAULT_SCOLL_TEXT_WIDTH, DEFAULT_SCOLL_TEXT_HEIGHT); 
        gridData.horizontalSpan = 3; 
        extraPointInfoText.setLayoutData(gridData); 
        extraPointInfoText.setEditable(false); 
        
	}	
	
	private void drawSingleLabelSpanColumns(Composite parentComposite, int spanColumns, 
			String labelString) {
        Label label = new Label(parentComposite, SWT.LEFT); 
        label.setText(labelString); 
        //make the label to fill the a number of columns, the actual number is determined by spanColumns
        GridData gridData = new GridData(); 
        gridData.horizontalSpan = spanColumns; 
        label.setLayoutData(gridData); 
	}
	
	private Text createTextfieldWithoutLabel(Composite parentComposite, 
			int textStyle, int textWidth, int textHeight, boolean isEditable) {
        Text text = new Text(parentComposite, textStyle);                        
        text.setLayoutData( new GridData( textWidth, textHeight ) );
        text.setEditable(isEditable);   
        return text; 
 	}
	
	/**
	 * Gets values of all attributes of the dialog.
	 */
	public HashMap<String, Object> getAttrFromDlg(){
		
	 	HashMap<String, Object> attr = new HashMap<String, Object>( );
   
    	return attr;
	}
	
	/**
	 * Sets values of all attributes of the dialog.
	 */
	public void setAttrForDlg( IAttribute attr ){
	}

}
