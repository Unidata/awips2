/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.TcmAttrDlg
 * 
 * 03 June 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import gov.noaa.nws.ncep.ui.pgen.PgenStaticDataProvider;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElementFactory;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableType;
import gov.noaa.nws.ncep.ui.pgen.elements.tcm.ITcm;
import gov.noaa.nws.ncep.ui.pgen.elements.tcm.Tcm;
import gov.noaa.nws.ncep.ui.pgen.elements.tcm.TcmFcst;
import gov.noaa.nws.ncep.ui.pgen.elements.tcm.TcmWindQuarters;

import java.io.File;
import java.util.Calendar;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.DateTime;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Singleton attribute dialog for text.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 09/11			?		B. Yin   	Initial Creation.
 * 08/21		  626		B. Yin		Change quadrants to the correct order
 *
 * </pre>
 * 
 * @author	B. Yin
 */

public class TcmAttrDlg extends AttrDlg implements ITcm, SelectionListener {
	
	static TcmAttrDlg INSTANCE = null;
	private static TcaAttrInfo info=null;
	
	private static final String[] fcstHrStr = {"OBS", "06", "12", "24", "36", "48", "72", "96", "120", "144"};

	private final String APPLY = "Apply";
	private final String DELETE = "Delete";
	private final String ADD = "Add";
	
	private Composite top = null;
	private Combo stormTypes = null;
	private Combo basinTypes = null;
    private Spinner advisoryNumber = null;
    private Text stormNameField = null;
    private Spinner stormNumber = null;
    
    private Combo fcstHrs = null;
    private Text validTime = null;
    private DateTime validDate = null;
    
    private Button apply;
    private Button deleteFcst;
    private Button addFcst;
    private Text gustField;
    private Text windMaxField;
    private Text spdField;
    private Text dirField;
    
    private Text pressureField;
    
    private Text ne12ftField, nw12ftField, sw12ftField, se12ftField; 
    
    private Text latField, lonField;
    private Text ne34Field, nw34Field, se34Field, sw34Field;
    private Text ne50Field, nw50Field, se50Field, sw50Field;
    private Text ne64Field, nw64Field, se64Field, sw64Field;
    
    private Text eyeSizeField; 
    private Text accuracyField;
    private Button corr;
    
    private Tcm tcm = null;
    private TcmWindQuarters waves = null;
    
    /**
	 * Private constructor
	 * @param parShell
	 * @throws VizException
	 */
	private TcmAttrDlg(Shell parShell) throws VizException {
		
        super(parShell);
        
    }
	
	/**
	 * Creates a TCA attribute dialog if the dialog does not exist 
	 * and returns the instance. If the dialog exists, return the instance.
	 *  
	 * @param parShell
	 * @return
	 */
	public static TcmAttrDlg getInstance( Shell parShell){
		
		if ( INSTANCE == null ){
					
			try {
				
				INSTANCE = new TcmAttrDlg( parShell );
				
			} catch (VizException e) {
				
				e.printStackTrace();
				
			}	
		}
		
		readOptions();
		return INSTANCE;
		
	} 

	/*
	 * read in all possible selections for the pulldown menus from the tcainfo.xml file
	 */
	private static void readOptions() {
		
		File tcainfoFile = PgenStaticDataProvider.getProvider().getStaticFile( 
				 PgenStaticDataProvider.getProvider().getPgenLocalizationRoot() + TcaAttrDlg.PGEN_TCA_ATTR_INFO); 	    
    		

		try {
			info = (TcaAttrInfo)SerializationUtil.jaxbUnmarshalFromXmlFile(tcainfoFile.getAbsoluteFile() );
		}
		catch ( Exception e) {
			e.printStackTrace();
		}
		
	}	
	/*
	 * Clear out advisories list each time attribute dialog is opened
	 * @see gov.noaa.nws.ncep.ui.pgen.attrDialog.AttrDlg#open()
	 */
    @Override
	public int open() {
    
    	int ret = super.open();
    	return ret;
    }
    
    /*
     *  Add buttons to the button bar.
     */
    @Override
    public void createButtonsForButtonBar( Composite parent ) {   
    	
    }

	/**
	 * Creates the dialog area
	 */
	@Override
	public Control createDialogArea(Composite parent) {
		
	        top = (Composite) super.createDialogArea(parent);
	        this.getShell().setText("TCM Attributes");

	        // Create the main layout for the dialog.
	        GridLayout mainLayout = new GridLayout(1, true);
	        mainLayout.marginHeight = 3;
	        mainLayout.marginWidth = 3;
	        top.setLayout(mainLayout);

	        /*
	         *  Initialize all of the Storm Information Section
	         */
			Group g1 = new Group(top,SWT.SHADOW_ETCHED_IN);
			g1.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
	        createStormInfoArea(g1);
			
	        /*
	         * Initialize section that allows users to add/modify breakpoints
	         * for each advisory
	         */
			Group g2 = new Group(top,SWT.SHADOW_ETCHED_IN);
			g2.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
			createTcmFcst(g2);
			
			//top.pack();
	        return top;
	        
	}
	
	/*
	 * creates the widgets in the storm information section of the dialog
	 */
	private void createStormInfoArea(Group g1) {

        FormLayout layout = new FormLayout();
        layout.marginHeight = 3;
        layout.marginWidth = 3;
		g1.setLayout(layout);

		/*
		 * Storm Type label
		 */
		Label typeLabel = new Label(g1,SWT.NONE);
		typeLabel.setText("Storm Type:");
		FormData fd = new FormData();
		fd.top = new FormAttachment(0,10);
		fd.top = new FormAttachment(0,10);

		//fd.left = new FormAttachment(stormTypes, 0, SWT.LEFT);
		//fd.bottom = new FormAttachment(stormTypes, 5, SWT.TOP);
		typeLabel.setLayoutData(fd);
		
		/*
		 * Storm Type pulldown
		 */
        stormTypes = new Combo( g1, SWT.DROP_DOWN | SWT.READ_ONLY );       
        for ( String st : info.getTypeList() ) {
            stormTypes.add( st );
        }
        stormTypes.setText(stormTypes.getItem(0));
		fd = new FormData();
		fd.left = new FormAttachment(0,10);
		fd.top = new FormAttachment(typeLabel, 5, SWT.BOTTOM);
		stormTypes.setLayoutData(fd);
		
		// reposition Storm Type label
		fd = (FormData)typeLabel.getLayoutData();
		fd.left = new FormAttachment(stormTypes, 0, SWT.LEFT);
		
		/*
		 * Basin label
		 */
		Label basinLabel = new Label(g1,SWT.NONE);
		basinLabel.setText("Basin:");
		fd = new FormData();
		fd.top = new FormAttachment(0,10);
		fd.left = new FormAttachment(typeLabel,150, SWT.RIGHT);
		//fd.top = new FormAttachment(statusItems, 15, SWT.BOTTOM);
		basinLabel.setLayoutData(fd);
		
		/*
		 * Basin pulldown
		 */
        basinTypes = new Combo( g1, SWT.DROP_DOWN | SWT.READ_ONLY );       
        for ( String st : info.getBasinList() ) {
            basinTypes.add( st );
        }
        basinTypes.setText(basinTypes.getItem(0));
		fd = new FormData();
		fd.left = new FormAttachment( stormTypes, 30, SWT.RIGHT);
		//fd.left = new FormAttachment(50, 0);
		fd.top = new FormAttachment(basinLabel, 5, SWT.BOTTOM);
		//fd.right = new FormAttachment(statusItems, 0, SWT.RIGHT);
		basinTypes.setLayoutData(fd);
		
		/*
		 * Storm Name label
		 */
		Label stormNameLabel = new Label(g1,SWT.NONE);
		stormNameLabel.setText("Name:");
		fd = new FormData();
		fd.top = new FormAttachment(stormTypes,15, SWT.BOTTOM);
		fd.left = new FormAttachment(stormTypes, 0, SWT.LEFT);
		stormNameLabel.setLayoutData(fd);
		
		/*
		 * Storm Name text field
		 */
		stormNameField = new Text(g1,SWT.SINGLE | SWT.BORDER);
		fd = new FormData();
		fd.top = new FormAttachment(stormTypes,10, SWT.BOTTOM);
		fd.left = new FormAttachment(stormNameLabel, 10, SWT.RIGHT);
		fd.right = new FormAttachment(stormTypes, 0, SWT.RIGHT);
		stormNameField.setLayoutData(fd);
		
		/*
		 * Storm number label
		 */
		Label stormNumberLabel = new Label(g1,SWT.NONE);
		stormNumberLabel.setText("Storm#:");
		fd = new FormData();
		//fd.top = new FormAttachment(0,10);
		fd.left = new FormAttachment(basinTypes, 0, SWT.LEFT);
		fd.top = new FormAttachment(stormNameField, 8, SWT.TOP);
		stormNumberLabel.setLayoutData(fd);
		
		/*
		 * Storm Number spinner
		 */
		stormNumber = new Spinner(g1, SWT.BORDER);
		fd = new FormData();
		fd.left = new FormAttachment(stormNumberLabel, 32, SWT.RIGHT);
		fd.top = new FormAttachment(stormNumberLabel, -5, SWT.TOP);
		stormNumber.setLayoutData(fd);
		stormNumber.setMinimum(1);
	
		/*
		 * Advisory number label
		 */
		Label advisoryNumberLabel = new Label(g1,SWT.NONE);
		advisoryNumberLabel.setText("Advisory#:");
		fd = new FormData();
		fd.left = new FormAttachment(stormNumberLabel, 0, SWT.LEFT);
		fd.top = new FormAttachment(stormNumberLabel, 55, SWT.BOTTOM);
		advisoryNumberLabel.setLayoutData(fd);
		
		/*
		 * Advisory Number spinner
		 */
		advisoryNumber = new Spinner(g1, SWT.BORDER);
		fd = new FormData();
		fd.left = new FormAttachment(advisoryNumberLabel, 10, SWT.RIGHT);
		fd.top = new FormAttachment(advisoryNumberLabel, -5, SWT.TOP);
		advisoryNumber.setLayoutData(fd);
		advisoryNumber.setMinimum(1);
		
		corr = new Button( g1, SWT.CHECK);
		corr.setText("Correction");
		fd = new FormData();
		fd.left = new FormAttachment(stormNumberLabel, 0, SWT.LEFT);
		fd.top = new FormAttachment(advisoryNumberLabel, 20, SWT.BOTTOM);
		corr.setLayoutData(fd);
		
		/*
		 * Eye size label
		 */
		Label eyeSizeLabel = new Label(g1,SWT.NONE);
		eyeSizeLabel.setText("EyeSize:");
		fd = new FormData();
		fd.top = new FormAttachment(stormNumberLabel,53, SWT.BOTTOM);
		fd.left = new FormAttachment(0,10);
		eyeSizeLabel.setLayoutData(fd);
		
		/*
		 * Eye Size text field
		 */
		eyeSizeField = new Text(g1,SWT.SINGLE | SWT.BORDER);
		fd = new FormData();
		fd.top = new FormAttachment(eyeSizeLabel,-3, SWT.TOP);
		fd.left = new FormAttachment(eyeSizeLabel, 82, SWT.RIGHT);
		fd.right = new FormAttachment(stormTypes, 0, SWT.RIGHT);
		eyeSizeField.setLayoutData(fd);
		
		/*
		 * Central pressure label
		 */
		Label pressureLabel = new Label(g1,SWT.NONE);
		pressureLabel.setText("CentralPressure:");
		fd = new FormData();
		fd.top = new FormAttachment(eyeSizeLabel,15, SWT.BOTTOM);
		fd.left = new FormAttachment(0,10);
		pressureLabel.setLayoutData(fd);
		
		/*
		 * pressure text field
		 */
		pressureField = new Text(g1,SWT.SINGLE | SWT.BORDER);
		fd = new FormData();
		fd.top = new FormAttachment(pressureLabel,-3, SWT.TOP);
		fd.left = new FormAttachment(eyeSizeField, 0, SWT.LEFT);
		fd.right = new FormAttachment(stormTypes, 0, SWT.RIGHT);
		pressureField.setLayoutData(fd);
		
		/*
		 * position accuracy label
		 */
		Label accuracyLabel = new Label(g1,SWT.NONE);
		accuracyLabel.setText("PositionAccuracy:");
		fd = new FormData();
		fd.top = new FormAttachment(pressureLabel,15, SWT.BOTTOM);
		fd.left = new FormAttachment(0,10);
		accuracyLabel.setLayoutData(fd);
		
		/*
		 * accuracy text field
		 */
		accuracyField = new Text(g1,SWT.SINGLE | SWT.BORDER);
		fd = new FormData();
		fd.top = new FormAttachment(accuracyLabel,-3, SWT.TOP);
		fd.left = new FormAttachment(accuracyLabel, 15, SWT.RIGHT);
		fd.right = new FormAttachment(stormTypes, 0, SWT.RIGHT);
		accuracyField.setLayoutData(fd);		
		
		
		/*
		 * Valid Time label
		 */
		Label validTimeLabel = new Label(g1,SWT.NONE);
		validTimeLabel.setText("Valid Time:");
		fd = new FormData();
		fd.top = new FormAttachment(stormNameLabel,18, SWT.BOTTOM);
		fd.left = new FormAttachment(0,10);
		validTimeLabel.setLayoutData(fd);

		validDate = new DateTime(g1, SWT.BORDER | SWT.DATE );
		fd = new FormData();
		fd.top = new FormAttachment(validTimeLabel,-3, SWT.TOP);
		fd.left = new FormAttachment(validTimeLabel, 10, SWT.RIGHT);
		validDate.setLayoutData(fd);
		/*
		 * valid Time text field ----  REPLACED WITH DateTime WIDGETS ABOVE
		 */
		validTime = new Text(g1, SWT.SINGLE | SWT.BORDER | SWT.CENTER);
		validTime.setTextLimit(4);
		fd = new FormData();
		fd.top = new FormAttachment(validDate,-1, SWT.TOP);
		fd.left = new FormAttachment(validDate, 10, SWT.RIGHT);
		//fd.right = new FormAttachment(stormTypes, 0, SWT.RIGHT);
		validTime.setLayoutData(fd);
		validTime.setText( getInitialTime() );
		validTime.addVerifyListener( new VerifyListener(){

			@Override
			public void verifyText(VerifyEvent ve) {
				final char BACKSPACE = 0x08;
				final char DELETE = 0x7F;
				
				if ( Character.isDigit(ve.character) || 
					  ve.character == BACKSPACE || ve.character == DELETE ) ve.doit = true;
				else {
					ve.doit = false;
		//			Display.getCurrent().beep();
				}
			}} );
		
		validTime.addModifyListener( new ModifyListener() {

			@Override
			public void modifyText(ModifyEvent e) {
				if ( isTimeValid( validTime.getText() ) )
					validTime.setBackground( Display.getCurrent().getSystemColor( SWT.COLOR_WHITE));
				else
					validTime.setBackground( Display.getCurrent().getSystemColor( SWT.COLOR_RED));
			}
			
		});
		 
		Label utcLabel = new Label(g1,SWT.NONE);
		utcLabel.setText("UTC");
		fd = new FormData();
		fd.top = new FormAttachment(validTime,5, SWT.TOP);
		fd.left = new FormAttachment(validTime, 5, SWT.RIGHT);
		utcLabel.setLayoutData(fd);
		
		/*
		 * NE 12ft label
		 */
		Label ne12ftLabel = new Label(g1,SWT.NONE);
		ne12ftLabel.setText("NE12FT:");
		fd = new FormData();
		fd.top = new FormAttachment(accuracyLabel,18, SWT.BOTTOM);
		fd.left = new FormAttachment(0,10);
		ne12ftLabel.setLayoutData(fd);
		
		/*
		 * NE 12ft text field
		 */
		ne12ftField = new Text(g1,SWT.SINGLE | SWT.BORDER);
		fd = new FormData();
		fd.top = new FormAttachment(ne12ftLabel,5, SWT.BOTTOM);
		fd.left = new FormAttachment(0, 10);
		ne12ftField.setLayoutData(fd);
		
		
		/*
		 * SE 12ft label
		 */
		Label se12ftLabel = new Label(g1,SWT.NONE);
		se12ftLabel.setText("SE12FT:");
		fd = new FormData();
		fd.top = new FormAttachment(ne12ftLabel,0, SWT.TOP);
		fd.left = new FormAttachment(ne12ftLabel,50, SWT.RIGHT);
		se12ftLabel.setLayoutData(fd);
		
		/*
		 * SE 12ft text field
		 */
		se12ftField = new Text(g1,SWT.SINGLE | SWT.BORDER);
		fd = new FormData();
		fd.top = new FormAttachment(se12ftLabel,5, SWT.BOTTOM);
		fd.left = new FormAttachment(se12ftLabel,0, SWT.LEFT);
		se12ftField.setLayoutData(fd);
		

		/*
		 * SW 12ft label
		 */
		Label sw12ftLabel = new Label(g1,SWT.NONE);
		sw12ftLabel.setText("SW12FT:");
		fd = new FormData();
		fd.top = new FormAttachment(se12ftLabel,0, SWT.TOP);
		fd.left = new FormAttachment(se12ftLabel,50, SWT.RIGHT);
		sw12ftLabel.setLayoutData(fd);
		
		/*
		 * SW 12ft text field
		 */
		sw12ftField = new Text(g1,SWT.SINGLE | SWT.BORDER);
		fd = new FormData();
		fd.top = new FormAttachment(sw12ftLabel,5, SWT.BOTTOM);
		fd.left = new FormAttachment(sw12ftLabel,0, SWT.LEFT);
		sw12ftField.setLayoutData(fd);

		/*
		 * NW 12ft label
		 */
		Label nw12ftLabel = new Label(g1,SWT.NONE);
		nw12ftLabel.setText("NW12FT:");
		fd = new FormData();
		fd.top = new FormAttachment(sw12ftLabel,0, SWT.TOP);
		fd.left = new FormAttachment(sw12ftLabel,50, SWT.RIGHT);
		nw12ftLabel.setLayoutData(fd);
		
		/*
		 * NW 12ft text field
		 */
		nw12ftField = new Text(g1,SWT.SINGLE | SWT.BORDER);
		fd = new FormData();
		fd.top = new FormAttachment(nw12ftLabel,5, SWT.BOTTOM);
		fd.left = new FormAttachment(nw12ftLabel,0, SWT.LEFT);
		nw12ftField.setLayoutData(fd);
		
	
	}
	
	private String getInitialTime() {
		
		Calendar now = Calendar.getInstance( TimeZone.getTimeZone("GMT") );
		int minute = now.get(Calendar.MINUTE);
		if ( minute >= 15 ) now.add(Calendar.HOUR_OF_DAY, 1);
		int hour = now.get(Calendar.HOUR_OF_DAY);

		return String.format("%02d00", hour);
	}

	/*
	 * Create widgets used in the breakpoint modification section of the dialog
	 */
	private void createTcmFcst(Group g2) {

        FormLayout layout = new FormLayout();
        layout.marginHeight = 3;
        layout.marginWidth = 3;
		g2.setLayout(layout);

		/*
		 * Composite used to group Breakpoint Attributes
		 */
		Composite fcstInfo = new Composite(g2, SWT.NONE);
		FormData fd = new FormData();
		fd.left = new FormAttachment(0, 10);
		fd.right = new FormAttachment(100, -10);
		fd.top = new FormAttachment(0,10);
		//fd.bottom = new FormAttachment( 100, -10);
		fcstInfo.setLayoutData(fd);

		RowLayout rl = new RowLayout();
		rl.type = SWT.VERTICAL;
		fcstInfo.setLayout(rl);
		
		Composite comp1 = new Composite(fcstInfo, SWT.NONE);
		comp1.setLayout(new FormLayout());
		/*
		 *  forecast hour label
		 */
		Label fcstHrLabel = new Label(comp1,SWT.NONE);
		fcstHrLabel.setText("Forecast Hour:");
		fd = new FormData();
		fd.top = new FormAttachment(0, 5);
		fd.left = new FormAttachment(0, 5);
		fcstHrLabel.setLayoutData(fd);
		
		/*
		 * forecast pulldown
		 */
		fcstHrs = new Combo( comp1, SWT.DROP_DOWN | SWT.READ_ONLY );       
        for ( String st : fcstHrStr ) {
        	fcstHrs.add( st );
        }
        
        fcstHrs.setText(fcstHrs.getItem(0));
        fd = new FormData();
		fd.top = new FormAttachment(0,0);
		fd.left = new FormAttachment(fcstHrLabel, 18, SWT.RIGHT);
		fcstHrs.setLayoutData(fd);
        
		fcstHrs.addSelectionListener(new SelectionAdapter() {

			@Override
			public void  widgetSelected(SelectionEvent e) {
				int hr = 0;
				try {
					hr = Integer.parseInt(((Combo)e.widget).getText());
				}
				catch ( Exception ex ){
					hr = 0;
				}
				
				addFcst.setEnabled(true);

				if ( tcm != null ){
					for (TcmFcst fcst : tcm.getTcmFcst()){
						if ( fcst.getFcstHr() == hr ){
							setTcmFcstInfo( fcst );
							addFcst.setEnabled(false);
							break;
						}
					}
				}
			}

			
			
		});
		
      //  GridData gd = new GridData(GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL);
      //  severityTypes.setLayoutData(gd);
		
    	Composite comp2 = new Composite(fcstInfo, SWT.NONE);
    	GridLayout gdl = new GridLayout(4, true);
		comp2.setLayout(gdl);	
		
		Label latLabel = new Label(comp2,SWT.NONE);
		latLabel.setText("Lat:");
		latField = new Text(comp2, SWT.SINGLE | SWT.BORDER);
		Label lonLabel = new Label(comp2,SWT.NONE);
		lonLabel.setText("Lon:");
		lonField = new Text(comp2, SWT.SINGLE | SWT.BORDER);
		
		/*
		 *  WindMax
		 */
		Label windMaxLabel = new Label(comp2,SWT.NONE);
		windMaxLabel.setText("WindMax:");
		windMaxField = new Text(comp2, SWT.SINGLE | SWT.BORDER);   

		/*
		 *  Gust
		 */
		Label gustLabel = new Label(comp2,SWT.NONE);
		gustLabel.setText("Gust:");
		gustField = new Text(comp2, SWT.SINGLE | SWT.BORDER);
		
		/*
		 *  StormDir
		 */
		Label dirLabel = new Label(comp2,SWT.NONE);
		dirLabel.setText("StormDir:");
		dirField = new Text(comp2, SWT.SINGLE | SWT.BORDER);   
		
		/*
		 * StormSpd
		 */
		Label spdLabel = new Label(comp2,SWT.NONE);
		spdLabel.setText("StormSpd:");
		spdField = new Text(comp2, SWT.SINGLE | SWT.BORDER);
	
		
		Composite comp3 = new Composite(fcstInfo, SWT.NONE);
    	GridLayout gdl1 = new GridLayout(5, true);
		comp3.setLayout(gdl1);	
		
		Label spdQuatroLabel = new Label(comp3,SWT.NONE);
		spdQuatroLabel.setText(" ");
	
		Label neLabel = new Label(comp3,SWT.NONE);
		neLabel.setText("NorthEast");
		Label swLabel = new Label(comp3,SWT.NONE);
		swLabel.setText("SouthEast");
		Label seLabel = new Label(comp3,SWT.NONE);
		seLabel.setText("SouthWest");				
		Label nwLabel = new Label(comp3,SWT.NONE);
		nwLabel.setText("NorthWest");


		Label spd34Label = new Label(comp3,SWT.NONE);
		spd34Label.setText("34 Knots");
		
		ne34Field = new Text(comp3, SWT.SINGLE | SWT.BORDER); 
		se34Field = new Text(comp3, SWT.SINGLE | SWT.BORDER); 
		sw34Field = new Text(comp3, SWT.SINGLE | SWT.BORDER); 
		nw34Field = new Text(comp3, SWT.SINGLE | SWT.BORDER); 

		Label spd50Label = new Label(comp3,SWT.NONE);
		spd50Label.setText("50 Knots");
		
		ne50Field = new Text(comp3, SWT.SINGLE | SWT.BORDER); 
		se50Field = new Text(comp3, SWT.SINGLE | SWT.BORDER); 
		sw50Field = new Text(comp3, SWT.SINGLE | SWT.BORDER); 
		nw50Field = new Text(comp3, SWT.SINGLE | SWT.BORDER); 
		
		Label spd64Label = new Label(comp3,SWT.NONE);
		spd64Label.setText("64 Knots");
		
		ne64Field = new Text(comp3, SWT.SINGLE | SWT.BORDER); 
		se64Field = new Text(comp3, SWT.SINGLE | SWT.BORDER);
		sw64Field = new Text(comp3, SWT.SINGLE | SWT.BORDER); 
		nw64Field = new Text(comp3, SWT.SINGLE | SWT.BORDER); 
		
		/*
		 * Composite used to group Apply, delete segment, and new segment buttons
		 */
		Composite forButtons = new Composite(g2, SWT.NONE);
		fd = new FormData();
		fd.left = new FormAttachment(0, 10);
		fd.right = new FormAttachment(100, -10);
		fd.top = new FormAttachment(fcstInfo, 15, SWT.BOTTOM);
		fd.bottom = new FormAttachment( 100, -10);
		forButtons.setLayoutData(fd);

		RowLayout row = new RowLayout(SWT.HORIZONTAL);
		row.pack = false;
		row.justify = true;
		forButtons.setLayout(row);
		/*
		 * used to notify drawing tool that a new advisory is to be created
		 */
		addFcst = new Button(forButtons, SWT.PUSH);
		addFcst.setText(ADD);
		addFcst.addSelectionListener(this);
		
		/*
		 * Apply button used to save changes to the current advisory
		 */
		apply = new Button(forButtons, SWT.PUSH);
		apply.setText(APPLY);
		apply.setEnabled(true);
		apply.addSelectionListener(this);

		/*
		 * Delete Segment button used to delete the currently selected advisory
		 */
		deleteFcst = new Button(forButtons, SWT.PUSH);
		deleteFcst.setText(DELETE);
		deleteFcst.setEnabled(false);
		deleteFcst.addSelectionListener(this);

	
		
		/*
		 * used to notify drawing tool that a new advisory is to be created
		 */
		Button closeSegment = new Button(forButtons, SWT.PUSH);
		closeSegment.setText("Close");
		closeSegment.addSelectionListener(this);
		
	}
	
	private boolean isTimeValid(String text) {
		int time = Integer.parseInt(text);
		int hour = time / 100;
		int minute = time % 100;
		
		if ( hour >= 0 && hour <= 23 &&
				minute >= 00 && minute <=59 ) return true;
		
		return false;
	}

	/*
	 * override to do nothing
	 */
	@Override
	public void enableButtons(){
		// do nothing
	}
	
	/**
	 * Sets values of all attributes of the dialog.
	 */
	public void setAttrForDlg( IAttribute attr ){
	
		if ( attr instanceof ITcm ) {
			
			ITcm itcm = (ITcm)attr;
			this.setStormName( itcm.getStormName() );
			this.setStormType( itcm.getStormType() );
			this.setStormNumber(itcm.getStormNumber());
			this.setAdvisoryNumber(itcm.getAdvisoryNumber());
			this.setBasin(itcm.getBasin());
			this.setEyeSize(itcm.getEyeSize());
			this.setPositionAccuracy(itcm.getPositionAccuracy());
			this.setCorrection(itcm.isCorrection());
			this.setCentralPressure((int)itcm.getCentralPressure());
			
			this.setAdvisoryTime(itcm.getAdvisoryTime());
			this.setWaves( itcm.getWaveQuarters() );
			
			this.setTcmFcstInfo( itcm.getTcmFcst().get(0));

		}
	}

	/*
	 * Return advisory number
	 * @see gov.noaa.nws.ncep.ui.pgen.tca.ITca#getAdvisoryNumber()
	 */
	public int getAdvisoryNumber() {
		return advisoryNumber.getSelection();
	}
	
	/*
	 * Set the advisory number
	 */
	private void setAdvisoryNumber(int adnum) {
		advisoryNumber.setSelection(adnum);
	}

	public String getStormName() {
		return stormNameField.getText();
	}
	
	private void setStormName(String name) {
		stormNameField.setText(name);
	}

	public int getStormNumber() {
		return stormNumber.getSelection();
	}
	
	private void setStormNumber( int num) {
		stormNumber.setSelection(num);
	}

	public String getStormType() {
		return stormTypes.getText();
	}
	
	private void setStormType(String type) {
		stormTypes.setText(type);
	}

	public Calendar getAdvisoryTime() {
		int time = Integer.parseInt( validTime.getText() );
		int hours = time / 100;
		int minutes = time % 100;
		
		Calendar advTime = Calendar.getInstance( TimeZone.getTimeZone("GMT") );
		advTime.set(validDate.getYear(), validDate.getMonth(), validDate.getDay(), 
				    hours, minutes, 0);
		advTime.set(Calendar.MILLISECOND, 0);
		return advTime;
	}
	
	private void setAdvisoryTime(Calendar time) {
		validDate.setYear( time.get(Calendar.YEAR));
		validDate.setMonth( time.get(Calendar.MONTH));
		validDate.setDay( time.get(Calendar.DAY_OF_MONTH));
		
		validTime.setText( String.format("%02d%02d", time.get(Calendar.HOUR_OF_DAY), time.get(Calendar.MINUTE) ));
		//validTime.setHours( time.get(Calendar.HOUR_OF_DAY));
		//validTime.setMinutes( time.get(Calendar.MINUTE));
		//validTime.setSeconds(0);
	}
	
	public String getBasin() {
		return basinTypes.getText();
	}
	
	private void setBasin(String basin) {
		basinTypes.setText(basin);
	}

	@Override
	public void widgetDefaultSelected(SelectionEvent e) {
		// TODO Auto-generated method stub
	}

	/*
	 * Called when Add, Apply, Delete or Close button pressed
	 * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
	 */
	@Override
	public void widgetSelected(SelectionEvent e) {
		
		if (e.widget instanceof Button ) {
			Button b = (Button)e.widget;
			
			/*
			 * Add a new TCM forecast 
			 */
			if ( b.getText().equals(ADD) ) {
				
				if ( tcm == null ) {
					tcm = new Tcm(this.getStormType(), this.getStormNumber(),
							this.getAdvisoryNumber(), this.getStormName(),
							this.getBasin(), this.getEyeSize(), this.getPositionAccuracy(),
							this.isCorrection(),
							this.getAdvisoryTime(), this.getCentralPressure());
					drawingLayer.addElement( tcm );
				}
				else {
					Tcm newTcm = (Tcm) tcm.copy();
					drawingLayer.replaceElement(tcm, newTcm);
					tcm = newTcm;
				}
				
    			TcmFcst elem = (TcmFcst)new DrawableElementFactory().create(
    					DrawableType.TCM_FCST, (IAttribute)this,
    			        pgenCategory, pgenType, new Coordinate(getLon(), getLat()),
    			        drawingLayer.getActiveLayer());
    			
    			elem.setSpeed(this.getStormSpeed());
    			elem.setDirection(this.getStormDirection());
    			elem.setGust(this.getGust());
    			elem.setWindMax(this.getWindMax());
    			elem.setParent(null);

    			tcm.addTcmFcst( elem );
    			if ( this.getFcstHr() == 0 ){
    				tcm.setWaveQuatro( this.getWaveQuarters());
    			}
    			
    			mapEditor.refresh();
    			
    			addFcst.setEnabled(false);
			}
			
			/*
			 * Remove q TCM forecast
			 */
			else if ( b.getText().equals(DELETE) ) {
			
			}
				
			/*
			 * Apply changes to the selected TCM
			 */
			else if ( b.getText().equals(APPLY) ) {
				Tcm newTcm = (Tcm)tcm.copy();
				newTcm.setStormType(this.getStormType());
				newTcm.setBasin(this.getBasin());
				newTcm.setStormName(this.getStormName());
				newTcm.setStormNumber(this.getStormNumber());
				newTcm.setAdvisoryNumber(this.getAdvisoryNumber());
				newTcm.setCorrection(this.isCorrection());
				newTcm.setEyeSize(this.getEyeSize());
				newTcm.setCentralPressure(this.getCentralPressure());
				newTcm.setPositionAccuracy(this.getPositionAccuracy());
				newTcm.setTime(this.getAdvisoryTime());
				newTcm.setWaveQuatro(this.getWaveQuarters());
				
				TcmFcst newFcst = null;;
				int ii = 0;
				for (TcmFcst fcst : newTcm.getTcmFcst() ){
					
					if ( this.getFcstHr() == fcst.getFcstHr() ){
			   			newFcst = (TcmFcst)new DrawableElementFactory().create(
		    					DrawableType.TCM_FCST, (IAttribute)this,
		    			        pgenCategory, pgenType, new Coordinate(getLon(), getLat()),
		    			        drawingLayer.getActiveLayer());
			   			newFcst.setSpeed(this.getStormSpeed());
		    			newFcst.setDirection(this.getStormDirection());
		    			newFcst.setGust(this.getGust());
		    			newFcst.setWindMax(this.getWindMax());
		    			newFcst.setParent(null);
		    			
			   			break;
					}
					ii++;
				}
						
				if ( newFcst != null ){
					newTcm.getTcmFcst().remove(ii);
					newTcm.getTcmFcst().add(ii, newFcst);
					
					if ( ii == 0 ) newTcm.getWaveQuarters().setLocation(newFcst.getLocation());
				}
				
				drawingLayer.replaceElement(tcm, newTcm);
				drawingLayer.removeGhostLine();
				drawingLayer.setSelected(newTcm);
				this.tcm = newTcm;
				mapEditor.refresh();

			}
			else if ( b.getText().equals("Close") ) {
				tcm = null;
				PgenUtil.setSelectingMode();
				super.close();
			}

		}
		
	}
	
	/**
	 * Gets latitude from TCM dialog
	 * @return
	 */
	private double getLat(){
		
		double ret = 35;
		try {
			ret = Double.parseDouble(latField.getText());
		}
		catch (Exception e){
			
		}
		
		if ( ret >= 90 || ret <= -90 ) ret = 35;
		
		return ret;
	}
	
	/**
	 * Get longitude from TCM dialog 
	 * @return
	 */
	private double getLon(){
		
		double ret = -95;
		try {
			ret = Double.parseDouble(lonField.getText());
		}
		catch (Exception e){
			
		}
		
		if ( ret > 180 || ret < -180 ) ret = -95;

		return ret;
	}

	/**
	 * Returns storm wind radius from TCM dialog
	 */
	@Override
	public double[][] getWindRadius() {
		
		double[][] ret = new double[4][3];
		ret[0][0] = getRadius( ne34Field ); 
		ret[1][0] = getRadius( se34Field ); 
		ret[2][0] = getRadius( sw34Field ); 
		ret[3][0] = getRadius( nw34Field ); 

		ret[0][1] = getRadius( ne50Field ); 
		ret[1][1] = getRadius( se50Field ); 
		ret[2][1] = getRadius( sw50Field ); 
		ret[3][1] = getRadius( nw50Field ); 
		
		ret[0][2] = getRadius( ne64Field ); 
		ret[1][2] = getRadius( se64Field ); 
		ret[2][2] = getRadius( sw64Field ); 
		ret[3][2] = getRadius( nw64Field ); 
		
		return ret;
	}
	
	/**
	 * Gets wave or wind radius from TCM dialog text fields
	 * @param field- text field
	 * @return - double, radius of wind or wave quarters
	 */
	private double getRadius( Text field ){
		double ret = 0;
		try {
			ret = Double.parseDouble(field.getText());
		}
		catch (Exception e){
			
		}
		return ret;
	}

	@Override
	public java.util.List<TcmFcst> getTcmFcst() {
		if ( tcm == null )
			return null;
		else return tcm.getTcmFcst();
	}

	/**
	 * Gets the central pressure(mb) from TCM dialog
	 */
	@Override
	public int getCentralPressure() {
		int ret = 0;
		try {
			ret = Integer.parseInt( pressureField.getText());
		}
		catch (Exception e){
			
		}
		
		return ret;
	}

	/**
	 * Set the central pressure field in TCM dialog
	 * @param pressure - int, central pressure(mb)
	 */
	private void setCentralPressure( int pressure ){
		pressureField.setText(Integer.toString(pressure));
	}
	
	/**
	 * Gets forecast hour selection from TCM dialog.
	 * OBS = 0
	 */
	@Override
	public int getFcstHr() {
		int ret = 0;
		try {
			ret = Integer.parseInt(fcstHrs.getText());
		}
		catch ( Exception e ){
			
		}
		return ret;
	}

	/**
	 * Return the 12 feet wave quarters from TCM dialog 
	 */
	@Override
	public TcmWindQuarters getWaveQuarters() {
		if ( waves == null ){
			return new TcmWindQuarters( tcm.getTcmFcst().get(0).getLocation(), 
						0, 
						getRadius( ne12ftField), 
						getRadius( se12ftField), 
						getRadius( sw12ftField),
						getRadius( nw12ftField) );
		}
		else return waves;
	}
	
	/**
	 * Gets the eye size from TCM dialog
	 */
	@Override
	public int getEyeSize() {
		int ret = 0;
		try {
			ret = Integer.parseInt(eyeSizeField.getText());
		}
		catch ( Exception e ){
			
		}
		return ret;
	}

	/**
	 * Sets the eye size field in TCM dialog
	 * @param eye -int, eye size
	 */
	private void setEyeSize( int eye ){
		eyeSizeField.setText(Integer.toString(eye));
	}
	
	/**
	 * Gets the position accuracy field in TCM dialog
	 */
	@Override
	public int getPositionAccuracy() {
		int ret = 0;
		try {
			ret = Integer.parseInt(accuracyField.getText());
		}
		catch ( Exception e ){
			
		}
		return ret;
	}

	/**
	 * Sets the position accuracy field in TCM dialog
	 * @param posAccu - int, position accuracy
	 */
	private void setPositionAccuracy( int posAccu ){
		accuracyField.setText(Integer.toString(posAccu));
	}

	/** 
	 * Return the status of correction check box in TCM dialog 
	 */
	@Override
	public boolean isCorrection(){
		return corr.getSelection();
	}
	
	/**
	 * Sets the correction check box in TCM dialog
	 * @param corrFlag
	 */
	private void setCorrection( boolean corrFlag ){
		corr.setSelection( corrFlag );
	}
	
	/**
	 * Sets the TCM 12 feet wave  quarters
	 * @param waves
	 */
	private void setWaves( TcmWindQuarters waves ){
		ne12ftField.setText( Integer.toString((int)waves.getQuarters()[0]));
		se12ftField.setText( Integer.toString((int)waves.getQuarters()[1]));
		sw12ftField.setText( Integer.toString((int)waves.getQuarters()[2]));
		nw12ftField.setText( Integer.toString((int)waves.getQuarters()[3]));
	}
	
	/**
	 * Sets the TCM forecast information panel.
	 * @param fcst
	 */
	private void setTcmFcstInfo( TcmFcst fcst ){
		int hr = fcst.getFcstHr();
		fcstHrs.setText( (hr==0)?"OBS":String.format("%1$02d", hr) );
		latField.setText( Double.toString(fcst.getLocation().y));
		lonField.setText( Double.toString(fcst.getLocation().x));
		
		setGust( fcst.getGust());
		setWindMax(fcst.getWindMax());
		setStormSpeed(fcst.getSpeed());
		setStormDirection(fcst.getDirection());
		
		ne34Field.setText(Integer.toString((int)fcst.getQuarters()[0].getQuarters()[0]));
		nw34Field.setText(Integer.toString((int)fcst.getQuarters()[0].getQuarters()[3]));
		sw34Field.setText(Integer.toString((int)fcst.getQuarters()[0].getQuarters()[2]));
		se34Field.setText(Integer.toString((int)fcst.getQuarters()[0].getQuarters()[1]));
		
		ne50Field.setText(Integer.toString((int)fcst.getQuarters()[1].getQuarters()[0]));
		nw50Field.setText(Integer.toString((int)fcst.getQuarters()[1].getQuarters()[3]));
		sw50Field.setText(Integer.toString((int)fcst.getQuarters()[1].getQuarters()[2]));
		se50Field.setText(Integer.toString((int)fcst.getQuarters()[1].getQuarters()[1]));
		
		ne64Field.setText(Integer.toString((int)fcst.getQuarters()[2].getQuarters()[0]));
		nw64Field.setText(Integer.toString((int)fcst.getQuarters()[2].getQuarters()[3]));
		sw64Field.setText(Integer.toString((int)fcst.getQuarters()[2].getQuarters()[2]));
		se64Field.setText(Integer.toString((int)fcst.getQuarters()[2].getQuarters()[1]));
	}
	
	/**
	 * Sets the working TCM element from TCM dialog
	 * @param tcm
	 */
	public void setTcm( Tcm tcm ){
		this.tcm = tcm;
	}
	
	/**
	 * Gets gust from TCM dialog
	 * @return - int, gust
	 */
	public int getGust(){
		int ret = 0;
		
		try{
			ret = Integer.parseInt( gustField.getText());
		}
		catch ( Exception e ){
			
		}
		
		return ret;
	}
	
	/**
	 * Sets gust field in TCM dialog
	 * @param gust - int, gust
	 */
	private void setGust( int gust ){
		gustField.setText( Integer.toString(gust));
	}
	
	/**
	 * Gets windMax from TCM dialog
	 * @return - int, wind max
	 */
	public int getWindMax(){
		int ret = 0;
		
		try{
			ret = Integer.parseInt( windMaxField.getText());
		}
		catch ( Exception e ){
			
		}
		
		return ret;
	}
	
	/**
	 * Sets the wind max field in TCM dialog
	 * @param wind - int, max wind 
	 */
	private void setWindMax( int wind ){
		windMaxField.setText( Integer.toString(wind));
	}
	
	/**
	 * Gets storm speed from TCM dialog
	 * @return - int, storm speed
	 */
	public int getStormSpeed(){
		int ret = 0;
		
		try{
			ret = Integer.parseInt( spdField.getText());
		}
		catch ( Exception e ){
			
		}
		
		return ret;
	}
	
	/**
	 * Sets the storm speed field in TCM dialog
	 * @param speed - int, storm speed
	 */
	private void setStormSpeed( int speed ){
		spdField.setText( Integer.toString(speed));
	}
	
	/**
	 * Gets the storm direction from TCM dialog
	 * @return - int, storm direction
	 */
	public int getStormDirection(){
		int ret = 0;
		
		try{
			ret = Integer.parseInt( dirField.getText());
		}
		catch ( Exception e ){
			
		}
		
		return ret;
	}
	
	/**
	 * Sets the storm direction field in TCM dialog
	 * @param dir - int, storm direction
	 */
	private void setStormDirection( int dir ){
		dirField.setText( Integer.toString(dir));
	}
}

