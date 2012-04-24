package gov.noaa.nws.ncep.viz.tools.customProjection;

import gov.noaa.nws.ncep.common.log.logger.NcepLogger;
import gov.noaa.nws.ncep.common.log.logger.NcepLoggerManager;
import gov.noaa.nws.ncep.gempak.parameters.marshaller.garea.GraphicsAreaCoordinates;
import gov.noaa.nws.ncep.viz.common.ui.DisplayViewLowerLeftAndUpperRightLongLatValues;
import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;
//import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;

import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.parameter.Parameter;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.geotools.referencing.operation.projection.MapProjection;
import org.opengis.parameter.InvalidParameterValueException;
import org.opengis.parameter.ParameterDescriptor;
import org.opengis.parameter.ParameterValueGroup;
import org.opengis.referencing.NoSuchIdentifierException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.cs.CoordinateSystem;
import org.opengis.referencing.cs.CoordinateSystemAxis;
import org.opengis.referencing.operation.Projection;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date             Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep. 24, 2009    Task 171   M. Gao	   Init Custom Project Dialog
 * Apr 07, 2010  Task 238,239  Archana     Used NCMapDescriptor instead of
 *                                         MapDescriptor  
 * 07/28/11        #450        G. Hull     NcPathManager
 * Nov 22, 2011                B. Hebbard  Comment out validateParameters() call from
 *                                         handleProjectionSelection(), to prevent pre-
 *                                         mature complaints about null Double values
 * 03/2012			  		   J. Wu       Remove dependency on NCMapDescriptor
 * 
 * </pre>
 * 
 * @author mgao
 * @version 1.0
 */

public class CreateCustomProjectionDialog extends CaveJFACEDialog {
    private static final IUFStatusHandler statusHandler = UFStatus.getHandler(CreateCustomProjectionDialog.class);
    
	private NcepLogger logger = NcepLoggerManager.getNcepLogger(this.getClass()); 

	private String dialogTitle = "Custom Pre-Defined Area"; 

	private final String THREE_DECIMAL_DIGIT_PATTERN = "#.###"; 
	
	private final String TWO_DECIMAL_DIGIT_PATTERN = "#.##"; 
	
    private DefaultMathTransformFactory factory;

    private ParameterValueGroup parameters;

    private Combo projList;

    private class ParamUI {
        public Label label;

        public Text text;

        public ParamUI(Label label, Text text) {
            this.label = label;
            this.text = text;
        }
    }

    private Map<Parameter<?>, ParamUI> paramMap = new HashMap<Parameter<?>, ParamUI>();

    private Composite paramComp;

    private Button validateBtn;

    private Text validationText;

    private Composite dlgComp;

    private Text llLatText;

    private Text llLonText;

    private Text urLatText;

    private Text urLonText;

    private Text gempakProjectionText, gempakGraphicAreaText; 
    
    private Label gempakProjectionLabel, gempakGraphicAreaLabel;
    
    private String gempakProjectionTextString, gempakGraphicAreaTextString; 
    
    private Label dummyLabel; 
    
    private Button gempakInputButton, applyButton; 
    
    private Composite gempakInputButtonComposite; 
    
    private Composite gempakFieldComp; 
    
    private Group paramGroup; 
    
    private String projectionListInitValue;
    
    private boolean projectionWithoutAngleValues; 
    
    private Button cornersBtn;

    private Button centerBtn;

    private Group cornersGroup;

    private Group centerGroup;

    private Text centerLatText;

    private Text centerLonText;

    private Text widthText;

    private Text heightText;

    private Label widthUnit;

    private Label heightUnit;

    private CoordinateReferenceSystem crs;

    private GeneralGridGeometry newMapGeom;

    public boolean isProjectionWithoutAngleValues() {
		return projectionWithoutAngleValues;
	}

	public void setProjectionWithoutAngleValues(boolean projectionWithoutAngleValues) {
		this.projectionWithoutAngleValues = projectionWithoutAngleValues;
	}

	/**
     * @param parentShell
     */
    public CreateCustomProjectionDialog(Shell parentShell) {
        super(parentShell);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveJFACEDialog#createDialogArea(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected Control createDialogArea(Composite parent) {
    	setDialogTitleText(dialogTitle); 

    	dlgComp = (Composite) super.createDialogArea(parent);
    	dlgComp.setLayoutData( new GridData( 460, 850 ) );

        Group gempakInputGroup = new Group(dlgComp, SWT.BORDER);
        gempakInputGroup.setText("GEMPAK");   //setText("Graphic Area & Projection");
        Layout layout = new GridLayout(1, true);
        gempakInputGroup.setLayout(layout);
        gempakInputGroup.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_FILL
                | GridData.GRAB_HORIZONTAL));

        gempakInputButtonComposite = new Composite(gempakInputGroup, SWT.NONE); 
        layout = new GridLayout(1, false);
        gempakInputButtonComposite.setLayout(layout);
        gempakInputButton = new Button(gempakInputButtonComposite, SWT.TOGGLE); 
        gempakInputButton.setText("GEMPAK...    "); 
        
        gempakFieldComp = new Composite(gempakInputGroup, SWT.NONE);
        layout = new GridLayout(2, false);
        gempakFieldComp.setLayout(layout);

        int textWidth = 260; 
        int textHeight = 15; 
        int labelWidth = 55; 
        int labelHeight = 15; 

        gempakGraphicAreaLabel = new Label(gempakFieldComp, SWT.NONE); 
        gempakGraphicAreaLabel.setLayoutData( new GridData( labelWidth, labelHeight ) );
        gempakGraphicAreaLabel.setText("GAREA");
        gempakGraphicAreaLabel.setVisible(false); 
        gempakGraphicAreaText = new Text(gempakFieldComp, SWT.BORDER);
        gempakGraphicAreaText.setLayoutData( new GridData( textWidth, textHeight ) );
        gempakGraphicAreaText.setVisible(false); 
        
        gempakProjectionLabel = new Label(gempakFieldComp, SWT.NONE); 
        gempakProjectionLabel.setLayoutData( new GridData( labelWidth, labelHeight ) );
        gempakProjectionLabel.setText("PROJ");
        gempakProjectionLabel.setVisible(false); 
        gempakProjectionText = new Text(gempakFieldComp, SWT.BORDER);
        gempakProjectionText.setLayoutData( new GridData( textWidth, textHeight ) );
        gempakProjectionText.setVisible(false); 

        Composite applyButtonComp = new Composite(gempakInputGroup, SWT.NONE);
        layout = new GridLayout(2, false);
        applyButtonComp.setLayout(layout);

        dummyLabel = new Label(applyButtonComp, SWT.NONE); 
        dummyLabel.setText("");
        dummyLabel.setLayoutData( new GridData( textWidth, textHeight ) );
        dummyLabel.setVisible(false); 
        applyButton = new Button(applyButtonComp, SWT.NONE); 
        applyButton.setText("Apply"); 
        applyButton.setLayoutData( new GridData( 50, textHeight+15 ) );
        applyButtonSelectionListenerAction(applyButton); 
        applyButton.setVisible(false); 

    	((GridData)gempakGraphicAreaLabel.getLayoutData()).exclude = true; 
    	((GridData)gempakGraphicAreaText.getLayoutData()).exclude = true; 
    	((GridData)gempakProjectionLabel.getLayoutData()).exclude = true; 
    	((GridData)gempakProjectionText.getLayoutData()).exclude = true; 
    	((GridData)dummyLabel.getLayoutData()).exclude = true; 
        
    	gempakInputButtonSelectionListenerAction(gempakInputButton) ; 
    	
        Composite projComp = new Composite(dlgComp, SWT.NONE);
        layout = new GridLayout(2, false);
        projComp.setLayout(layout);

        factory = new DefaultMathTransformFactory();
        Set<?> methods = factory.getAvailableMethods(Projection.class);

        String[] projections = new String[methods.size()];
        int i = 0;
        for (Object obj : methods) {
            if (obj instanceof MapProjection.AbstractProvider) {
                MapProjection.AbstractProvider prj = (MapProjection.AbstractProvider) obj;
                projections[i++] = prj.getName().getCode();
            }
        }
        Arrays.sort(projections);

        new Label(projComp, SWT.NONE).setText("Projection:");
        projList = new Combo(projComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        projList.setItems(projections);
        projList.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                handleProjectionSelection();
            }
        });

//        projList.addModifyListener(new ModifyListener() {
//        	public void modifyText(ModifyEvent e) {
//        		handleProjectionSelection(); 
//        	}
//        });
        
        
        paramGroup = new Group(dlgComp, SWT.BORDER);
        paramGroup.setText("Parameters");
        layout = new GridLayout(1, true);
        paramGroup.setLayout(layout);
        paramGroup.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_FILL
                | GridData.GRAB_HORIZONTAL));
//        layout = new GridLayout(2, false);
//        paramGroup.setLayout(layout);
//        paramGroup.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        ScrolledComposite paramScroll = new ScrolledComposite(paramGroup,
                SWT.V_SCROLL);
        layout = new GridLayout(1, true);
        paramScroll.setLayout(layout);
        paramScroll.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_FILL
                | GridData.GRAB_HORIZONTAL));

        paramComp = new Composite(paramScroll, SWT.NONE);
        paramScroll.setContent(paramComp);
        layout = new GridLayout(2, true);
        paramComp.setLayout(layout);

        if(projectionListInitValue == null) 
        	projectionListInitValue = projections[0]; 
        projList.setText(projectionListInitValue);
 //       handleProjectionSelection();  //disabled now. Not sure why it is called here. Now
        //this call throws null point exception because OK button was not created yet and 
        //validateParameters() sets okButton.setEnabled(false)

        /*
         * Now construct 'Define Extent By' block
         */
        Group defineByGroup = new Group(dlgComp, SWT.BORDER_SOLID);
        defineByGroup.setText("Define Extent by ");
        layout = new GridLayout(2, true);
        defineByGroup.setLayout(layout);
        defineByGroup.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_FILL
                | GridData.GRAB_HORIZONTAL));

        cornersBtn = new Button(defineByGroup, SWT.RADIO);
        cornersBtn.setText("Corners");
        cornersBtn.setSelection(true);

        centerBtn = new Button(defineByGroup, SWT.RADIO);
        centerBtn.setText("Center");
        
        /*
         * Now construct the corner section of the 'Extent' block
         */
        cornersGroup = new Group(dlgComp, SWT.BORDER);
        cornersGroup.setText("Extent");
//        layout = new GridLayout(1, true);
        layout = new GridLayout(3, false);
        cornersGroup.setLayout(layout);
        cornersGroup.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_FILL
                | GridData.GRAB_HORIZONTAL));

//        Composite extentComp = new Composite(cornersGroup, SWT.NONE);
//        layout = new GridLayout(3, true);
//        extentComp.setLayout(layout);
//        new Label(extentComp, SWT.NONE).setText("");
//        new Label(extentComp, SWT.NONE).setText("Latitude:");
//        new Label(extentComp, SWT.NONE).setText("Longitude:");
//
//        int latLonTextWidth = 60; 
//        int latLonTextHeight = 15; 
//
//        new Label(extentComp, SWT.NONE).setText("Lower Left:");
//        llLatText = new Text(extentComp, SWT.BORDER);
//        llLonText = new Text(extentComp, SWT.BORDER);
//        llLatText.setLayoutData( new GridData( latLonTextWidth, latLonTextHeight ) );
//        llLonText.setLayoutData( new GridData( latLonTextWidth, latLonTextHeight ) );
//
//        new Label(extentComp, SWT.NONE).setText("Upper Right:");
//        urLatText = new Text(extentComp, SWT.BORDER);
//        urLonText = new Text(extentComp, SWT.BORDER);
//        urLatText.setLayoutData( new GridData( latLonTextWidth, latLonTextHeight ) );
//        urLonText.setLayoutData( new GridData( latLonTextWidth, latLonTextHeight ) );

        new Label(cornersGroup, SWT.NONE).setText("");
        new Label(cornersGroup, SWT.NONE).setText("Latitude:");
        new Label(cornersGroup, SWT.NONE).setText("Longitude:");

        new Label(cornersGroup, SWT.NONE).setText("Lower Left:");
        llLatText = new Text(cornersGroup, SWT.BORDER);
        llLonText = new Text(cornersGroup, SWT.BORDER);

        new Label(cornersGroup, SWT.NONE).setText("Upper Right:");
        urLatText = new Text(cornersGroup, SWT.BORDER);
        urLonText = new Text(cornersGroup, SWT.BORDER);
        /*
         * Add modify listener to all llLatText, llLonText, urLatText, urLonText
         */
        lowerLeftAndUpperRightLatLonTextListenerAction(llLatText); 
        lowerLeftAndUpperRightLatLonTextListenerAction(llLonText); 
        lowerLeftAndUpperRightLatLonTextListenerAction(urLatText); 
        lowerLeftAndUpperRightLatLonTextListenerAction(urLonText); 
        
        /*
         * Now construct the center section of the 'Extent' block
         */
        
        centerGroup = new Group(dlgComp, SWT.BORDER);
        centerGroup.setText("Extent");
        layout = new GridLayout(3, false);
        centerGroup.setLayout(layout);
        centerGroup.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_FILL
                | GridData.GRAB_HORIZONTAL));

        new Label(centerGroup, SWT.NONE).setText("");
        new Label(centerGroup, SWT.NONE).setText("Latitude:");
        new Label(centerGroup, SWT.NONE).setText("Longitude:");

        new Label(centerGroup, SWT.NONE).setText("Center:");
        centerLatText = new Text(centerGroup, SWT.BORDER);
        centerLonText = new Text(centerGroup, SWT.BORDER);

        new Label(centerGroup, SWT.NONE).setText("Width:");
        widthText = new Text(centerGroup, SWT.BORDER);
        widthUnit = new Label(centerGroup, SWT.NONE);
        widthUnit.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_FILL
                | GridData.GRAB_HORIZONTAL));

        new Label(centerGroup, SWT.NONE).setText("Height:");
        heightText = new Text(centerGroup, SWT.BORDER);
        heightUnit = new Label(centerGroup, SWT.NONE);
        heightUnit.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_FILL
                | GridData.GRAB_HORIZONTAL));

        centerGroup.setVisible(false);
        ((GridData) centerGroup.getLayoutData()).exclude = true;

        centerBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (centerBtn.getSelection()) {

                    centerGroup.setVisible(true);
                    ((GridData) centerGroup.getLayoutData()).exclude = false;

                    cornersGroup.setVisible(false);
                    ((GridData) cornersGroup.getLayoutData()).exclude = true;

                    dlgComp.layout(); 

                    validateParameters();

//                    getShell().pack();
                }
            }
        });

        cornersBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (cornersBtn.getSelection()) {
                    centerGroup.setVisible(false);
                    ((GridData) centerGroup.getLayoutData()).exclude = true;

                    cornersGroup.setVisible(true);
                    ((GridData) cornersGroup.getLayoutData()).exclude = false;

                    dlgComp.layout(); 

                    validateParameters();

//                    getShell().pack();
                }
            }
        });
        
        Group validGroup = new Group(dlgComp, SWT.BORDER);
        layout = new GridLayout(2, false);
        validGroup.setLayout(layout);
        validGroup.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        validateBtn = new Button(validGroup, SWT.PUSH);
        validateBtn.setText("Validate");
        validateBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                validateParameters();
            }

        });

        validationText = new Text(validGroup, SWT.READ_ONLY | SWT.WRAP
                | SWT.BORDER | SWT.V_SCROLL);
        GridData layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        layoutData.minimumHeight = validationText.getLineHeight() * 3;
        validationText.setLayoutData(layoutData);
        
        //initialize the lat/long values using the current view extent
        initLatLonUsingCurrentViewExtent(); 

        applyDialogFont(dlgComp);  
        /*
         * move this call from the middle of the method to the bottom because 
         * some uninitialized parameters may throw NULL point exceptions if it 
         * is called too early. 
         */
//        handleProjectionSelection(); 
//        dlgComp.layout(); 
        return dlgComp;
    }
    
//    private void displayTextSize(Text text, String textTitle) {
//    	Point point = text.getSize(); 
//    	System.out.println("The size of Text := " + textTitle + "are,  x="+point.x + " and Y=" + point.y); 
//    }
    
    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveJFACEDialog#createContents(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected Control createContents(Composite parent) {
        Control contents = super.createContents(parent);
        handleProjectionSelection();
        return contents;
    }

    private void validateParameters() {
        crs = null;
        Button okButton = getButton(IDialogConstants.OK_ID);
        if(okButton != null)
        	okButton.setEnabled(false);

        // update edited parameter values
        for (Map.Entry<Parameter<?>, ParamUI> entry : paramMap.entrySet()) {
            if (!"null".equals(entry.getValue().text.getText())) {
                Parameter<?> param = entry.getKey();

                ParameterDescriptor<?> pd = param.getDescriptor();

                Class<?> clazz = pd.getValueClass();

                String text = entry.getValue().text.getText();

                try {
                    if (clazz.equals(Double.class)) {
                        Double d = Double.parseDouble(text);
                        param.setValue(d);
                    } else {
                        MessageBox mb = new MessageBox(this.getShell(),
                                SWT.ICON_ERROR);
                        mb.setMessage("Unexpected paramter type: "
                                + clazz.getSimpleName());
                        mb.setText("Error");
                        mb.open();
                        entry.getValue().text.setFocus();
                        return;
                    }
                } catch (NumberFormatException e) {
                    MessageBox mb = new MessageBox(this.getShell(),
                            SWT.ICON_ERROR);
                    mb.setMessage('"' + text + '"' + " is not a valid "
                            + clazz.getSimpleName() + " value.");
                    mb.setText("Error");
                    mb.open();
                    entry.getValue().text.setFocus();
                    return;
                } catch (InvalidParameterValueException e) {
                    MessageBox mb = new MessageBox(this.getShell(),
                            SWT.ICON_ERROR);
                    mb.setMessage(e.getMessage());
                    mb.setText("Error");
                    mb.open();
                    entry.getValue().text.setFocus();
                    return;
                }
            }
        }

        String name = parameters.getDescriptor().getName().getCode();
        try {
            crs = MapUtil.constructProjection(name, parameters);
        } catch (Exception e) {
            crs = null;
            validationText.setText(e.getLocalizedMessage());
        }

        if (crs == null) {
            widthUnit.setText("(?)");
            heightUnit.setText("(?)");

            setEnabledGroup(cornersGroup, false);
            setEnabledGroup(centerGroup, false);
            return;
        }

        CoordinateSystem cs = crs.getCoordinateSystem();

        widthUnit.setText('(' + cs.getAxis(0).getUnit().toString() + ')');
        heightUnit.setText('(' + cs.getAxis(1).getUnit().toString() + ')');

        setEnabledGroup(cornersGroup, true);
        setEnabledGroup(centerGroup, true);

        validateExtent();
        if (newMapGeom == null) {
            return;
        }

        validationText.setText("Valid");
        okButton.setEnabled(true);
    }
    
    /**
     * Helper method to set the dialog window title
     */
	private void setDialogTitleText(String dialogTitleText) {
		Shell shell = getShell(); 
		shell.setText(dialogTitleText); 
	}

    private void setEnabledGroup(Group group, boolean enabled) {
        group.setEnabled(enabled);
        for (Control control : group.getChildren()) {
            control.setEnabled(enabled);
        }
    }

    private void validateExtent() {
        newMapGeom = null;

        if (crs == null) {
            return;
        }

        CoordinateSystem cs = crs.getCoordinateSystem();

        Coordinate ll = null;
        Coordinate ur = null;
        Coordinate center = null;
        double width = -1;
        double height = -1;
        if (cornersBtn.getSelection()) {
            // get lat/lon extent
            ll = new Coordinate();
            ll.y = validateDouble(llLatText.getText(), -90, 90);
            if (Double.isNaN(ll.y)) {
                llLatText.setFocus();
                return;
            }

            ll.x = validateDouble(llLonText.getText(), -180, 180);
            if (Double.isNaN(ll.x)) {
                llLonText.setFocus();
                return;
            }

            ur = new Coordinate();
            ur.y = validateDouble(urLatText.getText(), -90, 90);
            if (Double.isNaN(ur.y)) {
                urLatText.setFocus();
                return;
            }

            ur.x = validateDouble(urLonText.getText(), -180, 180);
            if (Double.isNaN(ur.x)) {
                urLonText.setFocus();
                return;
            }
        } else if (centerBtn.getSelection()) {
            center = new Coordinate();
            center.y = validateDouble(centerLatText.getText(), -90, 90);
            if (Double.isNaN(center.y)) {
                centerLatText.setFocus();
                return;
            }

            center.x = validateDouble(centerLonText.getText(), -180, 180);
            if (Double.isNaN(center.x)) {
                centerLonText.setFocus();
                return;
            }

            CoordinateSystemAxis axis = cs.getAxis(0);
            width = validateDouble(widthText.getText(), 1,
                    axis.getMaximumValue() - axis.getMinimumValue());
            if (Double.isNaN(width)) {
                widthText.setFocus();
                return;
            }

            axis = cs.getAxis(1);
            height = validateDouble(heightText.getText(), 1,
                    axis.getMaximumValue() - axis.getMinimumValue());
            if (Double.isNaN(height)) {
                heightText.setFocus();
                return;
            }
        }

        // create the new projection
        try {
            if (cornersBtn.getSelection()) {
                newMapGeom = MapDescriptor.createGridGeometry(crs, ll, ur);
            } else if (centerBtn.getSelection()) {
                newMapGeom = MapDescriptor.createGridGeometry(crs, center,
                        width, height);
            }

        } catch (Exception e) {
            validationText.setText(e.getLocalizedMessage());
            return;
        }

    }

	/**
	 * Helper method, initialize the lat/lon values using the current view extent
	 */
	private void initLatLonUsingCurrentViewExtent() {
    	DisplayViewLowerLeftAndUpperRightLongLatValues longLatValuesInstance = 
    		DisplayViewLowerLeftAndUpperRightLongLatValues.getInstance(); 
    	if(!Double.isNaN(longLatValuesInstance.getLowerLeftLatitude()))
    		llLatText.setText(doubleValueFormater(longLatValuesInstance.getLowerLeftLatitude(), 3)); 
    	if(!Double.isNaN(longLatValuesInstance.getLowerLeftLongitude()))
    		llLonText.setText(doubleValueFormater(longLatValuesInstance.getLowerLeftLongitude(), 3)); 
    	if(!Double.isNaN(longLatValuesInstance.getUpperRightLatitude()))
    		urLatText.setText(doubleValueFormater(longLatValuesInstance.getUpperRightLatitude(), 3)); 
    	if(!Double.isNaN(longLatValuesInstance.getUpperRightLongitude()))
    		urLonText.setText(doubleValueFormater(longLatValuesInstance.getUpperRightLongitude(), 3)); 
	}
	
	/**
	 * taking the current llLat/Lon and urLat/Lon values to update the values in
	 * the singleton DisplayViewLowerLeftAndUpperRightLongLatValues
	 */
	private void updateValuesInDisplayViewLowerLeftAndUpperRightLongLatValues(String llLon, 
			String llLat, String urLon, String urLat) {
    	DisplayViewLowerLeftAndUpperRightLongLatValues longLatValuesInstance = 
    		DisplayViewLowerLeftAndUpperRightLongLatValues.getInstance(); 
    	longLatValuesInstance.updateLatLonValues(llLon, llLat, urLon, urLat); 
	}
	
	/**
	 * help class to format double value for display
	 */
    private String doubleValueFormater(double doubleValue, int decimalLength) {
    	NumberFormat formatter = null; 
    	if(decimalLength > 2)
    		formatter = new DecimalFormat(THREE_DECIMAL_DIGIT_PATTERN); 
    	else 
    		formatter = new DecimalFormat(TWO_DECIMAL_DIGIT_PATTERN); 
    		
    	return formatter.format(doubleValue); 
    }

    /**
     * Add selection listener to the gempak input button
     */
    private void gempakInputButtonSelectionListenerAction(final Button gempakButton) {
    	gempakButton.addSelectionListener( new SelectionAdapter() {
        	boolean visible = false; 
            public void widgetSelected(SelectionEvent event) {     
            	visible = !visible; 
            	if(visible == true) {
            		gempakButton.setText("Hide GEMPAK..."); 
            	} else {
            		gempakButton.setText("GEMPAK..."); 
            	}
            	gempakGraphicAreaLabel.setVisible(true); 
            	((GridData)gempakGraphicAreaLabel.getLayoutData()).exclude = !visible; 
            	/*
            	 * disabled the following two lines for R1G2-9 version of AWIPS II. The detail
            	 * explanation is with the method getCurrentMapExtentForGareaField(). 
            	 */
//            	String gareaFieldStringForCurrentLoadedMap = getCurrentMapExtentForGareaField(); 
//            	gempakGraphicAreaText.setText(gareaFieldStringForCurrentLoadedMap); 
            	gempakGraphicAreaText.setVisible(true); 
            	((GridData)gempakGraphicAreaText.getLayoutData()).exclude = !visible; 
            	
            	gempakProjectionLabel.setVisible(true); 
            	((GridData)gempakProjectionLabel.getLayoutData()).exclude = !visible; 
            	if(gempakProjectionTextString != null) 
            		gempakProjectionText.setText(gempakProjectionTextString); 
            	gempakProjectionText.setVisible(true);  
            	((GridData)gempakProjectionText.getLayoutData()).exclude = !visible; 
            	
            	dummyLabel.setVisible(true); 
            	((GridData)dummyLabel.getLayoutData()).exclude = !visible; 
            	
            	applyButton.setVisible(true);
            	((GridData)applyButton.getLayoutData()).exclude = !visible; 
            	
            	gempakFieldComp.layout(); 
                paramComp.setSize(paramComp.computeSize(SWT.DEFAULT, SWT.DEFAULT));
            	paramComp.layout();
                dlgComp.layout(); 
            }
        });
    }
    
    /*
     * Due to M1G2-9 migration, the initialization of the singleton class 
     * DisplayViewLowerLeftAndUpperRightLongLatValues is commented out in NOAA's
     * perspectives project. Class: NCPerspective 
     * Method: public boolean handleMouseWheel(Event event, int x, int y)
     * Thus, dynamic updating Custom Porjection's Garea field is disabled at 
     * this moment. Comments by Michael Gao on April 6, 2011
     */
//    private String getCurrentMapExtentForGareaField() {
//    	DisplayViewLowerLeftAndUpperRightLongLatValues lowerLeftAndUpperRightLatLonValues = 
//    		DisplayViewLowerLeftAndUpperRightLongLatValues.getInstance(); 
//    	StringBuilder stringBuilder = new StringBuilder(); 
//    	if(isDoubleValueValid(lowerLeftAndUpperRightLatLonValues.getLowerLeftLatitude()) &&
//    			isDoubleValueValid(lowerLeftAndUpperRightLatLonValues.getLowerLeftLongitude()) &&
//    			isDoubleValueValid(lowerLeftAndUpperRightLatLonValues.getUpperRightLatitude()) &&
//    			isDoubleValueValid(lowerLeftAndUpperRightLatLonValues.getUpperRightLongitude())) {
//    		stringBuilder.append(doubleValueFormater(lowerLeftAndUpperRightLatLonValues.getLowerLeftLatitude(), 3))
//    					 .append(";")
//    					 .append(doubleValueFormater(lowerLeftAndUpperRightLatLonValues.getLowerLeftLongitude(), 3))
//    					 .append(";")
//    					 .append(doubleValueFormater(lowerLeftAndUpperRightLatLonValues.getUpperRightLatitude(), 3))
//    					 .append(";")
//    					 .append(doubleValueFormater(lowerLeftAndUpperRightLatLonValues.getUpperRightLongitude(), 3)); 
//    	}
//    	return stringBuilder.toString(); 
//    }
    
	/**
	 * add selection listener to the button
	 */
	private void applyButtonSelectionListenerAction(Button applyButton) {
		applyButton.addSelectionListener( new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {               
            	String gempakProjectionString = gempakProjectionText.getText(); 
            	String gempakGraphicAreaString = gempakGraphicAreaText.getText(); 

            	String geogFilePath = NcPathManager.getInstance().getStaticFile(
            			NcPathConstants.GEOG_TBL ).getAbsolutePath();
            	String stationFilePath =  NcPathManager.getInstance().getStaticFile(
            			NcPathConstants.SFSTNS_TBL ).getAbsolutePath();
      	
            	ICustomProjectionService customProjectionService = new CustomProjectionServiceImpl(gempakProjectionString, gempakGraphicAreaString, 
            			geogFilePath, stationFilePath ); 

            	if(!customProjectionService.isGempakProjectionStringValid()) {
            		gempakProjectionText.setFocus(); 
            		return; 
            	} else if(!customProjectionService.isGempakGraphicsAreaStringValid()) {
            		gempakGraphicAreaText.setFocus(); 
            		return; 
            	}

            	/*
            	 * Store the valid projection text string in the instance variable 
            	 */
            	if(gempakProjectionString != null)
            		gempakProjectionTextString = gempakProjectionString; 
            	
            	setProjectionWithoutAngleValues(customProjectionService.isProjectionWithoutAngleValues()); 
            	
            	if(customProjectionService.isGraphicAreaTextWithValidGeogName()) {
            		handleProjectionTextWithValidGeogAbbreviation(customProjectionService); 
            	} else if(customProjectionService.isGraphicAreaTextWithValidStationName()) {
            		handleProjectionTextWithValidStationAbbreviation(customProjectionService);
            	} else {
            		handleGraphicAreaWithLatLonOnlyValues(customProjectionService); 
            	}
            	
            	validateParameters();
            }
        });
	}
	
	/**
	 * This method indicates if the graphic area text includes a valid 
	 * abbreviation name contained in geog.tbl table
	 */
	private boolean isGraphicAreaTextWithValidGeogName(GraphicsAreaCoordinates gAreaCoordinatesObject) {
		return gAreaCoordinatesObject.isValidGeogName(); 
	}
	
	/**
	 * This method indicates if the graphic area text includes a valid 
	 * abbreviation name contained in sfstns.tbl table
	 */
	private boolean isGraphicAreaTextWithValidStationName(GraphicsAreaCoordinates gAreaCoordinatesObject) {
		return gAreaCoordinatesObject.isValidStationName(); 
	}
	
	/**
	 * 
	 * @param graphicsAreaCoordinatesObject
	 * @param projectionValuesObject
	 */
//	private void handleProjectionTextWithValidGeogAbbreviation(GraphicsAreaCoordinates graphicsAreaCoordinatesObject, 
//			GempakProjectionValues projectionValuesObject) {
	private void handleProjectionTextWithValidGeogAbbreviation(ICustomProjectionService customProjectionService) {
		setLatLonTextFieldForCornerUsingGraphicsAreaCoordinates(customProjectionService); 
		setLatLonTextFieldForCenterUsingGraphicsAreaCoordinates(customProjectionService); 

		if(customProjectionService.isDefaultProjectionBeingUsed()) { 
//			projectionValuesObject = new GempakProjectionValues(graphicsAreaCoordinatesObject.getMapProjectionString()); 
//			setProjectionWithoutAngleValues(projectionValuesObject.isProjectionStringWithoutAngleValues()); 
			customProjectionService.handleValidGeogName(); 
		}
//		handleProjectionStringSetting(graphicsAreaCoordinatesObject, projectionValuesObject); 
		handleProjectionStringSetting(customProjectionService); 
	}
	
	/**
	 * 
	 * @param graphicsAreaCoordinatesObject
	 * @param projectionValuesObject
	 */
//	private void handleProjectionTextWithValidStationAbbreviation(GraphicsAreaCoordinates graphicsAreaCoordinatesObject, 
//			GempakProjectionValues projectionValuesObject) {
	private void handleProjectionTextWithValidStationAbbreviation(ICustomProjectionService customProjectionService) {
		setLatLonTextFieldForCornerUsingGraphicsAreaCoordinates(customProjectionService); 
		setLatLonTextFieldForCenterUsingGraphicsAreaCoordinates(customProjectionService); 

//		handleProjectionStringSetting(graphicsAreaCoordinatesObject, projectionValuesObject); 
		handleProjectionStringSetting(customProjectionService); 
	}

	/**
	 * This method retrieves values from GraphicAreaCoordinates object, 
	 * then updates GEOTool's related GUI fields using those values
	 */
//	private void handleGraphicAreaWithLatLonOnlyValues(GraphicsAreaCoordinates graphicsAreaCoordinatesObject, 
//			GempakProjectionValues projectionValuesObject) {
	private void handleGraphicAreaWithLatLonOnlyValues(ICustomProjectionService customProjectionService) {
		if(customProjectionService.isLowerLeftAndUpperRightLatLonValuesValid() || 
				customProjectionService.isCenterDeltaLatLonValuesValid()) {
			setLatLonTextFieldForCornerUsingGraphicsAreaCoordinates(customProjectionService); 
			setLatLonTextFieldForCenterUsingGraphicsAreaCoordinates(customProjectionService); 

//			handleProjectionStringSetting(graphicsAreaCoordinatesObject, projectionValuesObject); 
			handleProjectionStringSetting(customProjectionService); 
		}
	}
	
	/**
	 * set lower left and upper right lat/lon text fields using GraphicsAreaCoordinates
	 */
//	private void setLowerLeftAndUpperRightTextFieldUsingGraphicsAreaCoordinates(GraphicsAreaCoordinates graphicsAreaCoordinatesObject) {
	private void setLatLonTextFieldForCornerUsingGraphicsAreaCoordinates(ICustomProjectionService customProjectionService) {
		llLatText.setText(doubleValueFormater(customProjectionService.getLowerLeftLat(), 3)); 
		llLonText.setText(doubleValueFormater(customProjectionService.getLowerLeftLon(), 3)); 
		urLatText.setText(doubleValueFormater(customProjectionService.getUpperRightLat(), 3)); 
		urLonText.setText(doubleValueFormater(customProjectionService.getUpperRightLon(), 3)); 
	}
	
	private void setLatLonTextFieldForCenterUsingGraphicsAreaCoordinates(ICustomProjectionService customProjectionService) {
		/*
		 * set center lat and lon text areas
		 */
		centerLatText.setText(doubleValueFormater(customProjectionService.getCenterLat(), 3)); 
		centerLonText.setText(doubleValueFormater(customProjectionService.getCenterLon(), 3)); 
	}
	
	/**
	 * 
	 * @param graphicsAreaCoordinatesObject
	 */
//	private void handleProjectionStringSetting(GraphicsAreaCoordinates graphicsAreaCoordinatesObject, 
//			GempakProjectionValues projectionValuesObject) {
	private void handleProjectionStringSetting(ICustomProjectionService customProjectionService) {
		double [] gempakAngleValueArray = null; 
		if(customProjectionService.isDefaultProjectionBeingUsed() || 
				customProjectionService.isProjectionWithoutAngleValues()) {
//			gempakAngleValueArray = getDefaultGempakAngleValueArrayUsingLowerLeftAndUpperRightLatLon(graphicsAreaCoordinatesObject, 
//					projectionValuesObject); 
			gempakAngleValueArray = customProjectionService.getDefaultGempakAngleValueArrayUsingLowerLeftAndUpperRightLatLon(); 
		} else {
//			gempakAngleValueArray = getGempakAngleValueArrayUsingGempakProjectionValuesObject(projectionValuesObject); 
			gempakAngleValueArray = customProjectionService.getGempakAngleValueArrayUsingGempakProjectionValuesObject(); 
		}
		
//		String modifiedGampakProjectionName = buildGempakProjectionName(projectionValuesObject.getGempakProjectionName(), gempakAngleValueArray[0]); 
//		String modifiedGampakProjectionName = buildGempakProjectionName(customProjectionService.getGempakProjectionName(), gempakAngleValueArray[0]); 
		String modifiedGampakProjectionName = GempakProjectionValuesUtil.getModifiedGempakProjectionName(customProjectionService.getGempakProjectionName(), 
				gempakAngleValueArray[0]); 
		String geotoolsProjectionName = GempakProjectionValuesUtil.getGeotoolProjectionName(modifiedGampakProjectionName);
		if(GempakProjectionValuesUtil.isCylindricalProjectionForGeotools(geotoolsProjectionName))
			handleParameterValueAndViewModificationForCylindrical(geotoolsProjectionName, gempakAngleValueArray); 
		else if(GempakProjectionValuesUtil.isAzmProjectionForGeotools(geotoolsProjectionName))
			handleParameterValueAndViewModificationForAzm(geotoolsProjectionName, gempakAngleValueArray); 
		else if(GempakProjectionValuesUtil.isConProjectionForGeotools(geotoolsProjectionName))
			handleParameterValueAndViewModificationForCon(geotoolsProjectionName, gempakAngleValueArray); 
	}
	
	private String buildGempakProjectionName(String gampakProjectionName, double angle1) {
		String modifiedProjectionName = gampakProjectionName; 
		if("STR".equalsIgnoreCase(gampakProjectionName) && 
				!Double.isNaN(angle1) && angle1 != 90.0 && angle1 != -90) {
			modifiedProjectionName = modifiedProjectionName + "_OBLIQUE"; 
		}
		return modifiedProjectionName; 
	}
	
	private double[] getDefaultGempakAngleValueArrayUsingLowerLeftAndUpperRightLatLon(GraphicsAreaCoordinates graphicsAreaCoordinatesObject, 
			GempakProjectionValuesUtil projectionValuesObject) {
		double [] gempakAngleValueArray = createDoubleArray(3);
		if(graphicsAreaCoordinatesObject != null && projectionValuesObject != null && 
				!isStringEmpty(projectionValuesObject.getGempakProjectionName())) {
			String gempakProjectionName = projectionValuesObject.getGempakProjectionName(); 
			logger.debug("==method: getDefaultGempakAngleValueArrayUsingLowerLeftAndUpperRightLatLon==, gempakProjectionName="+gempakProjectionName); 
			gempakAngleValueArray[0] = graphicsAreaCoordinatesObject.getDefaultAngle1UsingGempakProjectionName(gempakProjectionName);
			gempakAngleValueArray[1] = graphicsAreaCoordinatesObject.getDefaultAngle2UsingGempakProjectionName(gempakProjectionName);
			if(projectionValuesObject.isGempakConProjection(gempakProjectionName)) {
				gempakAngleValueArray[2] = graphicsAreaCoordinatesObject.getDefaultAngle1UsingGempakProjectionName(gempakProjectionName);			}
		}
		return gempakAngleValueArray; 
	}

	private double[] getGempakAngleValueArrayUsingGempakProjectionValuesObject(GempakProjectionValuesUtil projectionValuesObject) {
		double [] gempakAngleValueArray = createDoubleArray(3);
		if(projectionValuesObject != null) {
			if("TVM".equalsIgnoreCase(projectionValuesObject.getGempakProjectionName())) {
				gempakAngleValueArray[0] = projectionValuesObject.getAngle2(); 
				gempakAngleValueArray[1] = projectionValuesObject.getAngle1(); 
				gempakAngleValueArray[2] = projectionValuesObject.getAngle3(); 
			} else {
				gempakAngleValueArray[0] = projectionValuesObject.getAngle1(); 
				gempakAngleValueArray[1] = projectionValuesObject.getAngle2(); 
				gempakAngleValueArray[2] = projectionValuesObject.getAngle3(); 
			}
		}
		return gempakAngleValueArray; 
	}
	
	private double[] getDefaultGempakAngleValueArrayUsingLowerLeftAndUpperRightLatLonTextFields(String geotoolsProjectionName, 
			Text lowerLeftLatText, Text lowerLeftLonText, Text upperRightLatText, Text upperRightLonText) {
		double [] gempakAngleValueArray = createDoubleArray(3);
		DisplayViewLowerLeftAndUpperRightLongLatValues viewExtentLatLonObject = DisplayViewLowerLeftAndUpperRightLongLatValues.getInstance(); 
		if(!Double.isNaN(viewExtentLatLonObject.getLowerLeftLatitude()) &&
				!Double.isNaN(viewExtentLatLonObject.getLowerLeftLongitude()) &&
				!Double.isNaN(viewExtentLatLonObject.getUpperRightLatitude()) &&
				!Double.isNaN(viewExtentLatLonObject.getUpperRightLongitude())) {
			
	        String lowLeftAndUpperRightLatLonString = concatenateLatLonStringUsingDoubleValues(viewExtentLatLonObject.getLowerLeftLatitude(), 
	        		viewExtentLatLonObject.getLowerLeftLongitude(), 
	        		viewExtentLatLonObject.getUpperRightLatitude(),
	        		viewExtentLatLonObject.getUpperRightLongitude()); 
	        GraphicsAreaCoordinates graphicsAreaCoordinatesObject = new GraphicsAreaCoordinates(lowLeftAndUpperRightLatLonString); 
	        boolean isGraphicAreaStringValid = graphicsAreaCoordinatesObject.parseGraphicsAreaString(lowLeftAndUpperRightLatLonString); 
	        graphicsAreaCoordinatesObject.setGraphicsAreaStrValid(isGraphicAreaStringValid); 
	        
			gempakAngleValueArray[0] = graphicsAreaCoordinatesObject.getDefaultAngle1UsingGeotoolsProjectionName(geotoolsProjectionName);
			gempakAngleValueArray[1] = graphicsAreaCoordinatesObject.getDefaultAngle2UsingGeotoolsProjectionName();
			if(GempakProjectionValuesUtil.isConProjectionForGeotools(geotoolsProjectionName)) {
				gempakAngleValueArray[2] = graphicsAreaCoordinatesObject.getDefaultAngle1UsingGeotoolsProjectionName(geotoolsProjectionName);		
			}
		}
		return gempakAngleValueArray; 
	}
	
	/**
	 * 
	 * @param projectionValuesObject
	 */
	private void handleParameterValueAndViewModificationForCylindrical(String geotoolsProjectionName, double[] angleValueArray) {
    	if(geotoolsProjectionName == null)
    		return; 
    	
    	projectionListInitValue = geotoolsProjectionName; 
    	
        try {
        	cleanUpParamMap(paramMap); 

            projList.setText(geotoolsProjectionName); 
            
            parameters = factory.getDefaultParameters(projList.getText());

            for (Object obj : parameters.values()) {
                Parameter<?> param = (Parameter<?>) obj;
                String paramName = param.getDescriptor().getName().getCode();
                Object paramValue = param.getValue();

                Label label = new Label(paramComp, SWT.NONE);
                label.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_FILL
                        | GridData.GRAB_HORIZONTAL));
                label.setText(paramName + " (" + param.getUnit() + ")");

                if (("semi_major".equals(paramName) || "semi_minor"
                        .equals(paramName))
                        && (paramValue == null)) {
                    paramValue = MapUtil.AWIPS_EARTH_RADIUS;
                } else if("latitude_of_origin".equals(paramName)) {
                	if(!Double.isNaN(angleValueArray[0]))
                		paramValue = angleValueArray[0]; 
                } else if("central_meridian".equals(paramName)) {
               	if(!Double.isNaN(angleValueArray[1]))
                		paramValue = angleValueArray[1]; 
                }

                Text text = new Text(paramComp, SWT.BORDER);
                text.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_FILL
                        | GridData.GRAB_HORIZONTAL));
                paramMap.put(param, new ParamUI(label, text));

                text.setText("" + paramValue);
            }
            paramComp.layout();
            paramComp.setSize(paramComp.computeSize(SWT.DEFAULT, SWT.DEFAULT));

        } catch (NoSuchIdentifierException e) {
            statusHandler.handle(Priority.PROBLEM, 
                    "Unexpected error retrieving parameters for projection", e);
        }
    }
	
	/**
	 * 
	 * @param projectionValuesObject
	 */
	private void handleParameterValueAndViewModificationForAzm(String geotoolsProjectionName, double[] angleValueArray) {
     	if(geotoolsProjectionName == null)
    		return; 
    	
    	projectionListInitValue = geotoolsProjectionName; 
    	
        try {
        	cleanUpParamMap(paramMap); 

            projList.setText(geotoolsProjectionName); 
            
            parameters = factory.getDefaultParameters(projList.getText());

            for (Object obj : parameters.values()) {
                Parameter<?> param = (Parameter<?>) obj;
                String paramName = param.getDescriptor().getName().getCode();
                Object paramValue = param.getValue();

                Label label = new Label(paramComp, SWT.NONE);
                label.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_FILL
                        | GridData.GRAB_HORIZONTAL));
                label.setText(paramName + " (" + param.getUnit() + ")");

                if (("semi_major".equals(paramName) || "semi_minor"
                        .equals(paramName))
                        && (paramValue == null)) {
                    paramValue = MapUtil.AWIPS_EARTH_RADIUS;
                } else if("latitude_of_origin".equals(paramName)) {
                	if(!Double.isNaN(angleValueArray[0]))  //get angle 1
                		paramValue = angleValueArray[0]; 
                } else if("central_meridian".equals(paramName)) {
                	if(!Double.isNaN(angleValueArray[1]))  //get angle 2
                		paramValue = angleValueArray[1]; 
                } else if("latitude_of_center".equals(paramName)) {
                	if(!Double.isNaN(angleValueArray[0]))
                		paramValue = angleValueArray[0]; 
                } else if("longitude_of_center".equals(paramName)) {
                	if(!Double.isNaN(angleValueArray[1]))  //get angle 2
                		paramValue = angleValueArray[1]; 
                }

                Text text = new Text(paramComp, SWT.BORDER);
                text.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_FILL
                        | GridData.GRAB_HORIZONTAL));
                paramMap.put(param, new ParamUI(label, text));

                text.setText("" + paramValue);
            }
            paramComp.layout();
            paramComp.setSize(paramComp.computeSize(SWT.DEFAULT, SWT.DEFAULT));

        } catch (NoSuchIdentifierException e) {
            statusHandler.handle(Priority.PROBLEM, 
                    "Unexpected error retrieving parameters for projection", e);
        }
    }
	
	private void handleParameterValueAndViewModificationForCon(String geotoolsProjectionName, double[] angleValueArray) {
    	if(geotoolsProjectionName == null)
    		return; 
    	
    	projectionListInitValue = geotoolsProjectionName; 
    	
        try {
        	cleanUpParamMap(paramMap); 

            projList.setText(geotoolsProjectionName); 
            
            parameters = factory.getDefaultParameters(projList.getText());

            for (Object obj : parameters.values()) {
                Parameter<?> param = (Parameter<?>) obj;
                String paramName = param.getDescriptor().getName().getCode();
                Object paramValue = param.getValue();

                Label label = new Label(paramComp, SWT.NONE);
                label.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_FILL
                        | GridData.GRAB_HORIZONTAL));
                label.setText(paramName + " (" + param.getUnit() + ")");

                if (("semi_major".equals(paramName) || "semi_minor"
                        .equals(paramName))
                        && (paramValue == null)) {
                    paramValue = MapUtil.AWIPS_EARTH_RADIUS;
                } else if("latitude_of_origin".equals(paramName)) {
                	if(!Double.isNaN(angleValueArray[0]))
                		paramValue = angleValueArray[0]; 
                } else if("central_meridian".equals(paramName)) {
                	if(!Double.isNaN(angleValueArray[1]))
                		paramValue = angleValueArray[1]; 
                } else if("standard_parallel_1".equals(paramName)) {
                	if(!Double.isNaN(angleValueArray[2]))
                		paramValue = angleValueArray[2]; 
                } 

                Text text = new Text(paramComp, SWT.BORDER);
                text.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_FILL
                        | GridData.GRAB_HORIZONTAL));
                paramMap.put(param, new ParamUI(label, text));

                text.setText("" + paramValue);
            }
            paramComp.layout();
            paramComp.setSize(paramComp.computeSize(SWT.DEFAULT, SWT.DEFAULT));

        } catch (NoSuchIdentifierException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unexpected error retrieving parameters for projection", e);
        }
    }
	
    /**
     * helper method to clean up paramMap
     */
    private void cleanUpParamMap(Map<Parameter<?>, ParamUI> targetMap ) {
        for (ParamUI pui : targetMap.values()) {
            pui.label.dispose();
            pui.text.dispose();
        }
        targetMap.clear();
    }
    
    /**
     * 
     */
    private void handleProjectionSelection() {
    	projectionListInitValue = projList.getText(); 
    	
        try {
        	cleanUpParamMap(paramMap); 

            parameters = factory.getDefaultParameters(projList.getText());
//            System.out.println("=============###############  projection Name ="+projList.getText()+" ######################===================="); 

            double[] angleDoubleValueArray = getDefaultGempakAngleValueArrayUsingLowerLeftAndUpperRightLatLonTextFields(projList.getText(), 
        			llLatText, llLonText, urLatText, urLonText); 
            
            for (Object obj : parameters.values()) {
                Parameter<?> param = (Parameter<?>) obj;
                String paramName = param.getDescriptor().getName().getCode();
                Object paramValue = param.getValue();

                Label label = new Label(paramComp, SWT.NONE);
//                Label label = new Label(paramGroup, SWT.NONE);
                label.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_FILL
                        | GridData.GRAB_HORIZONTAL));
                label.setText(paramName + " (" + param.getUnit() + ")");

                if (("semi_major".equals(paramName) || "semi_minor"
                        .equals(paramName))
                        && (paramValue == null)) {
                    paramValue = MapUtil.AWIPS_EARTH_RADIUS;
                } else {
                	if(GempakProjectionValuesUtil.isCylindricalProjectionForGeotools(projList.getText()))
                		paramValue = updateSingleParameterValueForCylindricalProjection(param, 
                				angleDoubleValueArray); 
                	else if(GempakProjectionValuesUtil.isAzmProjectionForGeotools(projList.getText()))
                		paramValue = updateSingleParameterValueForAzmProjection(param, 
                				angleDoubleValueArray);  
                	else if(GempakProjectionValuesUtil.isConProjectionForGeotools(projList.getText()))
                		paramValue = updateSingleParameterValueForConProjection(param, 
                				angleDoubleValueArray); 
                }
//                System.out.println("==========================="); 
//                System.out.println("============= paramName="+paramName); 
//                System.out.println("============= paramValue="+paramValue); 
//                System.out.println("==========================="); 

                Text text = new Text(paramComp, SWT.BORDER);
//                Text text = new Text(paramGroup, SWT.BORDER);
                text.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_FILL
                        | GridData.GRAB_HORIZONTAL));
                paramMap.put(param, new ParamUI(label, text));

                if(paramValue == null)
                	text.setText(""+0.0); 
                else 
                	text.setText("" + paramValue);
            }

            /*
             * a new change added in version R1G2-9
             */
            //validateParameters();

            paramComp.layout();
            paramComp.setSize(paramComp.computeSize(SWT.DEFAULT, SWT.DEFAULT));
//            paramGroup.layout();
//            getShell().pack(true);  //test
            dlgComp.layout(); 

        } catch (NoSuchIdentifierException e) {
            statusHandler.handle(Priority.PROBLEM, 
                    "Unexpected error retrieving parameters for projection", e);
        }
        clearUpLowerLeftAndUpperRightTextFields(llLatText, llLonText, urLatText, urLonText); 
    }
    
    private void clearUpLowerLeftAndUpperRightTextFields(Text llLat, Text llLon, Text urLat, Text urLon) {
    	if(llLat != null && !llLat.isDisposed())
    		llLat.setText(""); 
    	if(llLon != null && !llLon.isDisposed())
    		llLon.setText(""); 
    	if(urLat != null && !urLat.isDisposed())
    		urLat.setText(""); 
    	if(urLon != null && !urLon.isDisposed())
    		urLon.setText(""); 
    }

    private void lowerLeftAndUpperRightLatLonTextListenerAction(Text text) {
//    	text.addSelectionListener(new SelectionListener() {
//
//			@Override
//			public void widgetDefaultSelected(SelectionEvent e) {
//				String lowLeftAndUpperRightLatLonString = concatenateLatLonStringUsingTextFields(llLatText, 
//						llLonText, urLatText, urLonText); 
//				String geotoolsProjectionName = projList.getText();
//				updateParametersForGeotoolsProjection(parameters, 
//						lowLeftAndUpperRightLatLonString, geotoolsProjectionName); 
//			}
//
//			@Override
//			public void widgetSelected(SelectionEvent e) {
//				// Do nothing, this one does not work with Text
//			}
//        }); 
    	
    	text.addFocusListener(new FocusListener() {

			@Override
			public void focusGained(FocusEvent e) {
				// Do nothing
			}

			@Override
			public void focusLost(FocusEvent e) {
				if(isProjectionWithoutAngleValues()) {
					String lowLeftAndUpperRightLatLonString = concatenateLatLonStringUsingTextFields(llLatText, 
							llLonText, urLatText, urLonText); 
					String geotoolsProjectionName = projList.getText();
					updateParamUIAndParameterValueForGeotoolsProjection(parameters, 
							lowLeftAndUpperRightLatLonString, geotoolsProjectionName); 
				}
			}
    		
    	}); 
    	
//    	text.addModifyListener(new ModifyListener() {
//
//			@Override
//			public void modifyText(ModifyEvent e) {
//				String lowLeftAndUpperRightLatLonString = concatenateLatLonStringUsingTextFields(llLatText, 
//						llLonText, urLatText, urLonText); 
//				String geotoolsProjectionName = projList.getText();
//				updateParametersForGeotoolsProjection(parameters, 
//						lowLeftAndUpperRightLatLonString, geotoolsProjectionName); 
//			}
//    		
//    	}); 
    }
    
    private void updateParamUIAndParameterValueForGeotoolsProjection(ParameterValueGroup parameterValueGroup, 
    		String lowLeftAndUpperRightLatLonString,
    		String geotoolsProjectionName) {  
    	if(parameterValueGroup == null || lowLeftAndUpperRightLatLonString == null ||
    			isStringEmpty(geotoolsProjectionName))
    		return; 

    	double[] angleValueArray = calculateAngleValuesBasedOnGeotoolsProjectionName(lowLeftAndUpperRightLatLonString, 
    			geotoolsProjectionName); 

    	if(GempakProjectionValuesUtil.isCylindricalProjectionForGeotools(geotoolsProjectionName))
    		updateParamUIAndParameterValueForCylindricalProjection(parameterValueGroup, angleValueArray); 
    	else if(GempakProjectionValuesUtil.isAzmProjectionForGeotools(geotoolsProjectionName))
    		updateParamUIAndParameterValueForAzmProjection(parameterValueGroup, angleValueArray); 
    	else if(GempakProjectionValuesUtil.isConProjectionForGeotools(geotoolsProjectionName))
    		updateParamUIAndParameterValueForConProjection(parameterValueGroup, angleValueArray); 
    }
    
    private void updateParameterValueOnlyForGeotoolsProjection(ParameterValueGroup parameterValueGroup, 
    		String lowLeftAndUpperRightLatLonString,
    		String geotoolsProjectionName) {  
    	if(parameterValueGroup == null || lowLeftAndUpperRightLatLonString == null ||
    			isStringEmpty(geotoolsProjectionName))
    		return; 

    	double[] angleValueArray = calculateAngleValuesBasedOnGeotoolsProjectionName(lowLeftAndUpperRightLatLonString, 
    			geotoolsProjectionName); 

    	if(GempakProjectionValuesUtil.isCylindricalProjectionForGeotools(geotoolsProjectionName))
    		updateParameterValueOnlyForCylindricalProjection(parameterValueGroup, angleValueArray); 
    	else if(GempakProjectionValuesUtil.isAzmProjectionForGeotools(geotoolsProjectionName))
    		updateParameterValueOnlyForAzmProjection(parameterValueGroup, angleValueArray); 
    	else if(GempakProjectionValuesUtil.isConProjectionForGeotools(geotoolsProjectionName))
    		updateParameterValueOnlyForConProjection(parameterValueGroup, angleValueArray); 
    }
    
    private double[] calculateAngleValuesBasedOnGempakProjectionName(String lowLeftAndUpperRightLatLonString, 
			String gempakProjectionName) {
    	GraphicsAreaCoordinates graphicsAreaObject = new GraphicsAreaCoordinates(lowLeftAndUpperRightLatLonString); 
    	boolean isValidLatLonValues = graphicsAreaObject.parseGraphicsAreaString(lowLeftAndUpperRightLatLonString); 
    	graphicsAreaObject.setGraphicsAreaStrValid(isValidLatLonValues); 
    	
    	double[] angleValueArray = createDoubleArray(3); 
		double angleValue1 = graphicsAreaObject.getDefaultAngle1UsingGempakProjectionName(gempakProjectionName); 
		angleValueArray[0] = angleValue1; 
		double angleValue2 = graphicsAreaObject.getDefaultAngle2UsingGempakProjectionName(gempakProjectionName); 
		angleValueArray[1] = angleValue2; 
		if(GempakProjectionValuesUtil.isConProjectionForGeotools(gempakProjectionName)) {
			angleValueArray[2] = angleValue1; 
		}
		return angleValueArray; 
    }
    
    private double[] calculateAngleValuesBasedOnGeotoolsProjectionName(String lowLeftAndUpperRightLatLonString, 
			String geotoolsProjectionName) {
    	GraphicsAreaCoordinates graphicsAreaObject = new GraphicsAreaCoordinates(lowLeftAndUpperRightLatLonString); 
    	boolean isValidLatLonValues = graphicsAreaObject.parseGraphicsAreaString(lowLeftAndUpperRightLatLonString); 
    	graphicsAreaObject.setGraphicsAreaStrValid(isValidLatLonValues); 
    	
    	double[] angleValueArray = createDoubleArray(3); 
		double angleValue1 = graphicsAreaObject.getDefaultAngle1UsingGeotoolsProjectionName(geotoolsProjectionName); 
		angleValueArray[0] = angleValue1; 
		double angleValue2 = graphicsAreaObject.getDefaultAngle2UsingGeotoolsProjectionName(); 
		angleValueArray[1] = angleValue2; 
		if(GempakProjectionValuesUtil.isConProjectionForGeotools(geotoolsProjectionName)) {
			angleValueArray[2] = angleValue1; 
		}
		return angleValueArray; 
    }
    
    private double[] createDoubleArray(int arraySize) {
    	double[] doubleArray = new double[arraySize]; 
    	for(int i=0; i<doubleArray.length; i++)
    		doubleArray[i] = Double.NaN; 
    	return doubleArray; 
    }
    
    private void updateParamUIAndParameterValueForCylindricalProjection(ParameterValueGroup parameterValueGroup, 
    		double [] angleValueArray) {
    	for (Object obj : parameterValueGroup.values()) {
    		Parameter<?> param = (Parameter<?>) obj;
    		ParamUI currentParamUi = paramMap.get(param);

    		String paramName = param.getDescriptor().getName().getCode();
    		if("latitude_of_origin".equals(paramName)) {
    			if(!Double.isNaN(angleValueArray[0])) {
    				updateParamUIAndParameterValue(paramMap, 
    			    		currentParamUi, param, angleValueArray[0]); 
    			}
    		} else if("central_meridian".equals(paramName)) {
    			if(!Double.isNaN(angleValueArray[1])) {
    				updateParamUIAndParameterValue(paramMap, 
    			    		currentParamUi, param, angleValueArray[1]); 
    			}
    		}
    	}
    	paramComp.layout();
    	paramComp.setSize(paramComp.computeSize(SWT.DEFAULT, SWT.DEFAULT));
    }

    private void updateParamUIAndParameterValue(Map<Parameter<?>, ParamUI> paramMap, 
    		ParamUI currentParamUi, Parameter<?> unModifiedParam, double newAngleValue) {
		paramMap.remove(unModifiedParam); 
		unModifiedParam.setValue(newAngleValue);
		Text currentText = currentParamUi.text; 
		currentText.setText("" + newAngleValue);
		paramMap.put(unModifiedParam, new ParamUI(currentParamUi.label, currentText));
    }
    
    private void updateParameterValueOnlyForCylindricalProjection(ParameterValueGroup parameterValueGroup, 
    		double [] angleValueArray) {
    	for (Object obj : parameterValueGroup.values()) {
    		Parameter<?> param = (Parameter<?>) obj;
    		updateSingleParameterValueForCylindricalProjection(param, 
    	    		angleValueArray); 
    	}
    	paramComp.layout();
    	paramComp.setSize(paramComp.computeSize(SWT.DEFAULT, SWT.DEFAULT));
    }

    private Object updateSingleParameterValueForCylindricalProjection(Parameter<?> param, 
    		double [] angleValueArray) {
    	Object paramValue = param.getValue(); 
        String paramName = param.getDescriptor().getName().getCode();
        if("latitude_of_origin".equals(paramName)) {
        	if(!Double.isNaN(angleValueArray[0])) {
        		param.setValue(angleValueArray[0]);
        		paramValue = angleValueArray[0]; 
        	}
        } else if("central_meridian".equals(paramName)) {
        	if(!Double.isNaN(angleValueArray[1])) {
        		param.setValue(angleValueArray[1]);
        		paramValue = angleValueArray[1]; 
        	}
        }
        return paramValue; 
    }
    
    private void updateParamUIAndParameterValueForAzmProjection(ParameterValueGroup parameterValueGroup, 
    		double [] angleValueArray) {

    	for (Object obj : parameterValueGroup.values()) {
    		Parameter<?> param = (Parameter<?>) obj;
    		ParamUI currentParamUi = paramMap.get(param);

    		String paramName = param.getDescriptor().getName().getCode();
        	if("latitude_of_origin".equals(paramName)) {
        		if(!Double.isNaN(angleValueArray[0]))  {//get angle 1
    				updateParamUIAndParameterValue(paramMap, 
    			    		currentParamUi, param, angleValueArray[0]); 
        		}
        	} else if("central_meridian".equals(paramName)) {
        		if(!Double.isNaN(angleValueArray[1]))  { //get angle 2
    				updateParamUIAndParameterValue(paramMap, 
    			    		currentParamUi, param, angleValueArray[1]); 
        		}
        	} else if("latitude_of_center".equals(paramName)) {
        		if(!Double.isNaN(angleValueArray[0])) {
    				updateParamUIAndParameterValue(paramMap, 
    			    		currentParamUi, param, angleValueArray[0]); 
        		}
        	} else if("longitude_of_center".equals(paramName)) {
        		if(!Double.isNaN(angleValueArray[1]))  { //get angle 2
    				updateParamUIAndParameterValue(paramMap, 
    			    		currentParamUi, param, angleValueArray[1]); 
        		}
        	}
    	}
    }
    
    private void updateParameterValueOnlyForAzmProjection(ParameterValueGroup parameterValueGroup, 
    		double [] angleValueArray) {

    	for (Object obj : parameterValueGroup.values()) {
    		Parameter<?> param = (Parameter<?>) obj;
    		updateSingleParameterValueForAzmProjection(param, 
    	    		angleValueArray); 
    	}
    }
    
    private Object updateSingleParameterValueForAzmProjection(Parameter<?> param, 
    		double [] angleValueArray) {
    	Object paramValue = param.getValue(); 
    	String paramName = param.getDescriptor().getName().getCode();

    	if("latitude_of_origin".equals(paramName)) {
    		if(!Double.isNaN(angleValueArray[0]))  {//get angle 1
    			param.setValue(angleValueArray[0]); 
    			paramValue = angleValueArray[0]; 
    		}
    	} else if("central_meridian".equals(paramName)) {
    		if(!Double.isNaN(angleValueArray[1]))  { //get angle 2
    			param.setValue(angleValueArray[1]); 
    			paramValue = angleValueArray[1]; 
    		}
    	} else if("latitude_of_center".equals(paramName)) {
    		if(!Double.isNaN(angleValueArray[0])) {
    			param.setValue(angleValueArray[0]);
    			paramValue = angleValueArray[0]; 
    		}
    	} else if("longitude_of_center".equals(paramName)) {
    		if(!Double.isNaN(angleValueArray[1]))  { //get angle 2
    			param.setValue(angleValueArray[1]); 
    			paramValue = angleValueArray[1]; 
    		}
    	}
    	return paramValue; 
    }
    
    private void updateParamUIAndParameterValueForConProjection(ParameterValueGroup parameterValueGroup, 
    		double [] angleValueArray) {

    	for (Object obj : parameterValueGroup.values()) {
    		Parameter<?> param = (Parameter<?>) obj;
    		ParamUI currentParamUi = paramMap.get(param);

    		String paramName = param.getDescriptor().getName().getCode();
        	if("latitude_of_origin".equals(paramName)) {
        		if(!Double.isNaN(angleValueArray[0])) {
//        			System.out.println("updateSingleParameterValueForConProjection: latitude_of_origin="+angleValueArray[0]); 
    				updateParamUIAndParameterValue(paramMap, 
    			    		currentParamUi, param, angleValueArray[0]); 
        		}
        	} else if("central_meridian".equals(paramName)) {
        		if(!Double.isNaN(angleValueArray[1])) {
//        			System.out.println("updateSingleParameterValueForConProjection: central_meridian="+angleValueArray[1]); 
    				updateParamUIAndParameterValue(paramMap, 
    			    		currentParamUi, param, angleValueArray[1]); 
        		}
        	} else if("standard_parallel_1".equals(paramName)) {
        		if(!Double.isNaN(angleValueArray[2]))  {
//        			System.out.println("updateSingleParameterValueForConProjection: standard_parallel_1="+angleValueArray[2]); 
    				updateParamUIAndParameterValue(paramMap, 
    			    		currentParamUi, param, angleValueArray[2]); 
        		}
        	} 
    	}
    }
    
    private void updateParameterValueOnlyForConProjection(ParameterValueGroup parameterValueGroup, 
    		double [] angleValueArray) {

    	for (Object obj : parameterValueGroup.values()) {
    		Parameter<?> param = (Parameter<?>) obj;
    		updateSingleParameterValueForConProjection(param, 
    	    		angleValueArray);  
    	}
    }
    
    private Object updateSingleParameterValueForConProjection(Parameter<?> param, 
    		double [] angleValueArray) {
    	Object paramValue = param.getValue(); 
    	String paramName = param.getDescriptor().getName().getCode();

    	if("latitude_of_origin".equals(paramName)) {
    		if(!Double.isNaN(angleValueArray[0])) {
    			System.out.println("updateSingleParameterValueForConProjection: latitude_of_origin="+angleValueArray[0]); 
    			param.setValue(angleValueArray[0]);
    			paramValue = angleValueArray[0]; 
    		}
    	} else if("central_meridian".equals(paramName)) {
    		if(!Double.isNaN(angleValueArray[1])) {
    			System.out.println("updateSingleParameterValueForConProjection: central_meridian="+angleValueArray[1]); 
    			param.setValue(angleValueArray[1]);
    			paramValue = angleValueArray[1]; 
    		}
    	} else if("standard_parallel_1".equals(paramName)) {
    		if(!Double.isNaN(angleValueArray[2]))  {
    			System.out.println("updateSingleParameterValueForConProjection: standard_parallel_1="+angleValueArray[2]); 
    			param.setValue(angleValueArray[2]); 
    			paramValue = angleValueArray[2]; 
    		}
    	} 
    	return paramValue; 
    }
    
    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#okPressed()
     */
    @SuppressWarnings("unchecked")
    @Override
    protected void okPressed() {
        // update edited parameter values
        for (Map.Entry<Parameter<?>, ParamUI> entry : paramMap.entrySet()) {
            if (!"null".equals(entry.getValue().text.getText())) {
                Parameter<?> param = entry.getKey();

                ParameterDescriptor<?> pd = param.getDescriptor();

                Class<?> clazz = pd.getValueClass();

                String text = entry.getValue().text.getText();

                try {
                    if (clazz.equals(Double.class)) {
                        Double d = Double.parseDouble(text);
                        param.setValue(d);
                    } else {
                        MessageBox mb = new MessageBox(this.getShell(),
                                SWT.ICON_ERROR);
                        mb.setMessage("Unexpected paramter type: "
                                + clazz.getSimpleName());
                        mb.setText("Error");
                        mb.open();
                        entry.getValue().text.setFocus();
                        return;
                    }
                } catch (NumberFormatException e) {
                    MessageBox mb = new MessageBox(this.getShell(),
                            SWT.ICON_ERROR);
                    mb.setMessage('"' + text + '"' + " is not a valid "
                            + clazz.getSimpleName() + " value.");
                    mb.setText("Error");
                    mb.open();
                    entry.getValue().text.setFocus();
                    return;
                } catch (InvalidParameterValueException e) {
                    MessageBox mb = new MessageBox(this.getShell(),
                            SWT.ICON_ERROR);
                    mb.setMessage(e.getMessage());
                    mb.setText("Error");
                    mb.open();
                    entry.getValue().text.setFocus();
                    
//                    if(dlgComp != null)
//                    	dlgComp.dispose(); 
                    return;
                }
            }
        }

        // get lat/lon extent
        Coordinate ll = new Coordinate();
        ll.y = validateDouble(llLatText.getText(), -90, 90);
        if (Double.isNaN(ll.y)) {
            llLatText.setFocus();
            return;
        }

        ll.x = validateDouble(llLonText.getText(), -180, 180);
        if (Double.isNaN(ll.x)) {
            llLonText.setFocus();
            return;
        }

        Coordinate ur = new Coordinate();
        ur.y = validateDouble(urLatText.getText(), -90, 90);
        if (Double.isNaN(ur.y)) {
            urLatText.setFocus();
            return;
        }

        ur.x = validateDouble(urLonText.getText(), -180, 180);
        if (Double.isNaN(ur.x)) {
            urLonText.setFocus();
            return;
        }
        
        updateValuesInDisplayViewLowerLeftAndUpperRightLongLatValues(llLonText.getText(), 
        		llLatText.getText(), urLonText.getText(), urLatText.getText()); 
        
        /*
         * Using the updated llLat/llLon and urLat/urLon values to recalculate
         * the origin of latitude and central longitude if the angles of GEMPAK
         * are not provided
         */
		if(isProjectionWithoutAngleValues()) {
			String lowLeftAndUpperRightLatLonString = concatenateLatLonStringUsingTextFields(llLatText, 
					llLonText, urLatText, urLonText); 
			String geotoolsProjectionName = projList.getText();
			updateParameterValueOnlyForGeotoolsProjection(parameters, 
					lowLeftAndUpperRightLatLonString, geotoolsProjectionName); 
		}
		
        // create the new projection
        try {
            String name = parameters.getDescriptor().getName().getCode();
            CoordinateReferenceSystem crs = MapUtil.constructProjection(name,
                    parameters);

            GeneralGridGeometry newMapGeom = MapDescriptor.createGridGeometry(crs, ll, ur);

            AbstractEditor editor = (AbstractEditor) EditorUtil.getActiveEditor();

            for (IDisplayPane pane : editor.getDisplayPanes()) {
                // reset the display to fully zoomed extent

            	MapDescriptor oldDescriptor = (MapDescriptor) pane
                                .getRenderableDisplay().getDescriptor();
            	oldDescriptor.setGridGeometry(newMapGeom);

                // reset the display to fully zoomed extent
                pane.setZoomLevel(1.0f);
                pane.scaleToClientArea();

            }
            editor.refresh();

        } catch (IllegalArgumentException iae) {
            MessageBox mb = new MessageBox(this.getShell(), SWT.ICON_ERROR);
//            mb.setMessage("Error: " + iae.getMessage()+". ");
            mb.setMessage("Please correct some invalid parameters.");
            mb.setText("Error: Illegal Argument");
            mb.open();
//            UFStatus.handle(Priority.PROBLEM, Activator.PLUGIN_ID,
//                    StatusConstants.CATEGORY_WORKSTATION, null,
//                    "Error creating projection", iae);
            return;
        } catch (Exception e) {
//          UFStatus.handle(Priority.PROBLEM, Activator.PLUGIN_ID,
//                  StatusConstants.CATEGORY_WORKSTATION, null,
//                  "Error creating projection", e);
          MessageBox mb = new MessageBox(this.getShell(), SWT.ICON_ERROR);
          mb.setMessage("Error: " + e.getMessage());
          mb.setText("Error");
          mb.open();
          return;
      }
        super.okPressed();
    }

    private String concatenateLatLonStringUsingTextFields(Text llLatText, Text llLonText, 
    		Text urLatText, Text urLonText) {
    	String concatLatLonString = null; 
    	StringBuilder strBuildrer = new StringBuilder(); 
    	if(!isStringEmpty(llLatText.getText()) && !isStringEmpty(llLonText.getText()) && 
    			!isStringEmpty(urLatText.getText()) && !isStringEmpty(urLonText.getText())) {
    		strBuildrer.append(llLatText.getText())
    				   .append(";")
    				   .append(llLonText.getText())
    				   .append(";")
    				   .append(urLatText.getText())
    				   .append(";")
    				   .append(urLonText.getText()); 
    		concatLatLonString = strBuildrer.toString(); 
    	}
    	return concatLatLonString; 
    }
    
    private String concatenateLatLonStringUsingDoubleValues(double llLatValue, double llLonValue, 
    		double urLatValue, double urLonValue) {
    	StringBuilder strBuildrer = new StringBuilder(); 
    	strBuildrer.append(llLatValue)
    		.append(";")
    		.append(llLonValue)
    		.append(";")
    		.append(urLatValue)
    		.append(";")
    		.append(urLonValue); 
    	return strBuildrer.toString(); 
    }
    
    private boolean isLatitudeTextValid(String latString) {
    	return isLatLonTextValid(latString, -90.0, 90.0); 
    }
    
    private boolean isLongitudeTextValid(String lonString) {
    	return isLatLonTextValid(lonString, -180.0, 180.0); 
    }
    
    private boolean isLatLonTextValid(String latLonString, double min, double max) {
    	boolean isValid = true; 
    	double doubleValue = validateDouble(latLonString, min, max); 
    	if(Double.isNaN(doubleValue))
    		isValid = false; 
    	return isValid; 
    }
    
    private double validateDouble(String text, double min, double max) {
        double value;
        try {
            value = Double.parseDouble(text);
        } catch (NumberFormatException e1) {
            MessageBox mb = new MessageBox(this.getShell(), SWT.ICON_ERROR);
            mb.setMessage('"' + text + '"' + " is not a valid Double value.");
            mb.setText("Error");
            mb.open();
            return Double.NaN;
        }
        if ((value < min) || (value > max)) {
            MessageBox mb = new MessageBox(this.getShell(), SWT.ICON_ERROR);
            DecimalFormat df = new DecimalFormat("0.#");
            mb.setMessage("Value must be in the range [" + df.format(min)
                    + " .. " + df.format(max) + "]");
            mb.setText("Error");
            mb.open();
            return Double.NaN;
        }

        return value;
    }
    
    private boolean validateGempakProjectionString(GempakProjectionValuesUtil projectionValuesObject, 
    		GraphicsAreaCoordinates graphicsAreaCoordinatesObject) {
    	boolean isValid = true; 
    	if(!projectionValuesObject.isGempakProjectionStringValid()) {
            MessageBox mb = new MessageBox(this.getShell(), SWT.ICON_ERROR);
            mb.setMessage('"' + projectionValuesObject.getProjectionStringValue() + '"' + " is not a valid projection string value.");
            mb.setText("Error");
            mb.open();
            isValid = false; 
    	} else if(projectionValuesObject.isDefaultProjection() && 
    			!graphicsAreaCoordinatesObject.isValidGeogName()) {
    		MessageBox mb = new MessageBox(this.getShell(), SWT.ICON_ERROR);
    		mb.setMessage('"' + GempakProjectionValuesUtil.MISSING_VALID_PROJECTION_NAME);
    		mb.setText("Error: a valid GEMPAK projection abbreviation is needed");
    		mb.open();
    		isValid = false; 
    	}
    	return isValid; 
    }
    
    private boolean validateGempakGraphicsAreaString(GraphicsAreaCoordinates graphicsAreaCoordinatesObject) {
    	boolean isValid = true; 
    	if(!graphicsAreaCoordinatesObject.isGraphicsAreaStringValid()) {
            MessageBox mb = new MessageBox(this.getShell(), SWT.ICON_ERROR);
            mb.setMessage('"' + graphicsAreaCoordinatesObject.getGraphicsAreaString() + '"' + " is not a valid graphics area string value.");
            mb.setText("Error");
            mb.open();
            isValid = false; 
    	}
    	return isValid; 
    }
    
    /**
     * a helper method to check if a double is valid
     */
    private boolean isDoubleValueValid(double doubleNumber) {
    	boolean isValid = true; 
    	if(Double.isNaN(doubleNumber))
    		isValid = false; 
    	return isValid; 
    }
    
    /**
     * a helper method to check if a String is an empty String
     */
	private boolean isStringEmpty(String str) {
		boolean valid = true; 
		if(str != null && str.trim().length() != 0) 
			valid = false; 
		return valid; 
	}

}
