package gov.noaa.nws.ncep.viz.tools.loopManagement;


import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;
import gov.noaa.nws.ncep.viz.tools.loopManagement.DwellRateInfo.DwellRateEntry;
import gov.noaa.nws.ncep.viz.ui.display.NCLoopProperties;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.core.datastructure.LoopProperties.LoopMode;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Loop Management dialog box
 * 
 * <pre>
 * 
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sept 28, 2010   317      X. Guo    	Initial Creation.
 * Oct 07, 2010             X. Guo		Re-write functions to handle Loop Control
 * 03/07/11      migration  G. Hull     use NCLoopProperties                                    
 * 06/30/11                 C. Chen     changed to support all editors
 * 07/29/11       #450      G. Hull     use NCPathManager       
 * 01/11/2012     #497      S. Gurung   Initialize speedLevel                         
 * 
 * </pre>
 * 
 * @author xguo
 * @version 1
 */
public class LoopManagementDialog extends CaveJFACEDialog {

	
    private static final int SCALE_WIDTH = 200;

    private static final int LABEL_WIDTH = 56;
    
    private static final int DEF_HEIGHT = 30;
    
    private static final int DEF_SCALE_STEP_VALUE = 10;
    
    private int loopStopGapX = 92;
	private int labelGap = 20;
	private int labelWidth = 60;
	private int btnHeight = 20;
	
    private final String title = "Loop Management";

    private Scale loopSpeedScale;

    private Label loopSpeedLabel;

    private Scale firstFrameDwellScale;

    private Label firstFrameDwellLabel;

    private Scale lastFrameDwellScale;

    private Label lastFrameDwellLabel;

    private final AbstractEditor editor;

    private NCLoopProperties loopProperties;
    
    private boolean isLooping;
    
    private Button[] speed_buttons = null;
    
    private DwellRateInfo dwellRateInfo;
    
    private static List<DwellRateEntry> dwellRateList = null;


    private final int NOT_IN_SPEED_LEVEL = -1;
    
    private static int speedLevel = -1;  // 0, 1, 2, .... speedLevelNum-1
    
    private static boolean initDwellRate = false;
    
    private int speedLevelNum = -1;
    
    private static boolean loopStopCurrent = true;
    
    private static boolean loopLayerLink = false;
    /**
     * Constructor
     * 
     * @param parentShell
     */
    public LoopManagementDialog(Shell parentShell) {
        super(parentShell);
        this.editor = (AbstractEditor) EditorUtil.getActiveEditor();
    }

   
  
    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
     */
    @Override
    protected void buttonPressed(int buttonId) {
        super.buttonPressed(buttonId);
    }

    

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
     */
    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);
        if (title != null) {
            shell.setText(title);
        }
        
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#createButtonBar(org.eclipse.swt.widgets.Composite)
     */
    @Override
    protected Control createButtonBar(Composite parent) {
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
     */
    @Override
    protected Control createDialogArea(final Composite parent) {
        Composite comp = (Composite) super.createDialogArea(parent);
        
        readDwellRateTable();
        initLoopProperty ();
        
        createLoopControls(comp);
        createLoopStop (comp);
        createLoopLayerLink (comp);
        createBottomButtons(comp);

        initSpeedLevel();
        initDialogControls();

        return comp;
    }
    /*
     * Create Loop control group
     */
    private void createLoopControls(Composite comp) {
    	Group composite = new Group(comp, SWT.NORMAL );
    	composite.setText("Loop Controls:");
        composite.setLayout(new GridLayout(3, false));

        /*
         * Loop Speed
         */
        Label label = new Label(composite, SWT.BOLD);
        label.setText("Loop Speed: ");

        loopSpeedScale = new Scale(composite, SWT.HORIZONTAL);
        loopSpeedScale.setLayoutData(new GridData(SCALE_WIDTH, DEF_HEIGHT));
        loopSpeedScale.setMinimum(0);
        loopSpeedScale
                .setMaximum(calculateFrameScaleValue(loopProperties.getNcepMaxFrameTime ()));
        loopSpeedScale.setIncrement(1);
        loopSpeedScale.setPageIncrement(1);

        loopSpeedLabel = new Label(composite, SWT.NONE);
        loopSpeedLabel.setLayoutData(new GridData(LABEL_WIDTH, SWT.DEFAULT));
        loopSpeedScale.addListener(SWT.Selection, new Listener() {
            public void handleEvent(Event event) {
            	if (isLooping && loopProperties.getMode() == LoopMode.Forward) {
            		loopProperties.setFwdFrameTime(calculateFrameTime(loopSpeedScale));
            	}
            	else if (isLooping && loopProperties.getMode() == LoopMode.Backward) {
            		loopProperties.setRevFrameTime(calculateFrameTime(loopSpeedScale));
            	}
            	else {
            		loopProperties.setFwdFrameTime(calculateFrameTime(loopSpeedScale));
            		loopProperties.setRevFrameTime(calculateFrameTime(loopSpeedScale));
            	}
                speedLevel = NOT_IN_SPEED_LEVEL;
                update();
            }
        });

       
        /*
         * First Frame Dwell
         */
        label = new Label(composite, SWT.BOLD);
        label.setText("First Frame Dwell: ");

        firstFrameDwellScale = new Scale(composite, SWT.NONE);
        firstFrameDwellScale.setLayoutData(new GridData(SCALE_WIDTH,
        		DEF_HEIGHT));
        firstFrameDwellScale.setMinimum(0);
        firstFrameDwellScale
                .setMaximum(calculateFirstFrameDwellScaleValue(loopProperties.getNcepMaxFirstFrameTime ()));
        firstFrameDwellScale.setIncrement(1);
        firstFrameDwellScale.setPageIncrement(1);

        firstFrameDwellLabel = new Label(composite, SWT.NONE);
        firstFrameDwellLabel.setLayoutData(new GridData(LABEL_WIDTH,
                SWT.DEFAULT));
        firstFrameDwellScale.addListener(SWT.Selection, new Listener() {
            public void handleEvent(Event event) {
                loopProperties
                        .setFirstFrameDwell(calculateFirstFrameDwellTime(firstFrameDwellScale));
                speedLevel = NOT_IN_SPEED_LEVEL;
                update();
            }
        });

        /*
         * Last Frame Dwell
         */
        label = new Label(composite, SWT.BOLD); 
        label.setText("Last Frame Dwell: ");

        lastFrameDwellScale = new Scale(composite, SWT.NONE);
        lastFrameDwellScale
                .setLayoutData(new GridData(SCALE_WIDTH, DEF_HEIGHT));
        lastFrameDwellScale.setMinimum(0);
        lastFrameDwellScale
                .setMaximum(calculateLastFrameDwellScaleValue(loopProperties.getNcepMaxLastFrameTime ()));
        lastFrameDwellScale.setIncrement(1);
        lastFrameDwellScale.setPageIncrement(1);

        lastFrameDwellLabel = new Label(composite, SWT.NONE);
        lastFrameDwellLabel
                .setLayoutData(new GridData(LABEL_WIDTH, SWT.DEFAULT));
        lastFrameDwellScale.addListener(SWT.Selection, new Listener() {
            public void handleEvent(Event event) {
                loopProperties
                        .setLastFrameDwell(calculateLastFrameDwellTime(lastFrameDwellScale));
                speedLevel = NOT_IN_SPEED_LEVEL;
                update();
            }
        });
        
    	if ( speedLevelNum <= 0 ) return;
    	/*
         * Slow ... Fast Speed Level controls
         */
    	GridData speedGd = new GridData();
    	speedGd.horizontalAlignment = GridData.CENTER;
    	speedGd.grabExcessHorizontalSpace = true;
    	speedGd.horizontalSpan = 1;
    	composite.setLayoutData(speedGd);
 
    	label = new Label(composite, SWT.BOLD);
    	label.setText("Slow");

    	Group speed_group = new Group(composite, SWT.SHADOW_NONE );
    	speed_group.setLayout(new GridLayout(speedLevelNum, false));
    	speed_buttons = new Button[speedLevelNum];
    	for(int i = 0; i < speedLevelNum; i++) {
    		speed_buttons[i] = new Button(speed_group, SWT.RADIO);
    		speed_buttons[i].setText("");
    		speed_buttons[i].addListener(SWT.Selection, new Listener() {
    			public void handleEvent(Event event) {

    				for(int i = 0; i < speedLevelNum; i++) {
    					if (speed_buttons[i].getSelection()) {
    						speedLevel = i;
    						setSpeedLevel();
    				        update();
    				        break;
    					}	
    				}
    				
    			}
    		});
    	}

    	label = new Label(composite, SWT.BOLD);
    	label.setText("Fast");
    }

    /*
     * Create Loop Stop group
     */
    private void createLoopStop (Composite comp) {
		Group  loopStopGp = new Group(comp, SWT.SHADOW_ETCHED_IN);
		loopStopGp.setText("Loop Stop:");
		loopStopGp.setLayout( null );
        
		Button curBtn = new Button(loopStopGp, SWT.RADIO );
		curBtn.setText("");
		curBtn.setEnabled( true );
		if ( loopStopCurrent ) {
		    curBtn.setSelection(true);
		}
		else {
			curBtn.setSelection(false);
		}
		curBtn.setBounds(loopStopGp.getBounds().x+ loopStopGapX, loopStopGp.getBounds().y + labelGap,labelGap,btnHeight);

		Label curLabel = new Label(loopStopGp, SWT.BOLD);

        curLabel.setBounds(loopStopGp.getBounds().x+ loopStopGapX+labelGap+2, loopStopGp.getBounds().y + 
        		           labelGap+2,labelWidth,btnHeight);
        curLabel.setText("Current");
        
		curBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {           
				if ( ! loopStopCurrent ) {
					loopProperties.setLoopStopCurrent(true);
					update();
				}
				loopStopCurrent = true;
			}          		            	 	
		} );  

		Button endBtn = new Button(loopStopGp, SWT.RADIO);
		endBtn.setEnabled( true );
		endBtn.setBounds(loopStopGp.getBounds().x+ loopStopGapX*3, loopStopGp.getBounds().y + labelGap,labelGap,btnHeight);
        if ( loopStopCurrent ) {
		    endBtn.setSelection(false);
        }
        else {
        	endBtn.setSelection(true);
        }
		Label endLabel = new Label(loopStopGp, SWT.BOLD);
        endLabel.setText("End");
        endLabel.setBounds(loopStopGp.getBounds().x+ loopStopGapX*3 + labelGap+2,
        		            loopStopGp.getBounds().y + labelGap+2,labelWidth+36,btnHeight);

		endBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {           
                 if ( loopStopCurrent ) {
 					loopProperties.setLoopStopCurrent(false);
 					update();
 				 }
                 loopStopCurrent = false;
			}          		            	 	
		} );      	
    }

    /*
     * Create Loop Layer Link Group
     */
    private void createLoopLayerLink (Composite comp) {
		Group  loopLayerLkGp = new Group(comp, SWT.SHADOW_ETCHED_IN);
		loopLayerLkGp.setText("Loop Layer Link:");
		loopLayerLkGp.setLayout( null );
        
		Button enableBtn = new Button(loopLayerLkGp, SWT.RADIO );
		enableBtn.setText("");
		enableBtn.setEnabled( true );
		if ( loopLayerLink ) {
			enableBtn.setSelection(true);
		}
		else {
			enableBtn.setSelection(false);
		}
		enableBtn.setBounds(loopLayerLkGp.getBounds().x+ loopStopGapX, 
				            loopLayerLkGp.getBounds().y + labelGap,labelGap,btnHeight);

		Label enableLabel = new Label(loopLayerLkGp, SWT.BOLD);

		enableLabel.setBounds(loopLayerLkGp.getBounds().x+ loopStopGapX+labelGap+2, 
				              loopLayerLkGp.getBounds().y + labelGap+2,labelWidth,btnHeight);
		enableLabel.setText("Enable");
        
        enableBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {           
				loopLayerLink = true;
			}          		            	 	
		} );  

		Button disableBtn = new Button(loopLayerLkGp, SWT.RADIO);
		disableBtn.setEnabled( true );
		disableBtn.setBounds(loopLayerLkGp.getBounds().x+ loopStopGapX*3, 
				             loopLayerLkGp.getBounds().y + labelGap,labelGap,btnHeight);
        if ( loopLayerLink ) {
        	disableBtn.setSelection(false);
        }
        else {
        	disableBtn.setSelection(true);
        }
		Label endLabel = new Label(loopLayerLkGp, SWT.BOLD);
        endLabel.setText("Disable");
        endLabel.setBounds(loopLayerLkGp.getBounds().x+ loopStopGapX*3 + labelGap+2, 
        		           loopLayerLkGp.getBounds().y + labelGap+2,labelWidth+36,btnHeight);

        disableBtn.addListener( SWT.MouseUp, new Listener() {
			public void handleEvent(Event event) {           
                 loopLayerLink = false;
			}          		            	 	
		} );      	
    }
    
    /*
     * Bottom buttons
     */
    private void createBottomButtons(Composite comp) {
    	Composite Comp = new Composite(comp, SWT.NONE);
        
        GridData closeGd = new GridData();
        closeGd.horizontalAlignment = GridData.CENTER;
        closeGd.grabExcessHorizontalSpace = true;
        closeGd.horizontalSpan = 1;
        Comp.setLayoutData(closeGd);
        Comp.setLayout(new GridLayout(1, false));
        
        Button closeBtn = new Button(Comp, SWT.NONE);
        closeBtn.setText("Close");
        closeBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
            	close();
            }
        });
    }
    
    // Read Dwell Rate table
    private void readDwellRateTable() {
    	if (dwellRateList == null) {
    		File dwellRateFile = NcPathManager.getInstance().getStaticFile(
            		NcPathConstants.LOOP_SPEEDS_TBL );
    		if ( dwellRateFile == null || !dwellRateFile.exists() ) {
    			return;
    		}
    		dwellRateInfo = new DwellRateInfo(dwellRateFile.getAbsolutePath());
    		try {
    			if (dwellRateInfo.readTable()) {
    				dwellRateList = new ArrayList<DwellRateEntry>();
    				dwellRateList = dwellRateInfo.getDwellRate();
    			}
    			else {
    				return;
    			}
    		} catch (FileNotFoundException e) {
    			e.printStackTrace();
    		} catch (IOException e) {
    			e.printStackTrace();
    		}
    	}
    	
    	if (dwellRateList != null && dwellRateList.size() > 0 ) {
    		speedLevelNum = dwellRateList.size();
    		if (speedLevel >= speedLevelNum) speedLevel = (int) speedLevelNum / 2;
    	}
    	
    }
    
    /*
     * Initialize loop property
     */
    private void initLoopProperty () {
        if( !(editor.getLoopProperties() instanceof NCLoopProperties) ) {
        	System.out.println("sanity check: editor LoopProperties is not NCLoopProperties");
        }
        
        loopProperties = (NCLoopProperties) editor.getLoopProperties();
        isLooping = loopProperties.isLooping();
        setNcepFrameTime ();
    }
    /*
     * Initialize dialog control information
     */
    private void initDialogControls()  {
        
        if (!initDwellRate && speedLevelNum > 0) {
        	speedLevel = speedLevelNum / 2;
   	
        	setSpeedLevel();
        	initDwellRate = true;
        } else if (speedLevel >= 0 ) {
        	if ( speed_buttons != null ) speed_buttons[speedLevel].setSelection(true);
        }
        
        loopProperties.setLoopStopCurrent(loopStopCurrent);
        update();
        
    }

    /*
     * Set loop speed level based on the setting table 
     */
    private void setSpeedLevel() {
    	
    	// Get dwell rates from the table.
    	if (speedLevelNum > 0 && speedLevel >= 0 ) {
    		int fwdFrameTime = (int) (dwellRateList.get(speedLevel).getMidFrameDwell() * 1000);
    		if (fwdFrameTime > loopProperties.getNcepMaxFrameTime ()) {
    			fwdFrameTime = loopProperties.getNcepMaxFrameTime ();
    		}
    		
    		int firstFrameDwellTime = (int) (dwellRateList.get(speedLevel).getFirstFrameDwell() * 1000);
    		if ( firstFrameDwellTime > loopProperties.getNcepMaxFirstFrameTime()) {
    			firstFrameDwellTime = loopProperties.getNcepMaxFirstFrameTime();
    		}
    		loopProperties.setFirstFrameDwell(firstFrameDwellTime);

    		int lastFrameDwellTime = (int) (dwellRateList.get(speedLevel).getLastFrameDwell() * 1000);
    		if ( lastFrameDwellTime > loopProperties.getNcepMaxLastFrameTime()) {
    			lastFrameDwellTime = loopProperties.getNcepMaxLastFrameTime();
    		}
    		loopProperties.setLastFrameDwell(lastFrameDwellTime);
    		
    		loopProperties.setFwdFrameTime(fwdFrameTime);
			loopProperties.setRevFrameTime(fwdFrameTime);
    		if (isLooping) {
    			if (loopProperties.getMode() == LoopMode.Forward) {
    				loopProperties.setRevFrameTime(fwdFrameTime);
    			}
    			else if (loopProperties.getMode() == LoopMode.Backward) {
    				loopProperties.setFwdFrameTime(fwdFrameTime);
    			}
    		}

    		// Set the speed level
    		if ( speed_buttons != null ) speed_buttons[speedLevel].setSelection(true);
    	} 
    }
    
    /*
     * Calculate ncep frame time/step
     */
    private void setNcepFrameTime () {
    	int frameTime, firstFrameTime, lastFrameTime;
    	
       	// Get dwell rates from the table.
    	if ( speedLevelNum > 0 ) {
    		frameTime = (int) (dwellRateList.get(0).getMidFrameDwell() * 1000);
        	firstFrameTime = (int) (dwellRateList.get(0).getFirstFrameDwell() * 1000);
        	lastFrameTime = (int) (dwellRateList.get(0).getLastFrameDwell() * 1000);
    	} // use default value
    	else {
    		frameTime = loopProperties.getNcepMaxFrameTime();
    		firstFrameTime = loopProperties.getNcepMaxFirstFrameTime();
    		lastFrameTime = loopProperties.getNcepMaxLastFrameTime();
    	}
    	/** calculate ncep frame step*/
    	loopProperties.setNcepMaxFrameTime (frameTime);
    	loopProperties.setNcepFrameStep (DEF_SCALE_STEP_VALUE);
    	
    	/** calculate ncep first frame dwell step*/
    	loopProperties.setNcepMaxFirstFrameTime (firstFrameTime);
    	loopProperties.setNcepFirstFrameDwellStep (DEF_SCALE_STEP_VALUE);
    	
    	/** calculate ncep last frame dwell step*/
    	loopProperties.setNcepMaxLastFrameTime (lastFrameTime);
    	loopProperties.setNcepLastFrameDwellStep (DEF_SCALE_STEP_VALUE);
    }
    
    private void update() {

    	if (speedLevel < 0 && speedLevelNum > 0) {
    		if ( speed_buttons != null ) {
    			for (int i = 0; i < speedLevelNum; i++) {
    				speed_buttons[i].setSelection(false);
    			}
    		}
    	}
    	
    	// Update Loop speed
        int fwdFrameTime = loopProperties.getFwdFrameTime();
        int revFrameTime = loopProperties.getRevFrameTime();
        
        
        if (loopProperties.getMode() == LoopMode.Backward) {
        	loopSpeedScale.setSelection(calculateFrameScaleValue(revFrameTime));
        	loopSpeedLabel.setText(formatFrameTime(revFrameTime));
        }
        else {
        	loopSpeedScale.setSelection(calculateFrameScaleValue(fwdFrameTime));
        	loopSpeedLabel.setText(formatFrameTime(fwdFrameTime));
        }
        

        // Update first frame
        int firstFrameDwellTime = loopProperties.getFirstFrameDwell();
        firstFrameDwellScale
                .setSelection(calculateFirstFrameDwellScaleValue(firstFrameDwellTime));
        firstFrameDwellLabel.setText(formatDwellTime(firstFrameDwellTime));

        // Update last frame
        int lastFrameDwellTime = loopProperties.getLastFrameDwell();
        lastFrameDwellScale
                .setSelection(calculateLastFrameDwellScaleValue(lastFrameDwellTime));
        lastFrameDwellLabel.setText(formatDwellTime(lastFrameDwellTime));

        
        // If both frame times are 0, turn looping off 
        if ((fwdFrameTime == 0) && (revFrameTime == 0)) {
            loopProperties.setLooping(false);
        } else {
            loopProperties.setLooping(isLooping);
        }

        // update the editor
        editor.setLoopProperties(loopProperties);
    }
    
    /*
     * calculate Frame Time
     */
    private int calculateFrameTime(Scale scale) {
    	return loopProperties.getNcepFrameStep() * (scale.getSelection());
    }
    
    /*
     * calculate First Frame Dwell Time
     */
    private int calculateFirstFrameDwellTime(Scale scale) {
        return loopProperties.getNcepFirstFrameDwellStep() * (scale.getSelection());
    }
 
    /*
     * calculate Last Frame Dwell Time
     */
    private int calculateLastFrameDwellTime(Scale scale) {
        return loopProperties.getNcepLastFrameDwellStep() * (scale.getSelection());
    }

    /*
     * calculate Frame Scale Value
     */
    private int calculateFrameScaleValue(int frameTime) {
    	int value = frameTime/loopProperties.getNcepFrameStep();

    	return value;
    }

    /*
     * calculate First Frame Scale Value
     */
    private int calculateFirstFrameDwellScaleValue(int dwellTime) {
        int value = dwellTime / loopProperties.getNcepFirstFrameDwellStep();
    	return value;
    }
    
    /*
     * calculate Last Frame Scale Value
     */
    private int calculateLastFrameDwellScaleValue(int dwellTime) {
        int value =  dwellTime / loopProperties.getNcepLastFrameDwellStep();
    	return value;
    }

    /*
     * Generate Frame Time display format 
     */
    private String formatFrameTime(int val) {
       	double value = (double) val / 1000;
    	BigDecimal bd=new BigDecimal(value);
    	bd=bd.setScale(2,BigDecimal.ROUND_HALF_EVEN);
        return bd + " s/f";
    }

    /*
     * Generate Dwell Time display format 
     */
    private String formatDwellTime(int val) {
    	double value = (double) val / 1000;
    	BigDecimal bd=new BigDecimal(value);
    	bd=bd.setScale(2,BigDecimal.ROUND_HALF_EVEN);
        return bd + " sec";
    }
    
    private void initSpeedLevel() {    	
    	for (int i=0; i<speedLevelNum; i++) {
    		if (loopProperties.getFwdFrameTime() >= dwellRateList.get(i).getMidFrameDwell() * 1000) {
        		speedLevel = i;
        		break;
    		}
    	}
    }
    
}

