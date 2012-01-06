/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.viz.monitor.ui.dialogs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.monitor.data.RangeData;
import com.raytheon.uf.viz.monitor.data.RangesUtil;

/**
 * Edit thresholds dialog.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 2, 2009            lvenable     Initial creation
 * Aug 5, 2010  6396       wkwock      Change the layout of threshold edit dialog
 * Aug 5, 2010  4036       wkwock      Improve the determineError algorithm
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */
public abstract class EditThresholdsDlg extends Dialog
{
    /**
     * Dialog shell.
     */
    public Shell shell;
    
    /**
     * The display control.
     */
    private Display display;
    
    /**
     * Control font.
     */
    private Font controlFont;
    
    /**
     * Return value when the shell is disposed.
     */
    private Boolean returnValue = false;
    
    /**
     * flag indicating if the user is typing.
     */
    private boolean typing = false;
    
    /**
     * Visibility R index.
     */
    protected int visRIndex = 0;
    
    /**
     * Visibility Y index.
     */
    protected int visYIndex = 0;
    
    /**
     * Range Utility object containing the ranges. 
     */
    public RangesUtil rangeUtil;

    /**
     * Constructor.
     * @param parent Parent shell.
     * @param style Dialog style.
     */
    public EditThresholdsDlg(Shell parent, int style)
    {
        super(parent, style);
        
        rangeUtil = RangesUtil.getInstance();
    }
    
    /**
     * Open method used to display the dialog.
     * @return True/False.
     */
    public Object open()
    {        
        Shell parent = getParent();
        display = parent.getDisplay();
        shell = new Shell(parent, SWT.DIALOG_TRIM);
        
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 5;
        shell.setLayout(mainLayout);
        
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);
      
        // Initialize all of the controls and layouts
        initializeComponents();
        
        // Update the controls with the data
        updateControlsWithData();
        
        createBottomButtons();
        
        shell.pack();
        
        shell.open();
        while (!shell.isDisposed())
        {
            if (!display.readAndDispatch())
            {
                display.sleep();
            }
        }
        
        controlFont.dispose();
        
        return returnValue;
    }
    
    /**
     * Get the dialog shell.
     * @return The dialog shell.
     */
    public Shell getDialogShell()
    {
        return shell;
    }
    
    private void setSpinnerValue(final Spinner spinnerR, RangeData rd){
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 35;
        gd.horizontalIndent = 10;
        spinnerR.setDigits(0);
        spinnerR.setIncrement(1);
        spinnerR.setPageIncrement(5);
        /*
         * Set Max and Min spinner values on increment wider then range to make
         * roll over possible.
         */
        spinnerR.setMaximum(rd.getMax() + 1);
        spinnerR.setMinimum(rd.getMin() - 1);        
        spinnerR.setSelection(rd.getMin() + 1);
        spinnerR.setLayoutData(gd);
    }
    /**
     * Create the spinner controls.
     * @param parentComp PArent composite.
     * @param spinnerR The "R" spinner control.
     * @param spinnerY The "Y" spinner control.
     * @param rd Range data.
     * @param addListeners Flag indicating if control listeners should be added.
     * @param rValIsHigher Flag indicating if the "R" control should have a
     *                     higher value than the "Y" control.
     */
    protected void createSpinnerControls(Composite parentComp, final Spinner spinnerR,
            final Spinner spinnerY, RangeData rd, boolean addListeners, final boolean rValIsHigher)
    {        
        setSpinnerValue(spinnerR,rd);
        setSpinnerValue(spinnerY,rd);
        if (addListeners == true)
        {
            addListenersToSpinners(spinnerR, spinnerY, rValIsHigher);
        }
        
        GridData gd = new GridData(SWT.DEFAULT, SWT.CENTER, true, true);
        gd.horizontalIndent = 10;
        Label scaWindSpdRangeLbl = new Label(parentComp, SWT.NONE);
        scaWindSpdRangeLbl.setText(rd.getMin() + " to " + rd.getMax());
        scaWindSpdRangeLbl.setLayoutData(gd);
    }
    
    /**
     * Create the spinner controls.
     * @param parentComp PArent composite.
     * @param spinnerFromR The from "R" spinner control.
     * @param spinnerFromY The from "Y" spinner control.
     * @param spinnerToR The to "R" spinner control.
     * @param spinnerToY The to "Y" spinner control.
     * @param rd Range data.
     * @param addListeners Flag indicating if control listeners should be added.
     * @param rValIsHigher Flag indicating if the "R" control should have a
     *                     higher value than the "Y" control.
     */
    protected void createSpinnerControls(Composite parentComp, final Spinner spinnerFromY,
    		final Spinner spinnerFromR,final Spinner spinnerToR,final Spinner spinnerToY, RangeData rd, boolean addListeners, final boolean rValIsHigher)
    {
    	setSpinnerValue(spinnerFromR,rd);
        setSpinnerValue(spinnerFromY,rd);
        setSpinnerValue(spinnerToR,rd);
        setSpinnerValue(spinnerToY,rd);
        if (addListeners == true)
        {
            addListenersToSpinners(spinnerFromR, spinnerFromY, rValIsHigher);
            addListenersToSpinners(spinnerToR, spinnerToY, rValIsHigher);
        }

        GridData gd = new GridData(SWT.DEFAULT, SWT.CENTER, true, true);
        gd.horizontalIndent = 10;
        Label scaWindSpdRangeLbl = new Label(parentComp, SWT.NONE);
        scaWindSpdRangeLbl.setText(rd.getMin() + " to " + rd.getMax());
        scaWindSpdRangeLbl.setLayoutData(gd);
    }

    /**
     * Add selection/focus/key listeners to the "R" and "Y" spinners.
     * @param spinnerR "R" spinner control.
     * @param spinnerY "Y" spinner control.
     * @param rValIsHigher Flag indicating the "R" control will have a
     *                     higher value than the "Y" control.
     */
    private void addListenersToSpinners(final Spinner spinnerR, final Spinner spinnerY,
            final Boolean rValIsHigher)
    {
        spinnerR.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent e)
            {
                checkRSpinner(spinnerR, spinnerY, rValIsHigher);
            }
        });
        
        spinnerY.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent e)
            {
                if (typing == true)
                {
                    return;
                }
                
                checkYSpinner(spinnerR, spinnerY, rValIsHigher);
            }
        });
        
        spinnerR.addFocusListener(new FocusAdapter()
        {

            @Override
            public void focusLost(FocusEvent e)
            {
                checkRSpinner(spinnerR, spinnerY, rValIsHigher);
            }
            
        });
        
        spinnerY.addFocusListener(new FocusAdapter()
        {

            @Override
            public void focusLost(FocusEvent e)
            {
                checkYSpinner(spinnerR, spinnerY, rValIsHigher);
            }            
        });
        
        spinnerR.addKeyListener(new KeyAdapter()
        {

            @Override
            public void keyPressed(KeyEvent e)
            {                
                if (e.keyCode == SWT.KEYPAD_CR)
                {
                    typing = false;
                    checkRSpinner(spinnerR, spinnerY, rValIsHigher);
                    return;
                }
                typing = true;
            }

            @Override
            public void keyReleased(KeyEvent e)
            {
                typing = false;
            }
            
        });
        
        spinnerY.addKeyListener(new KeyAdapter()
        {

            @Override
            public void keyPressed(KeyEvent e)
            {                
                if (e.keyCode == SWT.KEYPAD_CR)
                {
                    typing = false;
                    checkYSpinner(spinnerR, spinnerY, rValIsHigher);
                    return;
                }
                typing = true;
            }

            @Override
            public void keyReleased(KeyEvent e)
            {                
                typing = false;
            }
            
        });
    }
    
    /**
     * Check the value of the "R" spinner vs. the "Y" spinner and set the "R" spinner control.
     * @param spinnerR "R" spinner control.
     * @param spinnerY "Y" spinner control.
     * @param rValIsHigher FlLag indicating the "R" spinner control has a
     *                     higher value than the "Y" spinner control.
     */
    private void checkRSpinner(final Spinner spinnerR, final Spinner spinnerY, final boolean rValIsHigher)
    {
        if (rValIsHigher == false) {
            if (spinnerR.getSelection() > spinnerY.getSelection()) {
                spinnerR.setSelection(spinnerY.getMinimum() + 1);
            }
            if (spinnerR.getSelection() < spinnerR.getMinimum() + 1) {
                spinnerR.setSelection(spinnerY.getSelection());
            }
        } else {
            if (spinnerR.getSelection() < spinnerY.getSelection()) {
                spinnerR.setSelection(spinnerR.getMaximum() - 1);
            }
            if (spinnerR.getSelection() > spinnerR.getMaximum() - 1) {
                spinnerR.setSelection(spinnerY.getSelection());
            }
        }
    }
    
    /**
     * Check the value of the "Y" spinner vs. the "R" spinner and set the "Y" spinner control.
     * @param spinnerR "R" spinner control.
     * @param spinnerY "Y" spinner control.
     * @param rValIsHigher FlLag indicating the "R" spinner control has a
     *                     higher value than the "Y" spinner control.
     */
    private void checkYSpinner(final Spinner spinnerR, final Spinner spinnerY, final boolean rValIsHigher)
    {
        if (rValIsHigher == false) {
            if (spinnerY.getSelection() < spinnerR.getSelection()) {
                spinnerY.setSelection(spinnerY.getMaximum() - 1);
            }
            if (spinnerY.getSelection() > spinnerY.getMaximum() - 1) {
                spinnerY.setSelection(spinnerR.getSelection());
            }
        } else {
            if (spinnerY.getSelection() > spinnerR.getSelection()) {
                spinnerY.setSelection(spinnerY.getMinimum() + 1);
            }
            if (spinnerY.getSelection() < spinnerR.getMinimum()+1) {
                spinnerY.setSelection(spinnerR.getSelection());
            }
        }
    }
    
    /**
     * Set up spinners to use decimals (.1 precision).
     * @param spnrR "R" spinner control.
     * @param spnrY "Y" spinner control.
     * @param rd Range data.
     */
    public void setupDecimalSpinners(final Spinner spnrR, final Spinner spnrY, RangeData rd)
    {
        spnrR.setDigits(1);
        spnrR.setMaximum(rd.getMax() * 10 + 1);
        spnrR.setMinimum(rd.getMin() * 10 - 1);
        
        
        spnrY.setDigits(1);
        spnrY.setMaximum(rd.getMax() * 10 + 1);
        spnrY.setMinimum(rd.getMin() * 10 - 1);        
    }
    
    public double getDecimalValueFromSpinner(final Spinner spnr)
    {
        return (double)(spnr.getSelection()/10.0);
    }
    
    /**
     * Add increment by 5 listeners to the the "R" and "Y" controls. 
     * @param spnrR "R" spinner control.
     * @param spnrY "Y" spinner control.
     */
    public void addIncrementListeners(final Spinner spnrR, final Spinner spnrY)
    {
        spnrR.setIncrement(5);
        spnrY.setIncrement(5);
        
        spnrR.addKeyListener(new KeyAdapter()
        {
            @Override
            public void keyPressed(KeyEvent e)
            {
                if (e.keyCode == SWT.KEYPAD_CR)
                {
                    spnrR.setSelection(roundedToNearest5(spnrR.getSelection()));
                }
            }            
        });
        
        spnrR.addFocusListener(new FocusAdapter()
        {
            @Override
            public void focusLost(FocusEvent e)
            {                                
                spnrR.setSelection(roundedToNearest5(spnrR.getSelection()));
            }            
        });
        
        spnrR.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                if (spnrR.getSelection() > spnrR.getMaximum() - 1) {
                    spnrR.setSelection(spnrR.getMinimum() + 1);
                }
                if (spnrR.getSelection() < spnrR.getMinimum() + 1) {
                    spnrR.setSelection(spnrR.getMaximum() - 1);
                }
            }
        });
        
        spnrY.addKeyListener(new KeyAdapter()
        {
            @Override
            public void keyPressed(KeyEvent e)
            {
                if (e.keyCode == SWT.KEYPAD_CR)
                {
                    spnrY.setSelection(roundedToNearest5(spnrY.getSelection()));
                }
            }            
        });
        
        spnrY.addFocusListener(new FocusAdapter()
        {
            @Override
            public void focusLost(FocusEvent e)
            {                                
                spnrY.setSelection(roundedToNearest5(spnrY.getSelection()));
            }            
        });

        spnrY.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                if (spnrY.getSelection() > spnrY.getMaximum() - 1) {
                    spnrY.setSelection(spnrY.getMinimum() + 1);
                }
                if (spnrY.getSelection() < spnrY.getMinimum() + 1) {
                    spnrY.setSelection(spnrY.getMaximum() - 1);
                }
            }
        });
    }
    
    /**
     * Create R/Y (red/yellew) range labels.
     * @param parentComp Parent composite.
     */
    protected void createRYRangeLabels(Composite parentComp)
    {
        /*
         * R, Y, and Range labels
         */
        // Filler label.
        new Label(parentComp, SWT.NONE);
        
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        gd.widthHint= 30;
        Label rLbl = new Label(parentComp, SWT.CENTER);
        rLbl.setText("r");
        rLbl.setFont(controlFont);
        rLbl.setLayoutData(gd);
        rLbl.setBackground(display.getSystemColor(SWT.COLOR_RED));
        
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        gd.widthHint= 30;
        Label yLbl = new Label(parentComp, SWT.CENTER);
        yLbl.setText("y");
        yLbl.setFont(controlFont);
        yLbl.setLayoutData(gd);
        yLbl.setBackground(display.getSystemColor(SWT.COLOR_YELLOW));
        
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, true, false);
        gd.horizontalIndent = 10;
        Label rangeLbl = new Label(parentComp, SWT.NONE);
        rangeLbl.setText("Range:");
        rangeLbl.setFont(controlFont);
        rangeLbl.setLayoutData(gd);
    }
    
    /**
     * Create Y/R/R/Y (yellow/red/red/yellew) range labels.
     * @param parentComp Parent composite.
     */
    protected void createYRRYRangeLabels(Composite parentComp)
    {
        /*
         * R, Y, and Range labels
         */
        // Filler label.
        new Label(parentComp, SWT.NONE);
        
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        gd.widthHint= 60;
        Label yFromLbl = new Label(parentComp, SWT.CENTER);
        yFromLbl.setText("y(from)");
        yFromLbl.setFont(controlFont);
        yFromLbl.setLayoutData(gd);
        yFromLbl.setBackground(display.getSystemColor(SWT.COLOR_YELLOW));
        
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        gd.widthHint= 60;
        Label rFromLbl = new Label(parentComp, SWT.CENTER);
        rFromLbl.setText("r(from)");
        rFromLbl.setFont(controlFont);
        rFromLbl.setLayoutData(gd);
        rFromLbl.setBackground(display.getSystemColor(SWT.COLOR_RED));

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        gd.widthHint= 40;
        Label rToLbl = new Label(parentComp, SWT.CENTER);
        rToLbl.setText("r(to)");
        rToLbl.setFont(controlFont);
        rToLbl.setLayoutData(gd);
        rToLbl.setBackground(display.getSystemColor(SWT.COLOR_RED));
        
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        gd.widthHint= 40;
        Label yToLbl = new Label(parentComp, SWT.CENTER);
        yToLbl.setText("y(to)");
        yToLbl.setFont(controlFont);
        yToLbl.setLayoutData(gd);
        yToLbl.setBackground(display.getSystemColor(SWT.COLOR_YELLOW));

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, true, false);
        gd.horizontalIndent = 10;
        Label rangeLbl = new Label(parentComp, SWT.NONE);
        rangeLbl.setText("Range:");
        rangeLbl.setFont(controlFont);
        rangeLbl.setLayoutData(gd);
    }

    /**
     * Create visibility controls.
     * @param parentComp Parent composite.
     * @param visRTF Visibility "R" text control.
     * @param visIncR Increase "R" button.
     * @param visDecR Decrease "R" button.
     * @param visYTF Visibility "Y" text control.
     * @param visIncY Increase "Y" button.
     * @param visDecY Decrease "Y" button.
     */
    protected Text[] createVisControls(Composite parentComp, Text visRTF, Button visIncR,
            Button visDecR, Text visYTF, Button visIncY, Button visDecY)
    {        
        /*
         * Vis R
         */
        GridData gd = new GridData();
        Composite visRComp = new Composite(parentComp, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        gl.horizontalSpacing = 1;
        visRComp.setLayout(gl);
        visRComp.setLayoutData(gd);
        
        gd = new GridData(40, SWT.DEFAULT);
        visRTF = new Text(visRComp, SWT.BORDER);
        visRTF.setEditable(false);
        visRTF.setLayoutData(gd);
        visRTF.setText(rangeUtil.getVisibility()[0]);
        
        visIncR = new Button(visRComp, SWT.ARROW | SWT.UP);        
        visDecR = new Button(visRComp, SWT.ARROW | SWT.DOWN);
        
        /*
         * Vis Y
         */
        gd = new GridData();
        Composite visYComp = new Composite(parentComp, SWT.NONE);
        gl = new GridLayout(3, false);
        gl.horizontalSpacing = 1;
        visYComp.setLayout(gl);
        visYComp.setLayoutData(gd);
        
        gd = new GridData(40, SWT.DEFAULT);
        visYTF = new Text(visYComp, SWT.BORDER);
        visYTF.setEditable(false);
        visYTF.setLayoutData(gd);
        visYTF.setText(rangeUtil.getVisibility()[0]);
        
        visIncY = new Button(visYComp, SWT.ARROW | SWT.UP);
        
        visDecY = new Button(visYComp, SWT.ARROW | SWT.DOWN);
        
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, true, true);
        gd.horizontalIndent = 10;
        Label scaWindSpdRangeLbl = new Label(parentComp, SWT.NONE);
        scaWindSpdRangeLbl.setText("1/16 to 10");
        scaWindSpdRangeLbl.setLayoutData(gd);
        
        addVisControlListeners(visRTF, visIncR, visDecR, visYTF, visIncY, visDecY);
        
        Text[] textArray = new Text[2];
        textArray[0] = visRTF;
        textArray[1] = visYTF;
        
        return textArray;
    }
    
    /**
     * Add selection listeners to the R/Y increase/decrease controls.
     * @param visRTF Visibility "R" text control.
     * @param visIncR Increase "R" button.
     * @param visDecR Decrease "R" button.
     * @param visYTF Visibility "Y" text control.
     * @param visIncY Increase "Y" button.
     * @param visDecY Decrease "Y" button.
     */
    private void addVisControlListeners(final Text visRTF, final Button visIncR,
            final Button visDecR, final Text visYTF, final Button visIncY, final Button visDecY)
    {
        /*
         * Visibility R value Up button.
         */
        visIncR.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent event)
            {
//                System.out.println("+++ visRIndex up = " + visRIndex);
//                System.out.println("+++ visYIndex = " + visYIndex);
                if (visRIndex == visYIndex)
                {
                    visRIndex = 0;
                }
                else
                {
                    ++visRIndex;
                }
                visRTF.setText(rangeUtil.getVisibility()[visRIndex]);
                // System.out.println("+++ visRIndex up = " + visRIndex);
                // System.out.println("+++ visYIndex = " + visYIndex);
            }
        });
        
        /*
         * Visibility R value Down button.
         */
        visDecR.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent event)
            {
//                System.out.println("--- visRIndex down = " + visRIndex);
//                System.out.println("--- visYIndex = " + visYIndex);
                if (visRIndex == 0)
                {
                    visRIndex = visYIndex;
                }
                else
                {
                    --visRIndex;
                }
                visRTF.setText(rangeUtil.getVisibility()[visRIndex]);

                // System.out.println("--- visRIndex down = " + visRIndex);
                // System.out.println("--- visYIndex = " + visYIndex);
            }
        });
        
        /*
         * Visibility Y value Up button.
         */
        visIncY.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent event)
            {
                if (visYIndex == (rangeUtil.getVisibility().length - 1))
                {
                    visYIndex = visRIndex;
                }
                else
                {
                    ++visYIndex;
                }
                visYTF.setText(rangeUtil.getVisibility()[visYIndex]);
            }
        });
        
        /*
         * Visibility Y value Down button.
         */
        visDecY.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent event)
            {               
                if (visYIndex == visRIndex)
                {
                    visYIndex = rangeUtil.getVisibility().length - 1;
                }
                else
                {
                    --visYIndex;
                }
                visYTF.setText(rangeUtil.getVisibility()[visYIndex]);
            }
        });
    }
    
    /**
     * Create the bottom action buttons.
     */
    private void createBottomButtons()
    {        
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite mainButtonComp = new Composite(shell, SWT.NONE);
        mainButtonComp.setLayout(new GridLayout(1, false));
        mainButtonComp.setLayoutData(gd);
        
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(mainButtonComp, SWT.NONE);
        buttonComp.setLayout(new GridLayout(3, false));
        buttonComp.setLayoutData(gd);
        
        gd = new GridData(100, SWT.DEFAULT);
        Button applyBtn = new Button(buttonComp, SWT.PUSH);
        applyBtn.setText("Apply");
        applyBtn.setLayoutData(gd);
        applyBtn.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent event)
            {
            	if (!determineError())
            	{
            		applyAction();
            	} else
            	{
            		showWindDirThresholdErrorMsgBx();
            	}
            }
        });
        
        gd = new GridData(130, SWT.DEFAULT);
        Button applyCloseBtn = new Button(buttonComp, SWT.PUSH);
        applyCloseBtn.setText("Apply && Close");
        applyCloseBtn.setLayoutData(gd);
        applyCloseBtn.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent event)
            {
            	if (!determineError()) {
                    applyAction();
                    shell.dispose();
            	} else 
            	{
            		showWindDirThresholdErrorMsgBx();
            	}
            }
        });
        
        gd = new GridData(100, SWT.DEFAULT);
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent event)
            {
                shell.dispose();
            }
        });
    }
    
    /**
     * Add a separator bar to a composite.
     * @param parentComp Parent composite.
     */
    protected void addSeparator(Composite parentComp)
    {
        GridLayout gl = (GridLayout)parentComp.getLayout();
        
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = gl.numColumns;
        Label sepLbl = new Label(parentComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);        
    }
    
    protected void showWindDirThresholdErrorMsgBx(){
    	MessageBox mb = new MessageBox(shell,SWT.OK);
    	mb.setMessage("Inconsistent Wind/Swell Direction Thresholds! See the User's Guide for Editing Wind/Swell Direction Thredsholds!");
    	mb.open();
    }
    
    protected int roundedToNearest5(int arg) {
        int result = 0;
        if (arg % 5 > 2) {
            result = arg + 5 - arg % 5;
        } else {
            result = arg - arg % 5;
        }
        return result;
    }
    
    /**
     * Initialize components.
     */
    protected abstract void initializeComponents();    
    
    protected abstract void updateControlsWithData();    
    protected abstract void updateData();
    protected abstract void applyAction();
 
    protected boolean determineError() {return false;}
    
    /**
     * In a circle, red must be in side of yellow
     */
    protected boolean determineError (int yellowFrom, int redFrom, int redTo, int yellowTo){
    	if (yellowTo < yellowFrom) {
    		if (redTo < yellowTo) {
    			if (redFrom < yellowTo) {
    				redFrom+=360;
    			}
    			redTo+=360;
    		}
    		yellowTo+=360;
    	}
    	
    	if ((yellowFrom <= redFrom) && (redFrom <= redTo) && (redTo <=yellowTo)) 
    		return false;

    	return true;

 };
}
