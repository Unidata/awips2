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
package com.raytheon.uf.viz.alertviz.ui.dialogs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.alertviz.ui.color.ColorWheelComp;
import com.raytheon.uf.viz.alertviz.ui.color.IColorWheelChange;

/**
 * This class displays the color dialog for changing text color.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05 Oct 2008             lvenable    Initial creation.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class AlertVisColorDlg extends Dialog implements IColorWheelChange
{
    /**
     * Dialog shell.
     */
    private Shell shell;
    
    /**
     * The display control.
     */
    private Display display;
    
    /**
     * Return object when the shell is disposed.
     */
    private Boolean returnObj = null;
    
    /**
     * Label displaying the selected foreground and
     * background colors.
     */
    private Label colorLabel;
    
    /**
     * Label displaying the selected foreground and
     * background colors but the foreground/background
     * colors are reversed.
     */
    private Label reverseColorLabel;
    
    /**
     * Label background color.
     */
    private Color labelBackGroundColor;
    
    /**
     * Label foreground color.
     */
    private Color labelForeGroundColor;
    
    /**
     * Large font for the color label.
     */
    private Font largeFont;
    
    /**
     * "Text color" label for the color wheel.
     */
    private final String TEXT_COLOR = "Text Color";
    
    /**
     * "Background color" label for the color wheel.
     */
    private final String BACKGROUND_COLOR = "Background Color";
    
    /**
     * Color wheel composite for the foreground (text) color.
     */
    private ColorWheelComp foregroundColorWheel;
    
    /**
     * Color wheel composite for the background color.
     */
    private ColorWheelComp backGroundColorWheel;
    
    /**
     * Old foreground RGB color.
     */
    private RGB oldForegroundRGB;
    
    /**
     * Old background RGB color.
     */
    private RGB oldBackgroundRGB;
    
    /**
     * New foreground RGB color.
     */
    private RGB newForegroundRGB;    
    
    /**
     * New background RGB color.
     */
    private RGB newBackgroundRGB;
    
    /**
     * Constructor.
     * @param parentShell Parent shell.
     * @param fgRGB Foreground RGB.
     * @param bgRGB Background RGB.
     */
    public AlertVisColorDlg(Shell parentShell, RGB fgRGB, RGB bgRGB)
    {    
        super(parentShell, 0);
        
        this.oldForegroundRGB = fgRGB;
        this.oldBackgroundRGB = bgRGB;
    }
    
    /**
     * Open method used to display the color dialog.
     * @return True/False.
     */
    public Object open()
    {        
        Shell parent = getParent();
        display = parent.getDisplay();
        shell = new Shell(parent, SWT.DIALOG_TRIM);
        shell.setText("Alert Visualization Color Dialog");
        
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        shell.setLayout(mainLayout);
      
        // Initialize all of the controls and layouts
        initializeComponents();
        
        shell.pack();        
        shell.open();       
        
        while (!shell.isDisposed())
        {
            if (!display.readAndDispatch())
            {
                display.sleep();
            }
        }
        
        labelBackGroundColor.dispose();
        labelForeGroundColor.dispose();
        largeFont.dispose();
        
        return returnObj;
    }
    
    /**
     * Initialize the controls on the display.
     */
    private void initializeComponents()
    {        
        
        // Setup the font and colors.
        largeFont = new Font(display, "Monospace", 16, SWT.BOLD);
        labelBackGroundColor = new Color(display, oldBackgroundRGB);
        labelForeGroundColor = new Color(display, oldForegroundRGB);
        
        // Create the main composite.
        Composite mainComposite = new Composite(shell, SWT.NONE);
        mainComposite.setLayout(new GridLayout(2, false));
        
        // Create the color wheels.
        createColorChangeControls(mainComposite);
        
        // Create the buttons at the bottom of the display.
        createBottomButtons();
    }
    
    /**
     * Create the color wheels to change the text foreground and
     * background colors.
     * @param parentComp Parent composite.
     */
    private void createColorChangeControls(Composite parentComp)
    {
        Composite colorComp = new Composite(parentComp, SWT.NONE);
        colorComp.setLayout(new GridLayout(1, false));
        
        foregroundColorWheel = new ColorWheelComp(colorComp, this, TEXT_COLOR, true);
        foregroundColorWheel.showRgbSliders(true);
        foregroundColorWheel.setColor(labelForeGroundColor.getRGB());
        
        backGroundColorWheel = new ColorWheelComp(colorComp, this, BACKGROUND_COLOR, true);
        backGroundColorWheel.showRgbSliders(true);
        backGroundColorWheel.setColor(labelBackGroundColor.getRGB());
        
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        colorLabel = new Label(colorComp, SWT.CENTER);
        colorLabel.setText("Sample Text");
        colorLabel.setFont(largeFont);
        colorLabel.setBackground(labelBackGroundColor);
        colorLabel.setForeground(labelForeGroundColor);
        colorLabel.setLayoutData(gd);
        
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        reverseColorLabel = new Label(colorComp, SWT.CENTER);
        reverseColorLabel.setText("Reversed Colors");
        reverseColorLabel.setFont(largeFont);
        reverseColorLabel.setBackground(labelForeGroundColor);
        reverseColorLabel.setForeground(labelBackGroundColor);
        reverseColorLabel.setLayoutData(gd);
    }
    
    /**
     * Create the bottom controls buttons.
     */
    private void createBottomButtons()
    {
        Composite buttonArea = new Composite(shell, SWT.NONE);
        buttonArea.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        buttonArea.setLayout(new GridLayout(1, false));

        // The intent is for this composite to be centered
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite buttons = new Composite(buttonArea, SWT.NONE);
        buttons.setLayoutData(gd);
        buttons.setLayout(new GridLayout(2, true));

        gd = new GridData(120, SWT.DEFAULT);
        Button applyColorsBtn = new Button(buttons, SWT.PUSH);
        applyColorsBtn.setText("Apply Colors");
        applyColorsBtn.setLayoutData(gd);
        applyColorsBtn.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent event)
            {
                returnObj = true;
                newForegroundRGB = colorLabel.getForeground().getRGB();
                newBackgroundRGB = colorLabel.getBackground().getRGB();
                shell.dispose();
            }
        });

        gd = new GridData(120, SWT.DEFAULT);
        Button cancelBtn = new Button(buttons, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent event)
            {
                returnObj = false;
                shell.dispose();
            }
        });
    }
    
    /**
     * Change the color of the label foreground or background
     * color based on the selection in the color wheels.
     */
    public void colorChange(RGB rgb, String colorWheelTitle)
    {
        if (colorWheelTitle.compareTo(TEXT_COLOR) == 0)
        {
            labelForeGroundColor.dispose();
            labelForeGroundColor = new Color(display, rgb);
            colorLabel.setForeground(labelForeGroundColor);
            reverseColorLabel.setBackground(labelForeGroundColor);
        }
        else
        {
            labelBackGroundColor.dispose();
            labelBackGroundColor = new Color(display, rgb);
            colorLabel.setBackground(labelBackGroundColor);
            reverseColorLabel.setForeground(labelBackGroundColor);
        }        
    }
    
    /**
     * Get the foreground RGB.
     * @return The foreground RGB.
     */
    public RGB getForegroungRGB()
    {
        return newForegroundRGB;
    }
    
    /**
     * Get the background RGB.
     * @return The background RGB.
     */
    public RGB getBackgroungRGB()
    {
        return newBackgroundRGB;
    }
}
