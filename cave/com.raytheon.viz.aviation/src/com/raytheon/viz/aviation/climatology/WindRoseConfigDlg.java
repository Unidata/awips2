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
package com.raytheon.viz.aviation.climatology;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.INIConfiguration;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.ColorDialog;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * WindRoseConfigDlg class displays the configuration dialog for wind rose.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 28 FEB 2008  938        lvenable    Initial creation 
 * 18 JUN 2008  1119       lvenable    Updated dialog to reflect user changes.
 * 04 OCT 2012  1229       rferrel     Made non-blocking.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class WindRoseConfigDlg extends CaveSWTDialog {

    /**
     * Calm wind color label.
     */
    private Label calmColorLbl;

    /**
     * Variable wind color label.
     */
    private Label variableColorLbl;

    /**
     * Wind speed 1 color label.
     */
    private Label windSpeed1ColorLbl;

    /**
     * Wind speed 2 color label.
     */
    private Label windSpeed2ColorLbl;

    /**
     * Wind speed 3 color label.
     */
    private Label windSpeed3ColorLbl;

    /**
     * Above wind speed 3 color label.
     */
    private Label aboveColorLbl;

    /**
     * Calm wind color.
     */
    private Color calmColor;

    /**
     * Variable wind color.
     */
    private Color variableColor;

    /**
     * Wind speed 1 color.
     */
    private Color windSpeed1Color;

    /**
     * Wind speed 2 color.
     */
    private Color windSpeed2Color;

    /**
     * Wind speed 3 color.
     */
    private Color windSpeed3Color;

    /**
     * Above wind speed 3 color.
     */
    private Color aboveColor;

    /**
     * Wind speed 1 text control..
     */
    private Spinner windSpeed1Spnr;

    /**
     * Wind speed 2 text control..
     */
    private Spinner windSpeed2Spnr;

    /**
     * Wind speed 3 text control..
     */
    private Spinner windSpeed3Spnr;

    /**
     * Points combo box.
     */
    private Combo pointsCbo;

    /**
     * Wind Rose configuration data.
     */
    private WindRoseConfigData windRoseConfigData;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public WindRoseConfigDlg(Shell parent, WindRoseConfigData windRoseConfigData) {
        super(parent, SWT.DIALOG_TRIM, CAVE.PERSPECTIVE_INDEPENDENT
                | CAVE.DO_NOT_BLOCK);
        setText("Config");

        this.windRoseConfigData = windRoseConfigData;
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    @Override
    protected void disposed() {
        // Dispose of the colors.
        calmColor.dispose();
        variableColor.dispose();
        windSpeed1Color.dispose();
        windSpeed2Color.dispose();
        windSpeed3Color.dispose();
        aboveColor.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        initializeColors();

        createColorControls();

        addSeparator();

        createBottomButtons();
    }

    /**
     * Initialize the colors for the labels.
     */
    private void initializeColors() {
        calmColor = new Color(getDisplay(), windRoseConfigData.getCalmRgb());
        variableColor = new Color(getDisplay(),
                windRoseConfigData.getVariableRgb());
        windSpeed1Color = new Color(getDisplay(),
                windRoseConfigData.getVar1Rgb());
        windSpeed2Color = new Color(getDisplay(),
                windRoseConfigData.getVar2Rgb());
        windSpeed3Color = new Color(getDisplay(),
                windRoseConfigData.getVar3Rgb());
        aboveColor = new Color(getDisplay(), windRoseConfigData.getAboveRgb());
    }

    /**
     * Create the controls to display/change the colors.
     */
    private void createColorControls() {
        Composite colorComp = new Composite(shell, SWT.NONE);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(4, false);
        colorComp.setLayout(gl);
        colorComp.setLayoutData(gd);

        // Calm
        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        gd.horizontalSpan = 2;
        Label calmLbl = new Label(colorComp, SWT.RIGHT);
        calmLbl.setText("Calm:");
        calmLbl.setLayoutData(gd);

        gd = new GridData(140, SWT.DEFAULT);
        calmColorLbl = new Label(colorComp, SWT.BORDER | SWT.CENTER);
        if (windRoseConfigData.calmDefaultColor() == true) {
            calmColorLbl.setText("White");
        } else {
            calmColorLbl.setText("RGB = " + calmColor.getRed() + ", "
                    + calmColor.getGreen() + ", " + calmColor.getBlue());
        }

        calmColorLbl.setBackground(calmColor);
        calmColorLbl.setToolTipText("RGB = " + calmColor.getRed() + ", "
                + calmColor.getGreen() + ", " + calmColor.getBlue());
        calmColorLbl.setLayoutData(gd);

        Button changeCalmBtn = new Button(colorComp, SWT.PUSH);
        changeCalmBtn.setText("Change...");
        changeCalmBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                RGB rgb = changeLabelColor(calmColor);

                if (rgb != null) {
                    if (calmColor != null) {
                        calmColor.dispose();
                    }
                    calmColor = new Color(getDisplay(), rgb);
                    calmColorLbl.setForeground(getTextColor(rgb));
                    calmColorLbl.setBackground(calmColor);
                    calmColorLbl.setText("RGB = " + rgb.red + ", " + rgb.green
                            + ", " + rgb.blue);
                    calmColorLbl.setToolTipText("RGB = " + rgb.red + ", "
                            + rgb.green + ", " + rgb.blue);
                }
            }
        });

        // Variable
        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        gd.horizontalSpan = 2;
        Label variableLbl = new Label(colorComp, SWT.RIGHT);
        variableLbl.setText("Variable:");
        variableLbl.setLayoutData(gd);

        gd = new GridData(140, SWT.DEFAULT);
        variableColorLbl = new Label(colorComp, SWT.BORDER | SWT.CENTER);
        if (windRoseConfigData.variableDefaultColor() == true) {
            variableColorLbl.setText("Brown");
        } else {
            variableColorLbl
                    .setText("RGB = " + variableColor.getRed() + ", "
                            + variableColor.getGreen() + ", "
                            + variableColor.getBlue());
        }
        variableColorLbl.setBackground(variableColor);
        variableColorLbl.setToolTipText("RGB = " + variableColor.getRed()
                + ", " + variableColor.getGreen() + ", "
                + variableColor.getBlue());
        variableColorLbl.setLayoutData(gd);

        Button changeVariableBtn = new Button(colorComp, SWT.PUSH);
        changeVariableBtn.setText("Change...");
        changeVariableBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                RGB rgb = changeLabelColor(variableColor);

                if (rgb != null) {
                    if (variableColor != null) {
                        variableColor.dispose();
                    }
                    variableColor = new Color(getDisplay(), rgb);
                    variableColorLbl.setForeground(getTextColor(rgb));
                    variableColorLbl.setBackground(variableColor);
                    variableColorLbl.setText("RGB = " + rgb.red + ", "
                            + rgb.green + ", " + rgb.blue);
                    variableColorLbl.setToolTipText("RGB = " + rgb.red + ", "
                            + rgb.green + ", " + rgb.blue);
                }
            }
        });

        // Wind Speed 1
        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        Label windSpeed1Lbl = new Label(colorComp, SWT.RIGHT);
        windSpeed1Lbl.setText("< ");
        windSpeed1Lbl.setLayoutData(gd);

        gd = new GridData(30, SWT.DEFAULT);
        windSpeed1Spnr = new Spinner(colorComp, SWT.BORDER);
        windSpeed1Spnr.setDigits(0);
        windSpeed1Spnr.setIncrement(1);
        windSpeed1Spnr.setPageIncrement(3);
        windSpeed1Spnr.setMinimum(0);
        windSpeed1Spnr.setMaximum(100);
        windSpeed1Spnr.setSelection(windRoseConfigData.getVar1Max());
        windSpeed1Spnr.setLayoutData(gd);

        gd = new GridData(140, SWT.DEFAULT);
        windSpeed1ColorLbl = new Label(colorComp, SWT.BORDER | SWT.CENTER);
        if (windRoseConfigData.var1DefaultColor() == true) {
            windSpeed1ColorLbl.setText("Blue");
        } else {
            windSpeed1ColorLbl.setText("RGB = " + windSpeed1Color.getRed()
                    + ", " + windSpeed1Color.getGreen() + ", "
                    + windSpeed1Color.getBlue());
        }
        windSpeed1ColorLbl.setBackground(windSpeed1Color);
        windSpeed1ColorLbl.setToolTipText("RGB = " + windSpeed1Color.getRed()
                + ", " + windSpeed1Color.getGreen() + ", "
                + windSpeed1Color.getBlue());
        windSpeed1ColorLbl.setLayoutData(gd);

        Button changeWindSpeed1Btn = new Button(colorComp, SWT.PUSH);
        changeWindSpeed1Btn.setText("Change...");
        changeWindSpeed1Btn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                RGB rgb = changeLabelColor(windSpeed1Color);

                if (rgb != null) {
                    if (windSpeed1Color != null) {
                        windSpeed1Color.dispose();
                    }
                    windSpeed1Color = new Color(getDisplay(), rgb);
                    windSpeed1ColorLbl.setForeground(getTextColor(rgb));
                    windSpeed1ColorLbl.setBackground(windSpeed1Color);
                    windSpeed1ColorLbl.setText("RGB = " + rgb.red + ", "
                            + rgb.green + ", " + rgb.blue);
                    windSpeed1ColorLbl.setToolTipText("RGB = " + rgb.red + ", "
                            + rgb.green + ", " + rgb.blue);
                }
            }
        });

        // Wind Speed 2
        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        Label windSpeed2Lbl = new Label(colorComp, SWT.RIGHT);
        windSpeed2Lbl.setText("< ");
        windSpeed2Lbl.setLayoutData(gd);

        gd = new GridData(30, SWT.DEFAULT);
        windSpeed2Spnr = new Spinner(colorComp, SWT.BORDER);
        windSpeed2Spnr.setDigits(0);
        windSpeed2Spnr.setIncrement(1);
        windSpeed2Spnr.setPageIncrement(3);
        windSpeed2Spnr.setMinimum(0);
        windSpeed2Spnr.setMaximum(100);
        windSpeed2Spnr.setSelection(windRoseConfigData.getVar2Max());
        windSpeed2Spnr.setLayoutData(gd);

        gd = new GridData(140, SWT.DEFAULT);
        windSpeed2ColorLbl = new Label(colorComp, SWT.BORDER | SWT.CENTER);
        if (windRoseConfigData.var1DefaultColor() == true) {
            windSpeed2ColorLbl.setText("Green");
        } else {
            windSpeed2ColorLbl.setText("RGB = " + windSpeed2Color.getRed()
                    + ", " + windSpeed2Color.getGreen() + ", "
                    + windSpeed2Color.getBlue());
        }
        windSpeed2ColorLbl.setBackground(windSpeed2Color);
        windSpeed2ColorLbl.setToolTipText("RGB = " + windSpeed2Color.getRed()
                + ", " + windSpeed2Color.getGreen() + ", "
                + windSpeed2Color.getBlue());
        windSpeed2ColorLbl.setLayoutData(gd);

        Button changeWindSpeed2Btn = new Button(colorComp, SWT.PUSH);
        changeWindSpeed2Btn.setText("Change...");
        changeWindSpeed2Btn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                RGB rgb = changeLabelColor(windSpeed2Color);

                if (rgb != null) {
                    if (windSpeed2Color != null) {
                        windSpeed2Color.dispose();
                    }
                    windSpeed2Color = new Color(getDisplay(), rgb);
                    windSpeed2ColorLbl.setForeground(getTextColor(rgb));
                    windSpeed2ColorLbl.setBackground(windSpeed2Color);
                    windSpeed2ColorLbl.setText("RGB = " + rgb.red + ", "
                            + rgb.green + ", " + rgb.blue);
                    windSpeed2ColorLbl.setToolTipText("RGB = " + rgb.red + ", "
                            + rgb.green + ", " + rgb.blue);
                }
            }
        });

        // Wind Speed 3
        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        Label windSpeed3Lbl = new Label(colorComp, SWT.RIGHT);
        windSpeed3Lbl.setText("< ");
        windSpeed3Lbl.setLayoutData(gd);

        gd = new GridData(30, SWT.DEFAULT);
        windSpeed3Spnr = new Spinner(colorComp, SWT.BORDER);
        windSpeed3Spnr.setDigits(0);
        windSpeed3Spnr.setIncrement(1);
        windSpeed3Spnr.setPageIncrement(3);
        windSpeed3Spnr.setMinimum(0);
        windSpeed3Spnr.setMaximum(100);
        windSpeed3Spnr.setSelection(windRoseConfigData.getVar3Max());
        windSpeed3Spnr.setLayoutData(gd);

        gd = new GridData(140, SWT.DEFAULT);
        windSpeed3ColorLbl = new Label(colorComp, SWT.BORDER | SWT.CENTER);
        if (windRoseConfigData.var1DefaultColor() == true) {
            windSpeed3ColorLbl.setText("Red");
        } else {
            windSpeed3ColorLbl.setText("RGB = " + windSpeed3Color.getRed()
                    + ", " + windSpeed3Color.getGreen() + ", "
                    + windSpeed3Color.getBlue());
        }
        windSpeed3ColorLbl.setBackground(windSpeed3Color);
        windSpeed3ColorLbl.setToolTipText("RGB = " + windSpeed3Color.getRed()
                + ", " + windSpeed3Color.getGreen() + ", "
                + windSpeed3Color.getBlue());
        windSpeed3ColorLbl.setLayoutData(gd);

        Button changeWindSpeed3Btn = new Button(colorComp, SWT.PUSH);
        changeWindSpeed3Btn.setText("Change...");
        changeWindSpeed3Btn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                RGB rgb = changeLabelColor(windSpeed3Color);

                if (rgb != null) {
                    if (windSpeed3Color != null) {
                        windSpeed3Color.dispose();
                    }
                    windSpeed3Color = new Color(getDisplay(), rgb);
                    windSpeed3ColorLbl.setForeground(getTextColor(rgb));
                    windSpeed3ColorLbl.setBackground(windSpeed3Color);
                    windSpeed3ColorLbl.setText("RGB = " + rgb.red + ", "
                            + rgb.green + ", " + rgb.blue);
                    windSpeed3ColorLbl.setToolTipText("RGB = " + rgb.red + ", "
                            + rgb.green + ", " + rgb.blue);
                }
            }
        });

        // Above
        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        gd.horizontalSpan = 2;
        Label aboveLbl = new Label(colorComp, SWT.RIGHT);
        aboveLbl.setText("Above:");
        aboveLbl.setLayoutData(gd);

        gd = new GridData(140, SWT.DEFAULT);
        aboveColorLbl = new Label(colorComp, SWT.BORDER | SWT.CENTER);
        if (windRoseConfigData.var1DefaultColor() == true) {
            aboveColorLbl.setText("Purple");
        } else {
            aboveColorLbl.setText("RGB = " + aboveColor.getRed() + ", "
                    + aboveColor.getGreen() + ", " + aboveColor.getBlue());
        }
        aboveColorLbl.setBackground(aboveColor);
        aboveColorLbl.setToolTipText("RGB = " + aboveColor.getRed() + ", "
                + aboveColor.getGreen() + ", " + aboveColor.getBlue());
        aboveColorLbl.setLayoutData(gd);

        Button changeAboveBtn = new Button(colorComp, SWT.PUSH);
        changeAboveBtn.setText("Change...");
        changeAboveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                RGB rgb = changeLabelColor(aboveColor);

                if (rgb != null) {
                    if (aboveColor != null) {
                        aboveColor.dispose();
                    }
                    aboveColor = new Color(getDisplay(), rgb);
                    aboveColorLbl.setForeground(getTextColor(rgb));
                    aboveColorLbl.setBackground(aboveColor);
                    aboveColorLbl.setText("RGB = " + rgb.red + ", " + rgb.green
                            + ", " + rgb.blue);
                    aboveColorLbl.setToolTipText("RGB = " + rgb.red + ", "
                            + rgb.green + ", " + rgb.blue);
                }
            }
        });

        // Points combo box
        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        gd.horizontalSpan = 2;
        Label pointsLbl = new Label(colorComp, SWT.RIGHT);
        pointsLbl.setText("Points:");
        pointsLbl.setLayoutData(gd);

        gd = new GridData(60, SWT.DEFAULT);
        pointsCbo = new Combo(colorComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        pointsCbo.add("8");
        pointsCbo.add("16");
        pointsCbo.add("36");

        if (windRoseConfigData.getPoints() == 8) {
            pointsCbo.select(0);
        } else if (windRoseConfigData.getPoints() == 16) {
            pointsCbo.select(1);
        } else {
            pointsCbo.select(2);
        }
        pointsCbo.setLayoutData(gd);
    }

    /**
     * Add a horizontal separator to the display.
     */
    private void addSeparator() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

    /**
     * Create the OK, Save, and Cancel buttons.
     */
    private void createBottomButtons() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        buttonComp.setLayout(new GridLayout(3, false));
        buttonComp.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                // Accept changes code goes here...
                if (validateVariableRanges() == false) {
                    displayMessage("Error", "Invalid wind thresholds.\n"
                            + "Condition v1 < v2 < v3 not met.");
                    return;
                }

                try {
                    updateData();
                } catch (ConfigurationException e) {
                    // TODO Auto-generated catch block
                    System.err.println(e.getStackTrace());
                } catch (LocalizationOpFailedException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                } catch (IOException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }

                shell.dispose();
            }
        });

        gd = new GridData(80, SWT.DEFAULT);
        Button saveBtn = new Button(buttonComp, SWT.PUSH);
        saveBtn.setText("Save");
        saveBtn.setLayoutData(gd);
        saveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                // Accept changes code goes here...
                if (validateVariableRanges() == false) {
                    displayMessage("Error", "Invalid wind thresholds.\n"
                            + "Condition v1 < v2 < v3 not met.");
                    return;
                }

                try {
                    updateData();
                } catch (ConfigurationException e) {
                    // TODO Auto-generated catch block
                    System.err.println(e.getStackTrace());
                } catch (LocalizationOpFailedException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                } catch (IOException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }

                shell.dispose();
            }
        });

        gd = new GridData(80, SWT.DEFAULT);
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    /**
     * Change the wind label color.
     * 
     * @param color
     *            Current color.
     * @return The new RGB value. If null is returned then no color was
     *         selected.
     */
    private RGB changeLabelColor(Color color) {
        ColorDialog colorDlg = new ColorDialog(shell);

        // Set the selected color in the dialog from
        // user's selected color
        colorDlg.setRGB(color.getRGB());

        // Change the title bar text
        colorDlg.setText("Select a Color");

        // Open the dialog and retrieve the selected color
        RGB rgb = colorDlg.open();

        return rgb;
    }

    /**
     * The text color returned is either white or black depending on how dark
     * the background RGB value is. The color returned does not get disposed.
     * 
     * @param rgb
     *            RGB background color.
     * @return White or Black system color.
     */
    private Color getTextColor(RGB rgb) {
        if (rgb.red < 165 && rgb.green < 165 && rgb.blue < 165) {
            return getDisplay().getSystemColor(SWT.COLOR_GRAY);
        }

        if (rgb.red < 165 && rgb.green < 165 && rgb.blue > 165) {
            return getDisplay().getSystemColor(SWT.COLOR_GRAY);
        }

        return getDisplay().getSystemColor(SWT.COLOR_BLACK);
    }

    private boolean validateVariableRanges() {
        if (windSpeed1Spnr.getSelection() < windSpeed2Spnr.getSelection()
                && windSpeed2Spnr.getSelection() < windSpeed3Spnr
                        .getSelection()) {
            return true;
        }

        return false;
    }

    /**
     * Update data.
     * 
     * @throws IOException
     * @throws ConfigurationException
     * @throws LocalizationOpFailedException
     * @throws LocalizationCommunicationException
     */
    private void updateData() throws IOException, ConfigurationException,
            LocalizationOpFailedException {
        windRoseConfigData.setCalmRgb(calmColor.getRGB());
        windRoseConfigData.setVariableRgb(variableColor.getRGB());
        windRoseConfigData.setVar1Rgb(windSpeed1Color.getRGB());
        windRoseConfigData.setVar2Rgb(windSpeed2Color.getRGB());
        windRoseConfigData.setVar3Rgb(windSpeed3Color.getRGB());
        windRoseConfigData.setAboveRgb(aboveColor.getRGB());

        windRoseConfigData.setVar1Max(windSpeed1Spnr.getSelection());
        windRoseConfigData.setVar2Max(windSpeed2Spnr.getSelection());
        windRoseConfigData.setVar3Max(windSpeed3Spnr.getSelection());

        windRoseConfigData.setPoints(Integer.valueOf(pointsCbo.getText())
                .intValue());

        // We must save the data here so that the python code can see the
        // changes.
        String filepath = "aviation/config/windrose.cfg";
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
        LocalizationFile lFile = pm.getLocalizationFile(context, filepath);
        File file = lFile.getFile();

        if (!file.getParentFile().exists()) {
            file.getParentFile().mkdirs();
        }

        INIConfiguration config;
        config = new INIConfiguration();

        config.setProperty("wind.num_spd", "4");
        config.setProperty("wind.num_dir", pointsCbo.getText());

        config.setProperty("wind_calm.value", "0");
        config.setProperty("wind_calm.color",
                RGBColors.getColorName(calmColor.getRGB()));

        config.setProperty("wind_vrb.value", "0");
        config.setProperty("wind_vrb.color",
                RGBColors.getColorName(variableColor.getRGB()));

        config.setProperty("wind_1.value",
                Integer.toString(windSpeed1Spnr.getSelection()));
        config.setProperty("wind_1.color",
                RGBColors.getColorName(windSpeed1Color.getRGB()));

        config.setProperty("wind_2.value",
                Integer.toString(windSpeed2Spnr.getSelection()));
        config.setProperty("wind_2.color",
                RGBColors.getColorName(windSpeed2Color.getRGB()));

        config.setProperty("wind_3.value",
                Integer.toString(windSpeed3Spnr.getSelection()));
        config.setProperty("wind_3.color",
                RGBColors.getColorName(windSpeed3Color.getRGB()));

        config.setProperty("wind_4.color",
                RGBColors.getColorName(aboveColor.getRGB()));

        FileWriter writer = new FileWriter(file);
        config.save(writer);
        writer.close();
        lFile.save();
    }

    /**
     * Display message
     * 
     * @param title
     *            The title
     * @param message
     *            The message
     */
    private void displayMessage(String title, String message) {
        MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
        mb.setText(title);
        mb.setMessage(message);
        mb.open();
    }
}
