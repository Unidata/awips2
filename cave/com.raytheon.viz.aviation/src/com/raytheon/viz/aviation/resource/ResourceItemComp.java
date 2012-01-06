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
package com.raytheon.viz.aviation.resource;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.ColorDialog;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.FontDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Spinner;

import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.viz.aviation.resource.ResourceConfigMgr.ResourceTag;
import com.raytheon.viz.aviation.resource.ResourceConfigMgr.ResourceType;

/**
 * This is a composite that holds all of the resource editor configuration
 * controls.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 17, 2009            lvenable     Initial creation
 * Feb 18, 2011            rferrel      Selecting audio file now works.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class ResourceItemComp extends Composite {
    /**
     * Resource label displaying the resource "name".
     */
    private Label resourceLbl;

    /**
     * Resize callback called when a control may be resized do to a font change.
     */
    private IResize resizeCB;

    /**
     * Map containing the resource tag (key) and the String value.
     */
    private HashMap<ResourceTag, String> resTagValMap;

    /**
     * Array of all the Reset buttons.
     */
    private ArrayList<Button> resetButtons;

    /**
     * Temporary font.
     */
    private Font tmpFont;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     * @param resizeCB
     *            Resize callback function.
     */
    public ResourceItemComp(Composite parent, IResize resizeCB) {
        super(parent, SWT.NONE);

        this.resizeCB = resizeCB;

        initialize();
    }

    /**
     * Initialize method.
     */
    private void initialize() {
        resTagValMap = new HashMap<ResourceTag, String>();
        resetButtons = new ArrayList<Button>();

        ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();

        Set<ResourceTag> tags = configMgr.getResourceTags();

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(3, false);
        gl.verticalSpacing = 1;
        gl.marginHeight = 1;
        this.setLayout(gl);
        this.setLayoutData(gd);

        for (ResourceTag tag : tags) {

            createResourceControls(configMgr, tag);
        }

        this.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                tmpFont.dispose();
            }
        });
    }

    /**
     * Create the resource controls.
     * 
     * @param configMgr
     *            Configuration manager.
     * @param tag
     *            Resource tag.
     */
    private void createResourceControls(ResourceConfigMgr configMgr,
            ResourceTag tag) {
        GridData gd = new GridData();
        gd.minimumWidth = 80;
        gd.verticalIndent = 3;
        Button resetBtn = new Button(this, SWT.PUSH);
        resetBtn.setText("Reset");
        resetBtn.setLayoutData(gd);
        resetBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                Button sourceBtn = (Button) e.getSource();
                resetResourceItem(sourceBtn);
                resizeAction();
            }
        });

        gd = new GridData();
        gd.horizontalIndent = 20;
        gd.verticalIndent = 3;
        resourceLbl = new Label(this, SWT.NONE);
        resourceLbl.setText(tag.getTagName());
        resourceLbl.setToolTipText(configMgr.getResourceDesc(tag));
        resourceLbl.setLayoutData(gd);

        Control control = settingControl(configMgr, tag);

        // Set the reset button to have the control in the setData method.
        resetBtn.setData(control);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 3;
        gd.verticalIndent = 3;
        Label sepLbl = new Label(this, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);

        resetButtons.add(resetBtn);
    }

    private String audioFilterPath;

    /**
     * Create the right control for the specific resource tag.
     * 
     * @param configMgr
     *            Configuration manager.
     * @param tag
     *            Resource tag.
     * @return Control to place on the display.
     */
    private Control settingControl(ResourceConfigMgr configMgr,
            final ResourceTag tag) {
        ResourceType resType = configMgr.getResourceType(tag);

        if (resType == ResourceType.FONT) {
            Button fontBtn = new Button(this, SWT.PUSH);
            updateFontButton(configMgr, fontBtn, tag);

            GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
            gd.minimumWidth = 80;
            gd.horizontalIndent = 50;
            gd.verticalIndent = 3;
            fontBtn.setLayoutData(gd);
            fontBtn.setData(tag);
            fontBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    Button sourceBtn = (Button) e.getSource();
                    showFontDialog(sourceBtn);
                }
            });

            resTagValMap.put(tag, configMgr.getResourceAsString(tag));

            return fontBtn;
        } else if (resType == ResourceType.COLOR) {
            Button colorBtn = new Button(this, SWT.PUSH);

            updateColorButton(configMgr, colorBtn, tag);

            GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
            gd.minimumWidth = 80;
            gd.horizontalIndent = 50;
            gd.verticalIndent = 3;
            colorBtn.setLayoutData(gd);
            colorBtn.setData(tag);
            colorBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    Button sourceBtn = (Button) e.getSource();
                    showColorDialog(sourceBtn);
                }
            });

            resTagValMap.put(tag, configMgr.getResourceAsString(tag));

            return colorBtn;
        } else if (resType == ResourceType.CHECK) {
            Button checkBtn = new Button(this, SWT.CHECK);
            GridData gd = new GridData(19, SWT.DEFAULT);
            gd.horizontalIndent = 50;
            gd.verticalIndent = 3;
            checkBtn.setLayoutData(gd);
            checkBtn.setData(tag);
            checkBtn.setSelection(configMgr.getResourceAsBoolean(tag));
            checkBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    Button sourceBtn = (Button) e.getSource();
                    resTagValMap.put((ResourceTag) sourceBtn.getData(),
                            String.valueOf(sourceBtn.getSelection()));
                }
            });

            resTagValMap.put(tag, configMgr.getResourceAsString(tag));

            return checkBtn;
        } else if (resType == ResourceType.COMBO) {
            Combo comboControl = new Combo(this, SWT.DROP_DOWN | SWT.READ_ONLY);
            GridData gd = new GridData();
            gd.widthHint = 120;
            gd.minimumWidth = 120;
            gd.horizontalIndent = 50;
            gd.verticalIndent = 3;
            comboControl.setLayoutData(gd);
            comboControl.setData(tag);
            comboControl.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    Combo sourceCbo = (Combo) e.getSource();
                    resTagValMap.put((ResourceTag) sourceCbo.getData(),
                            sourceCbo.getItem(sourceCbo.getSelectionIndex()));
                }
            });

            updateComboControl(configMgr, comboControl, tag);

            resTagValMap.put(tag, configMgr.getResourceAsString(tag));

            return comboControl;
        } else if (resType == ResourceType.SPINNER) {
            SpinnerData spData = configMgr.getSpinnerData(tag);
            GridData gd = new GridData(50, SWT.DEFAULT);
            gd.horizontalIndent = 50;
            gd.verticalIndent = 3;
            Spinner spnr = new Spinner(this, SWT.BORDER);
            spnr.setIncrement(1);
            spnr.setPageIncrement(spData.getPageInc());
            spnr.setMaximum(spData.getMax());
            spnr.setMinimum(spData.getMin());
            spnr.setSelection(configMgr.getResourceAsInt(tag));
            spnr.setLayoutData(gd);

            spnr.setData(tag);

            spnr.addFocusListener(new FocusAdapter() {
                @Override
                public void focusLost(FocusEvent e) {
                    Spinner s = (Spinner) e.getSource();
                    resTagValMap.put((ResourceTag) s.getData(),
                            String.valueOf(s.getSelection()));
                }
            });

            resTagValMap.put(tag, configMgr.getResourceAsString(tag));

            return spnr;
        } else if (resType == ResourceType.FILE) {
            Button fileBtn = new Button(this, SWT.PUSH);
            String filename = configMgr.getResourceAsString(tag);
            if (filename.contains("/")) {
                int slashIndex = filename.lastIndexOf("/");
                audioFilterPath = filename.substring(0, slashIndex);
                fileBtn.setText(filename.substring(slashIndex + 1));
                fileBtn.setToolTipText(filename);
            } else {
                File f = PathManagerFactory.getPathManager().getStaticFile(
                        "alertVizAudio");
                audioFilterPath = f.getAbsolutePath();
                fileBtn.setToolTipText(null);
                fileBtn.setText(filename);
            }
            GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
            gd.minimumWidth = 80;
            gd.horizontalIndent = 50;
            gd.verticalIndent = 3;
            fileBtn.setLayoutData(gd);
            fileBtn.setData(tag);
            fileBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    Button fileBtn = (Button) e.getSource();
                    String[] extensions = { "*.*", "*.aiff;*.aif;*.aifc",
                            "*.au", "*.wav;*.wv" };
                    String[] names = { "All Files (*.*)",
                            "AIFF (*.aiff;*.aif;*.aifc)", "SUN (*.au)",
                            "Waveform (*.wav;*.wv)" };
                    FileDialog dlg = new FileDialog(getParent().getShell(),
                            SWT.OPEN);
                    dlg.setFilterNames(names);
                    dlg.setFilterExtensions(extensions);
                    dlg.setFilterPath(audioFilterPath);
                    String filename = dlg.open();

                    if (filename != null) {
                        resTagValMap.put(tag, filename);
                        audioFilterPath = dlg.getFilterPath();
                        fileBtn.setText(filename.substring(filename
                                .lastIndexOf("/") + 1));
                        fileBtn.setToolTipText(filename);
                    }
                }
            });

            resTagValMap.put(tag, configMgr.getResourceAsString(tag));

            return fileBtn;
        }

        return null;
    }

    /**
     * Show the font dialog.
     * 
     * @param sourceBtn
     *            Button that will have the font changed.
     */
    private void showFontDialog(Button sourceBtn) {
        FontDialog dlg = new FontDialog(this.getShell());

        Font font = sourceBtn.getFont();

        if (font != null) {
            dlg.setFontList(font.getFontData());
        }

        if (dlg.open() != null) {
            ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();

            sourceBtn.getFont().dispose();

            // Create the new font and set it into the label
            tmpFont = new Font(this.getShell().getDisplay(), dlg.getFontList());

            FontData fd = tmpFont.getFontData()[0];

            StringBuilder sb = new StringBuilder();
            sb.append(fd.getName()).append("-").append(fd.getHeight())
                    .append("-")
                    .append(configMgr.getStyleString(fd.getStyle()));

            sourceBtn.setFont(tmpFont);
            sourceBtn.setText(createDisplayFont(fd));

            resTagValMap.put((ResourceTag) sourceBtn.getData(), sb.toString());

            resizeAction();
        }
    }

    /**
     * Show the color dialog.
     * 
     * @param sourceBtn
     *            Button that will have its color changed.
     */
    private void showColorDialog(Button sourceBtn) {
        RGB currentRGB = sourceBtn.getBackground().getRGB();

        ColorDialog colorDlg = new ColorDialog(this.getShell());
        colorDlg.setRGB(currentRGB);
        colorDlg.open();

        RGB rgb = colorDlg.getRGB();

        if (rgb == null) {
            return;
        }

        Color c = new Color(this.getDisplay(), rgb);
        sourceBtn.setBackground(c);
        c.dispose();

        sourceBtn.setText(RGBColors.getColorName(rgb));

        resTagValMap.put((ResourceTag) sourceBtn.getData(),
                RGBColors.getHexString(rgb));
    }

    /**
     * Create the font that will be displayed in the control.
     * 
     * @param fd
     *            Font data.
     * @return Font as a human readable string.
     */
    private String createDisplayFont(FontData fd) {
        ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();

        StringBuilder sb = new StringBuilder();
        sb.append(fd.getName()).append("-").append(fd.getHeight()).append("-")
                .append(configMgr.getStyleString(fd.getStyle()));

        return sb.toString();
    }

    /**
     * Update the font button.
     * 
     * @param configMgr
     *            Configuration manager.
     * @param fontBtn
     *            Font button to be updated.
     * @param tag
     *            Resource tag.
     */
    private void updateFontButton(ResourceConfigMgr configMgr, Button fontBtn,
            ResourceTag tag) {
        String fontStr = configMgr.getResourceAsString(tag);

        String[] string = fontStr.split("-");

        tmpFont = new Font(getParent().getDisplay(), new FontData(string[0],
                Integer.valueOf(string[1]), configMgr.getStyleInt(string[2])));

        fontBtn.setFont(tmpFont);
        fontBtn.setText(fontStr);
    }

    /**
     * Update the color button.
     * 
     * @param configMgr
     *            Configuration manager.
     * @param colorBtn
     *            Color button to be updated.
     * @param tag
     *            Resource tag.
     */
    private void updateColorButton(ResourceConfigMgr configMgr,
            Button colorBtn, ResourceTag tag) {
        String colorStr = configMgr.getResourceAsString(tag);

        RGB rgb = RGBColors.getRGBColor(colorStr);

        Color c = new Color(getParent().getDisplay(), rgb);
        colorBtn.setBackground(c);
        c.dispose();

        colorBtn.setText(RGBColors.getColorName(rgb));
    }

    /**
     * Update the combo control.
     * 
     * @param configMgr
     *            Configuration manager.
     * @param comboCtrl
     *            Combo control.
     * @param tag
     *            Resource tag.
     */
    private void updateComboControl(ResourceConfigMgr configMgr,
            Combo comboCtrl, ResourceTag tag) {
        String[] entries = configMgr.getComboValues(tag);

        for (String entry : entries) {
            comboCtrl.add(entry);
        }

        String item = configMgr.getResourceAsString(tag);
        comboCtrl.select(comboCtrl.indexOf(item));
    }

    /**
     * Reset the resource item to the default value.
     * 
     * @param resetBtn
     *            The selected reset button.
     */
    private void resetResourceItem(Button resetBtn) {
        Control c = (Control) resetBtn.getData();
        ResourceTag tag = (ResourceTag) c.getData();

        ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();

        String defaultRes = configMgr.resetResource(tag);

        resTagValMap.put(tag, defaultRes);

        ResourceType type = configMgr.getResourceType(tag);

        switch (type) {
        case COLOR:
            updateColorButton(configMgr, (Button) c, tag);
            break;

        case FONT:
            updateFontButton(configMgr, (Button) c, tag);
            break;

        case SPINNER:
            Spinner spnr = (Spinner) c;
            spnr.setSelection(configMgr.getResourceAsInt(tag));
            break;

        case COMBO:
            Combo comboCtrl = (Combo) c;
            String item = configMgr.getResourceAsString(tag);
            comboCtrl.select(comboCtrl.indexOf(item));
            break;

        case CHECK:
            Button checkBtn = (Button) c;
            checkBtn.setSelection(configMgr.getResourceAsBoolean(tag));
            break;

        case FILE:
            Button fileBtn = (Button) c;
            fileBtn.setText(defaultRes);
            fileBtn.setToolTipText(null);
            break;
        }
    }

    /**
     * Save the edited resources.
     */
    public void saveResources() {
        ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();

        for (ResourceTag tag : resTagValMap.keySet()) {
            configMgr.setResourceString(tag, resTagValMap.get(tag));
        }

        configMgr.saveResources();
    }

    /**
     * Restore all of the resources.
     */
    public void restoreAllResources() {
        MessageBox confirmCloseMB = new MessageBox(this.getShell(),
                SWT.ICON_QUESTION | SWT.OK | SWT.CANCEL);
        confirmCloseMB.setText("Confirm Restore");
        confirmCloseMB
                .setMessage("This will delete your configuration and reset all\n"
                        + "of the resources.\n\nReally want to continue?");
        int result = confirmCloseMB.open();

        if (result == SWT.CANCEL) {
            return;
        }

        for (Button resetBtn : resetButtons) {
            resetResourceItem(resetBtn);
        }
        resizeAction();
    }

    private void resizeAction() {
        this.pack();
        resizeCB.resizeAction();
    }
}
