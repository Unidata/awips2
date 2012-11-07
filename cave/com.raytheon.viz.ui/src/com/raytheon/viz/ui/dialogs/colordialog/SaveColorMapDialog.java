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

package com.raytheon.viz.ui.dialogs.colordialog;

import java.io.File;
import java.util.Arrays;

import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.ColorMapLoader;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.viz.ui.UiPlugin;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Dialog for the save to office button, that saves a file to the server.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 24, 2007            njensen     Initial creation
 * Oct 17, 2012 1229       rferrel     Dialog is non-blocking.
 * 
 * </pre>
 * 
 * @author njensen
 */
public class SaveColorMapDialog extends CaveSWTDialog {

    private Text filenameText;

    private List filesList;

    private final ColorMap colorMapToSave;

    private boolean siteContext = false;

    private String currentColormapName;

    /**
     * Constructor
     * 
     * @param parent
     *            the parent
     * @param aColorMap
     *            the color map to save
     * @param aSiteContext
     *            if true will save to site context, if false will save to user
     *            context
     */
    public SaveColorMapDialog(Shell parent, ColorMap aColorMap,
            boolean aSiteContext, String aCurrentColormapName) {
        super(parent, SWT.NONE, CAVE.DO_NOT_BLOCK);
        colorMapToSave = aColorMap;
        siteContext = aSiteContext;
        currentColormapName = new String(aCurrentColormapName == null ? ""
                : aCurrentColormapName);
        int index = currentColormapName.lastIndexOf(File.separator);
        if (index > -1) {
            currentColormapName = currentColormapName.substring(index + 1,
                    currentColormapName.length());
        }
        if (siteContext) {
            setText("Save color table for office");
        } else {
            setText("Save color table for "
                    + LocalizationManager.getInstance().getCurrentUser());
        }
    }

    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        return mainLayout;
    }

    @Override
    protected Object constructShellLayoutData() {
        return new GridData();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        createComponents();
        createBottomButtons();
    }

    private void createComponents() {
        Composite composite = new Composite(shell, SWT.NONE);
        composite.setLayout(new GridLayout(1, true));

        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.widthHint = 230;
        filenameText = new Text(composite, SWT.SINGLE | SWT.BORDER);
        filenameText.setLayoutData(gd);

        filesList = new List(composite, SWT.SINGLE | SWT.V_SCROLL | SWT.BORDER);
        filesList.setItems(getColormapNames());
        GridData fgd = new GridData(GridData.FILL_HORIZONTAL);
        fgd.heightHint = 200;
        filesList.setLayoutData(fgd);
        filesList.addSelectionListener(new SelectionListener() {

            public void widgetDefaultSelected(SelectionEvent e) {
                // TODO Auto-generated method stub
            }

            public void widgetSelected(SelectionEvent e) {
                int index = filesList.getSelectionIndex();
                filenameText.setText(filesList.getItem(index));
            }

        });
    }

    private void createBottomButtons() {
        Composite buttonArea = new Composite(shell, SWT.NONE);
        buttonArea.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        buttonArea.setLayout(new GridLayout(1, false));

        // The intent is for this composite to be centered
        Composite buttons = new Composite(buttonArea, SWT.NONE);
        buttons.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_CENTER
                | GridData.GRAB_HORIZONTAL));
        // !!! you need GRAB_HORIZONTAL otherwise the grid is only as wide as
        // its widest cell
        buttons.setLayout(new GridLayout(2, true));

        Button okBtn = new Button(buttons, SWT.PUSH);
        okBtn.setLayoutData(new GridData(GridData.FILL_BOTH));
        okBtn.setText("OK");
        okBtn.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent e) {
                // TODO Auto-generated method stub
            }

            public void widgetSelected(SelectionEvent e) {
                String filename = filenameText.getText();
                boolean okToSave = true;

                if (filename.length() < 1) {
                    filesList
                            .setSelection(new String[] { currentColormapName });
                    if (filesList.getSelectionCount() > 0) {
                        filename = currentColormapName;
                    }
                }

                if (filename.length() < 1) {
                    String message = "Please enter a name for the colormap.";
                    MessageDialog.openInformation(shell, "", message);
                    okToSave = false;
                }

                boolean exists2 = ColorUtil.checkColorMapLocal(filename,
                        siteContext);

                if (exists2) {
                    String message = "A color table named " + filename
                            + " already exists. Do you wish to overwrite?";
                    okToSave = MessageDialog.openQuestion(shell,
                            "Confirm Color Table Overwrite", message);
                }

                if (okToSave) {
                    try {
                        setReturnValue(ColorUtil.saveColorMapLocal(
                                colorMapToSave, filename, siteContext));
                    } catch (VizException e1) {
                        String err = "Error saving locally";
                        VizApp.logAndAlert(Status.ERROR, e1, err, err,
                                UiPlugin.getDefault(), UiPlugin.PLUGIN_ID);
                    }

                    // now save it to EDEX
                    try {
                        ColorUtil.saveColorMapServer(colorMapToSave, filename,
                                siteContext);
                    } catch (LocalizationException e1) {
                        String err = "Error saving to office";
                        VizApp.logAndAlert(Status.ERROR, e1, err, err,
                                UiPlugin.getDefault(), UiPlugin.PLUGIN_ID);
                    }

                    shell.dispose();
                }
            }
        });

        Button cancelBtn = new Button(buttons, SWT.PUSH);
        cancelBtn.setLayoutData(new GridData(GridData.FILL_BOTH));
        cancelBtn.setText("Cancel");
        cancelBtn.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent e) {
                // TODO Auto-generated method stub
            }

            public void widgetSelected(SelectionEvent e) {
                shell.dispose();
            }
        });
    }

    /**
     * Get a list of colormaps in the colormaps directory
     * 
     * @return the script names
     */
    private String[] getColormapNames() {

        String[] names = null;
        if (siteContext) {
            names = ColorMapLoader
                    .listColorMaps(LocalizationContext.LocalizationLevel.SITE);
        } else {
            names = ColorMapLoader
                    .listColorMaps(LocalizationContext.LocalizationLevel.USER);
        }

        for (int i = 0; i < names.length; i++) {
            String current = names[i];
            int index = names[i].lastIndexOf(File.separator);
            if (index > -1) {
                names[i] = current.substring(index + 1, current.length());
            }
        }
        Arrays.sort(names);
        return names;
    }
}
