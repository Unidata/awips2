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
package com.raytheon.viz.gfe.localization.dialogs;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.localization.perspective.service.LocalizationPerspectiveUtils;
import com.raytheon.viz.gfe.core.DataManagerUIFactory;
import com.raytheon.viz.gfe.core.IParmManager;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.localization.util.AbstractScriptUtil;
import com.raytheon.viz.gfe.localization.util.AbstractScriptUtil.Overwrite;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * Dialog for creating new smart tools.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Feb 20, 2008           njensen   Initial creation
 * Nov 12, 2012  1298     rferrel   Changes for non-blocking dialog.
 * Jan 19, 2016  4834     njensen   Cleaned up warnings
 * Aug 11, 2016  5816     randerso  Moved to gfe.localization.dialogs
 *                                  Code cleanup
 * 
 * </pre>
 * 
 * @author njensen
 */

public class NewToolDialog extends CaveJFACEDialog {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(NewToolDialog.class);

    private String title;

    private Text text;

    private List parms;

    private IInputValidator validator;

    private AbstractScriptUtil util;

    /**
     * @param parent
     * @param title
     * @param validator
     * @param util
     */
    public NewToolDialog(Shell parent, String title, IInputValidator validator,
            AbstractScriptUtil util) {
        super(parent);
        this.title = title;
        this.validator = validator;
        this.util = util;
        setShellStyle(SWT.DIALOG_TRIM | SWT.APPLICATION_MODAL);
    }

    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);
        if (title != null) {
            shell.setText(title);
        }
    }

    @Override
    protected void okPressed() {
        String toolname = text.getText();
        int index = parms.getSelectionIndex();
        if (!isInputValid()) {
            String error = "'Name' must be a valid identifier: ";
            error += "No special chracters (except underscore) are allowed.";
            MessageDialog.openWarning(getShell(), "Invalid Name", error);
        } else if (index < 0) {
            String error = "Please select a weather element to edit or variableElement.";
            MessageDialog.openWarning(getShell(), "No Weather Element", error);
        } else {
            String weatherElement = parms.getItem(parms.getSelectionIndex());

            Map<String, Object> args = new HashMap<>();
            args.put("parmToEdit", weatherElement);
            try {
                LocalizationFile fileToEdit = util.createNew(toolname,
                        Overwrite.DISALLOW, args);
                statusHandler.debug(util.getScriptType() + " " + toolname
                        + " created.");
                LocalizationPerspectiveUtils.openLocalizationFile(fileToEdit);
            } catch (Exception e) {
                String message = String
                        .format("Error creating %s '%s': %s",
                                util.getScriptType(), toolname,
                                e.getLocalizedMessage());
                statusHandler.error(message, e);
            }
            super.okPressed();
        }
    }

    @Override
    protected Control createDialogArea(final Composite parent) {
        Composite composite = (Composite) super.createDialogArea(parent);

        Composite comp = new Composite(composite, SWT.NONE);
        comp.setLayout(new GridLayout(1, false));
        comp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        Composite top = new Composite(comp, SWT.NONE);
        top.setLayout(new GridLayout(2, false));
        top.setLayoutData(new GridData(SWT.FILL, SWT.NONE, true, false));

        Label label = new Label(top, SWT.NONE);
        label.setText("Name");

        text = new Text(top, SWT.BORDER);
        text.setLayoutData(new GridData(SWT.FILL, SWT.NONE, true, false));
        text.setText("MyTool");

        Label weatherLabel = new Label(comp, SWT.NONE);
        weatherLabel.setLayoutData(new GridData(SWT.CENTER, SWT.CENTER, true,
                false));
        weatherLabel.setText("Weather Element To Edit");

        parms = new List(comp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        gridData.heightHint = parms.getItemHeight() * 15;
        parms.setLayoutData(gridData);
        parms.setItems(getParms());

        // select the active parm if not null otherwise select the first parm in
        // the list
        Parm parm = DataManagerUIFactory.getCurrentInstance()
                .getSpatialDisplayManager().getActivatedParm();
        if (parm != null) {
            parms.setSelection(new String[] { parm.getParmID().getParmName() });
        } else {
            parms.setSelection(0);
        }

        applyDialogFont(composite);
        return composite;
    }

    private String[] getParms() {
        IWorkbenchWindow window = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow();
        IParmManager pm = DataManagerUIFactory.getInstance(window)
                .getParmManager();
        ParmID[] parms = pm.getAvailableParms(pm.getMutableDatabase());
        java.util.List<String> parmNames = new ArrayList<String>(
                parms.length + 2);
        for (ParmID p : parms) {
            parmNames.add(p.getParmName());
        }
        Collections.sort(parmNames);
        parmNames.add("variableElement");
        parmNames.add("None");

        return parmNames.toArray(new String[parmNames.size()]);
    }

    private boolean isInputValid() {
        return (validator.isValid(text.getText()) == null);
    }
}
