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
/**
 * 
 */
package com.raytheon.viz.gfe.core.script.action;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.Activator;
import com.raytheon.viz.gfe.GFEException;
import com.raytheon.viz.gfe.constants.StatusConstants;
import com.raytheon.viz.gfe.core.script.CopyInputValidator;
import com.raytheon.viz.gfe.core.script.ExistMode;
import com.raytheon.viz.gfe.core.script.IScriptUtil;
import com.raytheon.viz.gfe.core.script.IScriptUtil.Overwrite;
import com.raytheon.viz.gfe.dialogs.ScriptNameInputDialog;

/**
 * An Action the user can select to copy an existing script to a new name.
 * 
 * @author wldougher
 * 
 */
public class CopyAction extends Action {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(CopyAction.class);

    /**
     * The name of the script, i.e., "Extrapolate"
     */
    protected String script;

    /**
     * The decorated script name, i.e., "Extrapolate.py"
     */
    protected String scriptf;

    /**
     * A utility class instance for this script type
     */
    IScriptUtil util;

    /**
     * Constructor.
     */
    public CopyAction(String script, IScriptUtil util) {
        super("Copy");
        this.script = script;
        this.util = util;
        scriptf = util.scripted(script);
    }

    /**
     * Display a dialog to the user that asks for the name of the destination of
     * the copy. If the user enters a valid script name and clicks OK, perform
     * the copy.
     * 
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
        // Set up user-input dialog
        Shell shell = Display.getCurrent().getActiveShell();
        ScriptNameInputDialog copyNameDialog = new ScriptNameInputDialog(shell,
                "Copy " + script, "Copy Name", null, new CopyInputValidator(
                        script, util), util);
        copyNameDialog.setMode(ExistMode.ERR_EXISTS);
        String type = util.getScriptType();
        // Get the new name from the user
        int rtnCode = copyNameDialog.open();
        if (Window.OK == rtnCode) {
            String newName = copyNameDialog.getValue();
            try {
                // do the work
                LocalizationFile newScript = util.copy(script, newName,
                        LocalizationLevel.USER, Overwrite.SAFE);
                if (newScript != null) {
                    File file = newScript.getFile();
                    BufferedReader reader = null;
                    BufferedWriter writer = null;
                    StringBuilder fileContents = new StringBuilder();

                    try {
                        reader = new BufferedReader(new FileReader(file));
                        for (String line = reader.readLine(); line != null; line = reader
                                .readLine()) {
                            fileContents.append(line);
                            fileContents.append(System
                                    .getProperty("line.separator"));
                        }

                        String newFileContents = fileContents.toString()
                                .replaceAll("# " + script, "# " + newName);
                        newFileContents = newFileContents.replaceAll("def "
                                + script, "def " + newName);

                        writer = new BufferedWriter(new FileWriter(file));
                        writer.write(newFileContents);
                    } catch (IOException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Error copying " + type + " " + script, e);
                    } finally {
                        try {
                            if (reader != null) {
                                reader.close();
                            }
                            if (writer != null) {
                                writer.close();
                            }
                        } catch (IOException e) {
                            statusHandler.handle(Priority.PROBLEM,
                                    "Error copying " + type + " " + script, e);
                        }
                    }

                    newScript.save();
                }

                statusHandler.handle(Priority.VERBOSE, type + " " + script
                                + " copied to " + newName);
            } catch (LocalizationException e) {
                statusHandler.handle(Priority.PROBLEM, "Error copying "
                                + type + " " + script, e);
            } catch (GFEException e) {
                statusHandler.handle(Priority.PROBLEM, "Error copying "
                                + type + " " + script, e);
            }
        }
    }
}
