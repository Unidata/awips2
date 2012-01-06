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

package com.raytheon.viz.texteditor;

import java.io.File;
import java.util.HashMap;

import jep.JepException;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Widget;

import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.texteditor.dialogs.TextEditorDialog;
import com.raytheon.viz.texteditor.util.TextEditorUtil;

/**
 * Launch a python related tool script.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 08/12/2009   2191        rjpeter     Initial creation.
 * </pre>
 * 
 * @author rjpeter
 * 
 */
public class LaunchToolAction extends AbstractHandler {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(LaunchToolAction.class);

    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        Object trigger = arg0.getTrigger();
        TextEditorDialog dialog = null;

        if (trigger != null && trigger instanceof Event) {
            Event event = (Event) trigger;
            Widget w = event.widget;

            if (w instanceof MenuItem) {
                MenuItem item = (MenuItem) w;
                Menu menu = item.getParent();
                dialog = (TextEditorDialog) menu.getData("Dialog");

                // recurse back up tree in case we were multiple levels deep
                while (dialog == null) {
                    item = menu.getParentItem();
                    if (item != null) {
                        menu = item.getParent();
                        if (menu != null) {
                            dialog = (TextEditorDialog) menu.getData("Dialog");
                        } else {
                            break;
                        }
                    } else {
                        break;
                    }
                }
            }
        }

        if (dialog != null) {
            dialog.clearButtonology();

            String script = arg0.getParameter("script");
            if (script != null) {
                File file = PathManagerFactory.getPathManager().getStaticFile(
                        TextEditorUtil.TEXTEDITOR_PYTHON_DIR + "/" + script);

                if (file == null) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Unable to find file in localization with name: "
                                    + script);
                    return null;
                }

                String textEditorPythonPath = TextEditorUtil
                        .getPythonIncludeDir();
                PythonScript pythonScript = null;

                try {
                    file = PathManagerFactory.getPathManager().getStaticFile(
                            TextEditorUtil.TEXTEDITOR_PYTHON_DIR
                                    + "/ToolRunner.py");
                    if (file == null) {
                        throw new RuntimeException("Tool runner file missing");
                    }

                    pythonScript = new PythonScript(file.getAbsolutePath(),
                            textEditorPythonPath, this.getClass()
                                    .getClassLoader());
                    HashMap<String, Object> args = new HashMap<String, Object>();
                    int dotIndex = script.lastIndexOf(".");
                    String module = null;

                    // need to strip the file extension from the script when
                    // invoking
                    if (dotIndex > 0) {
                        module = script.substring(0, dotIndex);
                    } else {
                        module = script;
                    }

                    args.put("script", module);
                    args.put("args", arg0.getParameter("args"));
                    args.put(
                            "pythonTextEditorToolCallback",
                            new PythonTextEditorToolCallback(dialog.getParent()));
                    Object result = pythonScript.execute("process", args);

                    if (result != null) {
                        String[] tmp = { result.toString() };
                        dialog.postProductToEditor(tmp,
                                arg0.getParameter("args").split("\\s*,\\s*"));
                    }
                } catch (JepException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error running script " + script, e);

                } finally {
                    if (pythonScript != null) {
                        pythonScript.dispose();
                    }
                }

            }
        } else {
            statusHandler.handle(Priority.PROBLEM,
                    "Could not find TextEditor Dialog in calling event.");
        }

        return null;
    }
}
