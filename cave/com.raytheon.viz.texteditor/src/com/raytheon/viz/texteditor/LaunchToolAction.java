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

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Widget;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.viz.texteditor.dialogs.TextEditorDialog;
import com.raytheon.viz.texteditor.util.TextEditorUtil;

import jep.JepConfig;
import jep.JepException;

/**
 * Launch a python related tool script.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 12, 2009 2191        rjpeter     Initial creation
 * Jun 03, 2019 7852        dgilling    Update code for jep 3.8.
 *
 * </pre>
 *
 * @author rjpeter
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
                IPathManager pathManager = PathManagerFactory.getPathManager();

                LocalizationFile scriptFile = pathManager
                        .getStaticLocalizationFile(LocalizationUtil.join(
                                TextEditorUtil.TEXTEDITOR_PYTHON_DIR, script));
                if (!scriptFile.exists()) {
                    statusHandler.error(
                            "Unable to find file in localization with name: "
                                    + script);
                    return null;
                }
                scriptFile.getFile();

                LocalizationFile runnerScript = pathManager
                        .getStaticLocalizationFile(LocalizationUtil.join(
                                TextEditorUtil.TEXTEDITOR_PYTHON_DIR,
                                "ToolRunner.py"));
                if (!runnerScript.exists()) {
                    throw new RuntimeException("Tool runner file missing");
                }

                try (PythonScript pythonScript = new PythonScript(
                        new JepConfig()
                                .setIncludePath(
                                        TextEditorUtil.getPythonIncludeDir())
                                .setClassLoader(getClass().getClassLoader()),
                        runnerScript.getFile().getAbsolutePath())) {
                    Map<String, Object> args = new HashMap<>();
                    int dotIndex = script.lastIndexOf('.');
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
                    statusHandler.error("Error running script " + script, e);

                }
            }
        } else {
            statusHandler.error(
                    "Could not find TextEditor Dialog in calling event.");
        }

        return null;
    }
}
