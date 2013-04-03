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
package com.raytheon.uf.viz.localization.perspective;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.swt.graphics.Point;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.texteditor.ITextEditor;
import org.python.pydev.core.IInterpreterInfo;
import org.python.pydev.core.IInterpreterManager;
import org.python.pydev.core.REF;
import org.python.pydev.core.Tuple;
import org.python.pydev.plugin.PydevPlugin;
import org.python.pydev.runners.SimplePythonRunner;
import org.python.pydev.ui.pythonpathconf.InterpreterInfo;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.ui.perspectives.AbstractVizPerspectiveManager;

/**
 * Implementation for localization perspective, utilizing the localization
 * perspective as a start
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 18, 2010            mnash     Initial creation
 * Nov 02, 2012 1302       djohnson  Remove printStackTrace.
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class LocalizationPerspectiveManager extends
        AbstractVizPerspectiveManager {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(LocalizationPerspectiveManager.class);

    /** The edit position restore map */
    private final Map<IEditorInput, Point> restoreMap = new HashMap<IEditorInput, Point>();

    public LocalizationPerspectiveManager() {
        saveEditors = true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.perspectives.AbstractVizPerspectiveManager#open()
     */
    @Override
    protected void open() {
        try {
            // Attempt the initialization twice before reporting errors,
            // sometimes it can fail unexpectedly and recover a second time.
            // Goal is to auto setup python interpreter to use for editing
            // python files
            for (int i = 0; i < 2; ++i) {
                boolean retry = i > 0;
                // Setup python environment for pydev
                IInterpreterManager mgr = PydevPlugin
                        .getPythonInterpreterManager();
                String persistedStr = mgr.getPersistedString();
                if ((persistedStr == null) || "".equals(persistedStr.trim())) {
                    IInterpreterInfo iinfo = null;
                    String pathToFile = null;
                    String LD_PATH = System.getenv("PATH");
                    String[] folders = LD_PATH.split(File.pathSeparator); // Win32
                    for (String folder : folders) {
                        File python = new File(folder, "python");
                        if (python.exists() && python.isFile()
                                && python.canExecute()) {
                            pathToFile = python.getAbsolutePath();
                            break;
                        }
                    }

                    if (pathToFile != null) {
                        try {
                            // Taken from pydev source to get rid of UI prompt
                            File script = PydevPlugin
                                    .getScriptWithinPySrc("interpreterInfo.py");
                            Tuple<String, String> outTup = new SimplePythonRunner()
                                    .runAndGetOutputWithInterpreter(pathToFile,
                                            REF.getFileAbsolutePath(script),
                                            null, null, null,
                                            new NullProgressMonitor());
                            iinfo = InterpreterInfo
                                    .fromString(outTup.o1, false);
                            // end taken
                        } catch (Throwable e) {
                            if (retry) {
                                throw e;
                            }
                            continue;
                        }
                        if (iinfo == null) {
                            if (retry) {
                                throw new Exception(
                                        "Could not generate python info, python editors may not function 100%");
                            } else {
                                continue;
                            }
                        }
                        mgr.setPersistedString(iinfo.toString());
                        List<IInterpreterInfo> infoList = new ArrayList<IInterpreterInfo>();
                        infoList.add(iinfo);
                        mgr.setInfos(infoList);
                    } else {
                        if (retry) {
                            throw new Exception(
                                    "Could not find python on PATH, be sure to set environment variable");
                        } else {
                            continue;
                        }
                    }
                }
                // We made it here? success!
                break;
            }
        } catch (Throwable t) {
            statusHandler.handle(
                    Priority.PROBLEM,
                    "Error setting up python interpreter: "
                            + t.getLocalizedMessage(), t);
        }
    }

    @Override
    public void activateInternal() {
        super.activateInternal();
        for (IEditorReference ref : page.getEditorReferences()) {
            IEditorPart part = ref.getEditor(false);
            if (part != null) {
                IEditorInput input = part.getEditorInput();
                if (part instanceof ITextEditor) {
                    ITextEditor editor = (ITextEditor) part;
                    Point offsetLength = restoreMap.get(input);
                    if (offsetLength != null) {
                        editor.selectAndReveal(offsetLength.x, offsetLength.y);
                    }
                }
            }
        }
        restoreMap.clear();
    }

    @Override
    public void deactivate() {
        for (IEditorReference ref : page.getEditorReferences()) {
            IEditorPart part = ref.getEditor(false);
            if (part != null) {
                IEditorInput input = part.getEditorInput();
                if (part instanceof ITextEditor) {
                    ITextEditor editor = (ITextEditor) part;
                    ITextSelection selection = (ITextSelection) editor
                            .getSelectionProvider().getSelection();
                    if (selection != null) {
                        restoreMap.put(input, new Point(selection.getOffset(),
                                selection.getLength()));
                    }
                }
            }
        }
        super.deactivate();
    }

}
