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

package com.raytheon.viz.ui.actions;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import javax.imageio.ImageIO;
import javax.imageio.ImageWriter;
import javax.imageio.stream.FileImageOutputStream;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Save the current screen to a file
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jul 26, 2006             chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class SaveScreenAction extends AbstractScreenCaptureAction {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SaveScreenAction.class);

    private static final String ERROR_MESSAGE = "Unable to save image";

    private static final String DEFAULT_FILENAME = "*.png";

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands
     * .ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        IEditorPart part = EditorUtil.getActiveEditor();
        AbstractEditor editor = null;
        if (part instanceof AbstractEditor) {
            editor = (AbstractEditor) part;
        }

        if (editor == null) {
            return null;
        }

        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();

        String[] suffixList = ImageIO.getWriterFileSuffixes();
        ArrayList<String> filters = new ArrayList<String>();
        for (String suffix : suffixList) {
            if (!suffix.isEmpty()) {
                filters.add("*." + suffix);
            }
        }
        Collections.sort(filters);

        FileDialog fd = new FileDialog(shell, SWT.SAVE);
        fd.setFileName(DEFAULT_FILENAME);
        fd.setFilterExtensions(filters.toArray(new String[filters.size()]));
        String path = fd.open();

        if (path == null) {
            return null;
        }
        int pos = path.lastIndexOf('.');
        if (pos < 0) {
            String reason = "Missing filename extension";

            statusHandler.handle(Priority.PROBLEM, reason);
            return null;
        }

        String suffix = path.substring(path.lastIndexOf('.') + 1);
        String basePath = path.substring(0, path.lastIndexOf('.'));
        Iterator<ImageWriter> iter = ImageIO.getImageWritersBySuffix(suffix);
        if (!iter.hasNext()) {
            String reason = "Unsupported filetype: \"" + suffix + "\"";
            statusHandler.handle(Priority.PROBLEM, reason);

            return null;
        }
        ImageWriter writer = iter.next();

        String frameMode = arg0.getParameter("frameSelection");

        if (frameMode == null || frameMode.equalsIgnoreCase("current")) {
            BufferedImage bi = editor.screenshot();

            try {
                writer.setOutput(new FileImageOutputStream(new File(path)));
                writer.write(bi);
            } catch (IOException e) {
                String reason = "Error occurred while writing image";
                statusHandler.handle(Priority.PROBLEM, reason, e);
                throw new ExecutionException(reason, e);
            }
        } else if (frameMode.equalsIgnoreCase("all")) {
            List<BufferedImage> images = null;
            try {
                images = captureAllFrames(editor);
            } catch (VizException e) {
                String reason = "Error occurred while writing image";
                statusHandler.handle(Priority.PROBLEM, reason, e);
                throw new ExecutionException(reason, e);
            }

            for (int i = 0; i < images.size(); i++) {

                BufferedImage bi = images.get(i);

                path = basePath + "-" + (i + 1) + "." + suffix;

                try {
                    writer.setOutput(new FileImageOutputStream(new File(path)));
                    writer.write(bi);
                } catch (IOException e) {
                    String reason = "Error occurred while writing image";
                    statusHandler.handle(Priority.PROBLEM, reason, e);
                    throw new ExecutionException(reason, e);
                }
            }
        } else {
            String reason = "Invalid frameMode: " + frameMode;
            statusHandler.handle(Priority.PROBLEM, reason);
            throw new ExecutionException(reason);
        }
        return null;
    }
}
