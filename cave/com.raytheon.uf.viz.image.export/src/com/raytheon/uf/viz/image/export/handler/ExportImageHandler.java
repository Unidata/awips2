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
package com.raytheon.uf.viz.image.export.handler;

import java.awt.image.BufferedImage;
import java.io.File;
import java.util.LinkedHashMap;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.handlers.HandlerUtil;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.image.export.dialog.ImageExportDialog;
import com.raytheon.uf.viz.image.export.job.SaveGeotiffJob;
import com.raytheon.uf.viz.image.export.job.SaveImagesJob;
import com.raytheon.uf.viz.image.export.options.ImageExportOptions;
import com.raytheon.uf.viz.image.export.options.ImageExportOptions.FrameSelection;
import com.raytheon.uf.viz.image.export.options.ImageExportOptions.ImageFormat;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Save the current screen to an image file
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jul 26, 2006           chammack    Initial Creation.
 * Jan 20, 2014  2312     bsteffen    Move to image export plugin, animation.
 * Dec 04, 2014  DR16713  jgerth      Support for date and time in file name
 * Jul 07, 2015  4607     bsteffen    Extract SaveImagesJob and allow GeoTIFF export.
 * 
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class ExportImageHandler extends AbstractImageCaptureHandler {
    public static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ExportImageHandler.class);

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        IEditorPart part = EditorUtil.getActiveEditor();
        AbstractEditor editor = null;
        if (part instanceof AbstractEditor) {
            editor = (AbstractEditor) part;
        }

        if (editor == null) {
            return null;
        }

        Shell shell = HandlerUtil.getActiveShell(event);

        ImageExportOptions options = new ImageExportOptions();
        options.populate(editor);
        String frameSelection = event.getParameter("frameSelection");
        if ("current".equalsIgnoreCase(frameSelection)) {
            options.setFrameSelection(FrameSelection.CURRENT);
        } else if ("all".equalsIgnoreCase(frameSelection)) {
            options.setFrameSelection(FrameSelection.ALL);
        }

        String dialogType = event.getParameter("dialogType");
        if ("file".equalsIgnoreCase(dialogType)) {
            FileDialog fileDialog = new FileDialog(shell, SWT.SAVE);
            File file = options.getFileLocation();
            fileDialog.setFileName(file.getName());
            if (file.getParentFile() != null
                    && file.getParentFile().isDirectory()) {
                fileDialog.setFilterPath(file.getParent());
            }
            ImageFormat format = options.getImageFormat();
            String[] exts = format.getExtensions();
            StringBuilder filter = new StringBuilder(exts.length * 7);
            for (String suffix : exts) {
                if (!suffix.isEmpty()) {
                    if (filter.length() != 0) {
                        filter.append(';');
                    }
                    filter.append("*.").append(suffix);
                }
            }
            fileDialog.setFilterExtensions(new String[] { filter.toString() });
            fileDialog.setFilterNames(new String[] { format.getDescription() });
            String path = fileDialog.open();
            if (path == null) {
                return null;
            }

            options.setFileLocation(new File(path));

        } else {
            if (openOptionsDialog(shell, options) == false) {
                return null;
            }
        }

        LinkedHashMap<DataTime, BufferedImage> dtbiHash = new LinkedHashMap<DataTime, BufferedImage>();
        try {
            switch (options.getFrameSelection()) {
            case CURRENT:
                dtbiHash = captureCurrentFrames(editor);
                break;
            case ALL:
                dtbiHash = captureAllFrames(editor);
                break;
            case USER:
                dtbiHash = captureFrames(editor, options.getFirstFrameIndex(),
                        options.getLastFrameIndex());
                break;
            }
        } catch (VizException e) {
            String reason = "Error occurred while capturing images";
            statusHandler.handle(Priority.PROBLEM, reason, e);
            return null;
        }

        if (!dtbiHash.isEmpty()) {
            if (options.isGeoreference()) {
                new SaveGeotiffJob(options, dtbiHash);
            } else {
                new SaveImagesJob(options, dtbiHash);
            }
        }
        return null;
    }

    /**
     * Open an image dialog
     * 
     * @return false if the user canceled, true otherwise.
     */
    protected boolean openOptionsDialog(Shell shell, ImageExportOptions options) {
        ImageExportDialog dialog = new ImageExportDialog(shell, options);
        return dialog.open() != null;
    }
}
