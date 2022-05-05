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

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.printing.PrintDialog;
import org.eclipse.swt.printing.PrinterData;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.printing.PrintJob;
import com.raytheon.uf.viz.core.printing.PrintJob.PrintingException;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Print the current editor
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jul 26, 2006           chammack  Initial Creation.
 * Aug 08, 2008  703      randerso  fixed bug, changed to scale to fit paper and
 *                                  rotate if necessary
 * Jan 20, 2014  2312     bsteffen  Move to image export plugin.
 * Dec 04, 2014  16713    jgerth    Support for date and time in file name
 * Mar 23, 2017  6117     bsteffen  Workaround crash when printing images.
 * Apr 13, 2020  8120     randerso  Use PrintJob
 *
 * </pre>
 *
 * @author chammack
 */
public class PrintImageCaptureHandler extends AbstractImageCaptureHandler {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(PrintImageCaptureHandler.class);

    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        AbstractEditor editor = null;
        IEditorPart part = EditorUtil.getActiveEditor();
        if (part instanceof AbstractEditor) {
            editor = (AbstractEditor) part;
        }
        if (editor == null) {
            return null;
        }

        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();

        IDescriptor desc = editor.getActiveDisplayPane().getDescriptor();

        // display the printer dialog to get print options
        PrintDialog pd = new PrintDialog(shell);
        String frameMode = event.getParameter("frameSelection");
        if (frameMode == null || "current".equalsIgnoreCase(frameMode)) {
            // selection doesn't seem to work.
            pd.setScope(PrinterData.PAGE_RANGE);
            pd.setStartPage(desc.getFramesInfo().getFrameIndex() + 1);
            pd.setEndPage(desc.getFramesInfo().getFrameIndex() + 1);
        } else if ("all".equalsIgnoreCase(frameMode)) {
            pd.setScope(PrinterData.ALL_PAGES);
        } else {
            throw new ExecutionException("Invalid frameMode: " + frameMode);
        }
        PrinterData printerData = pd.open();
        if (printerData == null) {
            return null;
        }

        Display display = editor.getActiveDisplayPane().getDisplay();
        try (PrintJob printJob = new PrintJob(printerData)) {
            switch (pd.getScope()) {
            case PrinterData.ALL_PAGES: {
                try {
                    for (BufferedImage bi : captureAllFrames(editor).values()) {
                        printJob.printImage(display, bi, false, true);
                    }
                } catch (VizException e) {
                    throw new ExecutionException(
                            "Error occurred while writing image", e);
                }
                break;
            }
            case PrinterData.PAGE_RANGE: {
                try {
                    for (BufferedImage bi : captureFrames(editor,
                            pd.getStartPage() - 1, pd.getEndPage()).values()) {
                        printJob.printImage(display, bi, false, true);
                    }
                } catch (VizException e) {
                    throw new ExecutionException(
                            "Error occurred while writing image", e);
                }
                break;
            }
            case PrinterData.SELECTION: {
                BufferedImage bi = editor.screenshot();
                printJob.printImage(display, bi, false, true);
                break;
            }
            }

        } catch (PrintingException e) {
            statusHandler.error(e.getLocalizedMessage(), e);
        }

        return null;
    }

}
