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

package com.raytheon.uf.viz.d2d.nsharp.rsc.action;

import gov.noaa.nws.ncep.ui.nsharp.palette.NsharpPrintHandle;
import gov.noaa.nws.ncep.ui.nsharp.skewt.NsharpSkewTDescriptor;
import gov.noaa.nws.ncep.ui.nsharp.skewt.NsharpSkewTEditor;
import gov.noaa.nws.ncep.ui.nsharp.skewt.rsc.NsharpSkewTResource;
import gov.noaa.nws.ncep.ui.nsharp.skewt.rsc.NsharpSkewTResource.ElementStateProperty;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.printing.PrintDialog;
import org.eclipse.swt.printing.Printer;
import org.eclipse.swt.printing.PrinterData;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IFrameCoordinator.FrameChangeMode;
import com.raytheon.uf.viz.core.drawables.IFrameCoordinator.FrameChangeOperation;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.actions.PrintScreenAction;

/**
 * Print the current map
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jul 26, 2006             chammack    Initial Creation.
 * Aug 08, 2008      #703   randerso    fixed bug, changed to scale to fit 
 *                                      paper and rotate if necessary
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class NSharpPrintScreenAction extends PrintScreenAction {

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.
     * ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        NsharpSkewTEditor editor = null;
        IEditorPart part = EditorUtil.getActiveEditor();
        if (part instanceof NsharpSkewTEditor) {
            editor = (NsharpSkewTEditor) part;
        }
        if (editor == null) {
            return super.execute(event);
        }
        IDisplayPane pane = editor.getActiveDisplayPane();
        IDescriptor desc = pane.getDescriptor();

        NsharpSkewTDescriptor ndesc = editor.getNsharpSkewTDescriptor();

        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();

        // display the printer dialog to get print options
        PrintDialog pd = new PrintDialog(shell);
        String frameMode = event.getParameter("frameSelection");
        if (frameMode == null || frameMode.equalsIgnoreCase("current")) {
            // selection doesn't seem to work.
            pd.setScope(PrinterData.PAGE_RANGE);
            pd.setStartPage(getCurrentIndex(ndesc.getSkewtResource()) + 1);
            pd.setEndPage(getCurrentIndex(ndesc.getSkewtResource()) + 1);
        } else if (frameMode.equalsIgnoreCase("all")) {
            pd.setScope(PrinterData.ALL_PAGES);
        } else {
            throw new ExecutionException("Invalid frameMode: " + frameMode);
        }
        PrinterData printerData = pd.open();
        if (printerData == null) {
            return null;
        }

        NsharpPrintHandle handle = new NsharpPrintHandle();
        handle.createPrinter(printerData);
        if (handle.startJob()) {
            switch (pd.getScope()) {
            case PrinterData.ALL_PAGES: {
                try {
                    printAllFrames(handle, editor);
                } catch (VizException e) {
                    throw new ExecutionException(
                            "Error occurred while writing image", e);
                }
                break;
            }
            case PrinterData.PAGE_RANGE: {
                try {
                    printFrames(handle, editor, pd.getStartPage() - 1,
                            pd.getEndPage());
                } catch (VizException e) {
                    throw new ExecutionException(
                            "Error occurred while writing image", e);
                }
                break;
            }
            case PrinterData.SELECTION: {
                printImage(handle, editor);
                break;
            }
            }
            handle.endJob();

        }
        handle.disposePrinter();
        return null;
    }

    private int getCurrentIndex(NsharpSkewTResource rsc) {
        int index = rsc.getDataTimelineList().size();
        for (ElementStateProperty element : rsc.getDataTimelineList()) {
            index -= 1;
            if (element.getElementDescription().equals(
                    rsc.getPickedStnInfoStr())) {
                return index;
            }
        }
        return 0;
    }

    private void printAllFrames(NsharpPrintHandle printer,
            NsharpSkewTEditor editor) throws VizException {
        printFrames(printer, editor, 0, editor.getNsharpSkewTDescriptor()
                .getSkewtResource().getDataTimelineList().size());
    }

    private void printFrames(NsharpPrintHandle printer,
            NsharpSkewTEditor editor, int startIndex, int endIndex)
            throws VizException {
        NsharpSkewTDescriptor ndesc = editor.getNsharpSkewTDescriptor();
        IDisplayPane pane = editor.getActiveDisplayPane();
        String picked = ndesc.getSkewtResource().getPickedStnInfoStr();
        if (getCurrentIndex(ndesc.getSkewtResource()) > startIndex) {
            ndesc.getFrameCoordinator().changeFrame(FrameChangeOperation.FIRST,
                    FrameChangeMode.TIME_AND_SPACE);
            editor.getActiveDisplayPane().refresh();
        }
        renderPane(pane, editor.getLoopProperties());
        boolean first = true;
        for (int i = getCurrentIndex(ndesc.getSkewtResource()); i < endIndex; i++) {
            if (!first) {
                ndesc.getFrameCoordinator().changeFrame(
                        FrameChangeOperation.NEXT,
                        FrameChangeMode.TIME_AND_SPACE);
                pane.refresh();
                renderPane(pane, editor.getLoopProperties());
            }
            if (i >= startIndex) {
                printImage(printer, editor);
            }
            first = false;
        }
        while (!ndesc.getSkewtResource().getPickedStnInfoStr().equals(picked)) {
            ndesc.getFrameCoordinator().changeFrame(FrameChangeOperation.NEXT,
                    FrameChangeMode.TIME_AND_SPACE);
            pane.refresh();
            renderPane(pane, editor.getLoopProperties());
        }
    }

    private void printImage(NsharpPrintHandle printer, NsharpSkewTEditor editor) {
        printer.printPage(editor.getNsharpSkewTDescriptor());
    }

}
