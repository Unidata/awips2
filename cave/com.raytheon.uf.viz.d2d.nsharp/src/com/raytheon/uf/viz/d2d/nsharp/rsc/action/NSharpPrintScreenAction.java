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


import gov.noaa.nws.ncep.ui.nsharp.display.NsharpEditor;
import gov.noaa.nws.ncep.ui.nsharp.display.rsc.NsharpResourceHandler;
import gov.noaa.nws.ncep.ui.nsharp.view.NsharpPrintHandle;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.printing.PrintDialog;
import org.eclipse.swt.printing.PrinterData;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.image.export.handler.PrintImageCaptureHandler;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;

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
public class NSharpPrintScreenAction extends PrintImageCaptureHandler {

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.
     * ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        NsharpEditor editor = null;
        IEditorPart part = EditorUtil.getActiveEditor();
        if (part instanceof NsharpEditor) {
            editor = (NsharpEditor) part;
        }
        if (editor == null) {
            return super.execute(event);
        }

        NsharpResourceHandler handler = ((NsharpEditor) editor).getRscHandler();

        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                .getShell();

        // display the printer dialog to get print options
        PrintDialog pd = new PrintDialog(shell);
        String frameMode = event.getParameter("frameSelection");
        if (frameMode == null || frameMode.equalsIgnoreCase("current")) {
            // selection doesn't seem to work.
            pd.setScope(PrinterData.PAGE_RANGE);
            pd.setStartPage(handler.getCurrentIndex()+1);//Chin NsharpFrameIndexUtil.getCurrentIndex(handler) + 1);
            pd.setEndPage(handler.getCurrentIndex()+1); // Chin NsharpFrameIndexUtil.getCurrentIndex(handler) + 1);
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
                printImage(handle);
                break;
            }
            }
            handle.endJob();

        }
        handle.disposePrinter();
        return null;
    }



    private void printAllFrames(NsharpPrintHandle printer, NsharpEditor editor)
            throws VizException {
        printFrames(printer, editor, 0, editor.getRscHandler().getFrameCount()); //Chin NsharpFrameIndexUtil.getFrameCount(editor.getRscHandler()));
    }

    private void printFrames(NsharpPrintHandle printer, NsharpEditor editor,
            int startIndex, int endIndex) throws VizException {
        NsharpResourceHandler handler = ((NsharpEditor) editor).getRscHandler();
        IDisplayPane pane = editor.getActiveDisplayPane();
        int startingIndex = handler.getCurrentIndex();//Chin  NsharpFrameIndexUtil.getCurrentIndex(handler);
        LoopProperties loopProperties = ((AbstractEditor) editor)
                .getLoopProperties();
        renderPane(pane, loopProperties);
        for (int i = startIndex; i < endIndex; i++) {
            //Chin NsharpFrameIndexUtil.setCurrentIndex(handler, i);
        	if(handler.setCurrentIndex(i)== false)
        		continue;
            pane.refresh();
            renderPane(pane, loopProperties);
            printImage(printer);
        }
        handler.setCurrentIndex(startingIndex); // Chin  NsharpFrameIndexUtil.setCurrentIndex(handler, startingIndex);
        pane.refresh();
        renderPane(pane, loopProperties);
    }

    private void printImage(NsharpPrintHandle printer) {
        printer.printPage();
    }

}
