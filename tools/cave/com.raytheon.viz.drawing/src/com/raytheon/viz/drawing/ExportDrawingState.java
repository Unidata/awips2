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

package com.raytheon.viz.drawing;

import java.io.FileNotFoundException;
import java.io.PrintWriter;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.FileDialog;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.tools.map.AbstractMapTool;

/**
 * 
 * Export the drawing state of the XML
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  Nov 16, 2006             chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */

public class ExportDrawingState extends AbstractMapTool {

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.tools.AbstractTool#runTool()
     */
    @Override
    public Object execute(ExecutionEvent arg0) throws ExecutionException {
        super.execute(arg0);

        IDescriptor md = editor.getActiveDisplayPane().getDescriptor();

        DrawingLayer drawingLayer = null;
        ResourceList rscList = md.getResourceList();
        for (ResourcePair rp : rscList) {
            AbstractVizResource<?, ?> rsc = rp.getResource();
            if (rsc instanceof DrawingLayer) {
                drawingLayer = (DrawingLayer) rsc;
            }
        }

        if (drawingLayer != null) {

            String out = DrawingIO.getDrawingStateXML(drawingLayer);

            FileDialog saveAsDialog = new FileDialog(VizWorkbenchManager
                    .getInstance().getCurrentWindow().getShell(), SWT.SAVE);
            saveAsDialog.setFilterExtensions(new String[] { "*.xml" });

            String fileName = saveAsDialog.open();
            if (fileName != null) {
                try {
                    PrintWriter pw = new PrintWriter(fileName);
                    pw.write(out);
                    pw.close();
                } catch (FileNotFoundException e1) {
                    // TODO Auto-generated catch block
                    e1.printStackTrace();
                }

            }
        }

        return null;
    }

}
