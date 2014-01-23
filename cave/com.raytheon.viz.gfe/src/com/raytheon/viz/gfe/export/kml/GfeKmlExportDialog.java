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
package com.raytheon.viz.gfe.export.kml;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.kml.export.KmlExportDialog;
import com.raytheon.uf.viz.kml.export.KmlExportOptions;
import com.raytheon.uf.viz.kml.export.KmlPane;
import com.raytheon.viz.gfe.export.image.GfeImageExportDialog;

/**
 * A custom {@link KmlExportDialog} for GFE which adds on option to use the Grid
 * Manager Selected Time Range.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 23, 2014  2702     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class GfeKmlExportDialog extends KmlExportDialog {

    protected Button selectedFrameRangeButton;

    public GfeKmlExportDialog(Shell shell, KmlExportOptions options) {
        super(shell, options);
    }

    @Override
    protected void initializeFramesGroup(Group group) {
        super.initializeFramesGroup(group);
        selectedFrameRangeButton = new Button(group, SWT.RADIO);
        selectedFrameRangeButton.setText("Grid Manager Selected Time Range");
        GridData gridData = new GridData();
        gridData.horizontalSpan = 5;
        selectedFrameRangeButton.setLayoutData(gridData);
    }

    @Override
    protected void okPressed() {
        if (selectedFrameRangeButton.getSelection()) {
            KmlPane pane = options.getSinglPane();
            IRenderableDisplay renderableDispaly = pane.getDisplay();
            
            int[] frameRange = GfeImageExportDialog.getSelectedFrameRange(getShell(),
                    renderableDispaly);
            if(frameRange == null){
                return;
            }
            options.setFirstFrameIndex(frameRange[0]);
            options.setLastFrameIndex(frameRange[1]);
        }
        super.okPressed();
    }

}
