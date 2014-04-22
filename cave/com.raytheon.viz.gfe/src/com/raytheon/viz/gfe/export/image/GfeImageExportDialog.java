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
package com.raytheon.viz.gfe.export.image;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.image.export.dialog.ImageExportDialog;
import com.raytheon.uf.viz.image.export.options.ImageExportOptions;
import com.raytheon.uf.viz.image.export.options.ImageExportOptions.FrameSelection;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.DataManagerUIFactory;
import com.raytheon.viz.gfe.core.parm.ParmOp;
import com.raytheon.viz.ui.EditorUtil;

/**
 * A custom {@link ImageExportDialog} for GFE which adds on option to use the
 * Grid Manager Selected Time Range.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 22, 2014  2312     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GfeImageExportDialog extends ImageExportDialog {

    protected Button selectedFrameRangeButton;

    public GfeImageExportDialog(Shell parentShell, ImageExportOptions options) {
        super(parentShell, options);
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
            IDisplayPaneContainer container = EditorUtil
                    .getActiveVizContainer();
            IDisplayPane pane = container.getActiveDisplayPane();
            IRenderableDisplay renderableDispaly = pane.getRenderableDisplay();
            int[] frameRange = getSelectedFrameRange(getShell(),
                    renderableDispaly);
            if(frameRange == null){
                return;
            }
            options.setFrameSelection(FrameSelection.USER);
            options.setFirstFrameIndex(frameRange[0]);
            options.setLastFrameIndex(frameRange[1]);
        }
        super.okPressed();
    }

    public static int[] getSelectedFrameRange(Shell shell,
            IRenderableDisplay renderableDispaly) {
        DataManager dataManager = DataManagerUIFactory.getCurrentInstance();
        ParmOp parmOp = dataManager.getParmOp();
        TimeRange selectedRange = parmOp.getSelectionTimeRange();
        if (selectedRange == null || !selectedRange.isValid()) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("No Time Range Selected");
            mb.setMessage("No Time Range is selected, select a time range in the grid manager.");
            mb.open();
            return null;
        }
        IDescriptor descriptor = renderableDispaly.getDescriptor();
        FramesInfo fi = descriptor.getFramesInfo();
        DataTime[] times = fi.getFrameTimes();
        int start = -1;
        int end = -1;
        for (int i = 0; i < times.length; i += 1) {
            if (times[i] == null) {
                continue;
            }
            TimeRange validRange = times[i].getValidPeriod();
            if (selectedRange.overlaps(validRange)) {
                if (start == -1) {
                    start = i;
                }
                end = i;
            } else if (start != -1) {
                break;
            }
        }
        if (start == -1) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Invalid Selection");
            mb.setMessage("The selected time range does not contain any data, choose a new range.");
            mb.open();
            return null;
        }
        return new int[] { start, end + 1 };
    }
}
