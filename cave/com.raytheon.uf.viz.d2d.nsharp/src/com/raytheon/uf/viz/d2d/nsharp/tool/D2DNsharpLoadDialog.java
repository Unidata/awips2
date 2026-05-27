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
package com.raytheon.uf.viz.d2d.nsharp.tool;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

import gov.noaa.nws.ncep.ui.nsharp.view.AbstractNsharpLoadDialog;

/**
 *
 * The class for D2D NSHARP Sounding Load Dialog
 *
 * <pre>
*
* SOFTWARE HISTORY
*
* Date          Ticket#  Engineer  Description
* ------------- -------- --------- -----------------
* Mar 24, 2020  73172    smanoj   Initial creation
 *
 * </pre>
 *
 * @author smanoj
 */
public class D2DNsharpLoadDialog extends AbstractNsharpLoadDialog {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(D2DNsharpLoadDialog.class);

    private static D2DNsharpLoadDialog INSTANCE = null;

    public static D2DNsharpLoadDialog getAccess() {
        return INSTANCE;
    }

    public D2DNsharpLoadDialog(Shell parentShell) throws VizException {
        super(parentShell);
    }

    public boolean closeDiaOnly() {
        return (super.close());
    }

    public static D2DNsharpLoadDialog getInstance(Shell parShell) {
        if (INSTANCE == null) {
            try {
                INSTANCE = new D2DNsharpLoadDialog(parShell);
            } catch (VizException e) {
                statusHandler.handle(Priority.ERROR,
                        "Error display NSHARP D2D load dialog", e.getMessage());
            }
        }
        return INSTANCE;
    }

    @Override
    public void createLoadContents(Composite parent) {
        dialogParent = parent;
        this.shell = parent.getShell();

        mapRscData = new D2DNsharpMapResourceData();
        LoadProperties loadProperties = new LoadProperties();
        mapRsc = new D2DNsharpMapResource(mapRscData, loadProperties);

        obsDialog = new D2DNsharpObsSoundingDlgContents(dialogParent);
        pfcDialog = new D2DNsharpPfcSoundingDlgContents(dialogParent);
        mdlDialog = new D2DNsharpMdlSoundingDlgContents(dialogParent);

        switch (activeLoadSoundingType) {
        case MODEL_SND:
            mdlDialog.createMdlDialogContents();
            break;
        case PFC_SND:
            pfcDialog.createPfcDialogContents();
            break;

        default:
            obsDialog.createObsDialogContents();
            activeLoadSoundingType = OBSER_SND;
            break;
        }
        soundingTypeList.setSelection(activeLoadSoundingType);
    }

}