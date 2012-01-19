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
package com.raytheon.viz.gfe.ui;

import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.parm.ParmState;
import com.raytheon.viz.gfe.core.parm.ParmState.VectorMode;

/**
 * The dynamic vector edit mode menu
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01JUL2008		#1129	ebabin	Initial creation
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class VectorEditModeMenu extends EnumMenu {
    private static final String COMMAND_ID = "com.raytheon.viz.gfe.actions.UpdateVectorEditorMode";

    @Override
    protected String getCategoryId() {
        return "GFE";
    }

    @Override
    protected String getCommandId() {
        return COMMAND_ID;
    }

    @Override
    protected String getCommandName() {
        return "UpdateVectorEditorMode";
    }

    @Override
    protected Enum<?> getCurrentValue() {
        return ParmState.getCurrentVectorMode();
    }

    @Override
    protected void setCurrentValue(Enum<?> value) {
        DataManager dm = DataManager.getCurrentInstance();
        if (dm != null) {
            VectorMode mode = (VectorMode) value;
            dm.getParmOp().setVectorMode(mode);
        }
    }

}
