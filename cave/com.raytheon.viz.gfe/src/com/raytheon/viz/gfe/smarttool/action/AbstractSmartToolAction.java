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
package com.raytheon.viz.gfe.smarttool.action;

import org.eclipse.jface.action.Action;

import com.raytheon.viz.gfe.dialogs.EditActionsDialog;

/**
 * Abstract action for smart tools
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2008            njensen     Initial creation	
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public abstract class AbstractSmartToolAction extends Action {

    protected String name;

    protected boolean utility;

    protected EditActionsDialog dialog;

    public AbstractSmartToolAction(String smartToolName, String text,
            boolean utility) {
        super(text);
        name = smartToolName;
        this.utility = utility;
    }

    public AbstractSmartToolAction(String smartToolName, String text,
            boolean utility, EditActionsDialog parentDialog) {
        this(smartToolName, text, utility);
        dialog = parentDialog;
    }

}
