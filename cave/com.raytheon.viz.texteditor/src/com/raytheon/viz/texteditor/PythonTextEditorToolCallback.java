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

package com.raytheon.viz.texteditor;

import java.util.List;

import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.texteditor.command.CommandFactory;
import com.raytheon.viz.texteditor.command.CommandFailedException;
import com.raytheon.viz.texteditor.command.ICommand;
import com.raytheon.viz.texteditor.msgs.IPythonTextEditorToolCallback;
import com.raytheon.viz.texteditor.util.TextEditorUtil;

/**
 * Implementation of IPythonTextEditorToolCallback interface.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 08/19/2009   2191        rjpeter     Initial creation.
 * 04/14/2010   4734        mhuang      Corrected StdTextProduct import 
 *                                       dependency
 * 09/09/2014   3580        mapeters    Removed IQueryTransport usage 
 *                                      (no longer exists).
 * </pre>
 * 
 * @author rjpeter
 * 
 */
public class PythonTextEditorToolCallback implements
        IPythonTextEditorToolCallback {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(PythonTextEditorToolCallback.class);
    private final Shell shell;

    public PythonTextEditorToolCallback(Shell parent) {
        this.shell = parent;
    }

    public StdTextProduct[] executeAFOSCommand(String afosCommand) {
        StdTextProduct[] rval = null;

        try {
            ICommand command = CommandFactory.getAfosCommand(afosCommand);
            List<StdTextProduct> prods = command.executeCommand();
            rval = prods.toArray(new StdTextProduct[0]);
        } catch (CommandFailedException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error executing command from python", e);

        }

        return rval;
    }

    public StdTextProduct[] executeAwipsQuery(String wmoid, String siteid,
            String awipsid, String hdrtime, String bbbid, String lastHours,
            String fullRead) {
        StdTextProduct[] rval = null;

        try {
            ICommand command = CommandFactory.getCommand(wmoid, siteid,
                    awipsid, hdrtime, bbbid, lastHours, fullRead);
            List<StdTextProduct> prods = command.executeCommand();
            rval = prods.toArray(new StdTextProduct[0]);
        } catch (CommandFailedException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error executing command from python", e);

        }

        return rval;
    }

    public void displayMessage(String msg) {
        TextEditorUtil.userInformation(shell, msg);
    }
}
