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
package com.raytheon.viz.texteditor.command;

import java.util.List;

import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.dataplugin.text.dbsrv.IQueryTransport;

/**
 * A command.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 30, 2009 2191       rjpeter     Initial creation
 * Apr 14, 2010 4734       mhuang      Corrected StdTextProduct import 
 *                                      dependency
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public interface ICommand {

    /**
     * 
     * @return
     */
    public abstract CommandType getType();

    /**
     * 
     * @return
     */
    public abstract boolean isValid();

    /**
     * 
     * @return
     */
    public abstract String[] getCommandTextFields();

    /**
     * Even though a command may return numerous entries, the command should
     * know how to aggregate the numerous returns into a single entry.
     * 
     * @param transport
     *            Query transport mechanism
     * 
     * @return
     */
    public abstract List<StdTextProduct> executeCommand(
            IQueryTransport transport) throws CommandFailedException;
}