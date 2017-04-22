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
 * Sep 09, 2014 3580       mapeters    {@link #executeCommand()} takes no parameters.
 * Aug 28, 2016 5839       rferrel     Added methods to get next/previous/latest/All commands.
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
     * @return
     */
    public abstract List<StdTextProduct> executeCommand()
            throws CommandFailedException;

    /**
     * Get a command for the current product's next product.
     * 
     * @return nextCmd
     */
    public abstract ICommand getNext();

    /**
     * Get a command for the current product's previous product.
     * 
     * @return previousCmd
     */
    public abstract ICommand getPrevious();

    /**
     * Get a command for the all products based on the current product.
     * 
     * @return allCmd
     */
    public abstract ICommand getAll();

    /**
     * Get a command for the current product's latest product.
     * 
     * @return latestCmd
     */
    public abstract ICommand getLatest();
}