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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Contains the command history
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 30, 2009 2191       rjpeter     Initial creation
 * May 16, 2011 7545       cjeanbap    Add clearCommandMapIndexes().
 * Jun 05, 2011 9741       cjeanbap    Removed clearCommandMapIndexes().
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class CommandHistory {
    private static final int HISTORY_SIZE = 20;

    private List<ICommand> recentHistory;

    private Map<CommandType, List<ICommand>> commandMap;

    private Map<CommandType, Integer> commandIndexes;

    /**
     * 
     */
    public CommandHistory() {
        recentHistory = new ArrayList<ICommand>(HISTORY_SIZE);
        commandMap = new HashMap<CommandType, List<ICommand>>();
        commandIndexes = new HashMap<CommandType, Integer>();

        for (CommandType type : CommandType.values()) {
            commandMap.put(type, new ArrayList<ICommand>());
            commandIndexes.put(type, 0);
        }
    }

    /**
     * 
     * @param command
     */
    public void addCommand(ICommand command) {
        List<ICommand> commands = commandMap.get(command.getType());

        // verify history size
        if (commands.size() >= HISTORY_SIZE) {
            commands.remove(0);
        }

        // reset index to latest entry
        commandIndexes.put(command.getType(), commands.size() - 1);
        commands.add(command);

        // handle recent history
        if (recentHistory.size() >= HISTORY_SIZE) {
            recentHistory.remove(0);
        }

        recentHistory.add(command);
    }

    /**
     * Returns the recent command history. Index 0 is the earliest history, last
     * index is the most recent command.
     * 
     * @return
     */
    public List<ICommand> getRecentHistory() {
        return recentHistory;
    }

    /**
     * Returns the previous command, if already at the oldest command, the
     * oldest command is returned again. Null is only possible if there are no
     * commands in the history.
     * 
     * @return
     */
    public ICommand getPreviousCommand(CommandType type) {
        ICommand rval = null;

        List<ICommand> commands = commandMap.get(type);

        // verify list has items
        if (commands.size() > 0) {
            // decrement the index
            commandIndexes.put(type, commandIndexes.get(type) - 1);

            // verifyIndex may change the index and due to immutability of
            // Integer, will need to re-get the value
            verifyIndex(type);

            int index = commandIndexes.get(type);
            rval = commands.get(index);
        }

        return rval;
    }

    /**
     * Returns the next command in the history, if there are no more recent
     * commands, null is returned.
     * 
     * @return
     */
    public ICommand getNextCommand(CommandType type) {
        ICommand rval = null;

        List<ICommand> commands = commandMap.get(type);

        // verify list has items
        if (commands.size() > 0) {
            // increment the index
            commandIndexes.put(type, commandIndexes.get(type) + 1);

            // verifyIndex may change the index and due to immutability of
            // Integer, will need to re-get the value
            verifyIndex(type);

            int index = commandIndexes.get(type);

            // verify within list range
            if (index < commands.size()) {
                rval = commands.get(index);
            }
        }

        return rval;
    }

    /**
     * Returns the next command in the history, if there are no more recent
     * commands, the most recent command is returned. Null is only possible if
     * there are no commands in the history.
     * 
     * @return
     */
    public void resetIndex(CommandType type) {
        List<ICommand> commands = commandMap.get(type);

        // set the index
        commandIndexes.put(type, commands.size());
    }

    /**
     * Will ensure the index is between 0 and the list.size()
     * 
     * @param type
     */
    private void verifyIndex(CommandType type) {
        List<ICommand> commands = commandMap.get(type);
        int index = commandIndexes.get(type);

        if (index < 0) {
            index = 0;
        } else if (index > commands.size()) {
            index = commands.size();
        }

        commandIndexes.put(type, index);
    }
}
