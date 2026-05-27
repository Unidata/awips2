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
package com.raytheon.viz.gfe.procedures;

import java.util.Collection;
import java.util.List;

import com.raytheon.viz.gfe.smartscript.FieldDefinition;

/**
 * Collects all the metadata used by the GFE UI for procedures.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 24, 2015  #4263     dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public final class ProcedureMetadata {

    private final String name;

    private final Collection<String> menuNames;

    private final Collection<String> argNames;

    private final List<FieldDefinition> varDictWidgets;

    /**
     * Construct a new procedure metadata record.
     * 
     * @param name
     *            Name of the procedure.
     * @param menuName
     *            List of GFE menu names to show this tool under.
     * @param argNames
     *            Argument names for the procedure's execute method.
     * @param varDictWidgets
     *            A list of {@code FieldDefinition} objects that is used to
     *            construct the user GUI for this tool.
     */
    public ProcedureMetadata(String name, Collection<String> menuNames,
            Collection<String> argNames, List<FieldDefinition> varDictWidgets) {
        this.name = name;
        this.menuNames = menuNames;
        this.argNames = argNames;
        this.varDictWidgets = varDictWidgets;
    }

    public String getName() {
        return name;
    }

    public Collection<String> getMenuNames() {
        return menuNames;
    }

    public Collection<String> getArgNames() {
        return argNames;
    }

    public List<FieldDefinition> getVarDictWidgets() {
        return varDictWidgets;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("ProcedureMetadata [name=");
        builder.append(name);
        builder.append(", menuNames=");
        builder.append(menuNames);
        builder.append(", argNames=");
        builder.append(argNames);
        builder.append("]");
        return builder.toString();
    }
}
