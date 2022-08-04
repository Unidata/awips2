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
package com.raytheon.viz.gfe.smarttool.script;

import java.util.Collection;
import java.util.List;

import com.raytheon.viz.gfe.smartscript.FieldDefinition;

/**
 * Collects all the metadata used by the GFE UI for smart tools.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 22, 2015  #4263     dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public final class SmartToolMetadata {

    private final String name;

    private final String weatherElementEdited;

    private final Collection<String> screenList;

    private final boolean hideTool;

    private final String docString;

    private final List<FieldDefinition> varDictWidgets;

    /**
     * Construct a smart tool metadata record.
     * 
     * @param name
     *            Name of the smart tool.
     * @param weatherElementEdited
     *            The parm name of the parm this tool modifies.
     * @param screenList
     *            A list of parm names or parm types to show this tool for when
     *            showing tool inventories.
     * @param hideTool
     *            Whether or not to show this tool in inventories for menus or
     *            dialogs.
     * @param docString
     *            A short description of what this tool does.
     * @param varDictWidgets
     *            A list of {@code FieldDefinition} objects that is used to
     *            construct the user GUI for this tool.
     */
    public SmartToolMetadata(String name, String weatherElementEdited,
            Collection<String> screenList, boolean hideTool, String docString,
            List<FieldDefinition> varDictWidgets) {
        this.name = name;
        this.weatherElementEdited = weatherElementEdited;
        this.screenList = screenList;
        this.hideTool = hideTool;
        this.docString = docString;
        this.varDictWidgets = varDictWidgets;
    }

    public String getName() {
        return name;
    }

    public String getWeatherElementEdited() {
        return weatherElementEdited;
    }

    public Collection<String> getScreenList() {
        return screenList;
    }

    public boolean isHideTool() {
        return hideTool;
    }

    public String getDocString() {
        return docString;
    }

    public List<FieldDefinition> getVarDictWidgets() {
        return varDictWidgets;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("SmartToolMetadata [name=");
        builder.append(name);
        builder.append(", weatherElementEdited=");
        builder.append(weatherElementEdited);
        builder.append(", screenList=");
        builder.append(screenList);
        builder.append(", hideTool=");
        builder.append(hideTool);
        builder.append(", docString=");
        builder.append(docString);
        builder.append("]");
        return builder.toString();
    }
}
