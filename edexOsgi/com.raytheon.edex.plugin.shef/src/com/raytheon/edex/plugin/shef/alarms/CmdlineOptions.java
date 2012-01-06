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
package com.raytheon.edex.plugin.shef.alarms;

import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;

/**
 * Convenience wrapper around org.apache.commons.cli.Options
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * June 15, 2011    9377     jnjanga     Initial creation
 * 
 * 
 * </pre>
 * 
 * @author jnjanga
 * @version 1.0
 */

public class CmdlineOptions extends Options {

    private static final long serialVersionUID = 1L;

    CmdlineOptions() {
        super();
    }

    public void addOption(CmdlineOptionId optId) {
        Option opt = new Option(optId.toString(), true, optId.description());
        addOption(opt);
    }

    public void addOption(CmdlineOptionId optId, boolean hasArg) {
        Option opt = new Option(optId.toString(), hasArg, optId.description());
        addOption(opt);
    }

    public void addMandatoryOption(CmdlineOptionId optId) {
        Option opt = new Option(optId.toString(), true, optId.description());
        opt.setRequired(true);
        addOption(opt);
    }

    public boolean hasOption(CmdlineOptionId optId) {
        return hasOption(optId.toString());
    }

}