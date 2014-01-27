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

package com.raytheon.uf.common.localization.msgs;

import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * The delete command removes a localization file.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 30, 2007            njensen     Initial creation
 * Aug 22, 2008  #1502     bclement    Added JAXB/Serializable annotations
 * Oct 01, 2013   2361     njensen     Removed XML annotations
 * 
 * </pre>
 * 
 * @author njensen
 */

@DynamicSerialize
public class DeleteUtilityCommand extends AbstractPrivilegedUtilityCommand {

    public DeleteUtilityCommand() {

    }

    public DeleteUtilityCommand(LocalizationContext context, String filename) {
        super(context);
        this.filename = filename;
    }

}
