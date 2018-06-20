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

package com.raytheon.viz.ui.tools.map;

import com.raytheon.viz.ui.tools.AbstractTool;

/**
 * Adds convenience methods for editor to avoid casting
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Oct 10, 2006           chammack    Initial Creation.
 * Sep 03, 2013  2310     bsteffen    Remove unused function, mark deprecated.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 * @deprected Extend {@link AbstractTool} instead.
 */
@Deprecated
public abstract class AbstractMapTool extends AbstractTool {

    /**
     * Constructor
     * 
     */
    public AbstractMapTool() {
        super();
    }

}
