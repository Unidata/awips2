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
package com.raytheon.viz.mpe.ui.dialogs.gagetable;

import java.util.EventObject;

/**
 * Gage Table event update object.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 11, 2009 2476       mpduff     Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

public class GageTableUpdateEvent extends EventObject {

    private static final long serialVersionUID = -3374697794591550956L;
    
    private boolean changed = false;

    /**
     * Constructor.
     * 
     * @param source
     *      The source object.
     * @param changed
     *      Has data changed?
     */
    public GageTableUpdateEvent(Object source, boolean changed) {
        super(source);
        
        this.changed = changed;
    }

    /**
     * @return the changed
     */
    public boolean isChanged() {
        return changed;
    }
}
