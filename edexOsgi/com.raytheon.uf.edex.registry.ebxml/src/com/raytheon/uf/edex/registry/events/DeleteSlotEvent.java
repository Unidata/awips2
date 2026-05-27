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
package com.raytheon.uf.edex.registry.events;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.event.Event;
import com.raytheon.uf.common.util.CollectionUtil;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;

/**
 * Event containing slots to be deleted by the registry garbage collector
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 4/11/2014    3011        bphillip    Initial Coding
 * 4/17/2014    3011        bphillip    Delete slot events now contain strings
 * 5/11/2020    8161        bsteffen    Change slot id to a long
 * 
 * </pre>
 * 
 * @author bphillip
 */
public class DeleteSlotEvent extends Event {

    private static final long serialVersionUID = -2818002679753482984L;

    private List<Long> slotsToDelete;;

    public DeleteSlotEvent() {
        super();
    }

    public DeleteSlotEvent(List<SlotType> slotsToDelete) {
        if (CollectionUtil.isNullOrEmpty(slotsToDelete)) {
            slotsToDelete = new ArrayList<>();
        } else {
            this.slotsToDelete = new ArrayList<>(slotsToDelete.size());
            for (SlotType slot : slotsToDelete) {
                this.slotsToDelete.add(slot.getId());
            }
        }
    }

    public List<Long> getSlotsToDelete() {
        return slotsToDelete;
    }

    public void setSlotsToDelete(List<Long> slotsToDelete) {
        this.slotsToDelete = slotsToDelete;
    }

}
