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
package com.raytheon.viz.gfe.smarttool;

import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.time.TimeRange;

/**
 * Ported from A1.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 4, 2010            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class EditAction {

    private String itemName;

    private String element;

    private TimeRange timeRange;

    private ReferenceData refSet;

    private boolean emptyEditAreaFlag;

    private MissingDataMode missingDataMode;

    public EditAction(String itemName, String element, TimeRange timeRange,
            ReferenceData refSet, boolean emptyEditAreaFlag) {
        this(itemName, element, timeRange, refSet, emptyEditAreaFlag, null);
    }

    public EditAction(String itemName, String element, TimeRange timeRange,
            ReferenceData refSet, boolean emptyEditAreaFlag,
            MissingDataMode missingDataMode) {
        this.itemName = itemName;
        this.element = element;
        this.timeRange = timeRange;
        this.refSet = refSet;
        this.emptyEditAreaFlag = emptyEditAreaFlag;
        this.missingDataMode = missingDataMode;
    }

    public String getItemName() {
        return itemName;
    }

    public void setItemName(String itemName) {
        this.itemName = itemName;
    }

    public String getElement() {
        return element;
    }

    public void setElement(String element) {
        this.element = element;
    }

    public TimeRange getTimeRange() {
        return timeRange;
    }

    public void setTimeRange(TimeRange timeRange) {
        this.timeRange = timeRange;
    }

    public ReferenceData getRefSet() {
        return refSet;
    }

    public void setRefSet(ReferenceData refSet) {
        this.refSet = refSet;
    }

    public boolean isEmptyEditAreaFlag() {
        return emptyEditAreaFlag;
    }

    public void setEmptyEditAreaFlag(boolean emptyEditAreaFlag) {
        this.emptyEditAreaFlag = emptyEditAreaFlag;
    }

    public void setMissingDataMode(MissingDataMode missingDataMode) {
        this.missingDataMode = missingDataMode;
    }

    public MissingDataMode getMissingDataMode() {
        return missingDataMode;
    }

}
