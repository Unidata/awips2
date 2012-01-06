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
package com.raytheon.edex.plugin.sfcobs.decoder.synoptic;

import com.raytheon.edex.exception.DecoderException;
import com.raytheon.uf.common.dataplugin.sfcobs.ObsCommon;
import com.raytheon.uf.edex.decodertools.core.ReportParser;

/**
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20070925            391 jkorman     Initial Coding.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public abstract class AbstractSectionDecoder {

    private static final int[] GROUP_MASK = { 0x00000001, 0x00000002,
            0x00000004, 0x000000008, 0x00000010, 0x00000020, 0x00000040,
            0x00000080, 0x000000100, 0x00000200, };

    // The parent decoder of the section data.
    // private AbstractSynopticDecoder parent;

    // The group data.
    private int groupsPerformed = 0;

    protected AbstractSynopticDecoder decoderParent = null;

    public AbstractSectionDecoder(AbstractSynopticDecoder parent) {
        this.decoderParent = parent;
    }

    /**
     * 
     * @param reportParser
     * @throws DecoderException
     */
    public abstract void decode(ReportParser reportParser)
            throws DecoderException;

    /**
     * Get any decoded data from the section.
     * 
     * @param receiver
     *            An ObsCommon object to receive the decoded data.
     * @return The populated decoded data.
     */
    public abstract ObsCommon getDecodedData(ObsCommon receiver);

    /**
     * Clear all groups. Allows groups starting at group 0.
     */
    protected void resetGroups() {
        groupsPerformed = 0;
    }

    /**
     * Determine if a given group should be processed.
     * 
     * @param groupId
     *            The group index.
     * @return Should the group be processed.
     */
    protected boolean doGroup(int groupId) {
        boolean doIt = false;
        if ((groupId >= 0) && (groupId < GROUP_MASK.length)) {
            doIt = (GROUP_MASK[groupId] & groupsPerformed) == 0;
        }
        return doIt;
    }

    /**
     * Close a group for processing. By closing a group, this group and all
     * groups with a lower index are closed also.
     * 
     * @param groupId
     *            The group index to close.
     */
    protected void closeGroup(int groupId) {
        if ((groupId >= 0) && (groupId < GROUP_MASK.length)) {
            for (int i = 0; i < groupId; i++) {
                groupsPerformed |= GROUP_MASK[i];
            }
        }
    }

    public AbstractSynopticDecoder getParent() {
        return decoderParent;
    }
}
