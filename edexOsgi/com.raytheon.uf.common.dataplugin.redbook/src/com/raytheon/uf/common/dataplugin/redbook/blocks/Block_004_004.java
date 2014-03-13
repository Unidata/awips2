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
package com.raytheon.uf.common.dataplugin.redbook.blocks;

import java.nio.ByteBuffer;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 12, 2008 1131       jkorman     Initial implementation.
 * Apr 29, 2013 1958       bgonzale    Added class RedbookBlockHeader, and
 *                                     nested Factory class.
 * Mar 13, 2014 2907       njensen     split edex.redbook plugin into common and
 *                                     edex redbook plugins
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class Block_004_004 extends RedbookBlock {

    public static class Factory implements RedbookBlockFactory {
        @Override
        public RedbookBlock createBlock(RedbookBlockHeader header,
                ByteBuffer data) {
            return new Block_004_004(header, data);
        }
    }

    /**
     * 
     * @param header
     * @param separator
     */
    public Block_004_004(RedbookBlockHeader header, ByteBuffer data) {
        super(header, data);
        populate(data);
        if (hasChkSum()) {
            data.getShort();
        }
    }

    private void populate(ByteBuffer data) {
        if (hasLength()) {
            dropShortsFromTheBuffer(data);
        }
    }

    /**
     * 
     */
    @Override
    public StringBuilder toString(StringBuilder sb) {
        sb = super.toString(sb);

        return sb;
    }
}
