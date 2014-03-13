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
package com.raytheon.viz.redbook.blocks;

import java.nio.ByteBuffer;

import com.raytheon.uf.common.dataplugin.redbook.blocks.RedbookBlock;
import com.raytheon.uf.common.dataplugin.redbook.blocks.RedbookBlockHeader;
import com.raytheon.viz.redbook.rsc.RedbookLegend;
import com.raytheon.viz.redbook.rsc.RedbookLegend.Type;

/**
 * Base class for redbook text blocks
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Mar 18, 2010 3260        dfriedma    Initial creation
 * Apr 29, 2013 1958        bgonzale    New class RedbookBlockHeader.
 * Mar 13, 2014 2907        njensen     split edex.redbook plugin into common
 *                                      and edex redbook plugins
 * 
 * </pre>
 * 
 * @author dfriedma
 * @version 1.0
 */

public abstract class AbstractTextBlock extends RedbookBlock {

    protected int origXPos;

    protected int origYPos;

    public AbstractTextBlock(RedbookBlockHeader header, ByteBuffer data) {
        super(header, data);
    }

    public abstract TextBlock getTextBlock();

    public TextBlock determineTextBlockType(RedbookLegend legend) {
        TextBlock textBlock = getTextBlock();
        RedbookLegend.Type type = legend.isLegend(textBlock.text, origXPos,
                origYPos);
        textBlock.isLegend = type == Type.LEGEND;
        if (type == Type.GRAPHIC)
            legend.addCoordinate(origXPos, origYPos);
        return type != Type.IGNORE ? textBlock : null;
    }
}
