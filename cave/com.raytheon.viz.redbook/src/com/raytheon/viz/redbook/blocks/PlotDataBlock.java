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

import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.redbook.blocks.RedbookBlockHeader;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Implements the redbook plot data block
 * 
 * NOTE: it appears sometimes this block is used for ascii instead of the alpha
 * numeric block
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * May 22, 2008 1162        chammack    Initial creation
 * Apr 29, 2013 1958        bgonzale    New class RedbookBlockHeader.
 * Mar 13, 2014 2907        njensen     split edex.redbook plugin into common
 *                                      and edex redbook plugins
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class PlotDataBlock extends AbstractTextBlock {
    private TextBlock textBlock;

    public PlotDataBlock(RedbookBlockHeader header, ByteBuffer data,
            MathTransform mt, int maxX, int maxY) {
        super(header, data);

        /* int mode = ( */data.get() /* & 0xFF) */;

        int code = (data.get() & 0xFF);

        int length = getLength();

        int posX = (data.getShort() & 0xFFFF);
        int posY = maxY - (data.getShort() & 0xFFFF);

        origXPos = posX;
        origYPos = posY;

        int consumed = 10;
        StringBuffer sb = new StringBuffer();
        while (consumed < (length * 2)) {

            int i1 = data.get();
            char c1 = (char) i1;
            sb.append(c1);
            consumed++;
        }

        // Just read over the checksum for now.
        if (hasChkSum()) {
            data.getShort();
        }

        if (code == 0) {
            // Alphanumeric

            this.textBlock = new TextBlock();
            this.textBlock.text = sb.toString();
            double[] out = new double[2];
            try {
                mt.transform(new double[] { posX, posY }, 0, out, 0, 1);
            } catch (TransformException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
            this.textBlock.location = new Coordinate(out[0], out[1]);
            this.textBlock.offset = new int[] { 0, 0 };
        } else {
            // TODO: Handle code > 0
        }
    }

    /**
     * @return the textBlock
     */
    @Override
    public TextBlock getTextBlock() {
        return this.textBlock;
    }
}
