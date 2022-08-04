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

/**
 * Implements the redbook plot parameter block
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------	----------	-----------	--------------------------
 * May 22, 2008 1162        chammack    Initial creation
 * Apr 29, 2013 1958        bgonzale    New class RedbookBlockHeader
 * Mar 13, 2014	2907      	njensen    	split edex.redbook plugin into common and
 *                                      edex redbook plugins
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class PlotParametersBlock extends RedbookBlock {

    private int color;

    private int bgColor;

    private int lineChar;

    private int logicalFill;

    private int fillPattern;

    private int lineWidth;

    public PlotParametersBlock(RedbookBlockHeader header, ByteBuffer data) {

        super(header, data);

        // Set up some reasonable defaults
        this.lineWidth = 1;
        this.lineChar = 0;
        this.bgColor = 0;
        this.color = 0;
        this.logicalFill = 0;
        this.fillPattern = 0;

        int length = getLength();

        // Ignore zoom
        data.getShort();

        // This appears to be a deviation from the spec, that the block is
        // legally terminated early at this point
        if (length == 3)
            return;

        this.color = (data.get() & 0xFF);
        System.out.println("Color: " + color);

        this.bgColor = (data.get() & 0xFF);
        System.out.println("Background Color: " + bgColor);

        this.lineChar = (data.get() & 0xFF);
        switch (this.lineChar) {
        case 0:
            System.out.println("LineChar: continuous");
            break;
        case 1:
            System.out.println("LineChar: dotted");
            break;
        case 2:
            System.out.println("LineChar: short dashed");
            break;
        case 3:
            System.out.println("LineChar: long dashed");
            break;
        case 4:
            System.out.println("LineChar: sparse dotted");
            break;
        case 5:
            System.out.println("LineChar: symbolic");
            break;
        }

        this.lineWidth = (data.get() & 0xFF);

        // This appears to be a deviation from the spec, that the block is
        // legally terminated early at this point
        if (length == 5)
            return;

        data.getShort(); // eat char line 1
        data.getShort(); // eat char line 2

        this.logicalFill = (data.get() & 0xFF);
        System.out.println("Logical fill: " + logicalFill);

        this.fillPattern = (data.get() & 0xFF);
        System.out.println("Fill pattern: " + fillPattern);

        if (this.hasChkSum())
            data.getShort();
    }

    /**
     * @return the color
     */
    public int getColor() {
        return color;
    }

    /**
     * @return the bgColor
     */
    public int getBgColor() {
        return bgColor;
    }

    /**
     * @return the lineChar
     */
    public int getLineChar() {
        return lineChar;
    }

    /**
     * @return the logicalFill
     */
    public int getLogicalFill() {
        return logicalFill;
    }

    /**
     * @return the fillPattern
     */
    public int getFillPattern() {
        return fillPattern;
    }

    /**
     * @return the lineWidth
     */
    public int getLineWidth() {
        return lineWidth;
    }

}
