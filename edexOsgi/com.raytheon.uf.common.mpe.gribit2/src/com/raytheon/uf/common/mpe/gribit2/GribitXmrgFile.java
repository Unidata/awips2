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
package com.raytheon.uf.common.mpe.gribit2;

import java.awt.Rectangle;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.file.Path;

import com.raytheon.uf.common.xmrg.XmrgFile;

/**
 * Extension of {@link XmrgFile} that was created to allow the gribit port to
 * read the data section of the xmrg file the same way that legacy gribit did.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 15, 2016 4619       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class GribitXmrgFile extends XmrgFile {

    public GribitXmrgFile() {
    }

    public GribitXmrgFile(String fileName) {
        super(fileName);
    }

    public GribitXmrgFile(Path path) {
        super(path.toFile());
    }

    @Override
    protected void readData(ByteBuffer byteBuf) throws IOException {
        Rectangle hrapExtent = getHrapExtent();
        data = new short[hrapExtent.width * hrapExtent.height];

        int expectedLen = hrapExtent.width * 2;

        for (int i = 0; i < hrapExtent.height; i++) {
            readDataLength(expectedLen, byteBuf);

            int ib = i * hrapExtent.width;
            int ie = ib + hrapExtent.width - 1;
            for (int j = ib; j <= ie; j++) {
                data[j] = byteBuf.getShort();
            }

            readDataLength(expectedLen, byteBuf);
        }
    }
}