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

/**
 * 
 * RedbookBlock header mode and submode. Also contains generated key based on
 * mode and submode.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 25 APR 2013             bgonzale    Initial Coding.
 * Mar 13, 2014 2907       njensen     split edex.redbook plugin into common and
 *                                     edex redbook plugins
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */
public class RedbookBlockHeader {
    private static final String DEFAULT_KEY = "default";

    private static final String MODE_KEY_FMT = "%03d_%03d";

    public static final RedbookBlockHeader DEFAULT = new RedbookBlockHeader();

    public final int hdr;

    public final int mode;

    public final int subMode;

    public final String blockFactoryKey;

    /**
     * @param hdr
     * @param mode
     * @param subMode
     * @param blockKey
     */
    public RedbookBlockHeader(short hdrRaw, byte modeRaw, byte subModeRaw) {
        super();
        this.hdr = (hdrRaw & 0xFFFF);
        this.mode = (modeRaw & 0xFF);
        this.subMode = (subModeRaw & 0xFF);
        this.blockFactoryKey = String.format(MODE_KEY_FMT, mode, subMode);
    }

    private RedbookBlockHeader() {
        super();
        this.hdr = 0;
        this.mode = 0;
        this.subMode = 0;
        this.blockFactoryKey = DEFAULT_KEY;
    }

    public static boolean isDefaultBlockId(String blockId) {
        return DEFAULT_KEY.equals(blockId);
    }

    public boolean isProductId() {
        return (mode == 1) && (subMode == 1);
    }

    public boolean isUpperAirPlot() {
        return (mode == 2) && (subMode == 3);
    }
}