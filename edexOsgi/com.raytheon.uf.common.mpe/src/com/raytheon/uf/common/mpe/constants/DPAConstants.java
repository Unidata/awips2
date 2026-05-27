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
package com.raytheon.uf.common.mpe.constants;

/**
 * DPA Constants. Based on: /rary.ohd.pproc/inc/read_stage1_decoded.h.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 16, 2016 5631       bkowal      Initial creation
 * Oct 04, 2016 4622       skorolev    Added more constants
 *
 * </pre>
 *
 * @author bkowal
 */

public class DPAConstants {

    public static final int NUM_DPA_COLS = 131;

    public static final int NUM_DPA_ROWS = 131;

    public static final int NUM_DPA_ELEMENTS = (NUM_DPA_COLS * NUM_DPA_ROWS);

    public static final float DPA_MISSING = -9999f;

    public static final double MISSING_VALUE = -99.;

    public static final int MISSING_VALUE_INT = -99;

    public static final float PRECIP_OUTSIDE_RADAR = -99f;

    public static final float PRECIP_ZERO = -98f;

    public static final byte BEGIN_DPA = -1; // '0xFF': 1st half of 'OxFFFF'

    public static final short DPA_MARKER = 81; // '0x0051'

    public static final int NO_CLOSER_RECORDS = 31;

    public static final int DPA_HEADER_SHORT_COUNT = 50;

    public static final int DPA_MARKER_IDX = 31;

    public static final int TOP_OF_HOUR = 0;

    public static final int START_OF_DATA_BLOCK = 0;

    protected DPAConstants() {
    }
}