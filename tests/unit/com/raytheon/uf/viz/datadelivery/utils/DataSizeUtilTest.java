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
package com.raytheon.uf.viz.datadelivery.utils;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

import org.junit.Test;

/**
 * Test {@link DataSizeUtil}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 8, 2013   1420      mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class DataSizeUtilTest {
    private static long CONV = 1024;

    @Test
    public void testConvertOneKbInBytesToBytes() {
        long result = DataSizeUtil.BYTE.toByte(1024l);

        assertThat(result, is(equalTo(1024l)));
    }

    @Test
    public void testConvertOneKbInBytesToKB() {
        long bytes = 1024;
        long expected = 1;
        long result = DataSizeUtil.BYTE.toKB(bytes);

        assertThat(result, is(equalTo(expected)));
    }

    @Test
    public void testConvertOneMbInBytesToMB() {
        long bytes = CONV * CONV * 2;
        long expected = 2;
        long result = DataSizeUtil.BYTE.toMB(bytes);

        assertThat(result, is(equalTo(expected)));
    }

    @Test
    public void testConvertTenBytesToMB() {
        long bytes = 10;
        long expected = 0;
        long result = DataSizeUtil.BYTE.toMB(bytes);

        assertThat(result, is(equalTo(expected)));
    }

    @Test
    public void testConvertTwoGbInBytesToGB() {
        long bytes = CONV * CONV * CONV * 2;
        long expected = 2;
        long result = DataSizeUtil.BYTE.toGB(bytes);

        assertThat(result, is(equalTo(expected)));
    }

    @Test
    public void testConvertTenBytesToGB() {
        long bytes = 10;
        long expected = 0;
        long result = DataSizeUtil.BYTE.toGB(bytes);

        assertThat(result, is(equalTo(expected)));
    }

    @Test
    public void testConvertMethodOneKbToByte() {
        long kb = 1;
        long expected = 1024;
        long result = DataSizeUtil.BYTE.convert(kb, DataSizeUtil.KB);

        assertThat(result, is(equalTo(expected)));
    }

    // //////////////////////////////////////////////////////////

    @Test
    public void testConvertOneKBToBytes() {
        long kb = 1;
        long expected = 1024;
        long result = DataSizeUtil.KB.toByte(kb);

        assertThat(result, is(equalTo(expected)));
    }

    @Test
    public void testConvertOneKbToKB() {
        long kb = 1;
        long expected = 1;
        long result = DataSizeUtil.KB.toKB(kb);

        assertThat(result, is(equalTo(expected)));
    }

    @Test
    public void testConvertOneKbToMB() {
        long kb = 1;
        long expected = 0;
        long result = DataSizeUtil.KB.toMB(kb);

        assertThat(result, is(equalTo(expected)));
    }

    @Test
    public void testConvertTwoMbInKbToMB() {
        long kb = CONV * 2;
        long expected = 2;
        long result = DataSizeUtil.KB.toMB(kb);

        assertThat(result, is(equalTo(expected)));
    }

    @Test
    public void testConvertOneKbToGB() {
        long kb = 1;
        long expected = 0;
        long result = DataSizeUtil.KB.toGB(kb);

        assertThat(result, is(equalTo(expected)));
    }

    @Test
    public void testConvertTwoGbInKbToGB() {
        long kb = CONV * CONV * 2;
        long expected = 2;
        long result = DataSizeUtil.KB.toGB(kb);

        assertThat(result, is(equalTo(expected)));
    }

    @Test
    public void testConvertMethodOneMbToKB() {
        long mb = 1;
        long expected = 1024;
        long result = DataSizeUtil.KB.convert(mb, DataSizeUtil.MB);

        assertThat(result, is(equalTo(expected)));
    }

    // ///////////////////////

    @Test
    public void testConvertOneMbToBytes() {
        long mb = 1;
        long expected = 1024 * 1024;
        long result = DataSizeUtil.MB.toByte(mb);

        assertThat(result, is(equalTo(expected)));
    }

    @Test
    public void testConvertOneMbToKB() {
        long mb = 1;
        long expected = 1024;
        long result = DataSizeUtil.MB.toKB(mb);

        assertThat(result, is(equalTo(expected)));
    }

    @Test
    public void testConvertTwoMbToMB() {
        long mb = 2;
        long expected = 2;
        long result = DataSizeUtil.MB.toMB(mb);

        assertThat(result, is(equalTo(expected)));
    }

    @Test
    public void testConvertTwoMbToGB() {
        long mb = 2;
        long expected = 0;
        long result = DataSizeUtil.MB.toGB(mb);

        assertThat(result, is(equalTo(expected)));
    }

    @Test
    public void testConvertTwoGbInMbToGB() {
        long mb = 1024 * 2;
        long expected = 2;
        long result = DataSizeUtil.MB.toGB(mb);

        assertThat(result, is(equalTo(expected)));
    }

    @Test
    public void testConvertMethodOneGbToMB() {
        long gb = 1;
        long expected = 1024;
        long result = DataSizeUtil.MB.convert(gb, DataSizeUtil.GB);

        assertThat(result, is(equalTo(expected)));
    }

    // ///////////////////////

    @Test
    public void testConvertOneGbToBytes() {
        long gb = 1;
        long expected = 1024 * 1024 * 1024;
        long result = DataSizeUtil.GB.toByte(gb);

        assertThat(result, is(equalTo(expected)));
    }

    @Test
    public void testConvertOneGbToKB() {
        long gb = 1;
        long expected = 1024 * 1024;
        long result = DataSizeUtil.GB.toKB(gb);

        assertThat(result, is(equalTo(expected)));
    }

    @Test
    public void testConvertOneGbToMB() {
        long gb = 1;
        long expected = 1024;
        long result = DataSizeUtil.GB.toMB(gb);

        assertThat(result, is(equalTo(expected)));
    }

    @Test
    public void testConvertOneGbToGB() {
        long gb = 1;
        long expected = 1;
        long result = DataSizeUtil.GB.toGB(gb);

        assertThat(result, is(equalTo(expected)));
    }

    @Test
    public void testConvertMethodTwoGbToGB() {
        long gb = 2;
        long expected = 2;
        long result = DataSizeUtil.GB.convert(gb, DataSizeUtil.GB);

        assertThat(result, is(equalTo(expected)));
    }

    @Test
    public void testGetUnitByte() {
        String expected = "Byte";
        String result = DataSizeUtil.BYTE.getUnit();

        assertThat(result, is(equalTo(expected)));
    }

    @Test
    public void testGetUnitKB() {
        String expected = "KB";
        String result = DataSizeUtil.KB.getUnit();

        assertThat(result, is(equalTo(expected)));
    }

    @Test
    public void testGetUnitMB() {
        String expected = "MB";
        String result = DataSizeUtil.MB.getUnit();

        assertThat(result, is(equalTo(expected)));
    }

    @Test
    public void testGetUnitGB() {
        String expected = "GB";
        String result = DataSizeUtil.GB.getUnit();

        assertThat(result, is(equalTo(expected)));
    }
}
