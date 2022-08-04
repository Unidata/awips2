/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract EA133W-17-CQ-0082 with the US Government.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 *
 * Contractor Name:        Raytheon Company
 * Contractor Address:     2120 South 72nd Street, Suite 900
 *                         Omaha, NE 68124
 *                         402.291.0100
 *
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.edex.plugin.vaa.decoder;

import java.io.IOException;
import java.util.Iterator;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.dataplugin.exception.MalformedDataException;
import com.raytheon.uf.common.dataplugin.vaa.VAARecord;

/**
 * VAAParser test.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 7, 2021  8608       mapeters    Initial creation (extracted from VAAParser)
 *
 * </pre>
 *
 * @author mapeters
 */
// Allow System.out/err usage in tests
@SuppressWarnings("squid:S106")
public class VAAParserTest {

    /**
     *
     * @param args
     * @throws MalformedDataException
     * @throws IOException
     */
    public static final void main(String[] args)
            throws MalformedDataException, IOException {

        String msg1 = "\u0001\r\r\n738\r\r\nFVXX20 KNES 041708 CAA"
                + "\r\r\nVA ADVISORY" + "\r\r\nDTG: 20091104/1708Z"
                + "\r\r\nVAAC: WASHINGTON" + "\r\r\nVOLCANO:"
                + "\r\r\nSOUFRIERE HILLS 1600-05" + "\r\r\nPSN: N1642 W06210"
                + "\r\r\nAREA: W_INDIES" + "\r\r\nSUMMIT ELEV: 3002 FT (915 M)"
                + "\r\r\nADVISORY NR: 2009/146"
                + "\r\r\nINFO SOURCE: GOES-12. GFS WINDS."
                + "\r\r\nERUPTION DETAILS: CONTINUOUS EMISSIONS"
                + "\r\r\nOBS VA DTG: 04/1645Z" + "\r\r\nOBS VA CLD:"
                + "\r\r\nSFC/FL100 42NM WID LINE BTN N1638"
                + "\r\r\nW06611 - N1643 W06214. MOV W 7KT"
                + "\r\r\nFCST VA CLD +6HR: 04/2300Z SFC/FL100 40NM WID"
                + "\r\r\nLINE BTN N1640 W06614 - N1644 W06214."
                + "\r\r\nFCST VA CLD +12HR: 05/0500Z SFC/FL100 40NM WID"
                + "\r\r\nLINE BTN N1638 W06614 - N1643 W06214. SFC/FL100"
                + "\r\r\n40NM WID LINE BTN N1641 W06616 - N1643 W06214."
                + "\r\r\nFCST VA CLD +18HR: 05/1100Z"
                + "\r\r\nRMK: A SPREADING 42 NMI WIDE ASH PLUME MOVING AT"
                + "\r\r\nA MEASURED 7 KTS EXTENDS AT LEAST 211 NMI TO THE"
                + "\r\r\nWEST OF THE VOLCANO, OR TO ABOUT 66W.  NO"
                + "\r\r\nSIGNIFICANT CHANGE IN DIRECTION OR SPEED IS"
                + "\r\r\nANTICIPATED DURING THE NEXT 12 HOURS. ...BALDWIN"
                + "\r\r\nNXT ADVISORY: WILL BE ISSUED BY 20091104/2315Z"
                + "\r\r\n\u0003";
        Headers headers = new Headers();
        headers.put("ingestFileName", "FVXX20.20110106");
        VAAParser p = new VAAParser(msg1.getBytes(), "TEST01", headers);
        Iterator<VAARecord> it = p.iterator();
        while (it.hasNext()) {
            VAARecord r = it.next();
            System.out.println(r);
            System.out.println(r.getMessage());
        }
    }
}
