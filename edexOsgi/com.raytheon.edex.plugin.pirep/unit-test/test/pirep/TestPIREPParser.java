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
package test.pirep;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.junit.Test;

import com.raytheon.edex.plugin.pirep.decoder.PirepTools;
import com.raytheon.uf.edex.decodertools.core.BasePoint;

import static org.junit.Assert.*;


/**
 * Extracted methods tests from PirepParser.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 10, 2012            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public class TestPIREPParser {

    private static final String LATLON_PTRN = "((([0-8]\\d[0-5]\\d)|(9000))[NS] ((0\\d{2}[0-5]\\d)|([1][0-7]\\d[0-5]\\d)|(18000))[EW])";
    
    private static BasePoint parseLatLon(String latlon) {
        BasePoint point = null;

        // 012345678901
        // lldds llldds

        Integer lat_dd = PirepTools.parseInteger(latlon.substring(0, 2));
        Integer lat_mm = PirepTools.parseInteger(latlon.substring(2, 4));
        Integer lon_dd = PirepTools.parseInteger(latlon.substring(6, 9));
        Integer lon_mm = PirepTools.parseInteger(latlon.substring(9, 11));

        if ((lat_dd != null) && (lat_mm) != null) {
            if ((lon_dd != null) && (lon_mm) != null) {

                Double lat = lat_dd + (lat_mm / 60.0d);
                Double lon = lon_dd + (lon_mm / 60.0d);
                if (lat_dd.equals(0) && (lat_mm.equals(0))) {
                    lat = 0.0;
                } else {
                    switch (latlon.charAt(4)) {
                    case 'N': {
                        break;
                    }
                    case 'S': {
                        lat = lat * -1;
                        break;
                    }
                    default: {
                        lat = null;
                    }
                    }
                }
                if (lon_dd.equals(0) && (lon_mm.equals(0))) {
                    lon = 0.0;
                } else {
                    switch (latlon.charAt(11)) {
                    case 'E': {
                        break;
                    }
                    case 'W': {
                        lon = lon * -1;
                        break;
                    }
                    default: {
                        lon = null;
                    }
                    }
                }
                if (lat != null && lon != null) {
                    point = new BasePoint(lat, lon);
                }
            }
        }
        return point;
    }

    @Test
    public void testparseLatLon() {
        String[] latlons = { "0000N 00000W", "0000S 00000E", "9000S 00000W",
                "9000N 00000W", "0000N 09000W", "9000S 09000W", "9000N 09000W",

                "0000N 09000W", "4500S 09000W", "9000N 09000W",

                "9000N 09959W", "0000N 10000W",

                "4500S 09000W", "9000N 09000W",

                "9000N 18000E", "9000S 18000E", "9000N 18000W", "9000S 18000W",
                "9000N 17959W", "9000S 17959W",
        };

        Pattern p = Pattern.compile(LATLON_PTRN);

        for (String s : latlons) {
            Matcher m = p.matcher(s);
            if (m.find()) {
                BasePoint b = parseLatLon(m.group());
                if (b != null) {
                    System.out.println(String.format("%16s %10.6f %11.6f", s,
                            b.getLatitude(), b.getLongitude()));
                } else {
                    fail("Invalid parse " + s);
                }
            } else {
                fail("no match for " + s);
            }
        }
    }

    @Test
    public void testBearingDistancePattern() {
        final String bearingDistPattern = "^([A-Z,0-9]{3,4})\\s*(\\d{3})(\\d{3})$";

        String str = "123 123  123 \r SCT";

        str = str.replaceAll("[\r\n]", " ");
        str = str.replaceAll(" {2,}", " ");

        System.out.println("[" + str + "]");

        Pattern p = Pattern.compile(bearingDistPattern);
        Matcher m = p.matcher("OMA 080056");
        if(m.find()) {
            System.out.println(m.group(1));
            System.out.println(m.group(2) + " " + m.group(3));
        }
        m = p.matcher("OMA080056");
        if(m.find()) {
            System.out.println(m.group(1));
            System.out.println(m.group(2) + " " + m.group(3));
        }
        
    }
}
