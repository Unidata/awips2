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
package com.raytheon.viz.skewt.mockdata;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.sounding.VerticalSounding;

/**
 * This class implements an Upper Air sounding repository for test and demo
 * purposes. Current data includes soundings for Valley, Nebraska and Jackson,
 * Mississippi for Sept. 22, 2006 at 12Z.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06 Nov 2006             jkorman     Initial Coding
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class UpperAirRepository {
    // private static final int SFC = LayerType.SURFACE.ordinal();

    // private static final int MANP = LayerType.MAN_PRESSURE.ordinal();

    // private static final int SIGP = LayerType.SIG_PRESSURE.ordinal();

    // private static final int SIGW = LayerType.SIG_WIND.ordinal();

    // private static final int TROP = LayerType.TROPOPAUSE.ordinal();

    // private static final int MAXW = LayerType.MAX_WIND.ordinal();

    public static final double INVALID = -9999;

    public static final double MISSING = -9998;

    // No reported value for this type of layer
    public static final double NREPORT = -9997;

    public static final double FENCE = -9900;

    /**
     * Source data for KJAN, 72235 20060922:12Z
     * 
     * Currently the pilot data is not part of the sounding.
     * 
     * USUS44 KJAN 221223 MANJAN 72235 TTAA 72121 72235 99000 24416 18006 00089
     * ///// ///// 92772 21205 20540 85503 17218 22044 70136 09040 23538 50584
     * 07191 24532 40754 183// 25532 30962 35377 25047 25086 43772 24560 20232
     * 54767 24069 15412 65563 25567 10653 71161 24543 88125 71361 25061 77182
     * 25075 41112 51515 10164 00051 10194 20036 22041=
     * 
     * UMUS44 KJAN 221226 SGLJAN 72235 TTBB 72120 72235 00000 24416 11985 23605
     * 22889 20018 33779 13049 44764 12864 55739 10660 66734 10222 77725 09213
     * 88716 09656 99706 09024 11689 08858 22663 06658 33652 06069 44643 05461
     * 55618 02656 66589 00260 77585 00156 88582 00360 99579 00556 11569 01756
     * 22550 03158 33528 04590 44500 07191 55397 18787 66324 31579 77230 48969
     * 88120 71561 99100 71161 31313 05102 81103 41414 66400=
     * 
     * PPBB 72120 72235 90012 18006 19033 20037 90346 21042 21544 22042 90789
     * 22042 22042 22539 91124 24036 24031 23532 916// 23038 9205/ 24535 25532
     * 93035 25031 25053 24559 9425/ 24574 25569 9504/ 25061 25046=
     * 
     */
    // private static final String icaoKJAN = "KJAN";
    // private static final String bsnKJAN = "72235";
    // private static double[][] dataKJAN = {
    // { SFC, 1000, 101, 244, 16, 180, 06, },
    // { SIGP, 985, NREPORT, 236, 5, NREPORT, NREPORT, },
    // { MANP, 925, 772, 212, 5, 205, 40, },
    // { SIGP, 889, NREPORT, 200, 18, NREPORT, NREPORT, },
    // { MANP, 850, 1503, 172, 18, 220, 44, },
    // { SIGP, 779, NREPORT, 130, 49, NREPORT, NREPORT, },
    // { SIGP, 764, NREPORT, 128, 64, NREPORT, NREPORT, },
    // { SIGP, 739, NREPORT, 106, 60, NREPORT, NREPORT, },
    // { SIGP, 734, NREPORT, 102, 22, NREPORT, NREPORT, },
    // { SIGP, 725, NREPORT, 92, 13, NREPORT, NREPORT, },
    // { SIGP, 716, NREPORT, 96, 56, NREPORT, NREPORT, },
    // { SIGP, 706, NREPORT, 90, 24, NREPORT, NREPORT, },
    // { MANP, 700, 3136, 90, 40, 235, 38, },
    // { SIGP, 689, NREPORT, 88, 58, NREPORT, NREPORT, },
    // { SIGP, 663, NREPORT, 66, 58, NREPORT, NREPORT, },
    // { SIGP, 652, NREPORT, 60, 69, NREPORT, NREPORT, },
    // { SIGP, 643, NREPORT, 54, 61, NREPORT, NREPORT, },
    // { SIGP, 618, NREPORT, 26, 56, NREPORT, NREPORT, },
    // { SIGP, 589, NREPORT, 2, 60, NREPORT, NREPORT, },
    // { SIGP, 585, NREPORT, -1, 56, NREPORT, NREPORT, },
    // { SIGP, 582, NREPORT, -3, 60, NREPORT, NREPORT, },
    // { SIGP, 579, NREPORT, -5, 56, NREPORT, NREPORT, },
    // { SIGP, 569, NREPORT, -17, 56, NREPORT, NREPORT, },
    // { SIGP, 550, NREPORT, -31, 58, NREPORT, NREPORT, },
    // { SIGP, 528, NREPORT, -45, 90, NREPORT, NREPORT, },
    // { MANP, 500, 5840, -71, 91, 245, 32, },
    // { MANP, 400, 7540, -183, MISSING, 255, 32, },
    // { SIGP, 397, NREPORT, -187, 87, NREPORT, NREPORT, },
    // { SIGP, 324, NREPORT, -315, 79, NREPORT, NREPORT, },
    // { MANP, 300, 9620, -353, 77, 250, 47, },
    // { MANP, 250, 10860, -437, 72, 245, 60, },
    // { SIGP, 230, NREPORT, -489, 69, NREPORT, NREPORT, },
    // { MANP, 200, 12320, -547, 67, 240, 69, },
    // { MANP, 150, 14120, -655, 63, 255, 67, },
    // { TROP, 125, NREPORT, -713, 61, 250, 61, },
    // { SIGP, 120, NREPORT, -715, 61, NREPORT, NREPORT, },
    // { MANP, 100, 16530, -711, 61, 245, 43, } };
    /**
     * Source data for KOAX, 72558 20060922:12Z
     * 
     * Currently the pilot data is not part of the sounding.
     * 
     * USUS43 KOAX 221214 MANOAX 72558 TTAA 72121 72558 99951 14016 22512 00578
     * ///// ///// 92583 12412 23524 85288 08610 26028 70874 00208 25031 50551
     * 14557 24545 40716 27158 23537 30917 43141 21526 25037 53128 22522 20183
     * 48972 24043 15371 50176 23538 10633 53775 23025 88245 54128 23026 77999
     * 51515 10164 00004 10194 25026 25027=
     * 
     * UMUS43 KOAX 221215 SGLOAX 72558 TTBB 72120 72558 00951 14016 11850 08610
     * 22792 06434 33676 01903 44670 00915 55612 03925 66597 04156 77512 13924
     * 88508 14156 99474 17159 11458 19350 22441 21356 33354 33962 44343 35556
     * 55319 39958 66281 46518 77245 54128 88228 48160 99209 49569 11172 50376
     * 22122 51175 33110 55974 44100 53775 31313 45202 81107 41414 855//=
     * 
     * PPBB 72120 72558 90023 22512 24530 25530 90467 26028 25023 24525 9089/
     * 24527 25029 91246 25538 25045 24543 92056 23544 23534 23533 93013 21526
     * 21525 22018 9357/ 23032 23042 9402/ 24538 23039 9503/ 23025 22523=
     */
    // private static final String icaoKOAX = "KOAX";
    // private static final String bsnKOAX = "72558";
    // private static double[][] dataKOAX = {
    // { SFC, 951, 350, 140, 16, 225, 12, },
    // { MANP, 1000, 578, MISSING, MISSING, MISSING, MISSING, },
    // { SIGP, 951, NREPORT, 140, 16, NREPORT, NREPORT, },
    // { MANP, 925, 583, 124, 12, 235, 24, },
    // { MANP, 850, 1288, 86, 10, 260, 28, },
    // { SIGP, 792, NREPORT, 64, 34, NREPORT, NREPORT, },
    // { MANP, 700, 2874, 2, 8, 250, 31, },
    // { SIGP, 676, NREPORT, -19, 3, NREPORT, NREPORT, },
    // { SIGP, 670, NREPORT, -9, 15, NREPORT, NREPORT, },
    // { SIGP, 612, NREPORT, -39, 25, NREPORT, NREPORT, },
    // { SIGP, 597, NREPORT, -41, 56, NREPORT, NREPORT, },
    // { SIGP, 512, NREPORT, -139, 24, NREPORT, NREPORT, },
    // { SIGP, 508, NREPORT, -141, 56, NREPORT, NREPORT, },
    // { MANP, 500, 5510, -145, 57, 245, 45, },
    // { SIGP, 474, NREPORT, -171, 59, NREPORT, NREPORT, },
    // { SIGP, 458, NREPORT, -193, 50, NREPORT, NREPORT, },
    // { SIGP, 441, NREPORT, -213, 56, NREPORT, NREPORT, },
    // { MANP, 400, 7160, -271, 58, 235, 37, },
    // { SIGP, 354, NREPORT, -339, 62, NREPORT, NREPORT, },
    // { SIGP, 343, NREPORT, -355, 56, NREPORT, NREPORT, },
    // { SIGP, 319, NREPORT, -399, 58, NREPORT, NREPORT, },
    // { MANP, 300, 9170, -431, 41, 215, 26, },
    // { SIGP, 281, NREPORT, -465, 18, NREPORT, NREPORT, },
    // { MANP, 250, 10370, -531, 28, 225, 22, },
    // { TROP, 245, NREPORT, -541, 28, 230, 26, },
    // { SIGP, 228, NREPORT, -481, 60, NREPORT, NREPORT, },
    // { SIGP, 209, NREPORT, -495, 69, NREPORT, NREPORT, },
    // { MANP, 200, 11830, -489, 72, 240, 43, },
    // { SIGP, 172, NREPORT, -503, 76, NREPORT, NREPORT, },
    // { MANP, 150, 13710, -501, 76, 235, 38, },
    // { SIGP, 122, NREPORT, -511, 75, NREPORT, NREPORT, },
    // { SIGP, 110, NREPORT, -559, 74, NREPORT, NREPORT, },
    // { MANP, 100, 16330, -537, 75, 230, 25, }, };
    private static Map<String, VerticalSounding> soundings = null;

    /**
     * Retrieve a VerticalSounding instance corresponding to the specified
     * station id. Returns a null reference if no data is available to the
     * requested data.
     * 
     * @param stationId
     *            A station identifier. The identifier may be either an ICAO
     *            (KJAN) or WMO Block Station Number (72235).
     * @return The VerticalSounding instance, or null if not found.
     */
    public static VerticalSounding getSounding(String stationId) {
        if (soundings == null) {
            createSoundingData();
        }

        VerticalSounding sounding = null;

        if (soundings.containsKey(stationId)) {
            sounding = soundings.get(stationId);
        }
        return sounding;
    }

    /**
     * Create the mock sounding data.
     */
    private static void createSoundingData() {
        VerticalSounding s = null;
        soundings = new HashMap<String, VerticalSounding>();

        s = new VerticalSounding();

        s.setStationId("72558");
        // s = new VerticalSounding(icaoKJAN, dataKJAN, Calendar.getInstance()
        // .getTime(), 32.31972, -90.0775);
        // soundings.put(bsnKJAN, s);
        // soundings.put(icaoKJAN, s);

        // s = new VerticalSounding(icaoKOAX, dataKOAX, Calendar.getInstance()
        // .getTime(), 41.3166, -96.366);
        // soundings.put(bsnKOAX, s);
        // soundings.put(icaoKOAX, s);
    }
}
