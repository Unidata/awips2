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
package com.raytheon.uf.viz.monitor.ffmp.ui.dialogs;


/**
 * Common enums used by the Basin Trend graphing feature.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 05  2017 DR 14336   lshi         FFMP VGB value differences between A1 and A2
 *                                      changed "vgb" label to "gauge"
 * Feb 21, 2019 #7657      dgilling     Remove unused field yCoordHours field
 *                                      from TimeDuration.
 *
 * </pre>
 *
 * @author lshi
 */
public class BasinTrendCommon {

    private BasinTrendCommon() {
        throw new AssertionError();
    }

    public static enum PlotItems {
        RATE("rate"), QPE("qpe"), QPF("qpf"), QPFSCAN("QPFSCAN"), GUID(
                "guid"), RFCFFG("RFCFFG"), VGB("gauge");

        private final String itemName;

        PlotItems(String name) {
            itemName = name;
        }

        public String getItemName() {
            return itemName;
        }
    }

    public static enum Underlays {
        RATE("rate"), QPE("qpe"), RATIO("ratio"), DIFF("diff");

        private final String itemName;

        Underlays(String name) {
            itemName = name;
        }

        public String getUnderlayName() {
            return itemName;
        }
    }

    public static enum TimeDuration {
        ALL("All hr.", -1), HR_1("1 hr.", 1), HR_3("3 hr.", 3), HR_6("6 hr.",
                6), HR_12("12 hr.", 12), HR_24("24 hr.", 24);

        private final String timeDurName;

        private final int hours;

        TimeDuration(String name, int hrs) {
            this.timeDurName = name;
            this.hours = hrs;
        }

        public String getTimeDurName() {
            return timeDurName;
        }

        public int getHours() {
            return hours;
        }
    }
}
