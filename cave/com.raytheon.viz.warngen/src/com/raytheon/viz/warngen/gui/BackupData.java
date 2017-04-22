package com.raytheon.viz.warngen.gui;

/**
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 09/18/2014  ASM #15465  Qinglu Lin  Ignore the info after "/" if any.
 *
 * </pre>
 */

public class BackupData {
    public String site;

    public BackupData(String cwa) {
        cwa = cwa.trim();
        if (cwa.contains("/")) {
            String[] parts = cwa.split("/");
            site = parts[0];
        } else {
            site = cwa;
        }
    }
}
