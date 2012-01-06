package com.raytheon.viz.warngen.gui;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class BackupData {
    public String site;

    public String office;

    private static final Pattern cwaBackUp = Pattern
            .compile("([A-Z]{3})/([A-Z\\s/-]{1,})");

    public BackupData(String cwa) {
        cwa = cwa.trim();
        String[] parts = cwa.split("/");
        site = parts[0];
        office = parts[1];
        Matcher m = cwaBackUp.matcher(cwa);
        if (m.find()) {
            site = m.group(1);
            office = m.group(2);
        }
    }
}
