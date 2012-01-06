package com.raytheon.viz.warngen.gui;


public class DurationData {

    public String displayString;

    public int minutes;

    public DurationData(int duration) {
        minutes = duration;
        displayString = formatDuration(duration);
    }

    /**
     * formats a duration as a proper string
     * 
     * @param duration
     *            in minutes
     */
    private static String formatDuration(int minutes) {
        int days = 0;
        int hours = 0;
        String rval = "";

        if (minutes > 24 * 60) {
            days = minutes / (24 * 60);
            minutes = minutes % (24 * 60);
        }

        if (minutes > 120) {
            hours = minutes / 60;
            minutes = minutes % 60;
        }

        if (days != 0) {
            rval += days + (days > 1 ? " days " : " day ");
        }

        if (hours != 0) {
            rval += hours + (hours > 1 ? " hrs " : " hr ");
        }

        if (minutes != 0) {
            rval += minutes + " min ";
        }
        return rval;
    }
}
