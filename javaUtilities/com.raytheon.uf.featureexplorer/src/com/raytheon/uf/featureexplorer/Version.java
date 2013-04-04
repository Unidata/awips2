package com.raytheon.uf.featureexplorer;

/**
 * This class represents a version number that is separated by periods(.). The
 * length doesn't matter because the array sizes based on a string split. This
 * is also capable of comparing two version numbers of any length and
 * determining which is greater. TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 3, 2008             dglazesk    Initial creation
 * </pre>
 * 
 * @author dglazesk
 * @version 1.0
 */
public class Version implements Comparable<Version> {

    /**
     * This is meant to hold the string version as passed in to the constructor.
     */
    private String versionString;

    /**
     * This holds the split up version of the string version with all split
     * parts interpreted as ints.
     */
    private int[] intVersion;

    /**
     * Create a version and set the version string as version. This also sets up
     * the intVersion for comparison purposes.
     * 
     * @param version
     *            The string for the version number
     */
    public Version(String version) {
        versionString = version;

        String[] nums = version.split("[.]");
        intVersion = new int[nums.length];
        for (int i = 0; i < nums.length; ++i) {
            try {
                intVersion[i] = Integer.parseInt(nums[i]);
            } catch (NumberFormatException e) {
                intVersion[i] = 0;
            }
        }
    }

    /**
     * Get the integer array version of the version number. Good for comparison.
     * 
     * @return The integer array for the version as parsed in constructor
     */
    public int[] getIntVersion() {
        return intVersion;
    }

    /**
     * Returns the passed in version string.
     * 
     * @return The string version as passed in to the constructor
     */
    public String toString() {
        return versionString;
    }

    /**
     * Comparing function for the comparable interface. Returns -1 if the other
     * version number is greater. If the version numbers differ in the number of
     * integers, zeros will be added to the number that is shorter.
     * 
     * @param otherVersion
     *            The version object we are comparing against.
     * @return -1 if otherVersion is bigger, 1 if this is bigger, and 0 if equal
     */
    @Override
    public int compareTo(Version otherVersion) {
        int[] otherIntVersion = otherVersion.getIntVersion();

        // set the limit for the loop as bigger length
        int limit = intVersion.length;
        if (otherIntVersion.length > intVersion.length)
            limit = otherIntVersion.length;

        int me = 0;
        int you = 0;
        for (int i = 0; i < limit; ++i) {
            // me looking at you looking at me
            me = you = me = 0;

            // me still have ints
            if (intVersion.length > i)
                me = intVersion[i];
            // you still have ints
            if (otherIntVersion.length > i)
                you = otherIntVersion[i];

            // first difference in number tells us the greater one
            if (me > you)
                return 1;
            else if (me < you)
                return -1;
        }

        return 0;
    }
}
