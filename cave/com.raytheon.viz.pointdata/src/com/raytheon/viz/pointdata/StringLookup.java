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
package com.raytheon.viz.pointdata;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.TreeMap;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;

public class StringLookup {

    private static final String plotmodelDir = "plotModels";

    private final String filename;

    private final HashMap<String, String> lookup;

    private final TreeMap<Float, String> rangeLookup;

    private float firstValue = -9999;

    private float secondValue = -9999;

    private String defLookup;

    private String splitValue;

    private int splitValueIndex = -1;

    protected StringLookup(String filename) {
        this.filename = filename;
        this.lookup = new HashMap<String, String>();
        this.rangeLookup = new TreeMap<Float, String>();
    }

    public String getFilename() {
        return this.filename;
    }

    public String recursiveTranslation(String field) {
        /*
         * if(splitValue != null && field != null){ String[] tempFields =
         * field.split(splitValue); if(tempFields.length == 2){ field =
         * tempFields[this.splitValueIndex]; } }
         * if(this.lookup.containsKey(field)){ return this.lookup.get(field); }
         * else if(this.defLookup != null){ return this.defLookup; } else{
         * return null; }
         */

        // Initialize.
        int j, k, c, n, m;
        // int pos;
        String txtstr;
        // char cp = 0;
        // char cp0;

        // Empty input, empty output.
        if (field == null || field.length() == 0 || field.equals("-9999")) {
            if (this.defLookup != null) {
                return this.defLookup;
            }
            return null;
        }

        // Simple case of being able to translate the whole string.
        if (this.lookup.containsKey(field)) {
            return this.lookup.get(field);
        }

        // If required, perform an edit operation where we replace everthing
        // from the substring _left to the beginning of the string with the
        // string _leftTrans.
        n = field.length();
        if (splitValue != null && field != null) {
            String[] tempFields = field.split(splitValue);
            if (tempFields.length == 2) {
                field = tempFields[this.splitValueIndex];
                n = field.length();
            }
        }

        // Try to translate the whole thing again.
        if (this.lookup.containsKey(field)) {
            return this.lookup.get(field);
        }

        // Initialize the output with an invisible string the same length as
        // the input, loop through each possible substring size.
        int nLeft = n; // number of untranslated characters
        boolean none = true; // true if nothing has been translated.
        String output = this.invisibleString(n);
        for (m = n - 1; m >= 1; m--) {

            // If we have enough untranslated characters to translate a
            // substring
            // of this size, loop through each possible substring.
            if (nLeft < m || m > n - 1)
                continue;
            for (c = 0; c <= n - m; c++) {

                // If a translation for this substring is not found, go on.
                if (!this.lookup.containsKey(this.mid(field, c, m))) {
                    continue;
                } else {
                    txtstr = this.lookup.get(this.mid(field, c, m));
                }

                // Replace input substring with unprintable characters, put
                // the translation in the output string.
                j = txtstr.length();
                k = n - (c + m);
                field = this.left(field, c) + this.invisibleString(j)
                        + this.right(field, k);
                output = this.left(output, c) + txtstr + this.right(output, k);

                // Update next substring position, number untranslated and total
                // string length.
                c += j - 1;
                nLeft -= m;
                none = false;
                n = field.length();
                if (nLeft < m)
                    break;
            }
        }

        // Make anything that is all unprintable an empty string.
        if (nLeft == 0)
            field = "";
        else if (none)
            return defLookup;
        return output;
    }

    public String determineRange(String field) {
        String retVal = field;

        if (retVal != null) {
            try {
                float value = Float.parseFloat(field);
                if (value < this.firstValue) {
                    retVal = rangeLookup.ceilingEntry(value).getValue();
                } else if (value <= this.secondValue) {
                    retVal = rangeLookup.ceilingEntry(value).getValue();
                }
            } catch (Exception e) {
                // use default value
                if (this.defLookup != null) {
                    retVal = this.defLookup;
                }
            }
        }
        if (retVal == null) {
            if (this.defLookup != null) {
                retVal = this.defLookup;
            } else {
                retVal = " ";
            }
        }
        if (retVal.contains("~")) {
            retVal = retVal.replace('~', ' ');
        }
        return retVal;
    }

    private String mid(String stringToParse, int start, int length) {
        int lStart = start;
        int lLength = length;
        if (lStart >= stringToParse.length()) {
            lStart = stringToParse.length();
        }
        lLength = Math.min(stringToParse.length() - lStart, lLength);
        String returnString = null;
        try {
            returnString = stringToParse.substring(lStart, lStart + lLength);
        } catch (Exception e) {
            System.out.println(start + " " + lStart + " " + length + " "
                    + lLength + " " + stringToParse);
        }
        return returnString;

    }

    private String left(String stringToParse, int length) {
        if (length >= stringToParse.length()) {
            return stringToParse;
        } else {
            return stringToParse.substring(0, length);
        }
    }

    private String right(String stringToParse, int length) {
        if (length >= stringToParse.length()) {
            return stringToParse;
        } else {
            return stringToParse.substring(stringToParse.length() - length,
                    stringToParse.length());
        }
    }

    public void setLookupEntry(String field, String value) {
        this.lookup.put(field, value);
    }

    public void setRangeEntry(float field, String value) {
        this.rangeLookup.put(field, value);
    }

    public void setDefaultValue(String value) {
        this.defLookup = value;
    }

    public void setSplitValueIndex(int value) {
        this.splitValueIndex = value;
    }

    public void setSplitValue(String value) {
        this.splitValue = value;
    }

    public static StringLookup readS2SFile(String s2nFilename) {
        BufferedReader input = null;
        StringLookup lookup = new StringLookup(s2nFilename);
        File s2nFile = PathManagerFactory.getPathManager().getStaticFile(
                StringLookup.plotmodelDir + IPathManager.SEPARATOR
                        + s2nFilename);
        try {
            input = new BufferedReader(new FileReader(s2nFile));
            String line = null;
            while ((line = input.readLine()) != null) {
                if (line.matches("s2s")) {
                    continue;
                } else if (line.contains("left") || line.contains("right")) {
                    if (line.contains("left")) {
                        lookup.setSplitValueIndex(1);
                    } else if (line.contains("right")) {
                        lookup.setSplitValueIndex(0);
                    }
                    String[] splitValue = line.split("\\s");
                    lookup.setSplitValue("\\s*" + splitValue[1] + "\\s*");
                    continue;
                }
                String[] lookupValues = null;
                if (line.contains(":")) {
                    lookupValues = line.split("\\s*:\\s*");
                } else {
                    lookupValues = line.split("\\s");
                }
                if (lookupValues != null && lookupValues.length == 2) {
                    if (lookupValues[0].matches("default")) {
                        lookup.setDefaultValue(lookupValues[1]);
                    } else {
                        lookup.setLookupEntry(lookupValues[0], lookupValues[1]);
                    }
                } else if (lookupValues != null && lookupValues.length == 1) {
                    if (lookupValues[0].matches("default")) {
                        lookup.setDefaultValue(" ");
                    } else {
                        lookup.setLookupEntry(lookupValues[0], " ");
                    }
                }
            }
            input.close();
        } catch (FileNotFoundException ex) {
            ex.printStackTrace();
            return null;
        } catch (IOException ex) {
            ex.printStackTrace();
            return null;
        }
        return lookup;
    }

    public static StringLookup readR2SFile(String r2nFilename) {
        BufferedReader input = null;
        StringLookup lookup = new StringLookup(r2nFilename);
        File r2nFile = PathManagerFactory.getPathManager().getStaticFile(
                StringLookup.plotmodelDir + IPathManager.SEPARATOR
                        + r2nFilename);
        try {
            input = new BufferedReader(new FileReader(r2nFile));
            String line = null;
            ArrayList<String> tempLookupValues;
            boolean firstEntry = true;
            float first = -9999;
            float second = -9999;
            while ((line = input.readLine()) != null) {
                if (line.matches("r2s") || line.startsWith("#")) {
                    continue;
                }
                String[] lookupValues = null;
                if (line.contains(" ")) {
                    lookupValues = line.split(" ");
                }

                if (lookupValues != null) {
                    tempLookupValues = new ArrayList<String>();
                    for (int i = 0; i < lookupValues.length; i++) {
                        if (!lookupValues[i].equals(" ")
                                && !lookupValues[i].equals("")) {
                            tempLookupValues.add(lookupValues[i]);
                        }
                    }
                    lookupValues = tempLookupValues
                            .toArray(new String[tempLookupValues.size()]);
                    if (lookupValues.length == 2
                            && lookupValues[0].matches("default")) {
                        lookup.setDefaultValue(lookupValues[1]);
                    } else if (lookupValues.length == 3) {
                        try {

                            if (firstEntry) {
                                firstEntry = false;
                                second = Float.parseFloat(lookupValues[1]);
                                lookup.setRangeEntry(
                                        Float.parseFloat(lookupValues[0]),
                                        lookupValues[2]);
                                lookup.setRangeEntry(
                                        Float.parseFloat(lookupValues[1]),
                                        lookupValues[2]);

                            } else {
                                first = second;
                                second = Float.parseFloat(lookupValues[1]);
                                lookup.setRangeEntry(
                                        Float.parseFloat(lookupValues[1]),
                                        lookupValues[2]);
                            }
                        } catch (Exception e) {
                            e.printStackTrace();
                        }
                    }
                }
            }
            lookup.setFirstValue(first);
            lookup.setSecondValue(second);
            input.close();
        } catch (FileNotFoundException ex) {
            ex.printStackTrace();
            return null;
        } catch (IOException ex) {
            ex.printStackTrace();
            return null;
        }
        return lookup;
    }

    private String invisibleString(int length) {
        char character = ' ';
        char[] toReturn = new char[length];
        for (int i = 0; i < length; i++) {
            toReturn[i] = character;
        }
        return new String(toReturn);
    }

    private void setFirstValue(float firstValue) {
        this.firstValue = firstValue;
    }

    private void setSecondValue(float secondValue) {
        this.secondValue = secondValue;
    }
}
