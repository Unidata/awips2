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
package com.raytheon.viz.warngen.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Abbreviation
 * 
 * Provides a simple mapping of abbreviations to their longer notation. Format
 * is a simple \ and space delimited text file. Also supports the concept of a
 * "default".
 * 
 * Example:
 * 
 * <pre>
 * A \ ITEM A
 * B \ ITEM B
 * DEFAULT \ ITEM D
 * </pre>
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Nov 15, 2007             chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class Abbreviation {

    private static final String PLURAL_SUFFIX = "+";

    private static final String DEFAULT = "DEFAULT";

    private static final String DEFAULT_PLURAL = "DEFAULT" + "+";

    private Map<String, String> abbreviationMap;

    private String defaultAbbrev;

    private String defaultPluralAbbrev;

    public Abbreviation(File abbreviationFile) throws VizException {
        BufferedReader bufferedReader = null;
        FileReader fileReader = null;

        try {
            this.abbreviationMap = new HashMap<String, String>();
            fileReader = new FileReader(abbreviationFile);
            bufferedReader = new BufferedReader(fileReader);

            while (bufferedReader.ready()) {
                String line = bufferedReader.readLine();
                String[] tokens = line.split("\\\\");
                if (tokens[0].trim().equalsIgnoreCase(DEFAULT)) {
                    this.defaultAbbrev = tokens[1].trim().toUpperCase();
                } else if (tokens[0].trim().equalsIgnoreCase(DEFAULT_PLURAL)) {
                    this.defaultPluralAbbrev = tokens[1].trim().toUpperCase();
                } else {
                    this.abbreviationMap.put(tokens[0].trim().toUpperCase(),
                            tokens.length < 2 || tokens[1] == null ? ""
                                    : tokens[1].trim().toUpperCase());
                }
            }
        } catch (Exception e) {
            throw new VizException("Unable to set up abbreviation mapping ", e);

        } finally {
            try {
                if (bufferedReader != null)
                    bufferedReader.close();
            } catch (IOException e1) {
                // ignore
            }
            try {
                if (fileReader != null)
                    fileReader.close();
            } catch (IOException e) {
                // ignore
            }
        }
    }

    public String translate(String abbreviation) {
        String str = this.abbreviationMap.get(abbreviation.toUpperCase());
        if (str == null && this.defaultAbbrev != null) {
            return abbreviation.endsWith(PLURAL_SUFFIX) ? this.defaultPluralAbbrev
                    : this.defaultAbbrev;
        }
        return str;
    }

    public String translatePlural(String abbreviation) {
        return translate(abbreviation + PLURAL_SUFFIX);
    }

    public ArrayList<String> getValues() {
        ArrayList<String> values = new ArrayList<String>();
        for (String key : abbreviationMap.keySet()) {
            values.add(abbreviationMap.get(key));
        }

        return values;
    }

}
