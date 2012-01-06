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
package com.raytheon.uf.edex.decodertools.aircraft;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 10, 2011            jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class WordTranslator {


    private Map<Pattern, String> regex = new HashMap<Pattern, String>();
    
    private Map<String, Entry> words = new HashMap<String, Entry>();

    /**
     * 
     * @param word
     * @param translatedWord
     * @param firstId
     * @param secondId
     */
    public void enter(String word, String translatedWord, boolean isRegEx,
            Integer firstId, Integer secondId) {
        
        if(isRegEx) {
            regex.put(Pattern.compile(word), translatedWord);
        }
        
        words.put(word, new Entry(word, translatedWord, isRegEx, firstId,
                secondId));
    }

    /**
     * 
     * @param word
     * @return
     */
    public Entry translate(String word) {
        Entry e = words.get(word);
        if(e == null) {
            if(regex.size() > 0) {
                for(Pattern p : regex.keySet()) {
                    Matcher m = p.matcher(word);
                    if(m.matches()) {
                        words.get(regex.get(p));
                    }
                }
            }
        }
        return e;
    }

    /**
     * 
     * @param word
     * @return
     */
    public Boolean isRegEx(String word) {
        Entry entry = words.get(word);
        return (entry != null) ? entry.isRegEx() : null;
    }

    /**
     * 
     * @param word
     * @return
     */
    public Integer getFirstId(String word) {
        Entry entry = words.get(word);
        return (entry != null) ? entry.getFirstId() : null;
    }

    /**
     * 
     * @param word
     * @return
     */
    public Integer getSecondId(String word) {
        Entry entry = words.get(word);
        return (entry != null) ? entry.getSecondId() : null;
    }

    /**
     * @param args
     */
    public static void main(String[] args) {
        // TODO Auto-generated method stub

    }

}
