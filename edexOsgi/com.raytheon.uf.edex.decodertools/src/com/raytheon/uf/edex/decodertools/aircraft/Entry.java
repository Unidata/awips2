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

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 13, 2011            jkorman     Initial creation
 * Sep 18, 2014 3627       mapeters    Removed unused methods/fields.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class Entry {

    private final String translatedWord;

    private final Integer firstId;

    private final Integer secondId;

    public Entry(String translatedWord, Integer firstId, Integer secondId) {
        this.translatedWord = translatedWord;
        this.firstId = firstId;
        this.secondId = secondId;
    }

    /**
     * @return the translatedWord
     */
    public String getTranslatedWord() {
        return translatedWord;
    }

    /**
     * @return the firstId
     */
    public Integer getFirstId() {
        return firstId;
    }

    /**
     * @return the secondId
     */
    public Integer getSecondId() {
        return secondId;
    }

    /**
     * @param args
     */
    public static void main(String[] args) {

    }
}

