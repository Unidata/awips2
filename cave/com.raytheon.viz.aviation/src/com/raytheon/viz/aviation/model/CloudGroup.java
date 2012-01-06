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

package com.raytheon.viz.aviation.model;

/**
 * CloudGroup class represents the cloud group within the common part of a
 * normalized TAF and a normalized METAR.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 3/13/2008    933         grichard    Initial creation.
 * 5/29/2008    937         grichard    Taf refactor first cut.
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 * 
 */

public class CloudGroup {

    /**
     * The Cloud Category of a forecast.
     */
    private CloudCategory cloudCategory;

    /**
     * Method that returns the cloud category for a forecast.
     * 
     * @return cloudCategory
     */
    public CloudCategory getCldCat() {
        return cloudCategory;
    }

    /**
     * Method that sets the cloud category for a forecast.
     * 
     * @param cldCat
     */
    public void setCldCat(CloudCategory cldCat) {
        cloudCategory = cldCat;
    }

    /**
     * Cloud Categories
     */
    public static enum CloudCategory {
        /* Sky Clear (SKC) -- used in TAF */
        /* Clear (CLR) -- unused in TAF */
        /* Sky Clear (SKT) */
        /* Few (FEW) */
        /* Scattered (SCT) */
        /* Broken (BKN) */
        /* Overcast (OVC) */
        /* Vertical Visibility (VV) */
        /* No Cloud Detected (NCD) -- unused in TAF */
        /* No Significant Cloud (NSC) -- unused in TAF */
        SKC("SKC"), CLR("CLR"), SKT("SKT"), FEW("FEW"), SCT("SCT"), BKN("BKN"), OVC(
                "OVC"), VV("VV"), NCD("NCD"), NSC("NSC");
        /*
         * See: NWS Operations Manual W/OM12 Part D-31 for more information.
         */

        CloudCategory(String value) {
            this.value = value;
        }

        private final String value;

        public String value() {
            return value;
        }
    }

    /**
     * The Cloud Height of a forecast.
     */
    private int cloudHeight;

    /**
     * Method that returns the cloud height for a forecast.
     * 
     * @return cloudHeight
     */
    public int getCldHgt() {
        return cloudHeight;
    }

    /**
     * Method that sets the cloud height for a forecast.
     * 
     * @param cldHgt
     */
    public void setCldHgt(int cldHgt) {
        cloudHeight = cldHgt;
    }

    /** Genus for convective low level cloud - CB */
    private String genus;

    /**
     * Method that gets the genus.
     * 
     * @return the genus
     */
    public String getGenus() {
        return genus;
    }

    /**
     * Method that sets the genus.
     * 
     * @param genus
     *            the genus to set
     */
    public void setGenus(String genus) {
        this.genus = genus;
    }

}
