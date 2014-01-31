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
package com.raytheon.uf.edex.plugin.modelsounding.common;

/**
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20080317           1026 jkorman     Initial implementation.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public enum SoundingModels {

    MODEL_GFS("KWBC","GFS"),
    MODEL_ETA("KWNO","ETA");
    
    private final String associatedICAO;
    private final String reportType;
    
    private SoundingModels(String cccc, String rptType) {
        associatedICAO = cccc;
        reportType = rptType; 
    }
    
    /**
     * @return the associatedICAO
     */
    public String getAssociatedICAO() {
        return associatedICAO;
    }

    /**
     * @return the reportType
     */
    public String getReportType() {
        return reportType;
    }

    public static SoundingModels getModel(String cccc) {
        SoundingModels model = null;
        if(MODEL_GFS.associatedICAO.equals(cccc)) {
            model = MODEL_GFS;
        } else if(MODEL_ETA.associatedICAO.equals(cccc)) {
            model = MODEL_ETA;
        }
        return model;
    }
}
