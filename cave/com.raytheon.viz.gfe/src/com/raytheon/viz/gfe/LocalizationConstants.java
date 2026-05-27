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
package com.raytheon.viz.gfe;


/**
 * Localization Perspective constants.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 23, 2010            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class LocalizationConstants {
    public static String SMART_TOOLS = "smartTools";

    public static String SMART_TOOLS_TEXT = "Smart Tools";

    public static String CONFIG = "config";

    public static String CONFIG_TEXT = "Config Files";

    public static String PROCEDURES = "procedures";

    public static String PROCEDURES_TEXT = "Procedures";

    public static String TEXT_PRODUCTS = "textProducts";

    public static String TEXT_PRODUCTS_TEXT = "Text Products";

    /**
     * Save Action Parameters.
     */
    public enum SaveActionParameter {
        SAVE("save"), SAVE_AS("saveAs"), SAVE_ALL("saveAll");

        private String saveType = null;

        SaveActionParameter(String saveType) {
            this.saveType = saveType;
        }

        public String getType() {
            return saveType;
        }
    }

    public enum FileAction {
        SELECT_DIRECTORY, SELECT_FILE, EXPAND_DIRECTORY, NEW_FILE;
    }

}
