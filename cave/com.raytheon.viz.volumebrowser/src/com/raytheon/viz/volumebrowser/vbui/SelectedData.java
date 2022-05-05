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
package com.raytheon.viz.volumebrowser.vbui;

import java.util.List;

import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.viz.grid.xml.FieldDisplayTypesFactory;

/**
 * 
 * The class contains data for a Sources, Fields, Planes selection.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 9, 2009  #2161      lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class SelectedData {

    /**
     * Sources text (menu text).
     */
    private String sourcesText;

    /**
     * Sources key.
     */
    private String sourcesKey;

    /**
     * Fields text (menu text).
     */
    private String fieldsText;

    /**
     * Fields key.
     */
    private String fieldsKey;

    /**
     * Planes text (menu text).
     */
    private String planesText;

    /**
     * Planes key.
     */
    private String planesKey;

    /**
     * Unique key used to identify the Sources, Fields, Planes selection.
     */
    private String uniqueKey;

    /**
     * Constructor.
     */
    public SelectedData() {
    }

    /**
     * Constructor.
     * 
     * @param dataType
     *            Data type.
     * @param sourcesText
     *            Sources text (menu text).
     * @param sourcesKey
     *            Sources key.
     * @param fieldsText
     *            Fields text (menu text).
     * @param fieldsKey
     *            Fields key.
     * @param planesText
     *            Planes text (menu text).
     * @param planesKey
     *            Planes key.
     * @param uniqueKey
     *            Unique key used to identify the Sources, Fields, Planes
     *            selection.
     */
    public SelectedData(String sourcesText, String sourcesKey,
            String fieldsText, String fieldsKey, String planesText,
            String planesKey, String uniqueKey) {
        this.sourcesText = sourcesText;
        this.sourcesKey = sourcesKey;
        this.fieldsText = fieldsText;
        this.fieldsKey = fieldsKey;
        this.planesText = planesText;
        this.planesKey = planesKey;
        this.uniqueKey = uniqueKey;
    }

    /**
     * Get the Sources text (menu text).
     * 
     * @return The Sources text.
     */
    public String getSourcesText() {
        return sourcesText;
    }

    /**
     * Get the Sources key.
     * 
     * @return The Sources key.
     */
    public String getSourcesKey() {
        return sourcesKey;
    }

    /**
     * Get the Fields text (menu text).
     * 
     * @return The Fields text.
     */
    public String getFieldsText() {
        return fieldsText;
    }

    /**
     * Get the Fields key.
     * 
     * @return The Fields key.
     */
    public String getFieldsKey() {
        return fieldsKey;
    }

    /**
     * Get the Planes text (menu text).
     * 
     * @return The Planes text.
     */
    public String getPlanesText() {
        return planesText;
    }

    /**
     * Get the Planes key.
     * 
     * @return The Planes key.
     */
    public String getPlanesKey() {
        return planesKey;
    }

    /**
     * Get the unique key.
     * 
     * @return The unique key.
     */
    public String getUniqueKey() {
        return uniqueKey;
    }

    /**
     * @return the display types that this selected data can display
     */
    public List<DisplayType> getDisplayTypes() {

        FieldDisplayTypesFactory displayTypesFactory = FieldDisplayTypesFactory
                .getInstance();

        return displayTypesFactory.getDisplayTypes(fieldsKey);
    }

}
