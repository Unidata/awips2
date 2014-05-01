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
package com.raytheon.viz.grid.xml;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXB;

import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.core.rsc.DisplayType;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 21, 2010            jelkins     Initial creation
 * 
 * </pre>
 * 
 * @author jelkins
 * @version 1.0
 */

public class FieldDisplayTypesFactory {

    public final static String FIELD_DISPLAY_TYPES_FILE = "volumebrowser/FieldDisplayTypes.xml";

    public final static List<DisplayType> DEFAULT_DISPLAY_TYPES = new ArrayList<DisplayType>();

    public static final String DEFAULT_VOLUME_PLANE_TYPE = "MB";

    static {
        DEFAULT_DISPLAY_TYPES.add(DisplayType.CONTOUR);
        DEFAULT_DISPLAY_TYPES.add(DisplayType.IMAGE);
    }

    private static FieldDisplayTypesFactory instance;

    private final Map<String, List<DisplayType>> fieldDisplayTypeMap;

    private final Map<String, String> volumePlaneTypeMap;

    private FieldDisplayTypesFactory() {

        File path = PathManagerFactory.getPathManager().getStaticFile(
                FIELD_DISPLAY_TYPES_FILE);

        FieldDisplayTypesFile fieldDisplayTypesFile = JAXB.unmarshal(path,
                FieldDisplayTypesFile.class);

        List<FieldDisplayTypes> fields = fieldDisplayTypesFile
                .getFieldDisplayTypesFile();

        fieldDisplayTypeMap = new HashMap<String, List<DisplayType>>();

        volumePlaneTypeMap = new HashMap<String, String>();

        if (fields != null && fields.size() > 0) {
            for (FieldDisplayTypes field : fields) {
                if (field.getDisplayTypes() != null) {
                    for (String displayTypeString : field.getDisplayTypes()
                            .split(",")) {
                        DisplayType displayType = DisplayType
                                .valueOf(displayTypeString);
                        List<DisplayType> displayTypeSet = fieldDisplayTypeMap
                                .get(field.getKey());

                        if (displayTypeSet == null) {
                            displayTypeSet = new ArrayList<DisplayType>();
                            fieldDisplayTypeMap.put(field.getKey(),
                                    displayTypeSet);
                        }

                        displayTypeSet.add(displayType);
                    }
                }
                String volumePlaneType = field.getVolumePlaneType();
                if (volumePlaneType != null) {
                    volumePlaneTypeMap.put(field.getKey(), volumePlaneType);
                }
            }
        }
    }

    /**
     * 
     * @param fieldKey
     * @return the set of display types associated with the given fieldKey. If
     *         the field key isn't defined in the field display types file the
     *         default display types are returned.
     */
    public List<DisplayType> getDisplayTypes(String fieldKey) {

        List<DisplayType> displayTypes = fieldDisplayTypeMap.get(fieldKey);

        if (displayTypes == null) {
            return DEFAULT_DISPLAY_TYPES;
        } else {
            return displayTypes;
        }
    }

    public String getVolumePlaneType(String fieldKey) {
        String volumePlaneType = volumePlaneTypeMap.get(fieldKey);
        if (volumePlaneType == null) {
            return DEFAULT_VOLUME_PLANE_TYPE;
        } else {
            return volumePlaneType;
        }
    }

    public static FieldDisplayTypesFactory getInstance() {
        if (instance == null) {
            instance = new FieldDisplayTypesFactory();
        }
        return instance;
    }

}
