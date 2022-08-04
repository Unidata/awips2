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
package com.raytheon.uf.common.monitor.xml;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;

/**
 * Class containing the XML data specifying an array of AreaXML data.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -------------------------------------------
 * Dec 15, 2009  3963     lvenable  Initial creation
 * Jan 04, 2016  5115     skorolev  moved from com.raytheon.uf.viz.monitor.xml
 * May 07, 2019  7689     randerso  Code cleanup
 *
 * </pre>
 *
 * @author lvenable
 */
@XmlRootElement(name = "Thresholds")
@XmlAccessorType(XmlAccessType.NONE)
public class ThresholdsXML {
    /** Single Type JAXB Manager */
    private static final SingleTypeJAXBManager<ThresholdsXML> jaxb = SingleTypeJAXBManager
            .createWithoutException(ThresholdsXML.class);

    @XmlElements({ @XmlElement(name = "Area", type = AreaXML.class) })
    private List<AreaXML> areas = new ArrayList<>();

    private Map<String, AreaXML> areaMap;

    /**
     * Read threshold data from a localization file
     *
     * @param lf
     *            the localization file
     * @return the thresholds
     * @throws SerializationException
     * @throws IOException
     * @throws LocalizationException
     */
    public static ThresholdsXML readThresholdXml(ILocalizationFile lf)
            throws SerializationException, IOException, LocalizationException {
        try (InputStream is = lf.openInputStream()) {
            return jaxb.unmarshalFromInputStream(is);
        } catch (SerializationException | IOException
                | LocalizationException e) {
            throw e;
        }
    }

    /**
     * Save this threshold to a localization file
     *
     * @param lf
     *            the localization file
     *
     * @throws SerializationException
     *             , IOException , LocalizationException
     * @throws IOException
     * @throws LocalizationException
     */
    public void saveThresholdXml(ILocalizationFile lf)
            throws SerializationException, IOException, LocalizationException {
        try (SaveableOutputStream outStrm = lf.openOutputStream()) {
            jaxb.marshalToStream(this, outStrm);
            outStrm.save();
        } catch (SerializationException | IOException
                | LocalizationException e) {
            throw e;
        }
    }

    /**
     * Nullary constructor
     */
    public ThresholdsXML() {
    }

    /**
     * @return list of area thresholds
     */
    public List<AreaXML> getAreas() {
        return areas;
    }

    /**
     * @param areas
     *            the area thresholds to set
     */
    public void setAreas(List<AreaXML> areas) {
        this.areas = areas;
        this.areaMap = null;
    }

    private Map<String, AreaXML> getAreaMap() {
        if (areaMap == null) {
            areaMap = new HashMap<>();

            for (AreaXML area : areas) {
                areaMap.put(area.getAreaId(), area);
            }
        }
        return areaMap;
    }

    /**
     * @param areaID
     * @return the AreaXML for the specified AreaID
     */
    public AreaXML getArea(String areaID) {
        return getAreaMap().get(areaID);
    }

    /**
     * Add a new area to this ThresholdsXML
     *
     * @param area
     */
    public void addArea(AreaXML area) {
        String areaId = area.getAreaId();
        if (getAreaMap().containsKey(areaId)) {
            throw new IllegalArgumentException(
                    "ThresholdsXML already contains area " + areaId);
        }

        areas.add(area);
        areaMap.put(areaId, area);
    }
}
