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
package com.raytheon.uf.edex.plugin.acarssounding.tools;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.measure.UnitConverter;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.edex.decodertools.core.LatLonPoint;

import si.uom.SI;
import systems.uom.common.USCustomary;
import tec.uom.se.unit.MetricPrefix;

/**
 * Container for a list of airports read from an XML file
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------------------------
 * Apr 16, 2009           jkorman   Initial creation
 * Oct 22, 2013  2361     njensen   Use JAXBManager for XML
 * Aug 18, 2014  3530     bclement  moved from common to edex
 * Dec 10, 2015  5166     kbisanz   Update logging to use SLF4J
 * Aug 10, 2016  5757     nabowle   Code cleanup.
 * Jul 15, 2020  8191     randerso  Updated for changes to LatLonPoint
 *
 * </pre>
 *
 * @author jkorman
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class Airports {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    private static final UnitConverter NMtoKM = USCustomary.NAUTICAL_MILE
            .getConverterTo(MetricPrefix.KILO(SI.METRE));

    @XmlElement
    private List<Airport> airport;

    private transient Map<String, Airport> airportMap;

    public Airports() {
    }

    /**
     *
     */
    private void createMapping() {
        airportMap = new HashMap<>();
        for (Airport airportData : airport) {
            if (!airportMap.containsKey(airportData.getId())) {
                airportMap.put(airportData.getId(), airportData);
            } else {
                logger.error("Duplicate airport identification "
                        + airportData.getId());
            }
        }
    }

    /**
     *
     * @return the station
     */
    public List<Airport> getAirport() {
        return airport;
    }

    /**
     *
     * @param station
     *            the station to set
     */
    public void setAirport(List<Airport> airport) {
        this.airport = airport;
    }

    /**
     *
     * @param airportLoc
     */
    public void addAirport(Airport airportLoc) {
        if (this.airport == null) {
            airport = new ArrayList<>();
        }
        airport.add(airportLoc);
    }

    /**
     *
     * @param latitude
     * @param longitude
     * @param maxDistance
     *            Maximum distance in kilometers.
     * @return The closest airport, within the given maximum distance, to a
     *         given point.
     */
    public Airport nearest(Double latitude, Double longitude,
            Double maxDistance) {
        return nearest(new LatLonPoint(latitude, longitude), maxDistance);
    }

    /**
     *
     * @param latLon
     * @param maxDistance
     *            Maximum distance in kilometers.
     * @return The closest airport, within the given maximum distance, to a
     *         given point.
     */
    public Airport nearest(LatLonPoint latLon, Double maxDistance) {

        Airport nearestAirport = null;
        Double distance = maxDistance;

        for (Airport a : airport) {
            double d = latLon.distanceTo(a.getLatitude(), a.getLongitude());
            d = NMtoKM.convert(d);

            if (d < distance) {
                distance = d;
                nearestAirport = a;
            }
        }
        Airport a = null;
        if (nearestAirport != null) {
            a = nearestAirport.copy();
            a.setDistance(distance);
        }
        return a;
    }

    public Airport getAirport(String airportId) {
        return airportMap.get(airportId).copy();
    }

    /**
     * Create an instance of this class.
     *
     * @param filePath
     * @param fileName
     * @return
     */
    public static Airports loadFromFile(String filePath, String fileName,
            Logger logger) {

        Airports airports = null;

        try {
            SingleTypeJAXBManager<Airports> jaxb = new SingleTypeJAXBManager<>(
                    Airports.class);
            airports = jaxb
                    .unmarshalFromXmlFile(filePath + File.separator + fileName);
            logger.info(String.format("%4d airports read from config.",
                    airports.getAirport().size()));

        } catch (Exception e) {
            logger.error("Error unmarshaling airports ", e);
        }
        airports.createMapping();

        return airports;
    }

}
