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
package com.raytheon.uf.common.dataplugin.acarssounding.tools;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.edex.decodertools.core.LatLonPoint;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 16, 2009            jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class Airports implements ISerializableObject {

    private Log logger = LogFactory.getLog(getClass());

    // Average earth radius, fine for what we are doing.
    private static final double E_RADIUS = 6371.2213;

    @XmlElement
    private ArrayList<Airport> airport;

    private transient Map<String, Airport> airportMap;

    public Airports() {
    }

    /**
     * 
     */
    private void createMapping() {
        airportMap = new HashMap<String, Airport>();
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
    public ArrayList<Airport> getAirport() {
        return airport;
    }

    /**
     * 
     * @param station
     *            the station to set
     */
    public void setAirport(ArrayList<Airport> airport) {
        this.airport = airport;
    }

    /**
     * 
     * @param airportLoc
     */
    public void addAirport(Airport airportLoc) {
        if (this.airport == null) {
            airport = new ArrayList<Airport>();
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
    public Airport nearest(Double latitude, Double longitude, Double maxDistance) {
        return nearest(new LatLonPoint(latitude, longitude,
                LatLonPoint.INDEGREES), maxDistance);
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
            double d = latLon.distanceTo(a.getLatitude(), a.getLongitude(),
                    LatLonPoint.INDEGREES);
            d = d * E_RADIUS;

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
            Log logger) {

        Airports airports = null;

        try {
            airports = (Airports) SerializationUtil
                    .jaxbUnmarshalFromXmlFile(filePath + File.separator
                            + fileName);
            logger.info(String.format("%4d airports read from config.",
                    airports.getAirport().size()));

        } catch (SerializationException e) {
            logger.error("Error unmarshaling airports ", e);
        }
        airports.createMapping();

        return airports;
    }

}
