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
package com.raytheon.uf.common.dataplugin.gfe.request;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Class for requesting Grid Point data for a list of coordinates.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 4, 2009            njensen     Initial creation
 * Mar 6, 2013  1735       rferrel     Now handles a list of coordinates.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@DynamicSerialize
public class GetPointDataRequest extends AbstractGfeRequest {

    /** Id of data base containing the grid. */
    @DynamicSerializeElement
    private String databaseID;

    /** List of coordinates requesting values. */
    @DynamicSerializeElement
    private List<Coordinate> coordinates = new ArrayList<Coordinate>();

    /** Parameters to to obtain for each coordinate. */
    @DynamicSerializeElement
    private List<String> parameters = new ArrayList<String>();

    /** Start time for the interval to obtain values for. */
    @DynamicSerializeElement
    private long startTime;

    /** Number of hours to obtain values for. */
    @DynamicSerializeElement
    private int numberHours;

    /**
     * Parameter to add to the list.
     * 
     * @param parmShortName
     */
    public void addParameter(String parmShortName) {
        parameters.add(parmShortName);
    }

    /**
     * Obtain the data base ID for the grid.
     * 
     * @return databaseID
     */
    public String getDatabaseID() {
        return databaseID;
    }

    /**
     * Set the data base ID.
     * 
     * @param databaseID
     */
    public void setDatabaseID(String databaseID) {
        this.databaseID = databaseID;
    }

    /**
     * Get list of parameters to obtain.
     * 
     * @return parameters
     */
    public List<String> getParameters() {
        return parameters;
    }

    /**
     * Set the parmater list.
     * 
     * @param parameters
     */
    public void setParameters(List<String> parameters) {
        this.parameters = parameters;
    }

    /**
     * Get list of coordinates.
     * 
     * @return coordinates
     */
    public List<Coordinate> getCoordinates() {
        return coordinates;
    }

    /**
     * Set list of coordinates.
     * 
     * @param coordinates
     */
    public void setCoordinates(List<Coordinate> coordinates) {
        this.coordinates = coordinates;
    }

    /**
     * Add coordinate to the list.
     * 
     * @param coordinate
     */
    public void addCoordinate(Coordinate coordinate) {
        if (coordinates == null) {
            coordinates = new ArrayList<Coordinate>();
        }
        coordinates.add(coordinate);
    }

    /**
     * Get start time.
     * 
     * @return startTime
     */
    public long getStartTime() {
        return startTime;
    }

    /**
     * Set the start time.
     * 
     * @param startTime
     */
    public void setStartTime(long startTime) {
        this.startTime = startTime;
    }

    /**
     * Get the number of hours desired.
     * 
     * @return numberHours
     */
    public int getNumberHours() {
        return numberHours;
    }

    /**
     * Set the number of hours.
     * 
     * @param numberHours
     */
    public void setNumberHours(int numberHours) {
        this.numberHours = numberHours;
    }

}
