package gov.noaa.nws.mdl.viz.boundaryTool.boundaries.state.xml;

import gov.noaa.nws.mdl.viz.boundaryTool.boundaries.state.xml.Boundaries.ForbiddenBoundaryIds;
import gov.noaa.nws.mdl.viz.boundaryTool.common.boundary.BoundaryState;

import java.io.FileNotFoundException;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import javax.xml.bind.JAXBException;
import javax.xml.datatype.DatatypeConfigurationException;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;

/**
 * Methods to be called to read boundary xml file via LocalizationFile and
 * populate BoundaryState objects for the display active boundaries by
 * BounadaryDisplay class
 * 
 * <pre>
 * @author Mamoudou Ba
 * @version 1.0
 * </pre>
 * 
 */
public class ReadBoundariesXmlFile {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ReadBoundariesXmlFile.class);

    public void readBoundariesXmlFile(BoundaryState currentState)
            throws VizException, DatatypeConfigurationException,
            FileNotFoundException {
        Date d = null;

        long createTime = 0;
        long currTime = 0;
        float bndLifeSpan = 0;
        long oneHour = TimeUtil.MILLIS_PER_HOUR;
        String moving = "Moving";

        // Use Localization context to read boundary data
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.SITE);
        LocalizationFile boundaryFile = pm.getLocalizationFile(context,
                "awipsTools" + IPathManager.SEPARATOR + "boundaryTool.xml");

        if (boundaryFile.exists()) {
            try {
                // Read boundary data via localization
                Boundaries boundariesObj = boundaryFile.jaxbUnmarshal(
                        Boundaries.class, new JAXBManager(Boundaries.class));

                d = new Date(boundariesObj.getTimeInfo().getFileCreationUtime());

                currentState.creationFileTime = new DataTime(d);

                for (int i = 0; i < boundariesObj.getBoundary().size(); i++) {
                    int boundaryId = boundariesObj.getBoundary().get(i)
                            .getBoundaryId();
                    // Populate boundary active list for those active
                    // boundaries.

                    d = new Date(boundariesObj.getBoundary().get(i)
                            .getBoundaryCreationUtime());

                    DataTime t = new DataTime(SimulatedTime.getSystemTime()
                            .getTime());
                    currTime = t.getMatchRef();
                    createTime = boundariesObj.getBoundary().get(i)
                            .getBoundaryCreationUtime();
                    bndLifeSpan = (currTime - createTime) / oneHour;

                    if (bndLifeSpan >= (float) boundariesObj.getBoundary()
                            .get(i).getBoundaryLifeSpan()) {
                        // Add expired boundaries to forbidden list
                        if (currentState.forbiddenBoundaryIdsMap
                                .get(boundaryId) != null) {
                            currentState.forbiddenBoundaryIdsMap
                                    .remove(boundaryId);
                            currentState.forbiddenBoundaryIdsMap.put(
                                    boundaryId, boundaryId);
                        }
                        continue;
                    }

                    currentState.createTimeMap.put(boundaryId, new DataTime(d));
                    currentState.boundaryDurationMap.put(boundaryId,
                            boundariesObj.getBoundary().get(i)
                                    .getBoundaryLifeSpan());

                    d = new Date(boundariesObj.getBoundary().get(i)
                            .getBoundaryEditedUtime());
                    currentState.editedTimeMap.put(boundaryId, new DataTime(d));

                    d = new Date(boundariesObj.getBoundary().get(i)
                            .getBoundaryExpirationUtime());
                    currentState.expirationTimeMap.put(boundaryId,
                            new DataTime(d));

                    Coordinate[] coords = new Coordinate[boundariesObj
                            .getBoundary().get(i).getNumberOfVertices()];
                    float[] speed = new float[boundariesObj.getBoundary()
                            .get(i).getNumberOfVertices()];
                    float[] angle = new float[boundariesObj.getBoundary()
                            .get(i).getNumberOfVertices()];

                    for (int j = 0; j < boundariesObj.getBoundary().get(i)
                            .getVertex().size(); j++) {
                        coords[j] = new Coordinate(boundariesObj.getBoundary()
                                .get(i).getVertex().get(j).getLongitude(),
                                boundariesObj.getBoundary().get(i).getVertex()
                                        .get(j).getLatitude());
                        speed[j] = boundariesObj.getBoundary().get(i)
                                .getVertex().get(j).getSpeed();
                        angle[j] = boundariesObj.getBoundary().get(i)
                                .getVertex().get(j).getAzimuth();

                    }

                    currentState.boundariesMap.put(boundaryId,
                            new GeometryFactory().createLineString(coords));

                    if (boundariesObj.getBoundary().get(i).getBoundaryMode()
                            .equals(moving)) {
                        currentState.isMovingMap.put(boundaryId, true);
                        currentState.vertexAngleMap.put(boundaryId, angle);
                        currentState.vertexSpeedMap.put(boundaryId, speed);
                    } else {
                        currentState.isMovingMap.put(boundaryId, false);
                    }
                    currentState.boundaryTypeMap.put(boundaryId, boundariesObj
                            .getBoundary().get(i).getBoundaryType());
                    currentState.boundaryId = boundaryId;
                    if (currentState.boundariesMap.size() != 0) {
                        if (currentState.existingBoundaryNotEmptyMap
                                .get(boundaryId) != null) {
                            currentState.existingBoundaryNotEmptyMap
                                    .remove(boundaryId);
                        }

                        currentState.existingBoundaryNotEmptyMap.put(
                                boundaryId, true);
                    }
                }

                // Populate boundary forbidden id list

                List<ForbiddenBoundaryIds> forbiden = boundariesObj
                        .getForbiddenBoundaryIds();

                Iterator<ForbiddenBoundaryIds> iterator = forbiden.iterator();

                int id = 0;
                Iterable<String> s1 = null;

                while (iterator.hasNext()) {
                    s1 = iterator.next().getForbiddenId();
                    for (String e : s1) {
                        try {
                            id = Integer.parseInt(e);

                        } catch (NumberFormatException ex) {
                            statusHandler.handle(Priority.PROBLEM,
                                    "Problem in parsing the integer", ex);
                        }
                        currentState.forbiddenBoundaryIdsMap.put(id, id);
                    }
                }

            } catch (JAXBException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "problem in reading boundary data", e);
            } catch (LocalizationException e1) {
                statusHandler.handle(Priority.PROBLEM,
                        "Localization exception", e1);
            }
        }
    }
}
