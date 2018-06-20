package gov.noaa.nws.mdl.viz.boundaryTool.boundaries.state.xml;

import static java.lang.Math.PI;
import static java.lang.Math.cos;
import static java.lang.Math.sin;
import gov.noaa.nws.mdl.viz.boundaryTool.boundaries.state.xml.Boundaries.Boundary.Vertex;
import gov.noaa.nws.mdl.viz.boundaryTool.boundaries.state.xml.Boundaries.ForbiddenBoundaryIds;
import gov.noaa.nws.mdl.viz.boundaryTool.common.boundary.BoundaryState;
import gov.noaa.nws.mdl.viz.boundaryTool.common.boundary.BoundaryState.BoundaryPolyLine;

import java.io.IOException;
import java.sql.Date;
import java.util.ArrayList;
import java.util.GregorianCalendar;
import java.util.Iterator;
import java.util.List;

import javax.xml.bind.JAXBException;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.LineString;

/**
 * Methods to be called to save boundary xml file via LocalizationFile
 * 
 * <pre>
 * @author Mamoudou Ba
 * @version 1.0
 * </pre>
 */
public class WriteBoundariesXmlFile {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(WriteBoundariesXmlFile.class);
    private static JAXBManager jaxbManager;
    private static boolean initialized;

    /**
     * Populate the Boundaries Object and save it via LocalizationFile.
     */
    public void writeBoundariesXmlFile(BoundaryState currentState,
            DataTime[] dataTimes, int currentTimeIndex) throws VizException,
            DatatypeConfigurationException {
        BoundaryObjectFactory ofObj = new BoundaryObjectFactory();
        Boundaries boundariesObj = ofObj.createBoundaries();

        // Use Localization context to save boundary data
        IPathManager pm = PathManagerFactory.getPathManager();

        LocalizationContext context = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.SITE);

        LocalizationFile boundaryFile = pm.getLocalizationFile(context,
                "awipsTools" + IPathManager.SEPARATOR + "boundaryTool.xml");

        /*
         * Use ProfileDescription methods to populate the ProfileDescription
         * object with values. Fist the simple type variables.
         */

        // Populate the Boundaries Object

        // Current user name
        boundariesObj.setUserName(System.getProperty("user.name"));

        // Time info (Time frame)
        // currentTimeIndex = this.trackUtil.getCurrentFrame(info);

        long currentUTime = 0;
        long oneHour = TimeUtil.MILLIS_PER_HOUR;
        long expirationUTime = 0;
        long boundaryCreationUtime = 0;
        long boundaryEditedUtime = 0;
        Date d;
        Date expiration_date;
        // dataTimes = trackUtil.getDataTimes(info);

        currentUTime = dataTimes[currentTimeIndex].getMatchRef();
        d = new Date(currentUTime);

        GregorianCalendar c = new GregorianCalendar();
        c.setTime(d);

        XMLGregorianCalendar d2 = DatatypeFactory.newInstance()
                .newXMLGregorianCalendar(c);

        Boundaries.TimeInfo timeInfoObj = ofObj.createBoundariesTimeInfo();
        timeInfoObj.setFileCreationTime(d2);
        timeInfoObj.setFileCreationUtime(currentUTime);

        boundariesObj.setTimeInfo(timeInfoObj);

        // Populate active boundaries: loop over all existing boundaries

        int moveIndex = currentTimeIndex;

        BoundaryPolyLine[] currentTimePoints;
        LineString line;
        Iterator<Integer> iterator = currentState.boundariesMap.keySet()
                .iterator();

        while (iterator.hasNext()) {
            String boundaryMode = null;

            Boundaries.Boundary boundary = ofObj.createBoundariesBoundary();

            int boundaryId = iterator.next();
            boundary.setBoundaryId(boundaryId);
            if (currentState.isMovingMap.get(boundaryId)) {
                boundaryMode = "Moving";
                currentTimePoints = currentState.timePointsMap.get(boundaryId);
                line = currentTimePoints[moveIndex].polyline;

            } else {
                line = currentState.boundariesMap.get(boundaryId);
                boundaryMode = "Stationary";
            }
            boundary.setBoundaryMode(boundaryMode);
            boundary.setBoundaryType(currentState.boundaryTypeMap
                    .get(boundaryId));
            boundary.setBoundaryLifeSpan(currentState.boundaryDurationMap
                    .get(boundaryId));

            Coordinate[] coords = line.getCoordinates();
            boundary.setNumberOfVertices(coords.length);
            // Populate boundary's time attributes
            // Set boundary creation time attributes
            boundaryCreationUtime = currentState.createTimeMap.get(boundaryId)
                    .getMatchRef();
            expirationUTime = boundaryCreationUtime
                    + (currentState.boundaryDurationMap.get(boundaryId) * oneHour);
            d = new Date(boundaryCreationUtime);
            expiration_date = new Date(expirationUTime);
            DataTime expirationDataTime = new DataTime(expiration_date);

            // Set Boundary expiration Time
            if (currentState.expirationTimeMap.get(boundaryId) != null)
                currentState.expirationTimeMap.remove(boundaryId);
            currentState.expirationTimeMap.put(boundaryId, expirationDataTime);

            c = new GregorianCalendar();
            c.setTime(d);
            d2 = DatatypeFactory.newInstance().newXMLGregorianCalendar(c);
            boundary.setBoundaryCreationTime(d2);
            boundary.setBoundaryCreationUtime(boundaryCreationUtime);

            // Set modification Time attributes
            boundaryEditedUtime = currentState.editedTimeMap.get(boundaryId)
                    .getMatchRef();
            d = new Date(boundaryEditedUtime);
            c = new GregorianCalendar();
            c.setTime(d);
            d2 = DatatypeFactory.newInstance().newXMLGregorianCalendar(c);
            boundary.setBoundaryEditedTime(d2);
            boundary.setBoundaryEditedUtime(boundaryEditedUtime);

            // Set boundary expiration time attributes
            c = new GregorianCalendar();
            c.setTime(expiration_date);
            d2 = DatatypeFactory.newInstance().newXMLGregorianCalendar(c);
            boundary.setBoundaryExpirationTime(d2);
            boundary.setBoundaryExpirationUtime(expirationUTime);

            // populate vertices
            // boundaryObj.setNumberOfVertices(coords.length);

            List<Boundaries.Boundary.Vertex> vertexList = new ArrayList<Boundaries.Boundary.Vertex>();
            // add vertex to the list
            for (int i = 0; i < coords.length; i++) {
                // coordinates
                Boundaries.Boundary.Vertex vertex = ofObj
                        .createBoundariesBoundaryVertex();
                vertexList.add(vertex);

                Vertex vx = vertexList.get(i);
                vx.setLongitude((float) coords[i].x);
                vx.setLatitude((float) coords[i].y);

                // speed and angle
                if (currentState.isMovingMap.get(boundaryId)) {
                    vx.setAzimuth(currentState.vertexAngleMap.get(boundaryId)[i]);
                    vx.setSpeed(currentState.vertexSpeedMap.get(boundaryId)[i]);
                    // Compute u,v components of speed
                    float u = (float) (currentState.vertexSpeedMap
                            .get(boundaryId)[i] * sin(currentState.vertexAngleMap
                            .get(boundaryId)[i] * PI / 180.));
                    float v = (float) (currentState.vertexSpeedMap
                            .get(boundaryId)[i] * cos(currentState.vertexAngleMap
                            .get(boundaryId)[i] * PI / 180.));
                    vx.setU(u);
                    vx.setV(v);
                } else {
                    vx.setAzimuth((float) 0.0);
                    vx.setSpeed((float) 0.0);
                    vx.setU((float) 0.0);
                    vx.setV((float) 0.0);

                }
                boundary.getVertex().add(vx);

            }

            boundariesObj.getBoundary().add(boundary);
        }

        // Populate forbidden ids list

        Iterator<Integer> logIndex = currentState.logMap.keySet().iterator();

        while (logIndex.hasNext()) {
            Boundaries.Log log = ofObj.createBoundariesLog();
            int id = logIndex.next();
            log.getComment().add(currentState.logMap.get(id));
            boundariesObj.getLog().add(log);
        }

        Iterator<Integer> forbidenIds = currentState.forbiddenBoundaryIdsMap
                .keySet().iterator();

        while (forbidenIds.hasNext()) {
            ForbiddenBoundaryIds forbiden = ofObj
                    .createBoundariesForbiddenBoundaryIds();
            int id = forbidenIds.next();
            String s = "" + id;
            forbiden.getForbiddenId().add(s);
            boundariesObj.getForbiddenBoundaryIds().add(forbiden);
        }

        try {
            initialize();
        } catch (JAXBException e1) {
            statusHandler.handle(Priority.PROBLEM, "Initialization fails", e1);
        }
        try (SaveableOutputStream sos = boundaryFile.openOutputStream()) {
            jaxbManager.marshalToStream(boundariesObj, sos);
            sos.save();
        } catch (IOException | LocalizationException | SerializationException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

    /**
     * initialize jaxb manager
     * 
     * @throws JAXBException
     */
    private static synchronized void initialize() throws JAXBException {
        if (!initialized) {
            jaxbManager = new JAXBManager(Boundaries.class);
            initialized = true;
        }
    }

}
