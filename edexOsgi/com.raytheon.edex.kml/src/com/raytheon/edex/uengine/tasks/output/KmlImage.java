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

package com.raytheon.edex.uengine.tasks.output;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.PrintWriter;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.coverage.grid.InvalidGridGeometryException;
import org.geotools.geometry.Envelope2D;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.google.earth.kml._2.ContainerType;
import com.google.earth.kml._2.DocumentType;
import com.google.earth.kml._2.FeatureType;
import com.google.earth.kml._2.GroundOverlayType;
import com.google.earth.kml._2.KmlType;
import com.google.earth.kml._2.LatLonBoxType;
import com.google.earth.kml._2.LinkType;
import com.google.earth.kml._2.ObjectFactory;
import com.google.earth.kml._2.PlacemarkType;
import com.raytheon.edex.uengine.exception.MicroEngineException;
import com.raytheon.edex.uengine.tasks.ScriptTask;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.message.response.ResponseMessageGeneric;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;

/**
 * KmlImage task derived from KmlGen task of original uEngine. Generates a KML
 * file that references an image produced elsewhere in the script. Also supports
 * adding vector data to the KML file. The image that the KML references must be
 * in a cylindrical equidistanct (i.e. lat/lon) projection.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date             PR#             Engineer            Description
 * -----------      ----------      ------------        --------------------------
 * Apr 10, 2007                     njensen             Initial Creation
 * </PRE>
 * 
 */
public class KmlImage extends ScriptTask {

	private GridGeometry2D gridGeometry;

	private Envelope envelope;

	private File imageFile;

	private String fileLoc;

	private URI fileURI;

	private File fileOut;

	/**
	 * The vector data to add to the KML output.
	 */
	private ArrayList<PlacemarkType> vectorData = new ArrayList<PlacemarkType>();

	/**
	 * Constructor
	 * 
	 * @param aUri
	 *            uri of the image
	 * @param aTime
	 *            time associated with the image
	 * @param aGridGeometry
	 *            grid geometry of the image
	 */
	public KmlImage(URI fileURI, GridGeometry2D aGridGeometry) {
		this.fileURI = fileURI;
		imageFile = new File(this.fileURI);

		gridGeometry = aGridGeometry;
	}

	public KmlImage(URI fileURI, Envelope anEnvelope) {
		this.fileURI = fileURI;
		imageFile = new File(this.fileURI);
		envelope = anEnvelope;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.edex.uengine.js.tasks.ScriptTask#execute()
	 */
	@Override
	public Object execute() {

		KmlType kmlType = new KmlType();
		ArrayList<FeatureType> featureList = new ArrayList<FeatureType>();
		ArrayList<ResponseMessageGeneric> response = new ArrayList<ResponseMessageGeneric>();
		// for the time being, set the filename to one of the image names +
		// / .kml
		String filename = "";

		try {
			Point[] p = null;
			if (gridGeometry != null) {
				p = determineBoundsByGeometry(gridGeometry);
			} else if (envelope != null) {
				p = determineBoundsByEnvelope(envelope);
			} else {
				throw new MicroEngineException(
						"Unable to determine lat/lon bounds of image.");
			}

			GroundOverlayType overlay = new GroundOverlayType();
			LatLonBoxType llBox = new LatLonBoxType();
			llBox.setNorth(p[1].getY());
			llBox.setSouth(p[0].getY());
			llBox.setEast(p[1].getX());
			llBox.setWest(p[0].getX());
			overlay.setLatLonBox(llBox);
			// for now just set it to one of the image filenames
			filename = imageFile.getAbsolutePath();

			LinkType lt = new LinkType();
			lt.setHref(imageFile.getAbsolutePath());
			overlay.setIcon(lt);

			featureList.add(overlay);

			DocumentType container = new DocumentType();

			List<JAXBElement<? extends FeatureType>> features = container
					.getFeature();
			ObjectFactory factory = new ObjectFactory();
			JAXBElement<? extends FeatureType> element = (JAXBElement<? extends FeatureType>) factory
					.createGroundOverlay(overlay);
			features.add(element);
			ArrayList<ContainerType> containList = new ArrayList<ContainerType>();
			containList.add(container);

			JAXBContext context;
			try {
				context = JAXBContext.newInstance(KmlType.class);

				KmlType kml = new KmlType();
				kml.setFeature(factory.createDocument(container));

				Marshaller marshaller = context.createMarshaller();

				fileOut = new File(this.fileURI.getPath() + ".kml");
				PrintWriter writer = new PrintWriter(fileOut);

				marshaller.marshal(kml, writer);
				writer.close();
			} catch (JAXBException e) {
				response.add(new ResponseMessageGeneric(e.getMessage()));
			} catch (FileNotFoundException e) {
				response.add(new ResponseMessageGeneric(e.getMessage()));
			}

		} catch (TransformException te) {
			throw new MicroEngineException(
					"Error determing lat/lon for KML file.", te);
		} catch (FactoryException fe) {
			throw new MicroEngineException(
					"Error determing lat/lon for KML file.", fe);
		}

		return fileOut.toURI();
	}

	private Point[] determineBoundsByGeometry(GridGeometry2D aGeom)
			throws InvalidGridGeometryException, FactoryException,
			TransformException {
		Envelope2D env = gridGeometry.getEnvelope2D();
		GeometryFactory gf = new GeometryFactory();

		// Google Earth must have both the image and the KML coordinates in
		// a cylindrical equidistant projection
		MathTransform toLatLon = MapUtil.getTransformToLatLon(gridGeometry
				.getCoordinateReferenceSystem());
		double[] p1 = { env.getMinX(), env.getMinY() };
		double[] p2 = { env.getMaxX(), env.getMaxY() };

		toLatLon.transform(p1, 0, p1, 0, 1);
		toLatLon.transform(p2, 0, p2, 0, 1);
		Point[] p = new Point[2];
		p[0] = gf.createPoint(new Coordinate(p1[0], p1[1]));
		p[1] = gf.createPoint(new Coordinate(p2[0], p2[1]));

		return p;
	}

	private Point[] determineBoundsByEnvelope(Envelope anEnv) {
		GeometryFactory gf = new GeometryFactory();
		Point[] p = new Point[2];
		p[0] = gf.createPoint(new Coordinate(anEnv.getMinX(), anEnv.getMinY()));
		p[1] = gf.createPoint(new Coordinate(anEnv.getMaxX(), anEnv.getMaxY()));

		return p;
	}

	/**
	 * Creates the KML file on the file system
	 * 
	 * @param aKmlFile
	 *            the file to create
	 * @param aFilename
	 *            the path to the file to create
	 */
	private void generateKmlFile(KmlType aKmlFile, String aFilename) {
		FileOutputStream outStream = null;

		/*
		 * try { if (aKmlFile.getContainers().size() > 0) { aKmlFile =
		 * generateVectorKml(aKmlFile); outStream = new FileOutputStream(new
		 * File(URI.create(aFilename + ".kml"))); IBindingFactory bfact =
		 * BindingDirectory .getFactory(KmlFile.class); IMarshallingContext ctx =
		 * bfact.createMarshallingContext(); ctx.marshalDocument(aKmlFile,
		 * "UTF-8", null, outStream); } } catch (JiBXException je) { throw new
		 * MicroEngineException("Unable to write KML data to file.", je); }
		 * catch (IOException ie) { throw new MicroEngineException("Unable to
		 * write KML data to file.", ie); } finally { if (outStream != null) {
		 * try { outStream.close(); } catch (IOException e) {
		 * logger.error("Unable to close output stream", e); } } }
		 */
	}

	/**
	 * Adds the vector data stored in the task to the KML File
	 * 
	 * @param aKmlFile
	 *            the kml file to add vector data to
	 * @return the KmlFile with the vector data added
	 */
	// private KmlFile generateVectorKml(KmlFile aKmlFile) {
	// for (int i = 0; i < vectorData.size(); i++) {
	// aKmlFile.getContainers().get(0).getFeatures()
	// .add(vectorData.get(i));
	// }
	// return aKmlFile;
	// }
	/**
	 * Adds a point to the KmlFile
	 * 
	 * @param aName
	 *            the name of the point
	 * @param aValue
	 *            a lon/lat point e.g. -122.366212,37.818977
	 */
	public void addPoint(String aName, String aValue) {
		PlacemarkType placemark = new PlacemarkType();
		placemark.setName(aName);
		/*
		 * com.raytheon.edex.kml.geom.Point p = new
		 * com.raytheon.edex.kml.geom.Point(); Coordinates coords = new
		 * Coordinates(); coords.setCoordinates(aValue);
		 * p.setCoordinates(coords); placemark.setGeometry(p); if
		 * (!vectorData.contains(placemark)) { vectorData.add(placemark); }
		 */
	}

	/**
	 * Adds a line string to the KmlFile
	 * 
	 * @param aName
	 *            the name of the line string
	 * @param aValue
	 *            the coordinates of the line string separated by whitespace
	 *            e.g. -122.366212,37.818977 -122.365424,37.819294
	 */
	public void addLineString(String aName, String aValue) {
		/*
		 * Placemark placemark = new Placemark(); placemark.setName(aName);
		 * LineString ls = new LineString(); Coordinates coords = new
		 * Coordinates(); coords.setCoordinates(aValue);
		 * ls.setCoordinates(coords); GeometryElements elements = new
		 * GeometryElements();
		 * elements.setAltitudeMode(AltitudeModeEnum.relativeToGround);
		 * ls.setElements(elements); placemark.setGeometry(ls); if
		 * (!vectorData.contains(placemark)) { vectorData.add(placemark); }
		 */
	}

	/**
	 * Adds a linear ring to the KmlFile
	 * 
	 * @param aName
	 *            the name of the linear ring
	 * @param aValue
	 *            the coordinates of the linear ring separated by whitespace.
	 *            The last point should match the first point.
	 */
	public void addLinearRing(String aName, String aValue) {
		/*
		 * Placemark placemark = new Placemark(); placemark.setName(aName);
		 * LinearRing lr = new LinearRing(); Coordinates coords = new
		 * Coordinates(); coords.setCoordinates(aValue);
		 * lr.setCoordinates(coords); placemark.setGeometry(lr); if
		 * (!vectorData.contains(placemark)) { vectorData.add(placemark); }
		 */
	}

	/**
	 * Adds a polygon to the KmlFile
	 * 
	 * @param aName
	 *            the name of the polygon
	 * @param aValue
	 *            the coordinates of the polygon separated by whitespace. The
	 *            last point should match the first point.
	 */
	public void addPolygon(String aName, String aValue) {
		/*
		 * Placemark placemark = new Placemark(); placemark.setName(aName);
		 * Polygon poly = new Polygon(); Coordinates coords = new Coordinates();
		 * coords.setCoordinates(aValue); LinearRing outerBoundary = new
		 * LinearRing(); outerBoundary.setCoordinates(coords);
		 * poly.setOuterBoundaryIs(outerBoundary); placemark.setGeometry(poly);
		 * if (!vectorData.contains(placemark)) { vectorData.add(placemark); }
		 */
	}

	public GridGeometry2D getGridGeometry() {
		return gridGeometry;
	}

	public void setGridGeometry(GridGeometry2D aGridGeometry) {
		gridGeometry = aGridGeometry;
	}

	public File getImageFile() {
		return imageFile;
	}

	public void setUri(File anImageFile) {
		imageFile = anImageFile;
	}

	public ArrayList<PlacemarkType> getVectorData() {
		return vectorData;
	}

	public void setVectorData(ArrayList<PlacemarkType> aVectorData) {
		vectorData = aVectorData;
	}

}
