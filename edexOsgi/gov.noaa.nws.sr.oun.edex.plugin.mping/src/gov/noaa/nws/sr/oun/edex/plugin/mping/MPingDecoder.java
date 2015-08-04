package gov.noaa.nws.sr.oun.edex.plugin.mping;

import gov.noaa.nws.sr.oun.dataplugin.mping.MPingReport;
import gov.noaa.nws.sr.oun.edex.plugin.mping.dao.MPingReportDao;

import java.io.File;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.dom4j.Document;
import org.dom4j.DocumentException;
import org.dom4j.DocumentHelper;
import org.dom4j.Element;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataDescription;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;

/**
 * Decoder for mPING Reports
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 25, 2015            aanderson   Initial creation of history
 * </pre>
 * 
 * @author Aaron Anderson
 * @version 1.0
 */
public class MPingDecoder extends AbstractDecoder {

	private static IUFStatusHandler logger = UFStatus
			.getHandler(MPingDecoder.class);

	GeometryFactory geomFact = new GeometryFactory();

	DateFormat reportTimeFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ssX");

	MPingReportDao dao;

	private PointDataDescription pointDataDescription = null;

	private Map<File, PointDataContainer> containerMap = new HashMap<File, PointDataContainer>();
	private final String pluginName = "mping";

	public MPingDecoder() {
		try {
			pointDataDescription = PointDataDescription
					.fromStream(this.getClass().getResourceAsStream(
							"/res/pointdata/mping.xml"));
			dao = new MPingReportDao(pluginName);
			logger.info("PointDataDescription loaded");

		} catch (Exception e) {
			logger.error("Problem loading point Data Description "
					+ e.getMessage());

		}

	}

	public PluginDataObject[] decode(String input) throws Exception {
		logger.info("Starting Decode of mPING xml");
		ArrayList<MPingReport> list = new ArrayList<MPingReport>();
		try {
			// Parse the text input into a Document so that we can extract out
			// the root element.
			Document document = DocumentHelper.parseText(input);
			// Get root element of the document in xml
			Element e = document.getRootElement();
			// grab the results section of the kml file
			Element results = e.element("results");
			// iterate over the items in the Document section and only grab
			// report items.
			Iterator<Element> i = results.elementIterator("report");
			while (i.hasNext()) {
				// Grab next report
				Element report = i.next();
				try {
					// pull out the description_id tag
					Element description_id = report.element("description_id");
					// If there actually is mPING data then process it.
					if (description_id != null) {
						// Get individual items of report
						Element idElement = report.element("id");
						Element description = report.element("description");
						Element category = report.element("category");
						Element timeElement = report.element("obtime");
						Date reportTime = reportTimeFormat.parse(timeElement
								.getText());
						Element geom = report.element("geom");
						Element coordinates = geom.element("coordinates");
						Element lon = coordinates.element("longitude");
						Element lat = coordinates.element("latitude");
						
						// Create mping record
						MPingReport record = new MPingReport();

						// set the description and categories
						record.setDescription_id(Integer
								.parseInt(description_id.getText()));
						record.setDescription(description.getText());
						record.setCategory(category.getText());
						// Create new SurfaceObsLocation
						SurfaceObsLocation obsLoc = new SurfaceObsLocation();
						// Use id from mPING xml as stationid as this provides
						// for an always
						// unique value
						obsLoc.setStationId(idElement.getText());
						// Set geometry of report
						obsLoc.setGeometry(geomFact.createPoint(new Coordinate(
								Float.parseFloat(lon.getText()), Float
										.parseFloat(lat.getText()))));
						record.setLocation(obsLoc);

						// Add our DataTime
						record.setDataTime(new DataTime(reportTime));

						PointDataContainer pdc = getContainer(record);

						// Populate the point data.
						PointDataView view = pdc.append();
						view.setLong("description_id",
								record.getDescription_id());

						record.setPointDataView(view);

						// Add the record to the list
						list.add(record);
					}
				} catch (Exception ex) {
					logger.error("Error Creating Mping Record", ex);	
				}
			}
		} catch (DocumentException ex) {
			logger.error("Error parsing Mping XML", ex);
		}
		// Process the list and send back an array of PluginDataObjects
		return (list.toArray(new PluginDataObject[list.size()]));
	}

	public MPingReportDao getDao() {
		return dao;
	}

	public void setDao(MPingReportDao dao) {
		this.dao = dao;
	}

	/**
	 * 
	 * @param mpingData
	 * @return
	 */
	private PointDataContainer getContainer(MPingReport mpingData) {

		File file = dao.getFullFilePath(mpingData);
		PointDataContainer container = containerMap.get(file);
		if (container == null) {
			container = PointDataContainer.build(pointDataDescription);
			containerMap.put(file, container);
		}
		return container;
	}

}
