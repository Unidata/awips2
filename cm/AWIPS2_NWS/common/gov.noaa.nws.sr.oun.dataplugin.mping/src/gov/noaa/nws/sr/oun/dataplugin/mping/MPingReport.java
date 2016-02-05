package gov.noaa.nws.sr.oun.dataplugin.mping;

import java.util.Calendar;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.pointdata.IPointData;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Geometry;
/**
 * Record implementation for mPING Reports
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
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "mpingseq")
@Table(name = "mping", uniqueConstraints = { @UniqueConstraint(columnNames = { 
		"latitude", "longitude", "stationId", "refTime"}) })
@DynamicSerialize
public class MPingReport extends PersistablePluginDataObject implements
ISpatialEnabled, IPointData, IPersistable {
	private static final long serialVersionUID = 1L;
	
	@Embedded
	@DataURI(position = 1, embedded = true)
	@DynamicSerializeElement
	private SurfaceObsLocation location;

	@Embedded
	@DynamicSerializeElement
	private PointDataView pointDataView;
	
	@Transient
	@DynamicSerializeElement
	private String provider = null;
	
	@Transient
	@DynamicSerializeElement
	private String source = null;
	
	@Transient
	@DynamicSerializeElement
	int description_id;
	
	@DynamicSerializeElement
	String description;
	
	@DynamicSerializeElement
	String category;
	
	public MPingReport() {
		super();
	}

	public MPingReport(String dataUri) {
		super(dataUri);
	}

	public String getProvider() {
		return provider;
	}

	public void setProvider(String provider) {
		this.provider = provider;
	}

	public String getSource() {
		return source;
	}

	public void setSource(String source) {
		this.source = source;
	}

	public int getDescription_id() {
		return description_id;
	}

	public void setDescription_id(int description_id) {
		this.description_id = description_id;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getCategory() {
		return category;
	}

	public void setCategory(String category) {
		this.category = category;
	}

	@Override
	public SurfaceObsLocation getSpatialObject() {
		return location;
	}

	public SurfaceObsLocation getLocation() {
		return location;
	}

	public void setLocation(SurfaceObsLocation location) {
		this.location = location;
	}

	/**
	 * Get this observation's geometry.
	 * 
	 * @return The geometry for this observation.
	 */
	public Geometry getGeometry() {
		return location.getGeometry();
	}

	/**
	 * Get the geometry latitude.
	 * 
	 * @return The geometry latitude.
	 */
	public double getLatitude() {
		return location.getLatitude();
	}

	/**
	 * Get the geometry longitude.
	 * 
	 * @return The geometry longitude.
	 */
	public double getLongitude() {
		return location.getLongitude();
	}

	/**
	 * Get the station identifier for this observation.
	 * 
	 * @return the stationId
	 */
	public String getStationId() {
		return location.getStationId();
	}

	/**
	 * Get the elevation, in meters, of the observing platform or location.
	 * 
	 * @return The observation elevation, in meters.
	 */
	public Integer getElevation() {
		return location.getElevation();
	}

	/**
	 * Get whether the location for this observation is defined.
	 * 
	 * @return Is this location defined.
	 */
	public Boolean getLocationDefined() {
		return location.getLocationDefined();
	}

	@Override
	public PointDataView getPointDataView() {
		return pointDataView;
	}

	@Override
	public void setPointDataView(PointDataView pointDataView) {
		this.pointDataView = pointDataView;
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		Calendar c = getDataTime().getRefTimeAsCalendar();
		if (c != null) {
			sb.append(String.format("MPing:%1$tY%1$tm%1$td%1$tH%1$tM",
					getDataTime().getRefTimeAsCalendar()));
		} else {
			sb.append("MPing:YYYYMMDDHHmm");
		}
		sb.append(String.format("%6.2f %7.2f:", getLatitude(), getLongitude()));
		return sb.toString();
	}
	
	@SuppressWarnings("unchecked")
	@Override
    public void setDataURI(String dataURI) {
        super.setDataURI(dataURI);
        identifier = dataURI;
	}
	
	@Override
	public String getPluginName() {
		return "mping";
	}

	


}
