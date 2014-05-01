
package gov.noaa.nws.ncep.common.dataplugin.geomag.table;

import java.io.File;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * This object contains a list of magnetometer stations. 
 * An object of this class is used by JAXB when marshaling/unmarshaling a list of GeoMagStations
 * to/from an XML file.
 *
 *<pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer     Description
 * ------------ ---------- ----------- --------------------------
 * 03/29/2013   975        sgurung     Initial Creation 
 * </pre>
 * 
 * @author sgurung
 * @version 1
 */
@XmlRootElement(name = "GeoMagStationList")
@XmlAccessorType(XmlAccessType.NONE)
public class GeoMagStationList implements ISerializableObject {

	/**
	 * A list of the available GeoMagStaions 
	 */
	@XmlElement(name="geoMagStation")
	private ArrayList<GeoMagStation> stationList;
	
	/**
	 * Default constructor.
	 */
	public GeoMagStationList() {
		stationList = new ArrayList<GeoMagStation>();
	}
		
	/**
	 * Gets the list of GeoMagStaions
	 * @return the stationList
	 */
	public ArrayList<GeoMagStation> getGeoMagStationList() {
		return stationList;
	}

	 /**
     * 
     * @param file
     * @return
     * @throws JAXBException
     */
    public static GeoMagStationList fromFile(File file) throws JAXBException {
    	GeoMagStationList gml = null;

        JAXBContext ctx = JAXBContext.newInstance(GeoMagStationList.class);
        if (ctx != null) {
            Unmarshaller um = ctx.createUnmarshaller();
            if (um != null) {
            	gml = (GeoMagStationList) um.unmarshal(file);
            }
        }

        return gml;
    }

    /**
     * 
     * @param is
     * @return
     * @throws JAXBException
     */
    public static GeoMagStationList fromStream(InputStream is)
            throws JAXBException {
    	GeoMagStationList gml = null;

        JAXBContext ctx = JAXBContext.newInstance(GeoMagStationList.class);
        if (ctx != null) {
            Unmarshaller um = ctx.createUnmarshaller();
            if (um != null) {
            	gml = (GeoMagStationList) um.unmarshal(is);
            }
        }

        return gml;
    }
}
