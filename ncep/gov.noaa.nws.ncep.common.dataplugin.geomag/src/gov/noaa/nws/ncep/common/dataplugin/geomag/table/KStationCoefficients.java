package gov.noaa.nws.ncep.common.dataplugin.geomag.table;

import java.io.File;
import java.io.InputStream;
import java.util.ArrayList;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/*
 * The KStationCoefficients.
 * 
 * <pre>
 * SOFTWARE HISTORY
 *                   
 * ate          Ticket#     Engineer   Description
 * -----------  ----------  ---------- --------------------------
 * 05/14/2013   #989        qzhou      Initial Creation
 * </pre>
 * 
 * @author qzhou
 * @version 1
 */

@XmlRootElement(name = "kStationCoefficients")
@XmlAccessorType(XmlAccessType.NONE)
public class KStationCoefficients implements ISerializableObject{

		/**
		 * A list of the available KStationCoefficients 
		 */
		@XmlElement(name="kStationCoefficient")
		private ArrayList<KStationCoefficient> stationList;

		/**
		 * Default constructor.
		 */
		public KStationCoefficients() {
			stationList = new ArrayList<KStationCoefficient>();
		}
			
		/**
		 * Gets the list of KStationCoefficients
		 * @return the stationList
		 */
		public ArrayList<KStationCoefficient> getKStationCoefficients() {
			return stationList;
		}

		 /**
	     * 
	     * @param file
	     * @return
	     * @throws JAXBException
	     */
	    public static KStationCoefficients fromFile(File file) throws JAXBException {
	    	KStationCoefficients gml = null;
	    	
	        JAXBContext ctx = JAXBContext.newInstance(KStationCoefficients.class);
	        if (ctx != null) {
	            Unmarshaller um = ctx.createUnmarshaller();
	            if (um != null) {
	            	gml = (KStationCoefficients) um.unmarshal(file);
	            }
	        }
	        //System.out.println("*****fromFile"+gml.getK9Limit());
	        return gml;
	    }

	    /**
	     * 
	     * @param is
	     * @return
	     * @throws JAXBException
	     */
	    public static KStationCoefficients fromStream(InputStream is)
	            throws JAXBException {
	    	KStationCoefficients gml = null;
	    	
	        JAXBContext ctx = JAXBContext.newInstance(KStationCoefficients.class);
	        if (ctx != null) {
	            Unmarshaller um = ctx.createUnmarshaller();
	            if (um != null) {
	            	gml = (KStationCoefficients) um.unmarshal(is);
	            }
	        }

	        return gml;
	    }
}

