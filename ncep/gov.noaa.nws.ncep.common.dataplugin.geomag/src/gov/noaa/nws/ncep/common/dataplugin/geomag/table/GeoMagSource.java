package gov.noaa.nws.ncep.common.dataplugin.geomag.table;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
/**
 * Defines a magnetometer station.
 * 
 *<pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer     Description
 * ------------ ---------- ----------- --------------------------
 * 07/17/2013   975        qzhou       Initiate for reading source attributes
 * </pre>
 * 
 * @author qzhou
 * @version 1
 */
@XmlRootElement(name = "source")
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "")
public class GeoMagSource {

		@XmlAttribute
		private String name;
		
		@XmlAttribute
		private int priority;
				
		
		public String getName() {
			return name;
		}
		
		public void setName(String name) {
			this.name = name;
		}
			
		public int getPriority() {
			return priority;
		}
		
		public void setPriority(int priority) {
			this.priority = priority;
		}
		
}

