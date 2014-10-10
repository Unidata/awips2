/**
 * This code has unlimited rights, and is provided "as is" by the National Centers 
 * for Environmental Prediction, without warranty of any kind, either expressed or implied, 
 * including but not limited to the implied warranties of merchantability and/or fitness 
 * for a particular purpose.
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 **/
package gov.noaa.nws.ncep.common.dataplugin.geomag.table;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * <pre>
 * 
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer     Description
 * ------------ ---------- ----------- --------------------------
 * 04/29/2014   R4078       sgurung      Initial Creation
 * </pre>
 * 
 * @author sgurung
 * @version 1
 */
public class GeoMagStationTableReaderWriter {
    private final String PACKAGE = "gov.noaa.nws.ncep.common.dataplugin.geomag.table";

    /** The logger */
    protected transient Log logger = LogFactory.getLog(getClass());

    private String xmlFile = null;

    public GeoMagStationTableReaderWriter(String file) {
        xmlFile = file;
    }

    public List<GeoMagStation> readGeoMagStationList() throws JAXBException {

        JAXBContext context = JAXBContext.newInstance(PACKAGE);
        Unmarshaller unmarshaller = context.createUnmarshaller();
        GeoMagStationList sfstnlist = null;

        File file = new File(xmlFile);

        try {
            if (file.exists()) {
                sfstnlist = (GeoMagStationList) unmarshaller
                        .unmarshal(new FileReader(xmlFile));
                List<GeoMagStation> listOfItems = sfstnlist
                        .getGeoMagStationList();
                return listOfItems;
            }
        } catch (FileNotFoundException e) {
            logger.error(xmlFile + " not found!", e);
        }

        return null;

    }

    public void writeGeoMagStationList(GeoMagStationList sfstnlist)
            throws JAXBException {

        JAXBContext context = JAXBContext.newInstance(PACKAGE);
        Marshaller marshaller = context.createMarshaller();

        File file = new File(xmlFile);

        try {
            if (file.exists()) {
                marshaller.marshal(sfstnlist, file);
            }

        } catch (JAXBException e) {
            logger.error("Error marshalling " + xmlFile, e);
        }

    }
}
