package gov.noaa.nws.ncep.viz.rsc.solarimage.rsc;

import gov.noaa.nws.ncep.common.dataplugin.solarimage.SolarImageRecord;
//import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;
//import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.HDF5Util;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "SolarImageResourceData")
public class SolarImageResourceData extends AbstractRequestableResourceData {

    public Map<DataTime, SolarImageRecord> dataObjectMap;

    private static String GOESSXI = "SXI-FM3";

    private static String LASCO = "LASCO";

    private static String STEREO = "SECCHI";

    public SolarImageResourceData() {

        super();
        this.nameGenerator = new AbstractNameGenerator() {

            @Override
            public String getName(AbstractVizResource<?, ?> resource) {

                try {
                    if (metadataMap.get("telescope").getConstraintValue()
                            .equals("NSO-GONG")) {
                        return "H-alpha NSO/GONG";
                    }

                    String instr = metadataMap.get("instrument")
                            .getConstraintValue();

                    SolarImageRecord solarimagerec = null;
                    for (SolarImageRecord rec : dataObjectMap.values()) {
                        solarimagerec = rec;
                        break;
                    }
                    StringBuilder sb = new StringBuilder();
                    sb.append(solarimagerec.getSatellite() + "  ");
                    sb.append(solarimagerec.getInstrument() + "  ");
                  
                    sb.append(solarimagerec.getWavelength() + "  ");

                    if (instr.equals(STEREO) || instr.equals(LASCO)) {
                        ;
                    } else {
                        sb.append(solarimagerec.getWavelength() + "  ");
                    }

                    if (instr.equals(GOESSXI))
                        sb.append(solarimagerec.getIntTime() + "s");
                    return sb.toString();
                } catch (Exception e) {
                    return "";
                }
            }

        };
    }

    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
            throws VizException {

        dataObjectMap = new HashMap<DataTime, SolarImageRecord>();
        System.out.println("Cconstructing dataObjectMap");
        for (PluginDataObject pdo : objects) {
            if (pdo instanceof SolarImageRecord) {
                System.out.println("GOT:::::::::: " + pdo.getDataTime());
                dataObjectMap.put(pdo.getDataTime(), (SolarImageRecord) pdo);
            }
        }
        return new SolarImageResource(this, loadProperties);
    }

    public SolarImageRecord populateRecord(SolarImageRecord record) {
        IDataStore dataStore = getDataStore(record);
        record.retrieveFromDataStore(dataStore);
        return record;
    }

    private IDataStore getDataStore(SolarImageRecord record) {
        IDataStore dataStore = null;
        // try {
        // Map<String, Object> vals = new HashMap<String, Object>();
        // vals.put("dataURI", record.getDataURI());
        // vals.put("pluginName", record.getPluginName());

        // record = (SolarImageRecord) Loader.loadData(vals);

        File loc = HDF5Util.findHDF5Location(record);
        dataStore = DataStoreFactory.getDataStore(loc);

        // } catch (VizException e) {
        // e.printStackTrace();
        // }

        return dataStore;
    }

}
