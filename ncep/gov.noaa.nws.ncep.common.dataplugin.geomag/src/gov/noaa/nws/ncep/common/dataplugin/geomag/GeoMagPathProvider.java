package gov.noaa.nws.ncep.common.dataplugin.geomag;

import java.util.Date;

import com.raytheon.uf.common.dataplugin.persist.DefaultPathProvider;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;

public class GeoMagPathProvider extends DefaultPathProvider {
    private static GeoMagPathProvider instance = new GeoMagPathProvider();

    public static GeoMagPathProvider getInstance() {
        return instance;
    }

    protected GeoMagPathProvider() {

    }

    @Override
    public String getHDFFileName(String pluginName, IPersistable persistable) {
        if (persistable == null) {
            throw new IllegalArgumentException(
                    "Expected argument persistable is null");
        }

        if (!(persistable instanceof GeoMagRecord)) {
            throw new IllegalArgumentException(
                    "Argument persistable is of wrong type. Expected "
                            + GeoMagRecord.class + " but got "
                            + persistable.getClass());
        }

        if (pluginName == null) {
            throw new IllegalArgumentException(
                    "Expected argument pluginName not set on object "
                            + persistable.toString());
        }

        GeoMagRecord pdo = (GeoMagRecord) persistable;
        StringBuffer sb = new StringBuffer(64);
        sb.append(pluginName);

        if (pdo.getDataTime() != null) {
            Date time = (Date) pdo.getDataTime().getRefTime().clone();
            time.setHours(0);

            sb.append(fileNameFormat.get().format(time));
            sb.append(".h5");
        }
        return sb.toString();
    }
}
