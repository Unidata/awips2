package gov.noaa.nws.ncep.viz.gempak.nativelib;

import java.net.URL;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Path;
import org.osgi.framework.Bundle;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

public class LibraryLoader {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(LibraryLoader.class);

    public static void load(String libName) {
        URL url = null;
        try {
            Bundle b = Activator.getDefault().getBundle();
            url = FileLocator.find(b, new Path(System.mapLibraryName(libName)),
                    null);
            url = FileLocator.resolve(url);
            System.load(url.getPath());
        } catch (Exception e) {
            String msg = "Could not Load native Library: " + url.getFile();
            statusHandler.handle(Priority.PROBLEM, msg, e);
        }
    }
}
