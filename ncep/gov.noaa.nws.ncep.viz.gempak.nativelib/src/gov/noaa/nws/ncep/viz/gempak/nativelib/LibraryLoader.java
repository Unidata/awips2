package gov.noaa.nws.ncep.viz.gempak.nativelib;

import java.io.FileNotFoundException;
import java.net.URL;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Path;
import org.osgi.framework.Bundle;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Utility to load a specific native library
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- -----------------------------------------
 * --/--/----                         Initial Creation
 * Mar 20, 2014  2919     njensen     Safety checks, better error messages
 * 
 * </pre>
 * 
 */

public class LibraryLoader {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(LibraryLoader.class);

    public static void load(String libName) {
        URL url = null;
        Path path = null;
        try {
            Bundle b = Activator.getDefault().getBundle();
            path = new Path(System.mapLibraryName(libName));
            url = FileLocator.find(b, path, null);
            if (url == null) {
                throw new FileNotFoundException("Unable to locate "
                        + path.toString());
            }
            url = FileLocator.resolve(url);
            System.load(url.getPath());
        } catch (Exception e) {
            String msg = "Could not load native Library: ";
            if (url != null) {
                msg += url.getFile();
            } else {
                msg += path.toString();
            }
            statusHandler.handle(Priority.PROBLEM, msg, e);
        }
    }
}
