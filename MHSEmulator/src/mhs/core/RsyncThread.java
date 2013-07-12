package mhs.core;

import java.io.File;
import java.io.FileFilter;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Properties;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ??? ??  ????            bphillip     Initial creation
 * Jul 15, 2013  #2099     dgilling     Modify to support recursive file listing
 *                                      since export grids dir structure uses
 *                                      multiple folders.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class RsyncThread implements Runnable {

    private static final Map<String, Long> fileVersion = new HashMap<String, Long>();

    private Properties props;

    public RsyncThread(Properties props) {
        this.props = props;
    }

    @Override
    public void run() {
        String exportGridsDir = props.getProperty("EXPORT_GRIDS");
        String centralServerDir = props.getProperty("CENTRAL_SERVER");
        String packScriptDir = props.getProperty("UTIL_DIR");

        Collection<File> fileList = listCdfFiles(new File(exportGridsDir));
        for (File file : fileList) {
            if (file.isFile()) {
                String currentFilePath = file.getPath();

                boolean copy = true;
                if ((fileVersion.containsKey(currentFilePath))
                        && (fileVersion.get(currentFilePath) >= file
                                .lastModified())) {
                    copy = false;
                }

                if (copy) {
                    String[] copyCmd = new String[] {
                            packScriptDir + "/packageFile", currentFilePath,
                            packScriptDir, file.getName().substring(0, 3),
                            centralServerDir };
                    try {
                        Runtime.getRuntime().exec(copyCmd);
                        fileVersion.put(currentFilePath, file.lastModified());
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }
            }
        }
    }

    private Collection<File> listCdfFiles(File path) {
        Collection<File> fileList = new LinkedList<File>();
        FileFilter cdfFilter = new FileFilter() {

            @Override
            public boolean accept(File pathname) {
                return (pathname.isDirectory() || pathname.getName().endsWith(
                        ".netcdf"));
            }
        };
        innerListFiles(path, fileList, cdfFilter);
        return fileList;
    }

    private void innerListFiles(File path, Collection<File> fileList,
            FileFilter filter) {
        try {
            File[] matchingFiles = path.listFiles(filter);
            for (File file : matchingFiles) {
                if (file.isDirectory()) {
                    innerListFiles(file, fileList, filter);
                } else if (file.isFile()) {
                    fileList.add(file);
                }
            }
        } catch (SecurityException e) {
            e.printStackTrace();
        }
    }
}
