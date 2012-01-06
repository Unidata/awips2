package mhs.core;

import java.io.File;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

public class RsyncThread extends Thread {

    private Properties props;

    private Map<String, Long> fileVersion;

    public RsyncThread(Properties props) {
        this.props = props;
        fileVersion = new HashMap<String, Long>();
        this.setDaemon(true);
    }

    public void run() {
        while (true) {
            try {
                Thread.sleep(1000);
            } catch (Exception e) {
                e.printStackTrace();
            }

            String exportGridsDir = (String) props.getProperty("EXPORT_GRIDS");
            String centralServerDir = (String) props
                    .getProperty("CENTRAL_SERVER");
            File[] fileList = new File(exportGridsDir).listFiles();

            String currentFilePath = null;
            for (File file : fileList) {
                if (file.isDirectory()) {
                    continue;
                }
                currentFilePath = file.getPath();

                boolean copy = false;
                if (fileVersion.containsKey(currentFilePath)) {
                    if (fileVersion.get(currentFilePath).longValue() != file
                            .lastModified()) {
                        copy = true;
                    }
                } else {
                    copy = true;
                }
                if (copy) {
                    String[] copyCmd = new String[] {
                            centralServerDir + "/../util/packageFile",file.getPath(),
                            centralServerDir + "/../util/",
                            file.getName().substring(0, 3), centralServerDir };
                    try {
                        Runtime.getRuntime().exec(copyCmd);
                        fileVersion.put(file.getPath(), file.lastModified());
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }
            }
        }
    }
}
