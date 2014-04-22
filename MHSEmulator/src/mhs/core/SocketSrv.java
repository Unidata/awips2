package mhs.core;

import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.UnknownHostException;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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
 * Jul 15, 2013  #2009     dgilling     Code cleanup.
 * Jul 23, 2013  #2009     dgilling     Fix NullPointerException on start up.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class SocketSrv {

    private File propertiesFile;

    private boolean runRsync;

    private String myMHS;

    private Properties serverProps;

    private ExecutorService mhsRequestHandler;

    private ScheduledExecutorService rsyncThread;

    private int fileIndex = 0;

    private String fileBase;

    private String configDir;

    private String centralServerDir;

    private String binDir;

    private List<String> files = new ArrayList<String>();

    private Map<Integer, String> commandMap;

    private int serverPort;

    public static void main(String[] args) {
        if (!System.getProperty("user.name").equals("root")) {
            System.out
                    .println("Socket Server must be run as root user!  Current user: "
                            + System.getProperty("user.name"));
            System.exit(1);
        }

        File propertiesFile = new File(args[0]);
        String mhsId = args[1];
        boolean startRsync = Boolean.parseBoolean(args[2]);

        if (!propertiesFile.isFile()) {
            System.out.println("Specified properties file ["
                    + propertiesFile.toString() + "] does not exist. Exiting.");
            System.exit(1);
        }

        try {
            final SocketSrv server = new SocketSrv(propertiesFile, mhsId,
                    startRsync);
            Runtime.getRuntime().addShutdownHook(new Thread() {

                @Override
                public void run() {
                    server.shutdown();
                }
            });
            server.run();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private SocketSrv(File propertiesFile, String mhsId, boolean startRsync)
            throws UnknownHostException, IOException {
        System.out.println("Setting up server ("
                + InetAddress.getLocalHost().getCanonicalHostName() + ")");

        this.propertiesFile = propertiesFile;
        this.myMHS = mhsId;
        this.runRsync = startRsync;

        writeMyMHS(myMHS);

        this.commandMap = new HashMap<Integer, String>();
        this.configDir = this.propertiesFile.getParent();
        loadProperties();
        loadRcvHandlerTable();

        this.fileBase = serverProps.getProperty("DATA_FOLDER");
        this.centralServerDir = serverProps.getProperty("CENTRAL_SERVER");
        this.binDir = serverProps.getProperty("UTIL_DIR");
        this.serverPort = Integer.parseInt(serverProps
                .getProperty("SERVER_PORT"));

        System.out.println("\tReceived Data directory: " + fileBase);
        System.out.println("\tCentral Server Data Directory: "
                + centralServerDir);
        System.out.println("\tConfig directory: " + configDir);

        this.mhsRequestHandler = Executors.newSingleThreadExecutor();
        if (this.runRsync) {
            System.out.println("Starting Rsync Thread...");
            this.rsyncThread = Executors.newSingleThreadScheduledExecutor();
        }
    }

    public void run() throws IOException {
        if (rsyncThread != null) {
            Runnable rsyncJob = new RsyncThread(serverProps);
            rsyncThread
                    .scheduleWithFixedDelay(rsyncJob, 1, 1, TimeUnit.SECONDS);
        }

        ServerSocket socket = new ServerSocket(serverPort);
        while (!mhsRequestHandler.isShutdown()) {
            try {
                log("Waiting for connections...");
                final Socket conn = socket.accept();
                Runnable processTask = new Runnable() {

                    @Override
                    public void run() {
                        try {
                            handleRequest(conn);
                        } catch (Exception e) {
                            e.printStackTrace();
                        }
                    }
                };
                mhsRequestHandler.execute(processTask);
            } catch (RejectedExecutionException e) {
                if (!mhsRequestHandler.isShutdown()) {
                    e.printStackTrace();
                }
            }
        }
    }

    public void shutdown() {
        mhsRequestHandler.shutdown();
        if (rsyncThread != null) {
            rsyncThread.shutdown();
        }
    }

    private void handleRequest(Socket connection) throws IOException,
            InterruptedException {
        InetSocketAddress client = (InetSocketAddress) connection
                .getRemoteSocketAddress();
        log("Connected to client: " + client.getHostName() + " at " + client);

        loadProperties();
        String sender = getMhsOfSender(client);
        log("Message is from: " + sender);

        InputStream in = connection.getInputStream();
        byte[] message = null;
        Map<String, String> params = new HashMap<String, String>();
        String flag = "";
        while (true) {
            if (in.available() == 0) {
                Thread.sleep(100);
                continue;
            }

            message = getMessage(in);

            if (!flag.equals("-file")) {
                String strMessage = new String(message);
                if (strMessage.equals(MhsUtil.END_TOKEN)) {
                    log("Disconnected from  client: " + client);
                    if (params.containsKey("-c")) {
                        executeAction(sender, params);
                        files.clear();
                        params.clear();
                        flag = "";
                    }
                    break;
                }
                if (strMessage.startsWith("-")) {
                    flag = strMessage;
                } else {
                    params.put(flag, strMessage);
                }
            } else {
                log("File Received of size: " + message.length);
                files.add(writeToFile(myMHS + "-" + params.get("-MSGID"),
                        message));
                flag = "";
            }
        }
    }

    private void writeMyMHS(String myMHS) throws IOException {
        BufferedWriter out = null;
        try {
            out = new BufferedWriter(new FileWriter(MhsUtil.MY_MHS_FILE));
            out.write(myMHS + "\n");
            out.write(propertiesFile.getPath());
        } finally {
            if (out != null) {
                out.close();
            }
        }
    }

    private void loadProperties() throws IOException {
        FileInputStream fis = null;
        try {
            fis = new FileInputStream(propertiesFile);
            Properties newProps = new Properties();
            newProps.load(fis);
            serverProps = newProps;
        } finally {
            if (fis != null) {
                fis.close();
            }
        }
    }

    private void executeAction(String sender, Map<String, String> params)
            throws IOException, InterruptedException {
        int action = Integer.parseInt(params.get("-c"));

        if (!commandMap.containsKey(action)) {
            log("No action mapped for command: " + action);
            return;
        }

        String fileList = "";
        for (int i = 0; i < files.size(); i++) {
            fileList += files.get(i);
            if (i != (files.size() - 1)) {
                fileList += ",";
            }
        }
        String command = commandMap.get(action);

        Pattern pat = Pattern.compile("%ENCLOSE\\(([0-9]{1,5})\\)");
        Matcher matcher = pat.matcher(command);
        while (matcher.find()) {
            int fileNumber = Integer.parseInt(matcher.group(1));
            String encloseGroup = matcher.group();
            command = command.replace(encloseGroup, files.get(fileNumber - 1));
        }

        command = command.replace("%MSGID", myMHS + "-" + params.get("-MSGID"));
        if (params.containsKey("-s")) {
            command = command.replace("%SUBJECT", params.get("-s"));
        }
        if (!fileList.isEmpty()) {
            command = command.replace("%ENCLIST", fileList);
        }
        command = command.replace("%CENTRAL_SERVER", centralServerDir);
        command = command.replace("%BIN_DIR", binDir);
        command = command.replace("%DATA_DIR", fileBase);
        command = command.replace("%SENDER", sender.toLowerCase());
        String[] cmdArray = command.split(" ");
        log("Executing: " + command);

        Process p = null;
        try {
            p = Runtime.getRuntime().exec(cmdArray);
            p.waitFor();
        } finally {
            if (p != null) {
                p.destroy();
            }
        }
    }

    private byte[] getMessage(InputStream in) throws IOException {
        byte[] sizeBytes = new byte[4];
        readBytes(in, sizeBytes);
        int expectedSize = MhsUtil.byteArrayToInt(sizeBytes, 0);
        byte[] message = new byte[expectedSize];
        readBytes(in, message);
        return message;
    }

    private void readBytes(InputStream in, byte[] bytes) throws IOException {
        int expectedSize = bytes.length;
        int bytesRead = 0;
        int totalBytesRead = 0;
        while (totalBytesRead < expectedSize) {
            bytesRead = in.read(bytes, totalBytesRead, expectedSize
                    - totalBytesRead);
            totalBytesRead += bytesRead;
        }
    }

    private String writeToFile(String fileName, byte[] contents)
            throws IOException, InterruptedException {
        String fileFQN = fileBase + fileName + "." + getFileIndex();
        log("Writing file: " + fileFQN);

        BufferedOutputStream out = null;
        try {
            out = new BufferedOutputStream(new FileOutputStream(new File(
                    fileFQN)));
            out.write(contents);
        } finally {
            if (out != null) {
                out.close();
            }
        }

        Process p = null;
        try {
            p = Runtime.getRuntime().exec(
                    new String[] { "/bin/chmod", "777", fileFQN });
            p.waitFor();
        } finally {
            if (p != null) {
                p.destroy();
            }
        }
        return fileFQN;
    }

    private String getFileIndex() {
        fileIndex++;
        if (fileIndex == 1000) {
            fileIndex = 0;
        }

        NumberFormat formatter = new DecimalFormat("000");
        return formatter.format(fileIndex);
    }

    private void loadRcvHandlerTable() throws IOException {
        BufferedReader in = null;
        try {
            in = new BufferedReader(new FileReader(configDir + File.separator
                    + "rcv_handler.tbl"));

            String str = null;
            while ((str = in.readLine()) != null) {
                StringBuilder commandBuilder = new StringBuilder();
                String[] tokens = str.split(" ");
                for (int i = 1; i < tokens.length; i++) {
                    String cmd = tokens[i].trim();
                    if (!cmd.isEmpty()) {
                        if (i != 1) {
                            commandBuilder.append(' ');
                        }
                        commandBuilder.append(cmd);
                    }
                }

                String commandString = commandBuilder.toString().trim();
                commandMap.put(Integer.parseInt(tokens[0]), commandString);
            }
        } finally {
            if (in != null) {
                in.close();
            }
        }
    }

    private String getMhsOfSender(InetSocketAddress address) {
        String hostAddress = address.getAddress().getHostAddress();
        for (String mhsId : serverProps.stringPropertyNames()) {
            String value = serverProps.getProperty(mhsId);
            if (value.contains(",")) {
                String[] addrs = value.split(",");
                for (String addr : addrs) {
                    if (addr.contains(hostAddress)) {
                        return mhsId;
                    }
                }
            } else {
                if (value.contains(hostAddress)) {
                    return mhsId;
                }
            }
        }
        return null;
    }

    private void log(Object... msg) {
        MhsUtil.logMsg(serverProps.getProperty("LOG_DIR"), "server", msg);
    }
}
