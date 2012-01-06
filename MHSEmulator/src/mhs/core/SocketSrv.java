package mhs.core;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.InputStream;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class SocketSrv {

    private int fileIndex = 0;

    private String fileBase;

    private Properties serverProps;

    private String configDir;

    private String centralServerDir;

    private String binDir;

    private List<String> files = new ArrayList<String>();

    private Map<Integer, String> commandMap;

    private RsyncThread rsync;

    private String propertiesFile;

    private String myMHS;

    public static void main(String[] args) {

        if (!System.getProperty("user.name").equals("root")) {
            System.out
                    .println("Socket Server must be run as root user!  Current user: "
                            + System.getProperty("user.name"));
            System.exit(1);
        }
        try {
            SocketSrv srv = null;
            srv = new SocketSrv(args[0], args[1], args[2]);
            srv.run();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private SocketSrv(String propertiesFile, String myMHS, String startRsync)
            throws Exception {
        this.propertiesFile = propertiesFile;
        this.myMHS = myMHS;

        writeMyMHS(myMHS);
        System.out.println("Setting up server ("
                + InetAddress.getLocalHost().getCanonicalHostName() + ")");
        commandMap = new HashMap<Integer, String>();
        configDir = propertiesFile.substring(0,
                propertiesFile.lastIndexOf(File.separator) + 1);
        loadProperties(true);
        if (startRsync.equals("true")) {
            System.out.println("Starting Rsync Thread...");
            rsync = new RsyncThread(serverProps);
            rsync.start();
            System.out.println("Rsync Thread started!");
        }

    }

    private void writeMyMHS(String myMHS) throws Exception {
        BufferedWriter out = new BufferedWriter(new FileWriter(
                MhsUtil.MY_MHS_FILE));
        out.write(myMHS + "\n");
        out.write(this.propertiesFile);
        out.close();
    }

    private void loadProperties(boolean print) throws Exception {
        serverProps = new Properties();
        FileInputStream fis = new FileInputStream(propertiesFile);
        serverProps.load(fis);
        fis.close();
        fileBase = serverProps.getProperty("DATA_FOLDER");
        centralServerDir = serverProps.getProperty("CENTRAL_SERVER");
        binDir = serverProps.getProperty("UTIL_DIR");
        loadRcvHandlerTable();
        if (print) {
            System.out.println("      Received Data directory: " + fileBase);
            System.out.println("Central Server Data Directory: "
                    + centralServerDir);
            System.out.println("             Config directory: " + configDir);
        }
    }

    public void run() throws Exception {

        int port = Integer.parseInt((String) serverProps.get("SERVER_PORT"));
        ServerSocket srv = new ServerSocket(port);

        while (true) {
            log("Waiting for connections...");
            Socket socket = srv.accept();
            InetSocketAddress client = (InetSocketAddress) socket
                    .getRemoteSocketAddress();
            log("Connected to client: " + client.getHostName() + " at "
                    + client);
            loadProperties(false);
            String sender = getMhsOfSender(client);
            log("Message is from: " + sender);

            InputStream in = socket.getInputStream();
            byte[] message = null;
            Map<String, String> params = new HashMap<String, String>();
            String flag = "";
            while (true) {
                if (in.available() == 0) {
                    Thread.sleep(100);
                    continue;
                }

                message = getMessage(in);

                if (message.length < 50) {
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
                }
            }
        }
    }

    private void executeAction(String sender, Map<String, String> params)
            throws Exception {
        int action = Integer.parseInt(params.get("-c"));

        if (!commandMap.containsKey(action)) {
            log("No action mapped for command: " + action);
            return;
        }

        String fileList = "";
        for (int i = 0; i < files.size(); i++) {
            fileList += files.get(i);
            if (i != files.size() - 1) {
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
        //
        // Map<String, String> sysEnv = System.getenv();
        // Map<String, String> newEnv = new HashMap<String, String>();
        // for (String key : sysEnv.keySet()) {
        // newEnv.put(key, sysEnv.get(key));
        // }
        // newEnv.put("PATH", "/awips2/python/bin/:"+sysEnv.get("PATH"));
        // newEnv.put("LD_PRELOAD", "/awips2/python/lib/libpython2.7.so");
        // newEnv.put("LD_LIBRARY_PATH", "/awips2/python/lib");
        // String[] envp = new String[newEnv.keySet().size()];
        // int i = 0;
        // for (String key : newEnv.keySet()) {
        // envp[i] = key.trim() + "=" + newEnv.get(key).trim();
        // i++;
        // }
        Process p = null;
        try {
            p = Runtime.getRuntime().exec(cmdArray);
            p.waitFor();
        } finally {
            p.destroy();
        }
    }

    private byte[] getMessage(InputStream in) throws Exception {
        byte[] sizeBytes = new byte[4];
        readBytes(in, sizeBytes);
        int expectedSize = MhsUtil.byteArrayToInt(sizeBytes, 0);
        byte[] message = new byte[expectedSize];
        readBytes(in, message);
        return message;
    }

    private void readBytes(InputStream in, byte[] bytes) throws Exception {
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
            throws Exception {
        String fileFQN = fileBase + fileName + "." + getFileIndex();
        log("Writing file: " + fileFQN);
        FileOutputStream fos = new FileOutputStream(new File(fileFQN));
        fos.write(contents);
        fos.flush();
        fos.close();

        Process p = null;
        try {
            p = Runtime.getRuntime().exec(
                    new String[] { "/bin/chmod", "777", fileFQN });
            p.waitFor();
        } finally {
            p.destroy();
        }
        return fileFQN;
    }

    private String getFileIndex() {
        fileIndex++;
        if (fileIndex == 1000) {
            fileIndex = 0;
        }
        String fileNumber = String.valueOf(fileIndex);
        if (fileNumber.length() == 1) {
            fileNumber = "00" + fileNumber;
        } else if (fileNumber.length() == 2) {
            fileNumber = "0" + fileNumber;
        }
        return fileNumber;
    }

    private void loadRcvHandlerTable() throws Exception {
        BufferedReader in = null;
        try {
            in = new BufferedReader(new FileReader(configDir
                    + "rcv_handler.tbl"));
            String str = null;
            String[] tokens = null;

            while ((str = in.readLine()) != null) {
                String command = "";
                tokens = str.split(" ");
                for (int i = 1; i < tokens.length; i++) {
                    String cmd = tokens[i].trim();
                    if (!cmd.isEmpty()) {
                        if (i != 1) {
                            command += " ";
                        }
                        command += cmd;
                    }
                }
                command = command.trim();

                commandMap.put(Integer.parseInt(tokens[0]), command);
            }
            in.close();
        } finally {
            if (in != null) {
                in.close();
            }
        }
    }

    private String getMhsOfSender(InetSocketAddress address) {

        String hostAddress = address.getAddress().getHostAddress();
        for (Object key : serverProps.keySet()) {
            String value = serverProps.getProperty((String) key);
            if (value.contains(",")) {
                String[] addrs = value.split(",");
                for (String addr : addrs) {
                    if (addr.contains(hostAddress)) {
                        return (String) key;
                    }
                }
            } else {
                if (value.contains(hostAddress)) {
                    return (String) key;
                }
            }
        }
        return null;
    }

    private void log(Object... msg) {
        MhsUtil.logMsg(serverProps.getProperty("LOG_DIR"), "server", msg);
    }
}
