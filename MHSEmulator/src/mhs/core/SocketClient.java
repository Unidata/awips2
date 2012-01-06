package mhs.core;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

public class SocketClient {

    private Map<String, String> parameters = new HashMap<String, String>();

    private Properties serverProps;

    private String myMHS;

    private String propertiesFile;

    public static void main(String[] args) {
        try {
            new SocketClient(parseArgs(args)).msg_send();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private static Map<String, String> parseArgs(String[] args) {
        Map<String, String> params = new HashMap<String, String>();

        for (int i = 0; i < args.length; i++) {
            args[i] = args[i].trim();
            if (args[i].equals("-s")) {
                params.put("-s", args[i + 1]);
            } else if (args[i].equals("-a")) {
                params.put("-a", args[i + 1]);
            } else if (args[i].equals("-i")) {
                params.put("-i", args[i + 1]);
            } else if (args[i].equals("-c")) {
                params.put("-c", args[i + 1]);
            } else if (args[i].equals("-p")) {
                params.put("-p", args[i + 1]);
            } else if (args[i].equals("-e")) {
                params.put("-e", args[i + 1]);
            }
        }

        return params;
    }

    private SocketClient(Map<String, String> params) throws Exception {
        this.getMhsAndProperties();
        this.parameters = params;
        loadProperties();
    }

    private void getMhsAndProperties() throws Exception {
        BufferedReader in = null;
        in = new BufferedReader(new FileReader(MhsUtil.MY_MHS_FILE));
        myMHS = in.readLine().trim();
        propertiesFile = in.readLine().trim();
        in.close();
    }

    private void loadProperties() throws Exception {
        serverProps = new Properties();
        FileInputStream fis = new FileInputStream(new File(propertiesFile));
        serverProps.load(fis);
        fis.close();
    }

    private InetAddress getDest(String recipient) throws Exception {
        log("Getting destination of: " + recipient);
        InetAddress address = null;
        String addrs = serverProps.getProperty(recipient);
        if (addrs.contains(",")) {
            address = InetAddress.getByName(addrs.split(",")[0].trim());
        } else {
            address = InetAddress.getByName(addrs);
        }
        log("Address resolved to: " + address);

        return address;
    }

    private void msg_send() {
        String error = "";
        try {
            loadProperties();
        } catch (Exception e) {
            e.printStackTrace();
            error = e.getLocalizedMessage();
        }
        String[] recipients = getRecipientAddresses();
        String msg_id = "";
        try {
            msg_id = MhsUtil.getMsgId();
        } catch (Exception e1) {
            log(e1.getLocalizedMessage());
        }
        for (String recipient : recipients) {
            try {
                log("Connecting to Site: " + recipient);
                if (serverProps.getProperty(recipient) == null) {
                    log("Unknown MHS: " + recipient);
                    continue;
                }
                InetAddress addr = getDest(recipient);
                int port = Integer.parseInt(serverProps
                        .getProperty("SERVER_PORT"));

                // This constructor will block until the connection succeeds
                Socket socket = new Socket(addr, port);

                try {
                    OutputStream wr = socket.getOutputStream();

                    log("Sending Message...");

                    // send the msg id
                    sendMsg(wr, "-MSGID");
                    sendMsg(wr, msg_id);

                    // Send the subject
                    if (parameters.containsKey("-s")) {
                        sendMsg(wr, "-s");
                        sendMsg(wr, parameters.get("-s"));
                    }

                    // Send the WFO
                    if (parameters.containsKey("-i")) {
                        sendMsg(wr, "-i");
                        sendMsg(wr, parameters.get("-i"));
                    }

                    // Send the priority
                    if (parameters.containsKey("-p")) {
                        sendMsg(wr, "-p");
                        sendMsg(wr, parameters.get("-p"));
                    }

                    // Send the priority
                    if (parameters.containsKey("-c")) {
                        sendMsg(wr, "-c");
                        sendMsg(wr, parameters.get("-c"));
                    }

                    if (parameters.containsKey("-e")) {
                        String[] files = getEnclosures();
                        for (String file : files) {
                            sendMsg(wr, "-file");
                            sendFile(wr, file);
                        }
                    }

                    sendMsg(wr, MhsUtil.END_TOKEN);
                    log("Message Sent!");
                    wr.close();
                    socket.close();
                    log("Disconnecting from Site: " + recipient);
                } catch (IOException e) {
                    error = e.getLocalizedMessage();
                }

            } catch (Exception e) {
                error = e.getLocalizedMessage();
            }
        }
        if (error.isEmpty()) {
            try {
                System.out.println(myMHS + "-" + msg_id);
            } catch (Exception e) {
                System.out.println("Unable to get new message id number!");
                e.printStackTrace();
            }
        } else {
            System.out.println("MHS ERROR: " + error);
            System.exit(1);
        }

    }

    private String[] getRecipientAddresses() {
        String[] retVal = new String[] {};
        if (parameters.containsKey("-a")) {
            retVal = parameters.get("-a").split(",");
            for (int i = 0; i < retVal.length; i++) {
                retVal[i] = retVal[i].toUpperCase().trim();
                if (retVal[i].contains("TNCF")) {
                    retVal[i] = "TNCF";
                }
            }
        }
        return retVal;
    }

    private String[] getEnclosures() {
        String[] retVal = new String[] {};
        if (parameters.containsKey("-e")) {
            retVal = parameters.get("-e").split(",");
            for (int i = 0; i < retVal.length; i++) {
                retVal[i] = retVal[i].trim();
            }
        }
        return retVal;
    }

    private void sendFile(OutputStream wr, String file) throws Exception {
        byte[] fileBytes = readFile(file);
        wr.write(MhsUtil.intToByteArray(fileBytes.length));
        wr.write(fileBytes);
        wr.flush();
    }

    private void sendMsg(OutputStream wr, String msg) throws Exception {
        wr.write(MhsUtil.intToByteArray(msg.getBytes().length));
        wr.write(msg.getBytes());
        wr.flush();
    }

    private byte[] readFile(String fileName) throws Exception {
        File theFile = new File(fileName);
        byte[] fileContents = new byte[(int) theFile.length()];
        FileInputStream fis = new FileInputStream(theFile);
        BufferedInputStream bis = new BufferedInputStream(fis);
        bis.read(fileContents, 0, fileContents.length);
        return fileContents;
    }

    private void log(Object... msg) {
        MhsUtil.logMsg(serverProps.getProperty("LOG_DIR"), "client", msg);
    }
}
