package mhs.core;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.net.InetAddress;
import java.text.SimpleDateFormat;
import java.util.Date;

public class MhsUtil {

    public static final SimpleDateFormat logDateFormat = new SimpleDateFormat(
            "yyyyMMdd");

    public static final SimpleDateFormat logMsgFormat = new SimpleDateFormat(
            "yyyy-MM-dd HH:mm:ss :: ");

    public static final String END_TOKEN = "------!!!!END!!!!------";

    public static final File MY_MHS_FILE = new File(
            "/awips2/.myMHS");

    public static final File MSG_ID_FILE = new File(
            "/awips2/.msgCount");

    public static String getMsgId() throws Exception {
        if (!MSG_ID_FILE.exists()) {
            MSG_ID_FILE.createNewFile();
            BufferedWriter out = new BufferedWriter(new FileWriter(MSG_ID_FILE));
            out.write("0");
            out.close();
        }
        BufferedReader in = null;
        in = new BufferedReader(new FileReader(MSG_ID_FILE));
        String msgId = in.readLine().trim();
        int newMsgNumber = Integer.parseInt(msgId) + 1;
        in.close();
        BufferedWriter out = new BufferedWriter(new FileWriter(MSG_ID_FILE));
        out.write(String.valueOf(newMsgNumber));
        out.close();
        for (int i = msgId.length(); i < 6; i++) {
            msgId = "0" + msgId;
        }

        return msgId;
    }

    public static int byteArrayToInt(byte[] b, int offset) {
        int value = 0;
        for (int i = 0; i < 4; i++) {
            int shift = (4 - 1 - i) * 8;
            value += (b[i + offset] & 0x000000FF) << shift;
        }
        return value;
    }

    public static byte[] intToByteArray(int value) {
        byte[] b = new byte[4];
        for (int i = 0; i < 4; i++) {
            int offset = (b.length - 1 - i) * 8;
            b[i] = (byte) ((value >>> offset) & 0xFF);
        }
        return b;
    }

    public static void logMsg(String logDir, String mode, Object... msg) {
        String message = "";
        File logFile = null;

        try {
            logFile = new File(logDir
                    + InetAddress.getLocalHost().getCanonicalHostName() + "-"
                    + mode + "-" + MhsUtil.logDateFormat.format(new Date()));

            if (logFile != null) {
                if (!logFile.exists()) {
                    logFile.createNewFile();
                }
            }
            message += MhsUtil.logMsgFormat.format(new Date());
            for (Object obj : msg) {
                message += obj.toString() + " ";
            }
            message += "\n";

            BufferedWriter out = new BufferedWriter(new FileWriter(logFile,
                    true));
            out.write(message.trim());
            out.write("\n");
            out.close();

        } catch (Exception e) {
            // ignore
        }
    }

}
