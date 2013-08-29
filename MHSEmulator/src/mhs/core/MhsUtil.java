package mhs.core;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.net.InetAddress;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * Library module for MHS emulator.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ??? ??  ????            bphillip     Initial creation
 * Jul 15, 2013  #2099     dgilling     Use safer exception handling for file I/O.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class MhsUtil {

    public static final SimpleDateFormat logDateFormat = new SimpleDateFormat(
            "yyyyMMdd");

    public static final SimpleDateFormat logMsgFormat = new SimpleDateFormat(
            "yyyy-MM-dd HH:mm:ss :: ");

    public static final String END_TOKEN = "------!!!!END!!!!------";

    public static final File MY_MHS_FILE = new File("/awips2/.myMHS");

    public static final File MSG_ID_FILE = new File("/awips2/.msgCount");

    private MhsUtil() {
        throw new AssertionError();
    }

    public static String getMsgId() throws IOException {
        if (MSG_ID_FILE.createNewFile()) {
            BufferedWriter out = null;
            try {
                out = new BufferedWriter(new FileWriter(MSG_ID_FILE));
                out.write("0");
            } finally {
                if (out != null) {
                    out.close();
                }
            }
        }

        BufferedReader in = null;
        int newMsgNumber;
        try {
            in = new BufferedReader(new FileReader(MSG_ID_FILE));
            String msgId = in.readLine().trim();
            newMsgNumber = Integer.parseInt(msgId) + 1;
        } finally {
            if (in != null) {
                in.close();
            }
        }

        BufferedWriter out = null;
        try {
            out = new BufferedWriter(new FileWriter(MSG_ID_FILE));
            out.write(String.valueOf(newMsgNumber));
        } finally {
            if (out != null) {
                out.close();
            }
        }

        NumberFormat formatter = new DecimalFormat("000000");
        return formatter.format(newMsgNumber);
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
            logFile.createNewFile();

            message += MhsUtil.logMsgFormat.format(new Date());
            for (Object obj : msg) {
                message += obj.toString() + " ";
            }
            message += "\n";

            BufferedWriter out = null;
            try {
                out = new BufferedWriter(new FileWriter(logFile, true));
                out.write(message.trim());
                out.write("\n");
            } finally {
                if (out != null) {
                    out.close();
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

}
